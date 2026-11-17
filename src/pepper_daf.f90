!> DAF (Double precision Array File) files: opening and closing, reading and writing records, arrays
!> and summaries, searching the arrays, comments, and the transfer format.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
!>
!>## DAFAH (DAF, assign handles)
!>
!> PEPPER: SPICELIB's umbrella routine DAFAH is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_dafah. Its documentation follows.
!>
!>### Abstract
!>
!>     Assign handles to DAFs as they are opened.
!>
!>### Detailed Input
!>
!>     FNAME    on input, is the name of a DAF to be opened, or
!>              the name of a DAF about which some information
!>              (handle, logical unit) is requested.
!>
!>     FTYPE    on input, is a code for the type of data that is
!>              contained in the DAF file. This code has no meaning or
!>              interpretation at the level of the DAF file
!>              architecture, but is provided as a convenience for
!>              higher level software. The maximum length for the file
!>              type is four (4) characters. If the input string is
!>              longer than four characters, the first nonblank
!>              character and its three, or fewer, immediate nonblank
!>              successors will be used as the file type. The file
!>              type may not contain nonprinting characters, and it IS
!>              case sensitive.
!>
!>              NAIF has reserved for its own use file types
!>              consisting of the upper case letters (A-Z) and the
!>              digits 0-9. NAIF recommends lower case or mixed case
!>              file types be used by all others in order to avoid
!>              any conflicts with NAIF file types.
!>
!>     ND       on input, is the number of double precision components
!>              in each array summary of a new file.
!>
!>     NI       on input, is the number of integer components in each
!>              array summary in a new file.
!>
!>     IFNAME   is the internal file name for a DAF to be created.
!>
!>     RESV     is the number of records to be reserved in a DAF
!>              to be created.
!>
!>     HANDLE   on input, is the handle of a DAF about which some
!>              information (file name, logical unit) is requested,
!>              or the handle of a DAF to be closed.
!>
!>     UNIT     on input, is the logical unit connected to a DAF
!>              about which some information (file name, handle) is
!>              requested.
!>
!>     ACCESS   is the type of access a DAF is open for, that is,
!>              either reading or writing. The values of ACCESS
!>              may be
!>
!>                 'READ'
!>                 'WRITE'
!>
!>              Leading and trailing blanks are ignored, and case
!>              is not significant.
!>
!>### Detailed Output
!>
!>     FNAME    on output, is the name of a DAF for which
!>              the corresponding handle or logical unit has been
!>              supplied.
!>
!>     ND       on output, is the number of double precision
!>              components in each array summary of an existing file.
!>
!>     NI       on output, is the number of integer components in
!>              each array summary in an existing file.
!>
!>     HANDLE   on output, is the handle of a DAF for which
!>              the corresponding file name or logical unit has been
!>              supplied.
!>
!>     UNIT     on output, is the logical unit connected to a DAF
!>              for which the corresponding file name or handle has
!>              been supplied.
!>
!>     FHSET    is a SPICE set containing the handles of the
!>              currently open DAFs.
!>
!>### Parameters
!>
!>     RECL     is the record length of a DAF. Each record
!>              must be large enough to hold 128 double
!>              precision numbers or 1000 characters, whichever
!>              is greater. The units in which the record length
!>              must be specified vary from environment to
!>              environment. For example, VAX Fortran requires
!>              record lengths to be specified in longwords,
!>              where two longwords equal one double precision
!>              number. See the include file 'zzddhman.inc' for
!>              details.
!>
!>     FTSIZE   is the size of the file table maintained internally
!>              by DAFAH. In effect, FTSIZE is the maximum number
!>              of DAFs that the DAF routines allow to be open
!>              simultaneously. See the include file 'zzddhman.inc'
!>              for details.
!>
!>     FILEN    is the maximum filename length. See the include file
!>              'zzddhman.inc' for details.
!>
!>     INTEOC   is the ASCII decimal integer code of the character
!>              recognized by SPICE as representing the end of the
!>              comment data in the reserved record area.
!>
!>### Exceptions
!>
!>     1)  If DAFAH is called directly, the error SPICE(BOGUSENTRY)
!>         is signaled.
!>
!>     2)  See entry points DAFOPR, DAFOPW, DAFONW, DAFOPN, DAFCLS,
!>         DAFHSF, DAFHLU, DAFLUH, DAFHFN, DAFNFH, DAFHOF, and DAFSIH for
!>         exceptions specific to those entry points.
!>
!>### Files
!>
!>     All DAFs opened by this routine are specified by name.
!>
!>### Particulars
!>
!>     DAFAH serves as an umbrella, allowing data to be shared by its
!>     entry points:
!>
!>        DAFOPR         Open for read.
!>        DAFOPW         Open for write.
!>        DAFONW         Open new.
!>        DAFOPN         Open new. (Obsolete, use DAFONW )
!>
!>        DAFCLS         Close.
!>
!>        DAFHSF         Handle to summary format.
!>
!>        DAFHLU         Handle to logical unit.
!>        DAFLUH         Logical to handle.
!>
!>        DAFHFN         Handle to name.
!>        DAFFNH         File name to handle.
!>
!>        DAFHOF         Handles of open files.
!>        DAFSIH         Signal invalid handles.
!>
!>     Before a DAF can be used, it must be opened. Entry points
!>     DAFOPR and DAFOPW provide the only means for opening an
!>     existing DAF.
!>
!>     Several files may be opened for use simultaneously. (This makes
!>     it convenient to combine data from several files to produce a
!>     single result.) As each DAF is opened, it is assigned a file
!>     handle, which is used to keep track of the file internally, and
!>     which is used by the calling program to refer to the file in all
!>     subsequent calls to DAF routines.
!>
!>     DAFs may be opened for two kinds of access: read, and write.
!>     Files opened for read access may not be changed in any way. Files
!>     opened for write access may be both read and written.
!>
!>     DAFONW is used to open a new DAF file. This routine extends the
!>     functionality of DAFOPN by providing a mechanism for associating a
!>     type with the data in the DAF file. The use of this entry over
!>     DAFOPN is highly recommended.
!>
!>     Since the only reason for creating a new file is to write
!>     something in it, all new files are opened for write access.
!>
!>     Entry point DAFOPN, for opening a new DAF file, has been rendered
!>     obsolete by the new entry point DAFONW. The entry point DAFOPN
!>     will continue to be supported for purposes of backward
!>     compatibility, but its use in new software development is
!>     discouraged.
!>
!>     Entry point DAFCLS provides the only official means of closing
!>     a DAF that is currently open. Closing a DAF any other way (for
!>     example, by determining its logical unit and using the Fortran
!>     CLOSE statement directly) may affect your calling program in
!>     mysterious ways.
!>
!>     Entry point DAFHSF allows you to determine the summary format
!>     of any DAF that is currently open, without calling DAFRFR to
!>     re-read the file record.
!>
!>     Entry point DAFHOF allows you to determine which DAFs are open
!>     at any time. In particular, you can use DAFHOF to determine
!>     whether any file handle points to an open DAF.
!>
!>     Entry point DAFSIH signals errors when it is supplied with invalid
!>     handles, so it serves to centralize error handling associated
!>     with invalid handles.
!>
!>     The remaining entry points exist mainly to translate between
!>     alternative representations of DAFs. There are three ways to
!>     identify any open DAF: by name, by handle, and by logical
!>     unit. Given any one of these, you may use these entry points to
!>     find the other two.
!>
!>### Examples
!>
!>     See entry points DAFOPR, DAFOPW, DAFONW, DAFOPN, DAFCLS, DAFHSF,
!>     DAFHLU, DAFLUH, DAFHFN, DAFNFH, DAFHOF, and DAFSIH for examples
!>     specific to those entry points.
!>
!>### Restrictions
!>
!>     1)  The value of parameter RECL may need to be changed when DAFAH
!>         and its entry points are ported to a new environment (CPU and
!>         compiler).
!>
!>     2)  An integer overflow may occur if the number of files opened
!>         by a single program exceeds the maximum number that can be
!>         stored in an integer variable.
!>
!>### Literature References
!>
!>     [1]  "Sun FORTRAN Programmer's Guide", Sun Microsystems, Rev. A, 6
!>          May 1988.
!>
!>     [2]  "Microsoft Fortran Optimizing Compiler User's Guide",
!>          Microsoft Corporation, 1987.
!>
!>     [3]  "Lahey F77L EM/32 Programmers Reference Manual", version 4.0,
!>          p 144.
!>
!>     [4]  "Language Systems FORTRAN Reference Manual", Language Systems
!>          Corporation, p 12-7.
!>
!>     [5]  "FORTRAN/9000 Reference HP 9000 Series 700 Computers", 1st
!>          Edition, June 1991, Hewlett Packard Company, page 5-110.
!>
!>### Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>     K.R. Gehringer     (JPL)
!>     J.M. Lynch         (JPL)
!>     J.E. McLean        (JPL)
!>     H.A. Neilan        (JPL)
!>     M.J. Spencer       (JPL)
!>     W.L. Taber         (JPL)
!>     F.S. Turner        (JPL)
!>     I.M. Underwood     (JPL)
!>     E.D. Wright        (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 9.0.2, 25-NOV-2021 (JDR)
!>
!>        Edited the header of DAFAH umbrella routine and all its entry
!>        points to comply with NAIF standard.
!>
!>        Added DAFOPR, DAFOPW, DAFCLS and DAFHSF header examples.
!>
!>        Corrected spelling mistakes.
!>
!>-    SPICELIB Version 9.0.1, 10-OCT-2012 (EDW)
!>
!>        Edited DAFOPN $Abstract section to use "Deprecated" keyword
!>        and state replacement routine.
!>
!>        Corrected ordering of all header sections.
!>
!>        Added a functional code example to the $Examples section
!>        in DAFOPN and DAFCLS.
!>
!>        Removed the obsolete Reference citation to "NAIF
!>        Document 167.0."
!>
!>-    SPICELIB Version 9.0.0, 09-NOV-2006 (NJB)
!>
!>        Updated the entry point DAFONW so that a non-empty reserved
!>        record area will also be a valid empty comment area.  DAFONW
!>        now writes a EOC character to the first byte of the second
!>        record when the input number of reserved records NRESV is
!>        greater than zero.
!>
!>-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
!>
!>        Updated the following entry points in response to changes
!>        to the handle manager interfaces:
!>
!>           DAFCLS
!>           DAFOPR
!>           DAFOPW
!>           DAFONW
!>           DAFOPN
!>
!>        See the $Revisions section for details.
!>
!>        Minor bug fix to DAFFNH. An error was signaled but the
!>        intended call to CHKOUT and RETURN statement were omitted.
!>
!>-    SPICELIB Version 8.0.0, 14-NOV-2000 (FST)
!>
!>        Cleaned up entry point headers by removing duplicate
!>        entries from the $Revisions section where appropriate.
!>
!>        Integrated the new handle manager code into this module.
!>        The number of DAFs the system can load is now 1000,
!>        and some supported environments can read non-native
!>        binary DAFs. See the Convert User's Guide for details.
!>
!>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
!>
!>        The environment lines were expanded so that the supported
!>        environments are now explicitly given. New
!>        environments are WIN-NT
!>
!>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
!>
!>        CSPICE environments were added. Some typos were corrected.
!>
!>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
!>
!>        The environment lines were expanded so that the supported
!>        environments are now explicitly given. New
!>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!>
!>-    SPICELIB Version 7.0.1, 22-MAR-1999 (WLT)
!>
!>        The environment lines were expanded so that the supported
!>        environments are now explicitly given. Previously,
!>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!>        by the environment label SUN.
!>
!>-    SPICELIB Version 7.0.0, 22-MAR-1999 (FST)
!>
!>        To accommodate the DAF FTP validation check, the following
!>        entry points were modified:
!>
!>           DAFOPR, DAFOPW, DAFONW, DAFOPN.
!>
!>        See their headers and code for the details of the changes.
!>
!>-    SPICELIB Version 6.0.0, 05-APR-1998 (NJB)
!>
!>        Added references to the PC-LINUX environment.
!>
!>-    SPICELIB Version 5.1.0, 08-MAR-1996 (KRG)
!>
!>        The Following entry points have been modified: DAFONW and
!>        DAFOPN.
!>
!>        The modifications support the notion of a DAF comment area,
!>        and involve writing NULL filled reserved records when the
!>        number of reserved records is greater than zero (0).
!>
!>        Some nested IF...THEN...ELSE IF...THEN...END IF constructs
!>        were expanded to be independent IF...THEN...END IF tests.
!>        The tests were for IOSTAT errors on cascading write statements
!>        nested in the IF...ELSE IF... statements, and this was
!>        confusing. These tests were restructured so that IOSTAT is
!>        tested after each write statement which is equivalent to the
!>        original intent and easier to read.
!>
!>-    SPICELIB Version 5.0.0, 27-SEP-1993 (KRG)
!>
!>        The following entry points have had code modifications:
!>        DAFOPR, DAFOPW and DAFOPN.
!>
!>        A new entry point has been added: DAFONW.
!>
!>        The modifications are to allow a type to be associated with a
!>        DAF file.
!>
!>        A new parameter has been added to this subroutine's parameter
!>        list, FTYPE, so that type information may be passed to the
!>        entry point DAFONW. Two new variables were added to the
!>        routine as well, TARCH and TTYPE, which provide temporary
!>        storage for the file architecture and type.
!>
!>        Several new parameters have been added to the declarations for
!>        this routine:
!>
!>           ARCLEN   The length of a file architecture.
!>
!>           MAXPC    The maximum decimal value for the range of
!>                    printable characters.
!>
!>           MINPC    The minimum decimal value for the range of
!>                    printable characters.
!>
!>           TYPLEN   The length of a file type.
!>
!>        See the individual entry points for detailed descriptions of
!>        their modifications.
!>
!>        Removed the variables MINHAN and NIL, as they were not used in
!>        any of the entry points, yet they had values assigned to them
!>        through DATA statements.
!>
!>        Made all occurrences of error message formatting of filenames
!>        consistent. All filenames will be single quoted in the output
!>        error message.
!>
!>-    SPICELIB Version 4.0.0, 25-FEB-1993 (JML)
!>
!>        In the entry points DAFOPR, DAFOPW, and DAFFNH, the INQUIRE
!>        statement that checks if the file is already open now also
!>        checks that the file exists.
!>
!>        IOSTAT is now checked after all INQUIRE statements.
!>
!>        A new variable LUN is used in DAFOPR, DAFOPW, and DAFOPN
!>        for the logical unit number returned by GETLUN.
!>
!>        The IF-THEN statements in DAFOPR and DAFOPW were reorganized
!>        to make the routines more readable.
!>
!>        In DAFOPR and DAFOPW, a long error message was added for the
!>        case when the NAIF/DAF id word was not recognized. Also, the
!>        file is closed when this error is signaled.
!>
!>        In DAFOPR and DAFOPW, IOSTAT is now checked after the file
!>        record is read.
!>
!>        In DAFOPR, DAFOPW, DAFOPN, and DAFFNH, the file name is
!>        checked to see if it is blank.
!>
!>        In DAFOPR, DAFOPW, DAFOPN, and DAFFNH, the file name passed
!>        to the FORTRAN OPEN and INQUIRE statements has been chopped
!>        at the last non-blank character.
!>
!>        A minor error in the $Particulars section of the header of
!>        DAFCLS was corrected. It formerly stated that a file could be
!>        open more than once for read or write access instead of just
!>        read access.
!>
!>-    SPICELIB Version 3.2.0, 06-OCT-1992 (HAN)
!>
!>        Module was updated to include the record length and source
!>        for the Hewlett Packard UX 9000/750 environment. Moved FILEN
!>        to the $Declarations section, and corrected $Revisions section
!>        to include the last code change description, 3.1.0.
!>
!>-    SPICELIB Version 3.1.1, 10-MAR-1992 (WLT)
!>
!>        Comment section for permuted index source lines was added
!>        following the header.
!>
!>-    SPICELIB Version 3.1.0, 13-NOV-1991 (MJS)
!>
!>        Module was updated to operate in the Lahey F77 EM/32
!>        PC environment.
!>
!>-    SPICELIB Version 3.0.0, 03-SEP-1991 (NJB) (WLT)
!>
!>        DAFAH and its entry points were modified to permit multiple
!>        DAFs to be open for writing at the same time. Also, the
!>        entry points DAFHOF and DAFSIH were added.
!>
!>-    SPICELIB Version 2.0.0, 25-MAR-1991 (JEM) (MJS)
!>
!>        The variable MINHAN was initialized to zero and the variable
!>        NEXT was saved. DAFOPW now accepts the ID word 'NAIF/NIP'
!>        as well 'NAIF/DAF'. Spelling mistakes were corrected.
!>
!>-    SPICELIB Version 1.1.0, 05-NOV-1990 (HAN)
!>
!>        The parameter FTSIZE was increased from 4 to 20.
!>
!>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!>
!>        Literature references added to the header.
!>
!>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!>
!>### Revisions
!>
!>-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
!>
!>        The entry point ZZDDHCLS in the handle manager (ZZDDHMAN)
!>        had its argument list augmented to allow files to be
!>        deleted on close. This allows the removal of a series
!>        of "raw" CLOSE statements in a few of the entry points
!>        of this routine.
!>
!>-    SPICELIB Version 8.0.0, 14-NOV-2000 (FST)
!>
!>        The DAF system now utilizes the handle manager umbrella
!>        (ZZDDHMAN) and its entry points to provide most of the
!>        handle and logical unit based operations that DAFAH
!>        previously managed.
!>
!>        FTSIZE Files with UTSIZE Units:
!>
!>        In previous versions of the DAF system all files opened
!>        through the DAFAH entry points were connected to logical
!>        units. In contrast, the handle manager umbrella entry
!>        points allow FTSIZE files to be loaded (opened), while
!>        only utilizing UTSIZE (less than FTSIZE, see the include
!>        file 'zzddhman.inc') logical units.  The entry points in
!>        the handle manager automatically connect and disconnect
!>        loaded files from their logical units as new files are
!>        loaded and accessed.
!>
!>        Previously, one could buffer a logical unit associated
!>        with a particular handle and access the file directly
!>        with Fortran I/O statements.  To preserve this capability
!>        invoking DAFHLU locks a handle to its assigned logical
!>        unit, until that lock is removed (see ZZDDHUNL, an entry
!>        point in ZZDDHMAN) or the file is closed.  See the
!>        $Revisions section in the DAFHLU entry point for details.
!>
!>        Another consequence of the utilization of the handle
!>        manager code is that the process of connecting a file
!>        name to a HANDLE may require performing up to FTSIZE
!>        INQUIRE statements.  This is necessary to insure that
!>        different names referring to the same file return the
!>        same handle.  This was the case previously with the DAF
!>        system since an INQUIRE on a different, but equivalent,
!>        file name would produce the same logical unit.
!>
!>        FTP Error Detection:
!>
!>        The FTP error detection software is now integrated into
!>        the handle manager umbrella entry points, and as such
!>        is no longer present in DAFAH.
!>
!>        Non-Native Files:
!>
!>        In addition to expanding the number of loaded files the
!>        DAF system supports, the handle manager also detects and
!>        tracks binary file formats.  This allows a layer of
!>        private code that has been inserted between DAF routines
!>        and the Fortran I/O statements to provide translation
!>        services for DAF.  Some environments are now endowed with
!>        the ability to read files created with certain non-native
!>        binary file formats.  See the Convert User's Guide for
!>        details.
!>
!>-    SPICELIB Version 7.0.0, 22-MAR-1999 (FST)
!>
!>        Binary File Format Identification:
!>
!>        The file record now contains an 8 character string that
!>        identifies the binary file format utilized by DAFs.
!>        The purpose of this string's inclusion in the file record
!>        is preparatory in nature, to accelerate the migration to
!>        files that support the runtime translation update that
!>        is scheduled.
!>
!>        FTP Validation:
!>
!>        The DAF system now employs a validation scheme to assist
!>        users in detecting DAFs potentially corrupted via ASCII mode
!>        FTP transfers.  A string that contains sequences of
!>        characters commonly corrupted by improper FTP transfers is
!>        inserted into the unused portion of the file record. When any
!>        DAFAH entry point attempts to open a file, this string is
!>        located and examined.  If the string indicates the file is
!>        corrupted, the entry point signals an error.
!>
!>           Detection Scheme Implementation:
!>
!>           When a new DAF is created, the entry points DAFONW and
!>           DAFOPN(obsolete) retrieve the FTP validation string from
!>           the defining routine (ZZFTPSTR) and insert it into the
!>           tail of the file record.  A diagram illustrating the new
!>           file record for 32-bit environments with single byte
!>           characters follows:
!>
!>              +=============+
!>              | File Record |
!>              |    Data     |
!>              +=============+
!>                     |
!>               +=====|===+==========================+===+========+
!>               |     |   |    603 bytes of nulls    | | | nulls  |
!>               +=========+==========================+=|=+========+
!>           Byte 1                                     |         1024
!>                                                 +============+
!>                                                 | FTP        |
!>                                                 | Validation |
!>                                                 | String     |
!>                                                 +============+
!>
!>           As can be seen above, the file record is now null padded,
!>           which was not the case previously.
!>
!>           When an existing DAF is opened, the entry points DAFOPR
!>           and DAFOPW attempt to verify that the validation string is
!>           intact.  This is accomplished by reading the file
!>           record into a character string, and then passing the last
!>           half of this string into the validation subroutine
!>           ZZFTPCHK.  Only sending the latter half of the file record
!>           into ZZFTPCHK is done to prevent other portions of the file
!>           record from confusing the validation process.  The following
!>           three abnormal situations may arise during validation:
!>
!>              (1) Older DAFs without the FTP validation string are
!>                  not validated.  As far as the DAF open routines
!>                  are concerned such files are valid by default. The
!>                  only notable exception is that the garbage that
!>                  resides in the unused portion of the file record may
!>                  confuse ZZFTPCHK into thinking the validation
!>                  string is present.  (The probability of this event
!>                  is minimal and noted only for completeness.)
!>
!>              (2) Files with an older version of the validation
!>                  string are examined for errors supported by the
!>                  contemporaneous version of the Toolkit.
!>
!>              (3) Files with a newer version of the validation
!>                  string are examined for errors supported by the
!>                  current version of the Toolkit.
!>
!>           Updates to the FTP Validation String:
!>
!>           In the event that it becomes necessary to add additional
!>           test characters to the validation string, refer to
!>           ZZFTPSTR for the proper procedure.  The instructions
!>           provided there ensure that the above behavior is properly
!>           adhered to by the modifications.
!>
!>           FTP Validation Issues in Code Portability:
!>
!>           The scheme as currently implemented will function
!>           properly in any computing environment whose character data
!>           conforms to the single byte ASCII standards with a word
!>           size that is between 32 and 64 bits inclusive.  Refer to
!>           the above diagram that displays the new DAF file record
!>           and the following discussion for details.
!>
!>           Since the DAF file record block contains integer data,
!>           it may expand if the word size increases above the
!>           currently supported 32 bits.  However, the FTP validation
!>           string is extracted by reading in 1000 bytes of character
!>           data and examining bytes 500-1000. (See the parameters
!>           FTPBLK and FTPSTR if you need to alter these numbers).
!>           So as long as the alteration in word size does not cause
!>           the FTP string information to shift out of bytes 500-1000
!>           in the file record, the existing code will function
!>           properly.
!>
!>-    SPICELIB Version 3.2.0, 6-OCT-1992 (HAN)
!>
!>        The code was also reformatted so that a utility program can
!>        create the source file for a specific environment given a
!>        master source file.
!>
!>-    SPICELIB Version 3.0.0, 03-SEP-1991 (NJB) (WLT)
!>
!>        DAFAH and the entry point DAFOPW were modified to permit
!>        multiple DAFs to be open for writing at the same time.
!>        Also, the entry points DAFHOF and DAFSIH were added.  DAFHOF
!>        returns a set containing the handles of currently open DAFs.
!>        To accommodate the addition of DAFHOF, the argument FHSET
!>        was added to DAFAH's argument list, and local declarations
!>        for DAFHOF were added to DAFAH's declaration section.  DAFSIH
!>        signals an error if the file indicated by the handle is not
!>        open for the specified type of access.
!>
!>-    SPICELIB Version 2.0.0, 25-MAR-1991 (JEM) (MJS)
!>
!>        The entry point DAFOPW accepted only 'NAIF/DAF' as a valid
!>        ID word. It now accepts 'NAIF/NIP' as well for
!>        backwards compatibility. The entry point DAFOPR did not need
!>        this fix because it already accepts both ID words.
!>
!>-    SPICELIB Version 1.1.0,  5-NOV-1990 (HAN)
!>
!>        The parameter FTSIZE was increased from 4 to 20. The number
!>        4 was chosen for testing purposes and was not removed.
!>
!>### Implementation notes
!>
!>     As each file is opened, it is assigned a handle, and the
!>     internal file name is stored for comparison with other files.
!>     All names in the file table begin with FT.
!>
!>        HAN      Handle
!>        LNK      Number of links
!>        ND,
!>        NI       Summary format
!>
!>     The columns are stored in no particular order. New files are
!>     added to the end of the list; the list is repacked whenever a
!>     file is removed from the list.
!>
!>     NFT is the number of files currently opened: this may not be
!>     greater than FTSIZE. FINDEX refers to a file of interest within
!>     the table.
!>
!>     NEXT is incremented each time a file is opened to become the
!>     next file handle assigned.
!>
!>## DAFRWD (DAF, read, write double precision)
!>
!> PEPPER: SPICELIB's umbrella routine DAFRWD is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_dafrwd. Its documentation follows.
!>
!>### Abstract
!>
!>     Read, write, and rewrite double precision records to and
!>     from DAFs.
!>
!>### Detailed Input
!>
!>     HANDLE   is the handle associated with a DAF.
!>
!>     RECNO    is the record number of a double precision record
!>              within a DAF to be read or written.
!>
!>     BEGIN    is the first in word in a double precision record
!>              to be read.
!>
!>     END      is the last in word in a double precision record
!>              to be read.
!>
!>     DREC     contains a single double precision record, to be
!>              written to the specified DAF.
!>
!>### Detailed Output
!>
!>     DATA     contains a portion of a single double precision
!>              record, read from the specified DAF.
!>
!>     FOUND    is .TRUE. when the specified record is found, and is
!>              .FALSE. otherwise.
!>
!>     READS,
!>     REQS     are the number of physical reads and the number
!>              of requests processed by DAFRDR during the current
!>              execution of the calling program.
!>
!>### Parameters
!>
!>     RBSIZE   is the size of the record buffer maintained by
!>              DAFRWD. In effect, RBSIZE is the maximum number
!>              of records that can be stored (buffered) at any
!>              one time. Higher values of RBSIZE reduce the
!>              amount of time spent reading from disk at the
!>              cost of increasing the amount of space required
!>              by the calling program. The optimal value of
!>              RBSIZE may differ from environment to environment,
!>              and may even vary from application to application.
!>
!>### Exceptions
!>
!>     1)  If DAFRWD is called directly, the error SPICE(BOGUSENTRY)
!>         is signaled.
!>
!>     2)  See entry points DAFGDR, DAFGSR, DAFRDR, DAFWDR, and DAFNRR
!>         for exceptions specific to those entry points.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     DAFRWD serves as an umbrella, allowing data to be shared by its
!>     entry points:
!>
!>        DAFGDR         Read double precision record.
!>
!>        DAFGSR         Read summary/descriptor record.
!>
!>        DAFRDR         Read double precision record. (Obsolete, use
!>                       DAFGDR)
!>
!>        DAFWDR         Write double precision record.
!>
!>        DAFNRR         Number of reads, requests.
!>
!>     DAFGDR, DAFGSR, and DAFWDR are the only approved means for
!>     reading and writing double precision records to and from DAFs.
!>     DAFRDR continues to function, but only on files of the native
!>     binary format. They keep track of which records have been read
!>     most recently, and of which records have been requested most
!>     often, in order to minimize the amount of time spent actually
!>     reading from external storage.
!>
!>     DAFNRR may be used at any time during the execution of a
!>     program to determine the number of requests that have been
!>     processed, and the number of actual read operations needed
!>     to fulfill those requests. Ideally, the ratio of reads to
!>     requests should approach zero. In the worst case, the ratio
!>     approaches one. The ratio is related to the size of the
!>     record buffer, which controlled by parameter RBSIZE. The
!>     results returned by DAFNRR may be used to determine the
!>     optimal value of RBSIZE empirically.
!>
!>     All data records in a DAF can be treated as an undifferentiated
!>     collection of double precision numbers. Summary records must
!>     be read using the DAFGSR interface, but their contents are
!>     properly buffered in a single buffer with the data records.
!>     No special buffers are required for each new data type, or to
!>     keep summary records separate from data records.
!>
!>### Examples
!>
!>     See entry points DAFGDR, DAFGSR, DAFRDR, DAFWDR, and DAFNRR
!>     for examples specific to those entry points.
!>
!>### Restrictions
!>
!>     1)  An integer overflow may occur if the number of requests
!>         by a single program exceeds the maximum number that can
!>         be stored in an integer variable.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>     J.M. Lynch         (JPL)
!>     H.A. Neilan        (JPL)
!>     W.L. Taber         (JPL)
!>     F.S. Turner        (JPL)
!>     I.M. Underwood     (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 2.1.0, 12-AUG-2021 (NJB) (JDR)
!>
!>        Bug fixes: now NREAD is not incremented once it reaches
!>        INTMAX(), if it does.
!>
!>        Re-ordered header sections in all entry points. Corrected
!>        typos in comments.
!>
!>        Edited the header of the DAFRWD umbrella routine and all its
!>        entry entry points to comply with NAIF standard.
!>
!>        Removed DAF required reading from $Literature_References.
!>
!>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!>
!>        Added DAFGDR and DAFGSR entry points to allow read access
!>        to DAFs utilizing non-native, but supported, binary file
!>        formats.
!>
!>        DAFRDR was phased into obsolescence.
!>
!>        The umbrella no longer suffers from integer overflow if
!>        a sufficient number of successful read requests are made.
!>
!>        DAFWDR no longer uses DAFHLU to retrieve a logical unit
!>        for HANDLE. This call has been replaced with the handle
!>        manager interface, which does not lock handles to their
!>        logical units.
!>
!>-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
!>
!>        The loop in DAFRDR that moved buffered d.p.s into the output
!>        array DATA was modified to use the routine MOVED.
!>
!>-    SPICELIB Version 1.2.0, 01-AUG-1997 (NJB)
!>
!>        Unnecessary CHKIN and CHKOUT calls were removed from entry
!>        point DAFRDR.
!>
!>-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
!>
!>        1) In DAFRDR, the found flag is now set to false if the
!>           call to DAFHLU fails.
!>
!>        2) In the example code fragment in DAFRDR and DAFWDR, the
!>           calling sequence to MOVED was corrected.
!>
!>        3) In DAFRDR a variable name was changed.
!>
!>        4) In DAFNRR a cut and paste error in the header was fixed.
!>
!>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!>
!>        Comment section for permuted index source lines was added
!>        following the header.
!>
!>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!>
!>        Literature references added to the header.
!>
!>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!>
!>### Revisions
!>
!>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!>
!>        Updated this umbrella and its entry points in preparation
!>        for DAF's utilization of the handle manager. DAFRDR is
!>        obsolete, and will now signal errors when used to read
!>        records from DAFs using non-native, binary file formats.
!>
!>        Two new entry points were added: DAFGDR and DAFGDR. These
!>        are the translation-aware 'get data record' and 'get
!>        summary record' routines that all new software developed
!>        should utilize.
!>
!>-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
!>
!>        The loop in DAFRDR that moved buffered d.p.s into the output
!>        array DATA was modified to use the routine MOVED.
!>
!>-    SPICELIB Version 1.2.0, 01-AUG-1997 (NJB)
!>
!>        Unnecessary CHKIN and CHKOUT calls were removed from entry
!>        point DAFRDR.
!>
!>### Implementation notes
!>
!>     As double precision records are processed, they are stored in a
!>     record buffer. (File and character records are not buffered.)
!>     The user controls the number of records that may be stored at
!>     any one time by setting the value of the parameter RBSIZE before
!>     compiling the routine.
!>
!>     The record buffer contains one entry for each record that has
!>     been read.
!>
!>        +----------+----------+----------+----------+
!>        | File       Record     Request    Contents |
!>        | Handle     Number     Number              |
!>        +----------+----------+----------+----------+
!>        | INT        INT        INT        DP(128)  |
!>        +----------+----------+----------+----------+
!>
!>     The request number is a counter that is incremented every time
!>     a record is requested. When all the slots in the record buffer are
!>     full, the least recently requested record (the one with the lowest
!>     request number) is replaced by the new record.
!>
!>     In addition, a separate counter is used to keep track of the
!>     number of actual file reads performed. It is possible to tune
!>     the entire package by checking the read/request ratio for
!>     any specific buffer configuration.
!>
!>     Note also that whenever a write operation fails, the affected
!>     buffers (if any) should NOT be updated.
!>
!>## DAFANA (DAF, add new array)
!>
!> PEPPER: SPICELIB's umbrella routine DAFANA is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_dafana. Its documentation follows.
!>
!>### Abstract
!>
!>     Add a new array to an existing DAF.
!>
!>### Detailed Input
!>
!>     HANDLE   is the handle of a DAF opened for write access
!>              by a previous call to DAFOPW or DAFOPN.
!>
!>     SUM      is the summary for the array being added.
!>
!>     NAME     is the name of the array being added.
!>
!>     DATA     contains all or part of the data in the array.
!>
!>     N        is the number of elements in DATA.
!>
!>### Detailed Output
!>
!>     None.
!>
!>### Parameters
!>
!>     TBSIZE   is the size of the file table maintained internally
!>              by DAFANA,  TBSIZE is the maximum number of DAFs
!>              that can be in use simultaneously by this routine.
!>
!>### Exceptions
!>
!>     1)  If DAFANA is called directly, the error SPICE(BOGUSENTRY)
!>         is signaled.
!>
!>     2)  See entry points DAFBNA, DAFADA, DAFENA, and DAFCAD
!>         for exceptions specific to those entry points.
!>
!>### Files
!>
!>     See argument HANDLE, above.
!>
!>### Particulars
!>
!>     DAFANA serves as an umbrella, allowing data to be shared by its
!>     entry points:
!>
!>        DAFBNA         Begin new array.
!>        DAFADA         Add data to array.
!>        DAFCAD         Continue adding data.
!>        DAFENA         End new array.
!>
!>     The main function of these entry points is to simplify the
!>     addition of new arrays to existing DAFs.
!>
!>     An application can add data to a single DAF, or to multiple DAFs,
!>     simultaneously. In the case of writing to a single DAF, the
!>     creation of a new array requires four steps:
!>
!>        1) Open a DAF for write access, using either DAFOPW
!>           (if the file already exists) or DAFOPN (if it does not).
!>
!>              CALL DAFOPW ( FNAME, HANDLE )
!>
!>        2) Begin the new DAF by calling DAFBNA,
!>
!>              CALL DAFBNA ( HANDLE, SUM, NAME )
!>
!>        3) Add data to the array by calling DAFADA as many times
!>           as necessary,
!>
!>              CALL GET_DATA ( DATA, N, FOUND )
!>
!>              DO WHILE ( FOUND )
!>                 CALL DAFADA   ( DATA, N        )
!>                 CALL GET_DATA ( DATA, N, FOUND )
!>              END DO
!>
!>        4) End the array by calling DAFENA,
!>
!>              CALL DAFENA
!>
!>     Note that the data can be added in chunks of any size, so long
!>     as the chunks are ordered correctly.
!>
!>     In applications that add data to multiple DAFs simultaneously, it
!>     is necessary to specify which DAF to add data to. The DAFANA
!>     entry points that allow specification of a DAF via a file handle
!>     argument are DAFBNA (DAF, begin new array) and DAFCAD (DAF,
!>     continue adding data).  As in the single-DAF case, arrays are
!>     started by calls to DAFBNA, and data is added to arrays by calls
!>     to DAFADA. The last DAF designated by the input file handle
!>     supplied to DAFBNA or DAFCAD is the `current DAF'. If a
!>     DAF contains an array started by a call to DAFBNA but not yet
!>     completed by a call to DAFENA, we call this array the `current
!>     array' for that DAF. Each call to DAFADA will add data to the
!>     current array in the current DAF. A call to DAFENA will make the
!>     current array in the current DAF a permanent addition to that DAF.
!>
!>     The notion of `current DAF' as discussed here applies only to
!>     DAFs acted upon by entry points of DAFANA. In DAFFA, there is a
!>     DAF that is treated as the `current DAF' for searching; there is
!>     no connection between the DAFs regarded as current by DAFANA and
!>     DAFFA.
!>
!>     In the following example, we write data obtained from the routine
!>     GET_DATA into two separate DAFs. The first N/2 elements of the
!>     array DATA will be written to the first DAF; the rest of the
!>     array will be written to the second DAF.
!>
!>        1) Open the DAFs for write access, using either DAFOPW
!>           (if the files already exist) or DAFOPN (if they do not).
!>
!>              CALL DAFOPW ( FNAME1, HANDL1 )
!>              CALL DAFOPW ( FNAME2, HANDL2 )
!>
!>        2) Begin the new DAFs by calling DAFBNA,
!>
!>              CALL DAFBNA ( HANDL1, SUM1, NAME1 )
!>              CALL DAFBNA ( HANDL2, SUM2, NAME2 )
!>
!>        3) Add data to the arrays by calling DAFCAD and DAFADA as many
!>           times as necessary, selecting the file to add data to by
!>           calling DAFCAD:
!>
!>              CALL GET_DATA ( DATA, N, FOUND )
!>
!>              DO WHILE ( FOUND )
!>
!>                 CALL DAFCAD   ( HANDL1                          )
!>                 CALL DAFADA   ( DATA,               N/2         )
!>
!>                 CALL DAFCAD   ( HANDL2                          )
!>                 CALL DAFADA   ( DATA( N/2 + 1 ),    N - N/2     )
!>
!>                 CALL GET_DATA ( DATA, N, FOUND )
!>
!>              END DO
!>
!>        4) End each array by calling DAFENA, selecting the file
!>           in which to end the array by calling DAFCAD:
!>
!>              CALL DAFCAD ( HANDL1 )
!>              CALL DAFENA
!>
!>              CALL DAFCAD ( HANDL2 )
!>              CALL DAFENA
!>
!>### Examples
!>
!>     The numerical results shown for these examples may differ across
!>     platforms. The results depend on the SPICE kernels used as
!>     input, the compiler and supporting libraries, and the machine
!>     specific arithmetic implementation.
!>
!>     1) The following example illustrates one possible way to copy
!>        an array from one DAF to another, N words at a time.
!>
!>        Use the CK kernel below as the original DAF file.
!>
!>           vo2_swu_ck2.bc
!>
!>        Example code begins here.
!>
!>```fortran
!>      PROGRAM DAFANA_EX1
!>      IMPLICIT NONE
!>
!>C
!>C     Local parameters
!>C
!>      INTEGER               MAXNSZ
!>      PARAMETER           ( MAXNSZ = 1000 )
!>
!>      INTEGER               MAXND
!>      PARAMETER           ( MAXND  = 124  )
!>
!>      INTEGER               MAXNI
!>      PARAMETER           ( MAXNI  = 250  )
!>
!>      INTEGER               NWORDS
!>      PARAMETER           ( NWORDS = 100  )
!>
!>      INTEGER               MAXSUM
!>      PARAMETER           ( MAXSUM = 125  )
!>
!>C
!>C     Local variables.
!>C
!>      CHARACTER*(MAXNSZ)    NAME
!>
!>      DOUBLE PRECISION      DATA   ( NWORDS )
!>      DOUBLE PRECISION      DC     ( MAXND  )
!>      DOUBLE PRECISION      SUM    ( MAXSUM )
!>
!>      INTEGER               BIDX
!>      INTEGER               CHUNK
!>      INTEGER               EIDX
!>      INTEGER               IC     ( MAXNI  )
!>      INTEGER               ND
!>      INTEGER               NI
!>      INTEGER               ORIGIN
!>      INTEGER               TARGET
!>
!>      LOGICAL               FOUND
!>
!>C
!>C     Open the origin DAF file for reading.
!>C
!>      CALL DAFOPR ( 'vo2_swu_ck2.bc', ORIGIN )
!>
!>C
!>C     Start forward search in origin DAF.
!>C
!>      CALL DAFBFS ( ORIGIN )
!>
!>C
!>C     Find the first array in origin DAF.
!>C
!>      CALL DAFFNA ( FOUND  )
!>
!>C
!>C     Get the summary and name of the current array in the
!>C     ORIGIN DAF file
!>C
!>      CALL DAFGS  ( SUM  )
!>      CALL DAFGN  ( NAME )
!>
!>C
!>C     Unpack the summary.
!>C
!>      CALL DAFHSF ( ORIGIN, ND, NI )
!>      CALL DAFUS  ( SUM,    ND, NI, DC, IC )
!>
!>C
!>C     Open the target DAF file for writing. Use 'CK' as
!>C     data type, and reserve no records for comments.
!>C
!>      CALL DAFONW ( 'dafana_ex1.bc', 'CK', ND, NI,
!>     .              'CK file created for example 1 DAFANA', 0,
!>     .              TARGET )
!>
!>C
!>C     Begin a new array in the target DAF file, using the
!>C     origin SUM and NAME.
!>C
!>      CALL DAFBNA ( TARGET, SUM, NAME )
!>
!>C
!>C     Copy the complete array for the first segment of the
!>C     origin DAF file.
!>C
!>      BIDX = IC(NI-1)
!>      EIDX = IC(NI  )
!>
!>      DO WHILE ( BIDX .LE. EIDX )
!>
!>         CHUNK = MIN ( BIDX + NWORDS - 1, EIDX )
!>
!>         CALL DAFGDA ( ORIGIN, BIDX, CHUNK, DATA )
!>         CALL DAFADA ( DATA,   NWORDS )
!>
!>         BIDX = BIDX + NWORDS
!>
!>      END DO
!>
!>C
!>C     End the new array in the target DAF.
!>C
!>      CALL DAFENA
!>
!>C
!>C     Close the DAF files.
!>C
!>      CALL DAFCLS ( ORIGIN )
!>      CALL DAFCLS ( TARGET )
!>
!>      END
!>```
!>
!>        When this program is executed, no output is presented on
!>        screen. After run completion, a new CK file exists in the
!>        output directory.
!>
!>     2)  A simple example demonstrating simultaneous addition
!>         of data to multiple DAFs.
!>
!>         Assume we have data from a text file containing three
!>         columns of numbers. We will write the data from each
!>         column out to a separate DAF.
!>
!>         To confirm that the DAFs created by this program contain the
!>         correct contents, we will read the data from each DAF and
!>         combine it to create a matrix. This matrix should contain
!>         the same data as the file we assumed to be the source for
!>         our dataset.
!>
!>         The format of the output text should be as follows:
!>
!>            .-                   -.
!>            |  n11    n12    n13  |
!>            |  n21    n22    n23  |
!>            |   .      .      .   |
!>            |   .      .      .   |
!>            |   .      .      .   |
!>            `-                   -'
!>
!>         where the symbol nij indicates the jth number on the ith line
!>         of the source data file.
!>
!>        Example code begins here.
!>
!>```fortran
!>      PROGRAM DAFANA_EX2
!>      IMPLICIT NONE
!>
!>C
!>C     Assume we have read columns of d.p. numbers
!>C     from a text file. Write the data from each
!>C     column into a separate DAF.  Read these DAFs
!>C     and create a matrix containing the same data
!>C     as assumed input text file.
!>C
!>C     Since we do not need to retain any descriptive
!>C     information about the DAFs inside of the files
!>C     themselves, we'll use a summary format having
!>C     two integer components (the minimum--these are
!>C     reserved for use by the DAF routines) and zero
!>C     double precision components.
!>C
!>C     The internal file names and array names will
!>C     simply indicate the data sources.
!>C
!>
!>C
!>C     Local parameters
!>C
!>      INTEGER               FNMLEN
!>      PARAMETER           ( FNMLEN = 20 )
!>
!>      INTEGER               LINLEN
!>      PARAMETER           ( LINLEN = 80 )
!>
!>      INTEGER               MAXLNS
!>      PARAMETER           ( MAXLNS =  9 )
!>
!>      INTEGER               MAXCOL
!>      PARAMETER           ( MAXCOL =  3 )
!>
!>      INTEGER               ND
!>      PARAMETER           ( ND     =  0 )
!>
!>      INTEGER               NDAF
!>      PARAMETER           ( NDAF   =  3 )
!>
!>      INTEGER               NI
!>      PARAMETER           ( NI     =  2 )
!>
!>      INTEGER               NUMLEN
!>      PARAMETER           ( NUMLEN = 30 )
!>
!>      INTEGER               SIG
!>      PARAMETER           ( SIG    = 10 )
!>
!>C
!>C     Local variables
!>C
!>      CHARACTER*(FNMLEN)    DAFNAM ( NDAF   )
!>      CHARACTER*(FNMLEN)    INFILE
!>      CHARACTER*(LINLEN)    LINE
!>      CHARACTER*(NUMLEN)    NUMCH
!>      CHARACTER*(LINLEN)    PRSERR
!>      CHARACTER*(FNMLEN)    RESULT
!>
!>      DOUBLE PRECISION      DC     ( 1      )
!>      DOUBLE PRECISION      NUMBER ( MAXLNS, MAXCOL )
!>      DOUBLE PRECISION      NUMDP
!>      DOUBLE PRECISION      SUMMRY ( 1      )
!>
!>      INTEGER               FA
!>      INTEGER               HAN    ( NDAF   )
!>      INTEGER               I
!>      INTEGER               IA
!>      INTEGER               IC     ( NI     )
!>      INTEGER               J
!>      INTEGER               LENGTH
!>      INTEGER               NCOLS
!>      INTEGER               PTR
!>
!>      LOGICAL               EOF
!>      LOGICAL               FOUND
!>
!>C
!>C     Initial values
!>C
!>      DATA                  DAFNAM   /  'COLUMN1.DAF',
!>     .                                  'COLUMN2.DAF',
!>     .                                  'COLUMN3.DAF'  /
!>
!>      DATA                  NUMBER  /
!>     .               11.D0, 21.D0, 31.D0, 41.D0, 51.D0,
!>     .               61.D0, 71.D0, 81.D0, 91.D0,
!>     .               12.D0, 22.D0, 32.D0, 42.D0, 52.D0,
!>     .               62.D0, 72.D0, 82.D0, 92.D0,
!>     .               13.D0, 23.D0, 33.D0, 43.D0, 52.D0,
!>     .               63.D0, 73.D0, 83.D0, 93.D0        /
!>
!>C
!>C     Create the new DAFs, and start a new array in each
!>C     one.  Just use the file name for the internal file
!>C     name and array name, for each DAF.  No assignments
!>C     are required for the array summaries.
!>C
!>      DO I = 1, NDAF
!>
!>         CALL DAFOPN ( DAFNAM(I), ND, NI,
!>     .                 DAFNAM(I), 0,  HAN(I) )
!>
!>         CALL DAFBNA ( HAN(I), SUMMRY, DAFNAM(I) )
!>
!>      END DO
!>
!>C
!>C     At this point, we assume that we have read the
!>C     file line by line. Add the numbers from each column
!>C     to the corresponding DAF.
!>C
!>      DO I = 1, MAXLNS
!>
!>C
!>C        Add the number from the ith column to the array
!>C        in the ith DAF.  We'll use DAFCAD to select
!>C        the correct DAF to add data to.
!>C
!>         DO J = 1, NDAF
!>            CALL DAFCAD ( HAN(J)         )
!>            CALL DAFADA ( NUMBER(I,J), 1 )
!>         END DO
!>
!>      END DO
!>
!>C
!>C     Finish ("end") the arrays.  Again, we'll use DAFCAD
!>C     to select the DAFs in which the arrays are to be
!>C     finished.  After finishing each array, close the DAF
!>C     containing it.
!>C
!>      DO I = 1, NDAF
!>         CALL DAFCAD ( HAN(I) )
!>         CALL DAFENA
!>         CALL DAFCLS ( HAN(I) )
!>      END DO
!>
!>C
!>C     Now for the verification step.  We'll try to
!>C     print a matrix containing the same data as
!>C     the original input file.  The format of the numbers,
!>C     the delimiters separating the numbers, spacing, and
!>C     non-printing characters may differ.
!>C
!>C     Open the DAFs for reading.
!>C
!>      DO I = 1, NDAF
!>         CALL DAFOPR ( DAFNAM(I), HAN(I) )
!>      END DO
!>
!>C
!>C     Obtain the start and end addresses of the
!>C     data in each DAF.  To do this, we'll need to
!>C     obtain and unpack the array summaries.
!>C
!>C     If all went well, the addresses should be the
!>C     same for each DAF.  We'll assume that the initial
!>C     and final addresses in the first DAF are correct
!>C     for all three.
!>C
!>      CALL DAFBFS ( HAN(1) )
!>      CALL DAFFNA ( FOUND  )
!>      CALL DAFGS  ( SUMMRY )
!>      CALL DAFUS  ( SUMMRY, ND, NI, DC, IC )
!>
!>      IA      =  IC( NI-1 )
!>      FA      =  IC( NI   )
!>      LENGTH  =  FA - IA + 1
!>
!>C
!>C     Now read numbers from the DAFs and build up
!>C     lines of text.  Print these lines out.
!>C
!>      DO I = 0,  LENGTH - 1
!>
!>         LINE = ' '
!>
!>         DO J = 1, NDAF
!>
!>            CALL DAFRDA ( HAN(J), IA+I, IA+I, NUMDP )
!>
!>C
!>C           Convert the double precision number to a
!>C           character string, and append it to the current
!>C           line.
!>C
!>            CALL DPSTR  ( NUMDP,  SIG,        NUMCH )
!>            CALL SUFFIX ( NUMCH,  3,          LINE  )
!>
!>         END DO
!>
!>         WRITE(*,*) LINE
!>
!>      END DO
!>
!>C
!>C     Close the DAFs.
!>C
!>      DO I = 1, NDAF
!>         CALL DAFCLS( HAN(I) )
!>      END DO
!>
!>      END
!>```
!>
!>        When this program was executed on a Mac/Intel/gfortran/64-bit
!>        platform, the output was:
!>
!>             1.100000000E+01    1.200000000E+01    1.300000000E+01
!>             2.100000000E+01    2.200000000E+01    2.300000000E+01
!>             3.100000000E+01    3.200000000E+01    3.300000000E+01
!>             4.100000000E+01    4.200000000E+01    4.300000000E+01
!>             5.100000000E+01    5.200000000E+01    5.200000000E+01
!>             6.100000000E+01    6.200000000E+01    6.300000000E+01
!>             7.100000000E+01    7.200000000E+01    7.300000000E+01
!>             8.100000000E+01    8.200000000E+01    8.300000000E+01
!>             9.100000000E+01    9.200000000E+01    9.300000000E+01
!>
!>        Note that after run completion, three new DAF files exist in
!>        the output directory.
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
!>     K.R. Gehringer     (JPL)
!>     H.A. Neilan        (JPL)
!>     W.L. Taber         (JPL)
!>     F.S. Turner        (JPL)
!>     I.M. Underwood     (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 3.1.0, 13-AUG-2021 (JDR)
!>
!>        Added IMPLICIT NONE statement.
!>
!>        Edited the header of DAFANA and all entry points to comply with
!>        NAIF standard. Added complete code examples to DAFANA $Examples
!>        section based on the existing fragments.
!>
!>-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
!>
!>        Updated the entry points of DAFANA to enable its
!>        internal state table size, TBSIZE, to be smaller
!>        than the file table maintained by DAFAH: FTSIZE.
!>
!>-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
!>
!>        Updated to remove potential compiler warnings from the
!>        truncation of double precision numbers to integers.
!>
!>        Also changed was a numeric constant from 1.D0 to the
!>        equivalent, but more aesthetically pleasing 1.0D0.
!>
!>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!>
!>        Comment section for permuted index source lines was added
!>        following the header.
!>
!>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!>
!>        Updated to support simultaneous writes to multiple DAFs.
!>        The $Examples section of this routine now illustrates
!>        usage of the routine DAFCAD.
!>
!>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!>
!>        Literature references added to the header.
!>
!>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!>
!>### Revisions
!>
!>-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
!>
!>        This umbrella and its entry points were updated to
!>        work properly with the changes in the DAF system as
!>        a result of its utilization of the new handle manager.
!>
!>        Since DAFAH now tracks FTSIZE files as defined in
!>        the include file 'zzddhman.inc', it was decided that
!>        in the interest of releasing the toolkit this module
!>        would undergo simple changes.  As such most previous
!>        references to FTSIZE in this umbrella have been replaced
!>        with TBSIZE where appropriate.  DAFBNA now signals an
!>        error if there is not enough room to add a new DAF's
!>        dossier to the state table.  Also, after attempting to
!>        clean up all files listed in the state table that are
!>        not currently open, DAFBNA attempts to locate the
!>        first dossier with STADDG set to FALSE.  This is then
!>        freed to make room for the new DAF.  If DAFBNA fails
!>        to locate such a dossier in the state table, it
!>        signals the error SPICE(STFULL).
!>
!>        The parameter FILEN was removed, as it is defined
!>        on an environmental basis in the include file
!>        'zzddhman.inc'.
!>
!>-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
!>
!>        Updated to remove potential compiler warnings from the
!>        truncation of double precision numbers to integers. Two
!>        assignments to NARRAY were updated, being changed from:
!>
!>           NARRAY = SUMREC(ARYCNT)
!>
!>        to
!>
!>           NARRAY = IDINT ( SUMREC(ARYCNT) )
!>
!>        Also changed was a numeric constant from 1.D0 to the
!>        equivalent, but more aesthetically pleasing 1.0D0.
!>
!>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!>
!>        Updated to support simultaneous writes to multiple DAFs.
!>
!>        In previous versions of DAFANA, data could be added to only
!>        one DAF array at a time.  In fact, DAFAH allowed only one
!>        DAF to be open for writing at any time.  Therefore, there was
!>        no question about which DAF was being operated on by either of
!>        the DAFANA entry points that don't accept file handles as
!>        input arguments:  DAFADA and DAFENA.  In the current version
!>        of DAFANA, the entry points that don't accept file handles as
!>        inputs operate on the `current DAF'.  The current DAF is the
!>        last one in which a new array was started by DAFBNA, or in
!>        which addition of data to an array was continued by the new
!>        entry point DAFCAD.  DAFCAD was added to allow users to set
!>        the current DAF, so that additions of data to arrays in
!>        multiple DAFs can be interleaved.
!>
!>        Note that the notion of `current DAF' as discussed here applies
!>        only to DAFs acted upon by entry points of DAFANA. In DAFFA,
!>        there is a DAF that is treated as the `current DAF' for
!>        searching; there is no connection between the DAFs regarded
!>        as current by DAFANA and DAFFA.
!>
!>        The two principal changes to DAFANA are the addition of the
!>        new entry point DAFCAD, and the addition of a data structure
!>        called the `state table'. The state table is a collection of
!>        parallel arrays that maintain information about the state
!>        of each data addition that is currently in progress. The
!>        state table arrays are indexed by a singly linked list pool;
!>        this mechanism allows addition and deletion of information
!>        about data additions without requiring movement of data
!>        already in the state table.
!>
!>        The linked list pool contains an `active' list and a `free'
!>        list. Nodes in the active list are used to index elements of
!>        the state table where information about additions in progress
!>        is stored. The head node of the active list is of particular
!>        significance: the state information pointed to by this node
!>        is that of the current DAF. Nodes in the free list index
!>        elements of the state table that are available for use.
!>
!>        When an array is started in a DAF that is not already `known'
!>        to DAFANA, information about the DAF is added to the state
!>        table. If there are no free elements in the state table,
!>        the routine starting the array (DAFBNA) will perform garbage
!>        collection: the routine will test the handles of each file
!>        about which information in stored in the state table to see
!>        whether that file is still open. Nodes containing information
!>        about DAFs that are no longer open will be moved to the free
!>        list.
!>
!>        Whenever a DAF becomes the current DAF, the linked list
!>        that indexes the state table is adjusted so that the node
!>        pointing to information about the current DAF is at the head
!>        of the active list. This way, a slight efficiency is gained
!>        when repeated data additions are made to the same DAF, since
!>        the linear search through the state table for information on
!>        that DAF will be shortened.
!>
!>        Since the algorithms for maintenance of linked lists are well
!>        known, they are not documented here. However, see the
!>        internals of the SPICELIB routine SPKBSR for a nice diagram
!>        describing a similar data structure.
!>
!>        The state table contains two arrays that are quite large:
!>        there are buffers that contain the name and array summary for
!>        each array under construction. A parallel situation exists
!>        in DAFFA, where there are buffers that contain the last
!>        character record and summary record read from each DAF. The
!>        total storage required for these arrays (in DAFANA and DAFFA
!>        together) is 4000 * TBSIZE bytes. For this reason, it may be
!>        a good idea to reduce the value of TBSIZE in SPICELIB versions
!>        for machines where memory is scarce.
!>
!>        On a completely different topic: the local declarations in
!>        DAFANA have been alphabetized and separated by type, except
!>        for those relating to the state table. Several hard-coded
!>        constants have been replaced by parameters.
!>
!>### Implementation notes
!>
!>     These variables define the state of each DAF to which data
!>     is currently being added.  For each DAF that we're writing to, we
!>     maintain a copy of:
!>
!>        STFH           File handle.
!>
!>        STIFNM         Internal file name.
!>
!>        STADDG         (`State table: adding') Flag indicating
!>                       whether addition of data to an array is in
!>                       progress.
!>
!>        STFRST         Record number of initial summary record.
!>
!>        STLAST         Record number of final summary record.
!>
!>        STBEGN         Beginning address of new array.
!>
!>        STFREE         Address of next free word.
!>
!>        STLSUM         Local copy of the array summary for the current
!>                       array.
!>
!>        STNAME         Local copy of the array name for the current
!>                       array.
!>
!>     These variables are maintained in a table of parallel arrays;
!>     the size of the table is TBSIZE.
!>
!>     The table of state variables is indexed by a singly linked list
!>     of pointers.  This mechanism avoids the work of moving
!>     the state variable data about as information about DAFs is
!>     added to or deleted from the table.
!>
!>     The structure containing the linked list pointers is called a
!>     `pool.'  The pool contains a list of `active' nodes and a list
!>     of free nodes.  The head nodes of the active and free lists are
!>     maintained as the variables STHEAD (`state table head') and
!>     STFPTR (`state table free pointer'), respectively.  Every node in
!>     the pool is on exactly one of these lists.
!>
!>     The pool starts out with all of the nodes on the free list.
!>     DAFBNA initializes the pool.  As new DAFs are written to,
!>     DAFBNA adds information about them to the state table.  Every
!>     time a DAF array is started by DAFBNA, or selected for
!>     continuation by DAFCAD, the routine in question `moves' the
!>     DAF's state information to the head of the active list, if the
!>     state information is not already there.  This re-organization is
!>     accomplished by deleting the node for the DAF from its current
!>     position in the active list and inserting the node at the head of
!>     the list.  Thus, the change is made merely by setting pointers,
!>     not by moving chunks of data in the state table.
!>
!>     It may happen that there is no room left in the state table
!>     to accommodate information about a new DAF.  In this case,
!>     garbage collection must be performed:  DAFBNA frees all nodes in
!>     the table that index DAFs that are not currently open.
!>
!>     Note that the routine DAFADA does not modify the state table; it
!>     merely adds data to the DAF that is at the head of the active
!>     list.
!>
!>## DAFFA (DAF, find array)
!>
!> PEPPER: SPICELIB's umbrella routine DAFFA is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_daffa. Its documentation follows.
!>
!>### Abstract
!>
!>     Find arrays in a DAF.
!>
!>### Detailed Input
!>
!>     HANDLE   on input is the handle of the DAF to be searched.
!>
!>     SUM      on input is an array summary that replaces the
!>              summary of the current array in the DAF currently
!>              being searched.
!>
!>     NAME     on input is an array name that replaces the name
!>              of the current array in the DAF currently being
!>              searched.
!>
!>### Detailed Output
!>
!>     HANDLE   on output is the handle of the DAF currently being
!>              searched.
!>
!>     SUM      on output is the summary for the array found most
!>              recently.
!>
!>     NAME     on output is the name for the array found
!>              most recently.
!>
!>     FOUND    is .TRUE. whenever the search for the next or the
!>              previous array is successful, and is .FALSE. otherwise.
!>
!>### Parameters
!>
!>     TBSIZE   is the maximum number of files (DAS and DAF) that may be
!>              simultaneously open. TBSIZE is set to FTSIZE which is
!>              assigned and defined in zzdhman.inc.
!>
!>### Exceptions
!>
!>     1)  If DAFFA is called directly, the error SPICE(BOGUSENTRY)
!>         is signaled.
!>
!>     2)  See entry points DAFBFS, DAFFNA, DAFBBS, DAFFPA, DAFGS, DAFGN,
!>         DAFGH, DAFRS, DAFWS, DAFRN, and DAFCS for exceptions specific
!>         to those entry points.
!>
!>### Files
!>
!>     DAFs read by DAFFA and its entry points are opened
!>     elsewhere, and referred to only by their handles.
!>
!>### Particulars
!>
!>     DAFFA serves as an umbrella, allowing data to be shared by its
!>     entry points:
!>
!>        DAFBFS         Begin forward search.
!>        DAFFNA         Find next array.
!>
!>        DAFBBS         Begin backward search.
!>        DAFFPA         Find previous array.
!>
!>        DAFGS          Get summary.
!>        DAFGN          Get name.
!>        DAFGH          Get handle.
!>
!>        DAFRS          Replace summary.
!>        DAFWS          Write summary.
!>        DAFRN          Replace name.
!>
!>        DAFCS          Continue search.
!>
!>     The main function of these entry points is to allow the
!>     contents of any DAF to be examined on an array-by-array
!>     basis.
!>
!>     Conceptually, the arrays in a DAF form a doubly linked list,
!>     which can be searched in either of two directions: forward or
!>     backward. It is possible to search multiple DAFs simultaneously.
!>
!>     DAFBFS (begin forward search) and DAFFNA are used to search the
!>     arrays in a DAF in forward order. In applications that search a
!>     single DAF at a time, the normal usage is
!>
!>        CALL DAFBFS ( HANDLE )
!>        CALL DAFFNA ( FOUND  )
!>
!>        DO WHILE ( FOUND )
!>
!>           CALL DAFGS ( SUM  )
!>           CALL DAFGN ( NAME )
!>            .
!>            .
!>
!>           CALL DAFFNA ( FOUND )
!>
!>        END DO
!>
!>     DAFBBS (begin backward search) and DAFFPA are used to search the
!>     arrays in a DAF in backward order. In applications that search
!>     a single DAF at a time, the normal usage is
!>
!>        CALL DAFBBS ( HANDLE )
!>        CALL DAFFPA ( FOUND  )
!>
!>        DO WHILE ( FOUND )
!>
!>           CALL DAFGS ( SUM  )
!>           CALL DAFGN ( NAME )
!>            .
!>            .
!>
!>           CALL DAFFPA ( FOUND )
!>
!>        END DO
!>
!>     In applications that conduct multiple searches simultaneously,
!>     the above usage must be modified to specify the handle of the
!>     file to operate on, in any case where the file may not be the
!>     last one specified by DAFBFS or DAFBBS. The routine DAFCS
!>     (DAF, continue search) is used for this purpose. Below, we
!>     give an example of an interleaved search of two files specified
!>     by the handles HANDL1 and HANDL2. The directions of searches
!>     in different DAFs are independent; here we conduct a forward
!>     search on one file and a backward search on the other.
!>     Throughout, we use DAFCS to specify which file to operate on,
!>     before calling DAFFNA, DAFFPA, DAFGS, DAFRS, DAFWS, DAFGN, or
!>     DAFRN.
!>
!>        CALL DAFBFS ( HANDL1 )
!>        CALL DAFBBS ( HANDL2 )
!>
!>        CALL DAFCS  ( HANDL1 )
!>        CALL DAFFNA ( FOUND1 )
!>
!>        CALL DAFCS  ( HANDL2 )
!>        CALL DAFFPA ( FOUND2 )
!>
!>        DO WHILE ( FOUND1 .OR. FOUND2 )
!>
!>           IF ( FOUND1 ) THEN
!>
!>              CALL DAFCS ( HANDL1 )
!>              CALL DAFGS ( SUM    )
!>              CALL DAFGN ( NAME   )
!>               .
!>               .
!>              CALL DAFCS  ( HANDL1 )
!>              CALL DAFFNA ( FOUND1 )
!>
!>           END IF
!>
!>           IF ( FOUND2 ) THEN
!>
!>              CALL DAFCS ( HANDL2 )
!>              CALL DAFGS ( SUM    )
!>              CALL DAFGN ( NAME   )
!>               .
!>               .
!>              CALL DAFCS  ( HANDL2 )
!>              CALL DAFFPA ( FOUND2 )
!>
!>           END IF
!>
!>        END DO
!>
!>     At any time, the latest array found (whether by DAFFNA or DAFFPA)
!>     is regarded as the `current' array for the file in which the
!>     array was found. The last DAF in which a search was started,
!>     executed, or continued by any of DAFBFS, DAFBBS, DAFFNA, DAFFPA
!>     or DAFCS is regarded as the `current' DAF. The summary and name
!>     for the current array in the current DAF can be returned
!>     separately, as shown above, by calls to DAFGS (get summary) and
!>     DAFGN (get name). The handle of the current DAF can also be
!>     returned by calling DAFGH (get handle).
!>
!>     The summary and name of the current array in the current DAF can
!>     be updated (again, separately) by providing new ones through DAFRS
!>     (replace summary) and DAFRN (replace name). This feature
!>     should not be used except to correct errors that occurred during
!>     the creation of a file. Note that changes can only be made to
!>     files opened for write access. Also, the addresses of an array
!>     cannot be changed using these routines. (Another routine,
!>     DAFWS, is provided for this purpose, but should be used only
!>     to reorder the arrays in a file.)
!>
!>     Once a search has been begun, it may be continued in either
!>     direction. That is, DAFFPA may be used to back up during a
!>     forward search, and DAFFNA may be used to advance during a
!>     backward search.
!>
!>### Examples
!>
!>     The numerical results shown for these examples may differ across
!>     platforms. The results depend on the SPICE kernels used as
!>     input, the compiler and supporting libraries, and the machine
!>     specific arithmetic implementation.
!>
!>     1) The following program illustrates the way summaries and
!>        names for the arrays contained in a DAF can be modified.
!>
!>        This example is provided for educational purpose only.
!>
!>        Replace the body ID code 301 (Moon) with a test body ID,
!>        e.g. -999, in every descriptor of an SPK file; update the
!>        segment identifier to indicate that such change has been
!>        implemented.
!>
!>        Example code begins here.
!>
!>```fortran
!>      PROGRAM DAFFA_EX1
!>      IMPLICIT NONE
!>
!>C
!>C     SPICELIB functions
!>C
!>      INTEGER               CARDI
!>      INTEGER               RTRIM
!>
!>C
!>C     Local parameters.
!>C
!>      INTEGER               DSCSIZ
!>      PARAMETER           ( DSCSIZ  = 5    )
!>
!>      INTEGER               FILSIZ
!>      PARAMETER           ( FILSIZ  = 255  )
!>
!>      INTEGER               LBCELL
!>      PARAMETER           ( LBCELL = -5 )
!>
!>      INTEGER               MAXOBJ
!>      PARAMETER           ( MAXOBJ  = 1000 )
!>
!>      INTEGER               ND
!>      PARAMETER           ( ND      = 2    )
!>
!>      INTEGER               NI
!>      PARAMETER           ( NI      = 6    )
!>
!>      INTEGER               NEWCODE
!>      PARAMETER           ( NEWCODE = -999 )
!>
!>      INTEGER               OLDCODE
!>      PARAMETER           ( OLDCODE =  301 )
!>
!>      INTEGER               SGIDLN
!>      PARAMETER           ( SGIDLN  = 1000 )
!>
!>C
!>C     Local variables.
!>C
!>      CHARACTER*(FILSIZ)    FNAME
!>      CHARACTER*(SGIDLN)    SEGID
!>      CHARACTER*(SGIDLN)    UPDSID
!>
!>      DOUBLE PRECISION      DC     ( ND )
!>      DOUBLE PRECISION      SUM    ( DSCSIZ )
!>
!>      INTEGER               HANDLE
!>      INTEGER               I
!>      INTEGER               IC     ( NI )
!>      INTEGER               IDS    ( LBCELL : MAXOBJ )
!>      INTEGER               OBJ
!>
!>      LOGICAL               FOUND
!>      LOGICAL               UPDATE
!>
!>C
!>C     Get the SPK file name.
!>C
!>      CALL PROMPT ( 'Enter name of the SPK file > ', FNAME )
!>
!>C
!>C     Initialize the set IDS.
!>C
!>      CALL SSIZEI ( MAXOBJ, IDS )
!>
!>C
!>C     Find the set of objects in the SPK file.
!>C
!>      CALL SPKOBJ ( FNAME, IDS )
!>
!>      WRITE(*,'(A)') 'Objects in the original DAF file:'
!>      WRITE(*,'(20I4)') ( IDS(I), I= 1, CARDI ( IDS ) )
!>
!>C
!>C     Open for writing the SPK file.
!>C
!>      CALL DAFOPW ( FNAME, HANDLE )
!>
!>C
!>C     Search the file in forward order.
!>C
!>      CALL DAFBFS ( HANDLE )
!>      CALL DAFFNA ( FOUND  )
!>
!>      WRITE(*,'(A)') 'Original Segment IDs (forward order):'
!>
!>      DO WHILE ( FOUND )
!>
!>C
!>C        Fetch and unpack the descriptor (aka summary)
!>C        of the current segment, and get its name.
!>C
!>         CALL DAFGN ( SEGID )
!>         CALL DAFGS ( SUM   )
!>         CALL DAFUS ( SUM, ND, NI, DC, IC )
!>
!>C
!>C        Print the current segment name
!>C
!>         WRITE(*,'(2I6,2X,A)') IC(1), IC(2),
!>     .                         SEGID(:RTRIM(SEGID))
!>
!>C
!>C        Replace ID codes if necessary.
!>C
!>         UPDATE = .FALSE.
!>         IF ( IC(1) .EQ. OLDCODE ) THEN
!>
!>            IC(1)  = NEWCODE
!>            UPDATE = .TRUE.
!>
!>         END IF
!>         IF ( IC(2) .EQ. OLDCODE ) THEN
!>
!>            IC(2) = NEWCODE
!>            UPDATE = .TRUE.
!>
!>         END IF
!>
!>C
!>C        Update segment ID if necessary.
!>C
!>         IF ( UPDATE ) THEN
!>
!>            UPDSID = '# - Updated. Do not use.'
!>            CALL REPMC ( UPDSID, '#', SEGID(:RTRIM(SEGID)),
!>     .                   UPDSID                            )
!>            CALL DAFRN ( UPDSID )
!>
!>         END IF
!>
!>C
!>C        Re-pack the descriptor; replace the descriptor
!>C        in the file.
!>C
!>         CALL DAFPS ( ND, NI, DC, IC, SUM )
!>
!>         CALL DAFRS ( SUM )
!>
!>C
!>C        Find the next segment.
!>C
!>         CALL DAFFNA ( FOUND )
!>
!>      END DO
!>
!>C
!>C     Close the file.
!>C
!>      CALL DAFCLS ( HANDLE )
!>
!>C
!>C     Reset the set IDS.
!>C
!>      CALL SCARDI ( 0, IDS )
!>
!>C
!>C     Find the set of objects in the updated SPK file.
!>C
!>      CALL SPKOBJ ( FNAME, IDS )
!>
!>      WRITE(*,*) ' '
!>      WRITE(*,'(A)') 'Objects in the updated DAF file:'
!>      WRITE(*,'(20I4)') ( IDS(I), I= 1, CARDI ( IDS ) )
!>
!>C
!>C     Search the file in backwards order and output the
!>C     segment IDs.
!>C
!>      CALL DAFOPR ( FNAME, HANDLE )
!>
!>      CALL DAFBBS ( HANDLE )
!>      CALL DAFFPA ( FOUND  )
!>
!>      WRITE(*,'(A)') 'Updated Segment IDs (backwards order):'
!>
!>      DO WHILE ( FOUND )
!>
!>C
!>C        Fetch and unpack the descriptor (aka summary)
!>C        of the current segment, and get its name.
!>C
!>         CALL DAFGN ( SEGID )
!>         CALL DAFGS ( SUM   )
!>         CALL DAFUS ( SUM, ND, NI, DC, IC )
!>
!>         WRITE(*,'(2I6,2X,A)') IC(1), IC(2),
!>     .                         SEGID(:RTRIM(SEGID))
!>
!>C
!>C        Find the previous segment.
!>C
!>         CALL DAFFPA ( FOUND )
!>
!>      END DO
!>
!>C
!>C     Close the file.
!>C
!>      CALL DAFCLS ( HANDLE )
!>
!>      END
!>```
!>
!>        When this program was executed on a Mac/Intel/gfortran/64-bit
!>        platform, using the SPK file named de430.bsp, the output was:
!>
!>        Enter name of the SPK file > de430.bsp
!>        Objects in the original DAF file:
!>           1   2   3   4   5   6   7   8   9  10 199 299 301 399
!>        Original Segment IDs (forward order):
!>             1     0  DE-0430LE-0430
!>             2     0  DE-0430LE-0430
!>             3     0  DE-0430LE-0430
!>             4     0  DE-0430LE-0430
!>             5     0  DE-0430LE-0430
!>             6     0  DE-0430LE-0430
!>             7     0  DE-0430LE-0430
!>             8     0  DE-0430LE-0430
!>             9     0  DE-0430LE-0430
!>            10     0  DE-0430LE-0430
!>           301     3  DE-0430LE-0430
!>           399     3  DE-0430LE-0430
!>           199     1  DE-0430LE-0430
!>           299     2  DE-0430LE-0430
!>
!>        Objects in the updated DAF file:
!>        -999   1   2   3   4   5   6   7   8   9  10 199 299 399
!>        Updated Segment IDs (backwards order):
!>           299     2  DE-0430LE-0430
!>           199     1  DE-0430LE-0430
!>           399     3  DE-0430LE-0430
!>          -999     3  DE-0430LE-0430 - Updated. Do not use.
!>            10     0  DE-0430LE-0430
!>             9     0  DE-0430LE-0430
!>             8     0  DE-0430LE-0430
!>             7     0  DE-0430LE-0430
!>             6     0  DE-0430LE-0430
!>             5     0  DE-0430LE-0430
!>             4     0  DE-0430LE-0430
!>             3     0  DE-0430LE-0430
!>             2     0  DE-0430LE-0430
!>             1     0  DE-0430LE-0430
!>
!>     2) The following program compares data in two DAFs. The DAFs are
!>        expected to have the same number of arrays, the same number
!>        of elements in each corresponding array, and the same summary
!>        format.
!>
!>        Each difference whose magnitude exceeds a specified tolerance
!>        is flagged. The difference information is written to the
!>        screen.
!>
!>        Example code begins here.
!>
!>```fortran
!>      PROGRAM DAFFA_EX2
!>      IMPLICIT NONE
!>
!>C
!>C     Local parameters
!>C
!>      INTEGER               RTRIM
!>
!>C
!>C     Local parameters
!>C
!>      INTEGER               ARRYSZ
!>      PARAMETER           ( ARRYSZ = 10000000 )
!>
!>      INTEGER               ERRLEN
!>      PARAMETER           ( ERRLEN =  240 )
!>
!>      INTEGER               FILEN
!>      PARAMETER           ( FILEN  =  128 )
!>
!>      INTEGER               LINLEN
!>      PARAMETER           ( LINLEN =   80 )
!>
!>      INTEGER               MAXND
!>      PARAMETER           ( MAXND  =  125 )
!>
!>      INTEGER               MAXNI
!>      PARAMETER           ( MAXNI  =  250 )
!>
!>      INTEGER               MAXSUM
!>      PARAMETER           ( MAXSUM =  128 )
!>
!>      INTEGER               RLEN
!>      PARAMETER           ( RLEN   = 1000 )
!>
!>C
!>C     Local variables
!>C
!>      CHARACTER*(RLEN)      ANAME1
!>      CHARACTER*(RLEN)      ANAME2
!>      CHARACTER*(FILEN)     DAF1
!>      CHARACTER*(FILEN)     DAF2
!>      CHARACTER*(FILEN)     LOG
!>      CHARACTER*(ERRLEN)    PRSERR
!>      CHARACTER*(LINLEN)    STR
!>      CHARACTER*(LINLEN)    TOLCH
!>
!>      DOUBLE PRECISION      ARRAY1 ( ARRYSZ )
!>      DOUBLE PRECISION      ARRAY2 ( ARRYSZ )
!>      DOUBLE PRECISION      DC1    ( MAXND )
!>      DOUBLE PRECISION      DC2    ( MAXND )
!>      DOUBLE PRECISION      TOL
!>      DOUBLE PRECISION      DIFF
!>      DOUBLE PRECISION      SUM1   ( MAXSUM )
!>      DOUBLE PRECISION      SUM2   ( MAXSUM )
!>
!>      INTEGER               FA1
!>      INTEGER               FA2
!>      INTEGER               I
!>      INTEGER               IA1
!>      INTEGER               IA2
!>      INTEGER               IC1    ( MAXNI )
!>      INTEGER               IC2    ( MAXNI )
!>      INTEGER               HANDL1
!>      INTEGER               HANDL2
!>      INTEGER               LEN1
!>      INTEGER               LEN2
!>      INTEGER               ND1
!>      INTEGER               ND2
!>      INTEGER               NI1
!>      INTEGER               NI2
!>      INTEGER               PTR
!>
!>      LOGICAL               FOUND
!>
!>C
!>C     Start out by obtaining the names of the DAFs to be
!>C     compared.
!>C
!>      CALL PROMPT ( 'Enter name of first DAF  > ', DAF1 )
!>      CALL PROMPT ( 'Enter name of second DAF > ', DAF2 )
!>      CALL PROMPT ( 'Enter tolerance for data comparison > ',
!>     .              TOLCH                                   )
!>
!>      CALL NPARSD ( TOLCH, TOL, PRSERR, PTR )
!>
!>C
!>C     Open both DAFs for reading.
!>C
!>      CALL DAFOPR ( DAF1, HANDL1 )
!>      CALL DAFOPR ( DAF2, HANDL2 )
!>
!>C
!>C     Start forward searches in both DAFS.
!>C
!>      CALL DAFBFS ( HANDL1 )
!>      CALL DAFBFS ( HANDL2 )
!>
!>C
!>C     Obtain the summary formats for each DAF. Stop now
!>C     if the summary formats don't match.
!>C
!>      CALL DAFHSF ( HANDL1, ND1, NI1 )
!>      CALL DAFHSF ( HANDL2, ND2, NI2 )
!>
!>      IF (  ( ND1 .NE. ND2 ) .OR. ( NI1 .NE. NI2 )  ) THEN
!>
!>         STR = 'Summary formats do not match.  NI1 = #, '//
!>     .                      'NI2 = #, ND1 = #, ND2 = #.'
!>
!>         CALL REPMI  ( STR, '#', NI1, STR )
!>         CALL REPMI  ( STR, '#', NI2, STR )
!>         CALL REPMI  ( STR, '#', ND1, STR )
!>         CALL REPMI  ( STR, '#', ND2, STR )
!>
!>         WRITE(*,*) STR
!>
!>         CALL SIGERR ( 'Incompatible DAFs' )
!>
!>      END IF
!>
!>C
!>C     Find the first array in each DAF. Use DAFCS
!>C     (DAF, continue search) to set the handle of the DAF
!>C     to search in before calling DAFFNA.
!>C
!>      CALL DAFCS  ( HANDL1 )
!>      CALL DAFFNA ( FOUND  )
!>
!>      IF ( FOUND ) THEN
!>         CALL DAFCS  ( HANDL2 )
!>         CALL DAFFNA ( FOUND  )
!>      END IF
!>
!>      DO WHILE ( FOUND )
!>
!>C
!>C        Get the summary and name of each array, using
!>C        DAFCS to select the DAF to get the information
!>C        from. Unpack the summaries and find the beginning
!>C        and ending addresses of the arrays. Read the
!>C        arrays into the variables ARRAY1 and ARRAY2.
!>C
!>         CALL DAFCS ( HANDL1 )
!>         CALL DAFGN ( ANAME1 )
!>         CALL DAFGS ( SUM1   )
!>         CALL DAFUS ( SUM1, ND1, NI1, DC1, IC1 )
!>
!>         IA1  = IC1 ( NI1 - 1 )
!>         FA1  = IC1 ( NI1     )
!>         LEN1 = FA1 - IA1  + 1
!>
!>         IF (  LEN1  .GT.  ARRYSZ  ) THEN
!>            CALL SETMSG ( 'Buffer too small; need # elts.')
!>            CALL ERRINT ( '#', LEN1                       )
!>            CALL SIGERR ( 'ARRAYTOOSMALL'                 )
!>         ELSE
!>            CALL DAFGDA ( HANDL1, IA1, FA1, ARRAY1 )
!>         END IF
!>
!>         CALL DAFCS ( HANDL2 )
!>         CALL DAFGN ( ANAME2 )
!>         CALL DAFGS ( SUM2   )
!>         CALL DAFUS ( SUM2, ND2, NI2, DC2, IC2 )
!>
!>         IA2 = IC2 ( NI2 - 1 )
!>         FA2 = IC2 ( NI2     )
!>
!>         LEN2 = FA2 - IA2  + 1
!>
!>         IF (  LEN2  .GT.  ARRYSZ  ) THEN
!>
!>            CALL SETMSG ( 'Buffer too small; need # elts.')
!>            CALL ERRINT ( '#', LEN2                       )
!>            CALL SIGERR ( 'ARRAYTOOSMALL'                 )
!>
!>         ELSE IF ( LEN1 .NE. LEN2 ) THEN
!>
!>            CALL SETMSG ( 'DAF structures do not match. '//
!>     .                    'LEN1 = #, LEN2 = #. ' )
!>            CALL ERRINT ( '#', LEN1              )
!>            CALL ERRINT ( '#', LEN2              )
!>            CALL SIGERR ( 'Incompatible DAFs' )
!>
!>         ELSE
!>            CALL DAFGDA ( HANDL2, IA2, FA2, ARRAY2 )
!>         END IF
!>C
!>C
!>C        Compare the data in the two arrays. Log a message
!>C        for every instance of data that differs by more
!>C        than the allowed tolerance. Use the array names
!>C        to label the data sources.
!>C
!>         DO I = 1, LEN1
!>
!>            DIFF  =  ABS( ARRAY1(I) - ARRAY2(I) )
!>
!>            IF (  DIFF  .GT.  TOL  ) THEN
!>C
!>C              Get the array names.
!>C
!>               CALL DAFCS ( HANDL1 )
!>               CALL DAFGN ( ANAME1 )
!>               CALL DAFCS ( HANDL2 )
!>               CALL DAFGN ( ANAME2 )
!>
!>C
!>C              Construct the report strings. The number 14
!>C              below is the number of significant digits to
!>C              show in the strings representing d.p.
!>C              numbers.
!>C
!>
!>               WRITE(*,*) ' '
!>               WRITE(*,*) 'Difference of array ' //
!>     .                    'elements exceeded '   //
!>     .                    'tolerance.'
!>               WRITE(*,*) 'First array : ',
!>     .                     ANAME1(:RTRIM(ANAME1))
!>               WRITE(*,*) 'Second array: ',
!>     .                     ANAME2(:RTRIM(ANAME2))
!>
!>               STR = 'First value : #'
!>               CALL REPMD  ( STR, '#', ARRAY1(I), 14, STR )
!>               WRITE(*,*) STR
!>
!>               STR = 'Second value: #'
!>               CALL REPMD  ( STR, '#', ARRAY2(I), 14, STR )
!>               WRITE(*,*) STR
!>
!>               STR = 'Difference  :  #'
!>               CALL REPMD  ( STR, '#', DIFF,      14, STR )
!>               WRITE(*,*) STR
!>
!>            END IF
!>
!>         END DO
!>
!>C
!>C        Find the next pair of arrays.
!>C
!>         CALL DAFCS  ( HANDL1 )
!>         CALL DAFFNA ( FOUND  )
!>
!>         IF ( FOUND ) THEN
!>            CALL DAFCS  ( HANDL2 )
!>            CALL DAFFNA ( FOUND  )
!>         END IF
!>
!>      END DO
!>
!>C
!>C     Close the DAFs.
!>C
!>      CALL DAFCLS ( HANDL1 )
!>      CALL DAFCLS ( HANDL2 )
!>
!>      END
!>```
!>
!>        When this program was executed on a Mac/Intel/gfortran/64-bit
!>        platform, using the PCK (DAF) files named
!>        earth_720101_031229.bpc and earth_720101_070527.bpc, and a
!>        tolerance of '1.D0', the output was:
!>
!>        Enter name of first DAF  > earth_720101_031229.bpc
!>        Enter name of second DAF > earth_720101_070527.bpc
!>        Enter tolerance for data comparison > 1.D0
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8352636890345E+08
!>         Second value: -8.8352636783584E+08
!>         Difference  :  1.0676109790802E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8343997629503E+08
!>         Second value: -8.8343997451568E+08
!>         Difference  :  1.7793515920639E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8335358368661E+08
!>         Second value: -8.8335358119552E+08
!>         Difference  :  2.4910920858383E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8326719107819E+08
!>         Second value: -8.8326718787536E+08
!>         Difference  :  3.2028328180313E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8318079846977E+08
!>         Second value: -8.8318079455519E+08
!>         Difference  :  3.9145733118057E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8309440586135E+08
!>         Second value: -8.8309440123503E+08
!>         Difference  :  4.6263140439987E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8300801325293E+08
!>         Second value: -8.8300800791487E+08
!>         Difference  :  5.3380546569824E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8292162064451E+08
!>         Second value: -8.8292161459471E+08
!>         Difference  :  6.0497951507568E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8283522803609E+08
!>         Second value: -8.8283522127455E+08
!>         Difference  :  6.7615358829498E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8274883542767E+08
!>         Second value: -8.8274882795439E+08
!>         Difference  :  7.4732763767242E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8266244281925E+08
!>         Second value: -8.8266243463423E+08
!>         Difference  :  8.1850171089172E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8257605021083E+08
!>         Second value: -8.8257604131407E+08
!>         Difference  :  8.8967577219009E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8248965760241E+08
!>         Second value: -8.8248964799391E+08
!>         Difference  :  9.6084982156754E+00
!>
!>         Difference of array elements exceeded tolerance.
!>         First array : Earth PCK, ITRF93 Frame
!>         Second array: Earth PCK, ITRF93 Frame
!>         First value : -8.8240326499398E+08
!>         Second value: -8.8240325467375E+08
!>
!>        [...]
!>
!>        Warning: incomplete output. Only 100 out of 80582 lines have
!>        been provided.
!>
!>### Restrictions
!>
!>     1)  Calls that do or may change DAF addresses of DAF summaries,
!>         names, or data of a given DAF file should not be made during
!>         a search of that file initiated by either DAFBFS or DAFBBS.
!>         No such changes should be made between the start of a search
!>         and calls to any entry point that reads or writes to the
!>         summary of the "current array" found by that search, or
!>         that returns a "found" flag indicating whether the current
!>         array exists.
!>
!>         Changing the size of the comment area while a search is in
!>         progress can invalidate record numbers stored in local data
!>         structures of this routine. This can cause corrupted array
!>         summaries and names to be returned upon read access and file
!>         corruption to occur upon write access.
!>
!>         Adding arrays (aka "segments") while either a forward or
!>         backward search is in progress can cause the search to miss
!>         the new segments.
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
!>     F.S. Turner        (JPL)
!>     I.M. Underwood     (JPL)
!>     E.D. Wright        (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 3.2.0, 27-OCT-2021 (JDR) (NJB)
!>
!>        Added IMPLICIT NONE statement.
!>
!>        Edited the header of DAFFA umbrella routine and all its entry
!>        to comply with NAIF standard.
!>
!>        Updated $Restrictions sections of this routine and its entry
!>        points.
!>
!>-    SPICELIB Version 3.1.1, 14-MAR-2017 (NJB)
!>
!>        Updated second header code example in this routine: fixed
!>        error check for array overflow, corrected indentation of
!>        continuation characters, added IMPLICIT NONE, deleted unused
!>        declaration, increased buffer size, and changed DAFRDA call to
!>        DAFGDA call.
!>
!>        Updated header example in entry point DAFGH: changed DAFRDA
!>        call to DAFGDA call.
!>
!>-    SPICELIB Version 3.1.0, 10-FEB-2014 (EDW) (BVS)
!>
!>        Added a functional code example to the $Examples section
!>        in DAFBFS, DAFFNA, DAFGS.
!>
!>        Added check on value of "found" boolean returned from
!>        DAFGSR calls. Failure to check this value can cause an
!>        infinite loop during segment searches on damaged SPKs.
!>
!>        Eliminated unneeded $Revisions section.
!>
!>        Removed the obsolete Reference citation to "NAIF
!>        Document 167.0."
!>
!>        Added full declaration of HANDLE to the $Declarations section
!>        of the DAFCS header.
!>
!>-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
!>
!>        This umbrella and its entry points were updated to
!>        work properly with the changes in the DAF system as
!>        a result of its utilization of the new handle manager.
!>        Calls to DAFRDR were replaced with the translation-aware
!>        interface DAFGSR for retrieving summary records from
!>        DAFs.
!>
!>        Updated the entry points of DAFFA to enable its
!>        internal state table size, TBSIZE, to be smaller
!>        than the file table maintained by DAFAH: FTSIZE.
!>
!>        Since DAFAH now tracks FTSIZE files as defined in
!>        the include file 'zzddhman.inc', it was decided that
!>        in the interest of releasing the toolkit this module
!>        would undergo simple changes. As such most previous
!>        references to FTSIZE in this umbrella have been replaced
!>        with TBSIZE where appropriate. DAFBFS and DAFBBS now signal
!>        errors if there is not enough room to add a new DAF's
!>        dossier to the state table. Also, after attempting to
!>        clean up all files listed in the state table that are
!>        not currently open, DAFBFS and DAFBBS attempt to locate
!>        the first dossier with STADDG set to .FALSE. This is then
!>        freed to make room for the new DAF. If DAFBNA fails
!>        to locate such a dossier in the state table, it
!>        signals the error SPICE(STFULL).
!>
!>        The parameter FILEN was removed, as it is defined
!>        on an environmental basis in the include file
!>        'zzddhman.inc'.
!>
!>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!>
!>        Comment section for permuted index source lines was added
!>        following the header.
!>
!>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
!>
!>        Updated to support simultaneous searches of multiple DAFs.
!>
!>        In previous versions of DAFFA, only one search could be
!>        conducted at a time. Therefore, there was no question about
!>        which DAF was being operated on by any of the DAFFA entry
!>        points that don't accept file handles as input arguments.
!>        In the current version of DAFFA, the entry points that don't
!>        accept file handles as inputs operate on the `current DAF'.
!>        The current DAF is the last one in which a search was
!>        started by DAFBFS or DAFBBS, or continued by the new entry
!>        point DAFCS. DAFCS was added to allow users to set the
!>        current DAF, so that searches of multiple DAFs can be
!>        interleaved.
!>
!>        Note that the notion of `current DAF' as discussed here applies
!>        only to DAFs acted upon by entry points of DAFFA. In DAFANA,
!>        there is a DAF that is treated as the `current DAF' for
!>        adding data; there is no connection between the DAFs regarded
!>        as current by DAFFA and DAFANA.
!>
!>        The two principal changes to DAFFA are the addition of the
!>        new entry point DAFCS, and the addition of a data structure
!>        called the `state table'. The state table is a collection of
!>        parallel arrays that maintain information about the state
!>        of each search that is currently in progress. The arrays are
!>        indexed by a singly linked list pool; this mechanism allows
!>        addition and deletion of information about searches without
!>        requiring movement of data already in the state table. The
!>        linked list pool contains an `active' list and a `free' list.
!>        Nodes in the active list are used to index elements of the
!>        state table where data about searches in progress is stored.
!>        The head node of the active list is of particular significance:
!>        the state information pointed to by this node is that of the
!>        current DAF. Nodes in the free list index elements of the
!>        state table that are available for use.
!>
!>        When a search is started on a DAF that is not already `known'
!>        to DAFFA, information about the DAF is added to the state
!>        table. If there are no free elements in the state table,
!>        the routine starting the search (DAFBFS or DAFBBS) will
!>        perform garbage collection: the routine will test the handles
!>        of each file about which information in stored in the state
!>        table to see whether that file is still open. Nodes containing
!>        information about DAFs that are no longer open will be moved
!>        to the free list.
!>
!>        Whenever a DAF becomes the current DAF, the linked list
!>        that indexes the state table is adjusted so that the
!>        information about the current DAF is at the head of the list.
!>        This way, a slight efficiency is gained when repeated search
!>        accesses are made to the same DAF, since the linear search
!>        through the state table for information on that DAF will
!>        be shortened.
!>
!>        Since the algorithms for maintenance of linked lists are well
!>        known, they are not documented here. However, see the
!>        internals of the SPICELIB routine SPKBSR for a nice diagram
!>        describing a similar data structure.
!>
!>        The state table contains two arrays that are quite large:
!>        there are buffers that contain the last character record
!>        and summary record read from each DAF. A parallel situation
!>        exists in DAFANA, where the name and array summary for each
!>        array under construction are buffered. The total storage
!>        required for these arrays (in DAFANA and DAFFA together) is
!>        4000 * TBSIZE bytes. For this reason, it may be a good idea
!>        to reduce the value of TBSIZE in SPICELIB versions for
!>        machines where memory is scarce.
!>
!>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!>
!>        Literature references added to the header.
!>
!>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!>
!>### Implementation notes
!>
!>     These variables define the state of each DAF to which data
!>     is currently being added. For each DAF that we're writing to, we
!>     maintain a copy of:
!>
!>        STFH           File handle.
!>
!>        STPREV         Record number of previous array summary.
!>
!>        STTHIS         Record number of current array summary.
!>
!>        STNEXT         Record number of next array summary.
!>
!>        STNSEG         Number of summaries in current summary record.
!>
!>        STCURR         Index of current summary within summary record.
!>
!>        STNR           Last name record read.
!>
!>        STHVNR         Flag indicating whether name record containing
!>                       name of current array is buffered.
!>
!>        STSR           Last summary record read.
!>
!>     These variables are maintained in a table of parallel arrays;
!>     the size of the table is TBSIZE.
!>
!>     The table of state variables is indexed by a singly linked list
!>     of pointers. This mechanism avoids the work of moving
!>     the state variable data about as information about DAFs is
!>     added to or deleted from the table.
!>
!>     The structure containing the linked list pointers is called a
!>     `pool'. The pool contains a list of `active' nodes and a list
!>     of free nodes. The head nodes of the active and free lists are
!>     maintained as the variables STHEAD (`state table head') and
!>     STFPTR (`state table free pointer'), respectively. Every node in
!>     the pool is on exactly one of these lists.
!>
!>     The pool starts out with all of the nodes on the free list. The
!>     first one of DAFBFS or DAFBBS to be called initializes the pool.
!>     As new DAFs are searched, DAFBFS and DAFBBS add information about
!>     them to the state table. Every time a search is started by DAFBFS
!>     or DAFBBS, the routine in question `moves' the DAF's state
!>     information to the head of the active list, if the state
!>     information is not already there. This re-organization is
!>     accomplished by deleting the node for the DAF from its current
!>     position in the active list and inserting the node at the head of
!>     the list. Thus, the change is made merely by setting pointers,
!>     not by moving chunks of data in the state table.
!>
!>     It may happen that there is no room left in the state table
!>     to accommodate information about a new DAF. In this case,
!>     garbage collection must be performed:  whichever of DAFBFS or
!>     DAFBBS needs more room frees all nodes in the table that index
!>     DAFs that are not currently open.
!>
!>     Note that the routines DAFGS, DAFGN, DAFRS, DAFRN, and DAFWS do
!>     not modify the state table; they merely act on the current array
!>     in the DAF that is at the head of the active list.
submodule (pepper_instance_module) pepper_daf

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: acclen = 10  !! length of a file access method name (READ, WRITE, SCRATCH, NEW)
   real(wp), parameter :: base = 16.0_wp  !! base of the hexadecimal encoding of numbers in transfer files
   character(*), parameter :: begarr = 'BEGIN_ARRAY'  !! transfer file: the line before an array
   integer(ip), parameter :: bsize = 128  !! number of double precision numbers in a DAF record
   integer(ip), parameter :: bufsiz = 1024  !! size of a buffer of double precision numbers (transfer files)
   integer(ip), parameter :: crecl = 1000  !! length of a character record (an array's name in a transfer file)
   integer(ip), parameter :: crlen = 1000  !! number of characters in a DAF comment record
   integer(ip), parameter :: dprsiz = 128  !! number of double precision numbers in a DAF record
   character(*), parameter :: endarr = 'END_ARRAY'  !! transfer file: the line after an array
   integer(ip), parameter :: errlen = 80  !! length of an error message of a transfer conversion
   character(1), parameter :: expchr = '^'  !! the character before the exponent of an encoded number
   integer(ip), parameter :: fmtlen = 8  !! length of a binary file format name (e.g. BIG-IEEE)
   integer(ip), parameter :: idlen = 8  !! length of a DAF ID word
   integer(ip), parameter :: idwlen = 8  !! length of a file ID word (e.g. DAF/SPK)
   integer(ip), parameter :: ifnlen = 60  !! length of a DAF or DAS internal file name
   integer(ip), parameter :: inteoc = 4  !! character code ending the comment area (EOT)
   integer(ip), parameter :: inteol = 0  !! character code ending a line in a comment record (NUL)
   real(wp), parameter :: invbas = 1.0_wp/base  !! 1/BASE
   integer(ip), parameter :: wpr = 128  !! number of words (double precision numbers) in a DAF record
   integer(ip), parameter :: maxd = wpr  - 3  !! maximum number of double precision numbers of a summary record's summaries
   integer(ip), parameter :: maxc = maxd * 8  !! maximum number of characters of a name record's names
   integer(ip), parameter :: maxdig = 16  !! number of hexadecimal digits of an encoded double precision mantissa
   integer(ip), parameter :: maxenc = 64  !! length of an encoded number in a transfer file
   integer(ip), parameter :: maxi = maxd * 2  !! maximum number of integers of a summary record's summaries
   integer(ip), parameter :: maxnd = 124  !! maximum number of double precision components of a summary
   integer(ip), parameter :: maxndc = 124  !! maximum number of double precision components of a summary (packed)
   integer(ip), parameter :: maxni = 250  !! maximum number of integer components of a summary
   integer(ip), parameter :: maxnic = 250  !! maximum number of integer components of a summary (packed)
   integer(ip), parameter :: maxsum = 125  !! maximum size of a summary (double precision numbers)
   integer(ip), parameter :: mxcrec = 1000  !! number of characters of a comment record that hold comments
   integer(ip), parameter :: nil = -1  !! null pointer of a linked list
   character(1), parameter :: quote = ''''  !! the quote character
   integer(ip), parameter :: rbsize = 100  !! number of records in the record buffer
   integer(ip), parameter :: strlen = 255  !! length of a work string
   character(*), parameter :: totarr = 'TOTAL_ARRAYS'  !! transfer file: the line with the number of arrays
   integer(ip), parameter :: typlen = 4  !! length of a DAF file type (e.g. SPK)
   integer(ip), parameter :: wrksiz = 64  !! number of encoded numbers in a work buffer

contains

      !> Read N encoded integers from a text file, decoding them into
      !> their equivalent integers.
      !>
      !>## Detailed Input
      !>
      !>     UNIT     is the Fortran unit number for a previously opened text
      !>              file. All reading will begin at the CURRENT POSITION
      !>              in the text file.
      !>
      !>     N        is the number of encoded integers to be read from the
      !>              text file attached to UNIT.
      !>
      !>## Detailed Output
      !>
      !>     DATA     is the list of decoded integers read from the text file
      !>              attached to UNIT.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If N, the number of data items, is not positive, the error
      !>         SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>     2)  If an error occurs while reading from the text file attached
      !>         to UNIT, the error SPICE(FILEREADFAILED) is signaled.
      !>
      !>     3)  If an error occurs while decoding a number, the error
      !>         SPICE(DECODINGERROR) is signaled.
      !>
      !>## Files
      !>
      !>     See the description of UNIT in $Detailed_Input.
      !>
      !>## Particulars
      !>
      !>     This routine will read N encoded integers beginning at the
      !>     current position in a previously opened text file. The current
      !>     position in a file is defined to be the text line immediately
      !>     following the last text line that was written or read. The
      !>     integers will be decoded and placed into a list of integers
      !>     which will be passed back to the caller. The encoded integers
      !>     are represented as quoted character strings so that a Fortran
      !>     list directed read may be used to read the encoded values,
      !>     rather than a formatted read with the format specifier
      !>     FMT = '(A)'.
      !>
      !>     This routine is one of a pair of routines which are used to
      !>     encode and decode integers:
      !>
      !>           WRENCI -- Encode and write integers to a file.
      !>           RDENCI -- Read and decode integers from a file.
      !>
      !>     The encoding/decoding of integers is performed to provide a
      !>     portable means for transferring data values.
      !>
      !>     Currently the encoded integers are represented as signed
      !>     hexadecimal numbers See INT2HX.FOR and HX2INT.FOR for details.
      !>
      !>## Examples
      !>
      !>     Suppose we have the following input file which contains the values
      !>     1 - 100 encoded, and that the input file has already been opened
      !>     for reading. The arrow, '-->', indicates the current position in
      !>     the file.
      !>
      !>        -->'1' '2' '3' '4' '5' '6' '7' '8' '9' 'A' 'B' 'C' 'D' 'E'
      !>           'F' '10' '11' '12' '13' '14' '15' '16' '17' '18' '19'
      !>           '1A' '1B' '1C' '1D' '1E' '1F' '20' '21' '22' '23' '24'
      !>           '25' '26' '27' '28' '29' '2A' '2B' '2C' '2D' '2E' '2F'
      !>           '30' '31' '32' '33' '34' '35' '36' '37' '38' '39' '3A'
      !>           '3B' '3C' '3D' '3E' '3F' '40'
      !>           '41' '42' '43' '44' '45' '46' '47' '48' '49' '4A' '4B'
      !>           '4C' '4D' '4E' '4F' '50' '51' '52' '53' '54' '55' '56'
      !>           '57' '58' '59' '5A' '5B' '5C' '5D' '5E' '5F' '60' '61'
      !>           '62' '63' '64'
      !>
      !>     Then, the following code fragment would read and decode these
      !>     100 values.
      !>
      !>           N = 100
      !>           CALL RDENCI( UNIT, N, DATA )
      !>
      !>      Upon returning, the array data would contain the values 1 - 100.
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
      !>-    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed I/O type
      !>        of argument DATA in $Brief_I/O table.
      !>
      !>-    SPICELIB Version 1.0.0, 19-OCT-1992 (KRG)
      module subroutine rdenci ( ctx, unit, n, data )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: unit  !! Fortran unit number of input text file.
      integer(ip), intent(in) :: n  !! Number of integers to read and decode.
      integer(ip), intent(out) :: data(*)  !! List of decoded integers.

      character(errlen) :: errmsg
      character(maxenc) :: work(wrksiz)

      integer(ip) :: i
      integer(ip) :: iostat
      integer(ip) :: itmbeg
      integer(ip) :: nitms

      logical :: error

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     Check to see if the number of data items is less than or equal
!     to zero. If it is, signal an error.
!
      if ( n < 1 ) then

         call ctx%setmsg ( 'The number of data items to be read was' // &
                       ' not positive: #.'                        )
         call ctx%errint ( '#', n                                     )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)'                   )
         return

      end if
!
!     Initialize the beginning location to place the decoded data
!     items.
!
      itmbeg = 1
!
!     We read in the encoded numbers in blocks of size WRKSIZ, and if
!     there was not a read error we will attempt to decode the numbers.
!     We signal an error if either:
!
!                (1) there is a read error
!                (2) there is an error decoding a number.
!
!     NOTE: EOF is interpreted as a read error because we know a priori
!           exactly how many data items we need to read: N.
!
!     Begin decoding the encoded data items read from the input file
!     in blocks of size NITMS. Each time the number of data items
!     NITMS is reached, decode the encoded numbers into the data array.
!
      do while ( itmbeg <= n )
!
!        The number of items is either the size of the workspace, or
!        the number of data items which remain to be processed, which
!        should always be less than or equal to the size of the
!        workspace.
!
         nitms = min ( wrksiz, n - itmbeg + 1 )
!
!        Read in a block of data items to be decoded.
!
         read (unit,*,iostat=iostat) ( work(i), i = 1, nitms )
!
!        Check to see if we got a read error: IOSTAT .NE. 0. If we did,
!        then signal an error. EOF is considered to be a read error,
!        since we know exactly how many data items we expect to read.
!
         if ( iostat /= 0 ) then

            call ctx%setmsg ( 'Error reading from logical unit #,' // &
                          ' IOSTAT = #.'                        )
            call ctx%errint ( '#', unit                             )
            call ctx%errint ( '#', iostat                           )
            call ctx%sigerr ( 'SPICE(FILEREADFAILED)'               )
            return

         end if
!
!        Begin to decode the data items into the data array. Signal an
!        error if we cannot decode a data item.
!
         do i = 1, nitms

            call hx2int ( ctx, work(i), data(itmbeg + i - 1), error, errmsg )
            if ( error ) then

               call ctx%setmsg ( 'Decoding error occurred while'       // &
                             ' attempting to decode item #: #. #'   )
               call ctx%errint ( '#', i                                 )
               call ctx%errch  ( '#', work(i)                           )
               call ctx%errch  ( '#', errmsg                            )
               call ctx%sigerr ( 'SPICE(DECODINGERROR)'                 )
               return

            end if

         end do
!
!        Position the data item pointer at the next location to begin
!        placing the decoded items in the array DATA, and continue
!        processing the until done.
!
         itmbeg = itmbeg + nitms

      end do

      return
      end subroutine rdenci

      !> Convert a string representing a double precision number in a
      !> base 16 "scientific notation" into its equivalent double
      !> precision number.
      !>
      !>## Detailed Input
      !>
      !>     STRING   is a character string containing a base 16 "scientific
      !>              notation" representation of a double precision number
      !>              which is to be converted to a double precision number.
      !>              Examples of such a string are:
      !>
      !>                 '2A^3' = ( 2/16 + 10/( 16**2 ) ) * 16**3 = 672.0
      !>
      !>              and
      !>
      !>                 '-B^1' = - ( 11/16 ) * 16**1             = -11.0
      !>
      !>              The following table describes the character set used to
      !>              represent the hexadecimal digits and their corresponding
      !>              values.
      !>
      !>                 Character     Value         Character     Value
      !>                 ---------    -------        ---------    -------
      !>                    '0'         0.0D0           '8'         8.0D0
      !>                    '1'         1.0D0           '9'         9.0D0
      !>                    '2'         2.0D0         'A','a'      10.0D0
      !>                    '3'         3.0D0         'B','b'      11.0D0
      !>                    '4'         4.0D0         'C','c'      12.0D0
      !>                    '5'         5.0D0         'D','d'      13.0D0
      !>                    '6'         6.0D0         'E','e'      14.0D0
      !>                    '7'         7.0D0         'F','f'      15.0D0
      !>
      !>              The caret, or hat, character, '^', is used to
      !>              distinguish the exponent.
      !>
      !>              The plus sign, '+', and the minus sign, '-', are used,
      !>              and they have their usual meanings.
      !>
      !>              A base 16 "scientific notation" character string which
      !>              is to be parsed by this routine should consist of a sign,
      !>              '+' or '-' (the plus sign is optional for nonnegative
      !>              numbers), followed immediately by a contiguous sequence
      !>              of hexadecimal digits, the exponent character, and a
      !>              signed hexadecimal exponent. The exponent is required,
      !>              but the sign is optional for a nonnegative exponent.
      !>
      !>              A number in base 16 "scientific notation" consists of
      !>              a contiguous sequence of characters with one of the
      !>              following formats:
      !>
      !>                 (1)   h h h h  ... h ^H H  ... H
      !>                        1 2 3 4      n  1 2      m
      !>
      !>                 (2)   +h h h h  ... h ^H H  ... H
      !>                         1 2 3 4      n  1 2      m
      !>
      !>                 (3)   -h h h h  ... h ^H H  ... H
      !>                         1 2 3 4      n  1 2      m
      !>
      !>                 (4)    h h h h  ... h ^+H H  ... H
      !>                         1 2 3 4      n   1 2      m
      !>
      !>                 (5)   +h h h h  ... h ^+H H  ... H
      !>                         1 2 3 4      n   1 2      m
      !>
      !>                 (6)   -h h h h  ... h ^+H H  ... H
      !>                         1 2 3 4      n   1 2      m
      !>
      !>                 (7)   h h h h  ... h ^-H H  ... H
      !>                        1 2 3 4      n   1 2      m
      !>
      !>                 (8)   +h h h h  ... h ^-H H  ... H
      !>                         1 2 3 4      n   1 2      m
      !>
      !>                 (9)   -h h h h  ... h ^-H H  ... H
      !>                         1 2 3 4      n   1 2      m
      !>
      !>              where
      !>
      !>                 h  and H  denote hexadecimal digits;
      !>                  i      j
      !>
      !>                 ^         denotes exponentiation;
      !>
      !>              and
      !>
      !>                 + and - have their usual interpretations.
      !>
      !>              STRING may have leading and trailing blanks, but blanks
      !>              embedded within the significant portion of the input
      !>              string are not allowed.
      !>
      !>## Detailed Output
      !>
      !>     NUMBER   is the double precision value to be returned. The value
      !>              of this argument is not changed if an error occurs while
      !>              parsing the input string.
      !>
      !>     ERROR    is a logical flag which indicates whether an error
      !>              occurred while attempting to parse NUMBER from the input
      !>              character string STRING. ERROR will have the value
      !>              .TRUE. if an error occurs. It will have the value
      !>              .FALSE. otherwise.
      !>
      !>     ERRMSG   is a descriptive error message if an error occurs while
      !>              attempting to parse the number NUMBER from the
      !>              hexadecimal character string STRING, blank otherwise.
      !>
      !>## Parameters
      !>
      !>     MAXMAN   is the maximum number of digits in a hexadecimal
      !>              mantissa. The value of MAXMAN is 31.
      !>
      !>              The current value of MAXMAN is more than sufficient for
      !>              most double precision implementations, providing almost
      !>              twice as many digits as can actually be produced. This
      !>              value may be changed when a greater precision is known
      !>              to exist among all of the supported platforms.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If an unexpected character is encountered, an appropriate
      !>         error message will be set, and the routine will exit. The
      !>         value of NUMBER will be unchanged.
      !>
      !>     2)  If the input string represents a number that is larger in
      !>         absolute magnitude than the maximum representable
      !>         double precision number an appropriate error message
      !>         will be set, and the routine will exit. The value of
      !>         NUMBER will be unchanged.
      !>
      !>     3)  If the input string is blank, an appropriate error message
      !>         will be set, and the routine will exit. The value of
      !>         NUMBER will be unchanged.
      !>
      !>     4)  If the string has too many digits in the mantissa, then an
      !>         appropriate error message will be set, and the routine will
      !>         exit. The value of NUMBER will be unchanged.
      !>
      !>     5)  If the output error message string is not long enough to
      !>         contain the entire error message, the error message will be
      !>         truncated on the right.
      !>
      !>     6)  This routine does NOT check for underflow errors when
      !>         constructing a double precision number.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine will convert a character string containing a number
      !>     in base 16 "scientific notation" into its equivalent double
      !>     precision number.
      !>
      !>     This routine is one of a pair of routines which are used to
      !>     perform conversions between double precision numbers and
      !>     an equivalent base 16 "scientific notation" character string
      !>     representation:
      !>
      !>           DP2HX  -- Convert a double precision number into a base 16
      !>                     "scientific notation" character string.
      !>
      !>           HX2DP  -- Convert a base 16 "scientific notation"
      !>                     character string into a double precision number.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Convert a set of character strings containing a base 16
      !>        "scientific notation" representation of a double precision
      !>        number, to their double precision values.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM HX2DP_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER               ERRLEN
      !>      PARAMETER           ( ERRLEN = 80 )
      !>
      !>      INTEGER               STRLEN
      !>      PARAMETER           ( STRLEN = 17 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(ERRLEN)    ERRMSG
      !>      CHARACTER*(STRLEN)    NUMBER ( 16 )
      !>
      !>      DOUBLE PRECISION      VALUE
      !>
      !>      INTEGER               I
      !>
      !>      LOGICAL               ERROR
      !>
      !>C
      !>C     Assign an array of strings representing, in base 16
      !>C     "scientific notation", double precision numbers.
      !>C     Not all of them are valid representations.
      !>C
      !>      DATA                  NUMBER /
      !>     .                  '89705F4136B4A6^-7', '12357898765X34',
      !>     .                  '1^1',               '-1^1',
      !>     .                  '4^3',               '-4^3',
      !>     .                  '7F5EB^5',           '7F5eb^5',
      !>     .                  '1B^2',              '+1B^2',
      !>     .                  '+1B^+2',            '0^0',
      !>     .                  ' ',                 '-AB238Z^2',
      !>     .                  '234ABC',            '234ABC^'    /
      !>
      !>C
      !>C     Loop over the NUMBER array, call HX2DP for each
      !>C     element of NUMBER.
      !>C
      !>      WRITE(*,'(A)') 'string             number'
      !>      WRITE(*,'(A)') '-----------------  ----------------'
      !>
      !>      DO I= 1, 16
      !>
      !>         CALL HX2DP ( NUMBER(I), VALUE, ERROR, ERRMSG )
      !>
      !>         IF ( ERROR ) THEN
      !>
      !>            WRITE(*,'(A17,2X,A)') NUMBER(I), ERRMSG
      !>
      !>         ELSE
      !>
      !>            WRITE(*,'(A17,X,E17.9)') NUMBER(I), VALUE
      !>
      !>         END IF
      !>
      !>      END DO
      !>
      !>C
      !>C     Finally, try with a number that has too many digits in
      !>C     the mantissa.
      !>C
      !>      CALL HX2DP ( '4ABC123AB346523BDC568798C2473678^1',
      !>     .             VALUE, ERROR, ERRMSG )
      !>
      !>      WRITE(*,*)
      !>      WRITE(*,*) 'String 4ABC123AB346523BDC568798C2473678^1 '
      !>     .        // 'produces:'
      !>      WRITE(*,*) '   ', ERRMSG
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        string             number
      !>        -----------------  ----------------
      !>        89705F4136B4A6^-7   0.200000000E-08
      !>        12357898765X34     ERROR: Illegal character 'X' encountered.
      !>        1^1                 0.100000000E+01
      !>        -1^1               -0.100000000E+01
      !>        4^3                 0.102400000E+04
      !>        -4^3               -0.102400000E+04
      !>        7F5EB^5             0.521707000E+06
      !>        7F5eb^5             0.521707000E+06
      !>        1B^2                0.270000000E+02
      !>        +1B^2               0.270000000E+02
      !>        +1B^+2              0.270000000E+02
      !>        0^0                 0.000000000E+00
      !>                           ERROR: A blank input string is not allowed.
      !>        -AB238Z^2          ERROR: Illegal character 'Z' encountered.
      !>        234ABC             ERROR: Missing exponent.
      !>        234ABC^            ERROR: Missing exponent.
      !>
      !>         String 4ABC123AB346523BDC568798C2473678^1 produces:
      !>            ERROR: Too many digits in the mantissa (> 31).
      !>
      !>        Note: The hat or caret, '^', signals an exponent.
      !>
      !>        Note that some errors are machine dependent. For example,
      !>        for a VAX using D_floating arithmetic we get:
      !>
      !>           STRING = '23BCE^30'
      !>           NUMBER = ( Not defined )
      !>           ERROR  = .TRUE.
      !>           ERRMSG = 'ERROR: Number is too large to be represented.'
      !>
      !>           STRING = '-2abc3^22'
      !>           NUMBER = ( Not defined )
      !>           ERROR  = .TRUE.
      !>           ERRMSG = 'ERROR: Number is too small to be represented.'
      !>
      !>## Restrictions
      !>
      !>     1)  The current value of MAXMAN is more than sufficient for most
      !>         double precision implementations, providing almost twice as
      !>         many digits as can actually be produced.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) (BVS)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        The declaration of MAXMAN has been promoted to the
      !>        $Declarations section and the error produced when the maximum
      !>        number of digits for the mantissa is exceeded has been updated
      !>        to inform about MAXMAN value.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example based on existing example.
      !>
      !>        Updated $Brief_I/O, $Parameters, $Exceptions and $Restrictions
      !>        sections to properly describe MAXMAN.
      !>
      !>        Corrected $Revisions entries.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1994 (KRG)
      !>
      !>        Fixed a typo in the description of the input argument STRING.
      !>        The example showing the expansion of 160 into hexadecimal
      !>        was incorrect. 160 was replaced with 672 which makes the
      !>        example correct.
      !>
      !>-    SPICELIB Version 1.0.0, 26-OCT-1992 (KRG)
      module subroutine hx2dp ( ctx, string, number, error, errmsg )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: dpmax, dpmin
      use pepper_strings, only: repmc_inplace, repmi_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: string  !! Hex form string to convert to double precision.
      real(wp), intent(out) :: number  !! Double precision value to be returned.
      logical, intent(out) :: error  !! A logical flag which is .TRUE. on error.
      character(*), intent(out) :: errmsg  !! A descriptive error message.
      integer(ip), parameter :: maxman = 31
      real(wp), parameter :: dpval(0:maxdig-1) = &
         [ 0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, &
           7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp, 11.0_wp, 12.0_wp, 13.0_wp, &
           14.0_wp, 15.0_wp ]
      real(wp) :: tmpnum
      integer(ip) :: i
      integer(ip) :: iexpon
      integer(ip) :: ival(maxman+1)
      integer(ip) :: letter
      integer(ip) :: ndigit
      integer(ip) :: positn
      integer(ip) :: strbeg
      integer(ip) :: strend
      logical :: more
      logical :: negtiv
      logical :: fndexp
      if ( .not. allocated ( ctx%hx2dp_state ) ) allocate ( ctx%hx2dp_state )
      associate ( maxdp => ctx%hx2dp_state%maxdp, &
                  mindp => ctx%hx2dp_state%mindp, &
                  scales => ctx%hx2dp_state%scales, &
                  digbeg => ctx%hx2dp_state%digbeg, &
                  digend => ctx%hx2dp_state%digend, &
                  iexpch => ctx%hx2dp_state%iexpch, &
                  iminus => ctx%hx2dp_state%iminus, &
                  iplus => ctx%hx2dp_state%iplus, &
                  ispace => ctx%hx2dp_state%ispace, &
                  lccbeg => ctx%hx2dp_state%lccbeg, &
                  lccend => ctx%hx2dp_state%lccend, &
                  uccbeg => ctx%hx2dp_state%uccbeg, &
                  uccend => ctx%hx2dp_state%uccend, &
                  first => ctx%hx2dp_state%first )
      if ( first ) then
!
!        If this is the first call, set up the array that is used to
!        properly scale each of the hexadecimal digits when summing
!        them to build a double precision number. Right now, the value
!        of MAXMAN, the maximum number of digits in a hexadecimal
!        mantissa, is 31. MAXMAN = 31 is more than sufficient for most
!        current double precision implementations, providing almost
!        twice as many digits as can actually be produced. This value
!        may be changed when a greater precision is known to exist on
!        any of the supported platforms.
!
         first = .false.

         scales(1) = invbas
         do i = 2, maxman

            scales(i) = invbas * scales(i-1)

         end do
!
!        Initialize the upper and lower bounds for the decimal digits,
!        the upper and lower bounds for the uppercase hexadecimal
!        digits, the upper and lower bounds for the lowercase
!        hexadecimal digits, the space, the plus sign, and the minus
!        sign in the character sequence.
!
         digbeg = ichar( '0' )
         digend = ichar( '9' )
         uccbeg = ichar( 'A' )
         uccend = ichar( 'F' )
         lccbeg = ichar( 'a' )
         lccend = ichar( 'f' )
         iminus = ichar( '-' )
         iplus  = ichar( '+' )
         ispace = ichar( ' ' )
!
!        Also get the integer value for the exponent character.
!
         iexpch = ichar( expchr )
!
!        Initialize some boundary values for error checking while
!        constructing the desired double precision number. These
!        are used to help determine whether an overflow condition
!        is imminent due to the overly large magnitude of a positive
!        or negative number.
!
         mindp = dpmin * invbas
         maxdp = dpmax * invbas

      end if
!
!     There are no errors initially, so set the error flag to
!     .FALSE.
!
      error = .false.
!
!     If the string is blank, set the error flag and return immediately.
!
      if ( string == ' ' ) then

         error  = .true.
         errmsg = 'ERROR: A blank input string is not allowed.'
         return

      end if
!
!     Initialize a few other things.
!
      errmsg = ' '
      tmpnum = 0.0_wp
!
!     Assume that the number is nonnegative.
!
      negtiv = .false.
!
!     Skip any leading white space. We know that there is at least
!     one nonblank character at this point, so we will not loop
!     off the end of the string.
!
      strbeg = 1

      do while ( ichar( string(strbeg:strbeg ) ) == ispace )

         strbeg = strbeg + 1

      end do
!
!     Now, we want to find the end of the significant portion of
!     the input string and the position of the exponent character.
!
      strend = strbeg + 1
      more   = .true.

      do while ( more )

         if ( strend <= len( string ) ) then

            if ( string(strend:) /= ' ' ) then

               strend = strend + 1

            else

               more = .false.

            end if

         else

            more = .false.

         end if

      end do
!
!     At this point, STREND is one larger than the length of the
!     significant portion of the string because we incremented
!     its value after the test. We will subtract one from the
!     value of STREND so that it exactly represents the position
!     of the last significant character in the string.
!
      strend = strend - 1
!
!     Set the position pointer to the beginning of the significant
!     part, i.e., the nonblank part, of the string, because we are
!     now ready to try and parse the number.
!
      positn = strbeg
!
!     The first character should be either a plus sign, '+', a
!     minus sign, '-', or a digit, '0' - '9', 'A' - 'F', or
!     'a' - 'f'. Anything else is bogus and we will catch it in
!     the main loop below.
!
!     If the character is a minus sign, we want to set the value of
!     NEGTIV to .TRUE. and increment the position.
!
!     If the character is a plus sign, we want to increment the
!     position.
!
      if ( ichar( string(positn:positn) ) == iminus ) then

         negtiv = .true.
         positn = positn + 1

      else if ( ichar( string(positn:positn) ) == iplus ) then

         positn = positn + 1

      end if
!
!     Collect all of the digits in the mantissa, storing them
!     for later conversion. We do this because we want to add
!     the digits of the mantissa in increasing order so that we
!     do not lose any significance.
!
!     A normalized hexadecimal number must have an exponent,
!     which is represented by the hat character, EXPCHR, which
!     s why that test is part of the loop termination.
!
!     We currently have no digits, and we have not found the
!     exponent character yet.
!
      ndigit = 0
      fndexp = .false.
      do while ( ( positn <= strend ) .and. ( .not. fndexp ) )

         letter = ichar( string(positn:positn) )

         if ( ( letter >= digbeg ) .and. &
              ( letter <= digend ) ) then

            positn       = positn + 1
            ndigit       = ndigit + 1
            ival(ndigit) = letter - digbeg

         else if ( ( letter >= uccbeg ) .and. &
                   ( letter <= uccend ) ) then

            positn       = positn + 1
            ndigit       = ndigit + 1
            ival(ndigit) = 10 + letter - uccbeg

         else if ( ( letter >= lccbeg ) .and. &
                   ( letter <= lccend ) ) then

            positn       = positn + 1
            ndigit       = ndigit + 1
           ival(ndigit) = 10 + letter - lccbeg

         else if ( letter == iexpch ) then
!
!           We have found the exponent character, so set the
!           indicator and increment the position.
!
            fndexp = .true.
            positn = positn + 1

         else

            error  = .true.
            errmsg = 'ERROR: Illegal character ''#'' encountered.'
            call repmc_inplace ( errmsg, '#', char( letter ) )
            return

         end if
!
!        We need to make sure that the number of mantissa digits
!        remains less than or equal to the number of mantissa
!        digits that we declared, see the MAXMAN parameter.
!
         if ( ndigit > maxman ) then

            error  = .true.
            errmsg = 'ERROR: Too many digits in the mantissa (> #).'
            call repmi_inplace ( errmsg, '#', maxman )
            return

         end if

      end do
!
!     At this point, we have found an exponent character, and:
!
!        1) We are beyond the end of the significant portion of the
!           string, which is an error: no exponent digits were found.
!
!        2) We are positioned on the first digit of the exponent,
!           and are ready to try and parse it.
!
      if ( positn <= strend ) then
!
!        If there is at least one significant character left in the
!        string, we need to try and parse it as an exponent.
!
         call hx2int( ctx, string(positn:), iexpon, error, errmsg )

         if ( error ) then
!
!           If an error occurred while attempting to parse the
!           exponent, we simply want to exit. The error message
!           is already set.
!
            return

         end if

      else

         error  = .true.
         errmsg = 'ERROR: Missing exponent.'
         return

      end if
!
!     We now have everything that we need to build the double
!     precision number, a mantissa and an exponent. So, let's
!     start building the number. We need to be careful that we
!     do not overflow when we scale the number using the exponent.
!
!     First, we build up the mantissa ...
!
      if ( negtiv ) then

         do while ( ndigit > 0 )

            tmpnum = tmpnum - dpval(ival(ndigit)) * scales(ndigit)
            ndigit = ndigit - 1

         end do

      else

         do while ( ndigit > 0 )

            tmpnum = tmpnum + dpval(ival(ndigit)) * scales(ndigit)
            ndigit = ndigit - 1

         end do

      end if
!
!     At this point, one of the following is true:
!
!        1)  -1     <  TMPNUM <= -1/BASE
!
!        2)  1/BASE <= TMPNUM <  1
!
!     or
!
!        3) TMPNUM = 0.0D0
!
!     Now we to scale the normalized number using the exponent. If
!     the exponent is zero, we will simply fall through the loop
!     structures below at no greater cost than a few comparisons.
!
      if ( iexpon < 0 ) then
!
!        We do not check for any sort of underflow conditions.
!
         do i = 1, -iexpon

            tmpnum = tmpnum * invbas

         end do

      else

         if ( negtiv ) then

            do i = 1, iexpon

               if ( tmpnum >= mindp ) then

                  tmpnum = tmpnum * base

               else

                  error  = .true.
                  errmsg = 'ERROR: Number is too small to be'// &
                           ' represented.'
                  return

               end if

            end do

         else

            do i = 1, iexpon

               if ( tmpnum <= maxdp ) then

                  tmpnum = tmpnum * base

               else

                  error  = .true.
                  errmsg = 'ERROR: Number is too large to be'// &
                           ' represented.'
                  return

               end if

            end do

         end if

      end if
!
!     If we got to here, we have successfully parsed the hexadecimal
!     string into a double precision number. So, set the value and
!     return.
!
      number = tmpnum

      return
      end associate
      end subroutine hx2dp

      !> Read N encoded d.p. numbers from a text file, decoding them
      !> into their equivalent d.p. numbers.
      !>
      !>## Detailed Input
      !>
      !>     UNIT     is the Fortran unit number for a previously opened text
      !>              file. All reading will begin at the CURRENT POSITION
      !>              in the text file.
      !>
      !>     N        is the number of encoded double precision numbers, to be
      !>              read from the text file attached to UNIT.
      !>
      !>## Detailed Output
      !>
      !>     DATA     is the list of decoded double precision numbers read from
      !>              the text file attached to UNIT.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If N, the number of data items, is not positive, the error
      !>         SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>     2)  If an error occurs while reading from the text file attached
      !>         to UNIT, the error SPICE(FILEREADFAILED) is signaled.
      !>
      !>     3)  If an error occurs while decoding a number, the error
      !>         SPICE(DECODINGERROR) is signaled.
      !>
      !>## Files
      !>
      !>     See the description of UNIT in $Detailed_Input.
      !>
      !>## Particulars
      !>
      !>     This routine will read N encoded double precision numbers from
      !>     the current position in a previously opened text file. The
      !>     current position in a file is defined to be the text line
      !>     immediately following the last text line that was written or
      !>     read. The numbers will be decoded and placed into a list of
      !>     double precision numbers which will be passed back to the caller.
      !>     The encoded double precision numbers are represented as quoted
      !>     character strings so that a Fortran list directed read may be
      !>     used to read the encoded values, rather than a formatted read
      !>     with the format specifier FMT = '(A)'.
      !>
      !>     This routine is one of a pair of routines which are used to
      !>     encode and decode d.p. numbers:
      !>
      !>           WRENCD -- Encode and write d.p. numbers to a file.
      !>           RDENCD -- Read and decode d.p. numbers from a file.
      !>
      !>     The encoding/decoding of d.p. numbers is performed to provide a
      !>     portable means for transferring data values.
      !>
      !>     Currently the encoded d.p. numbers are represented in a base
      !>     16 ``scientific notation.'' See DP2HX.FOR and HX2DP.FOR for
      !>     details.
      !>
      !>## Examples
      !>
      !>     Suppose we have the following input file which contains the
      !>     values 1.0D0 - 100.0D0 in encoded format, and that the input
      !>     file has already been opened for reading. The arrow, '-->',
      !>     indicates the current position in the file.
      !>
      !>        -->'1^1' '2^1' '3^1' '4^1' '5^1' '6^1' '7^1' '8^1' '9^1'
      !>           'A^1' 'B^1' 'C^1' 'D^1' 'E^1' 'F^1' '1^2' '11^2' '12^2'
      !>           '13^2' '14^2' '15^2' '16^2' '17^2' '18^2' '19^2' '1A^2'
      !>           '1B^2' '1C^2' '1D^2' '1E^2' '1F^2' '2^2' '21^2' '22^2'
      !>           '23^2' '24^2' '25^2' '26^2' '27^2' '28^2' '29^2' '2A^2'
      !>           '2B^2' '2C^2' '2D^2' '2E^2' '2F^2' '3^2' '31^2' '32^2'
      !>           '33^2' '34^2' '35^2' '36^2' '37^2' '38^2' '39^2' '3A^2'
      !>           '3B^2' '3C^2' '3D^2' '3E^2' '3F^2' '4^2'
      !>           '41^2' '42^2' '43^2' '44^2' '45^2' '46^2' '47^2' '48^2'
      !>           '49^2' '4A^2' '4B^2' '4C^2' '4D^2' '4E^2' '4F^2' '5^2'
      !>           '51^2' '52^2' '53^2' '54^2' '55^2' '56^2' '57^2' '58^2'
      !>           '59^2' '5A^2' '5B^2' '5C^2' '5D^2' '5E^2' '5F^2' '6^2'
      !>           '61^2' '62^2' '63^2' '64^2'
      !>
      !>     Then the following code fragment would read and decode these
      !>     100 values.
      !>
      !>           N = 100
      !>           CALL RDENCD( UNIT, N, DATA )
      !>
      !>      Upon returning, the array data would contain the values
      !>      1.0D0 - 100.0D0.
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
      !>     F.S. Turner        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 03-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed I/O type
      !>        of argument DATA in $Brief_I/O table.
      !>
      !>-    SPICELIB Version 1.1.0, 19-MAR-1999 (FST)
      !>
      !>        Modified the long error message for SPICE(FILEREADFAILED)
      !>        to indicate the possibility of an incomplete text transfer
      !>        file as the cause.
      !>
      !>-    SPICELIB Version 1.0.0, 20-OCT-1992 (KRG)
      module subroutine rdencd ( ctx, unit, n, data )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: unit  !! Fortran unit number of input text file.
      integer(ip), intent(in) :: n  !! Number of d.p. numbers to read and decode.
      real(wp), intent(out) :: data(*)  !! List of decoded d.p. numbers.

      character(errlen) :: errmsg
      character(maxenc) :: work(wrksiz)

      integer(ip) :: i
      integer(ip) :: itmbeg
      integer(ip) :: nitms
      integer(ip) :: iostat

      logical :: error

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to see if the number of data items is less than or equal
!     to zero. If it is, signal an error.
!
      if ( n < 1 ) then

         call ctx%setmsg ( 'The number of data items to be read was' // &
                       ' not positive: #.'                        )
         call ctx%errint ( '#', n                                     )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)'                   )
         return

      end if
!
!     Initialize the beginning location to place the decoded data
!     items.
!
      itmbeg = 1
!
!     We read in the encoded numbers in blocks of size WRKSIZ, and if
!     there was not a read error we will attempt to decode the numbers.
!     We signal an error if either:
!
!                (1) there is a read error
!                (2) there is an error decoding the number.
!
!     NOTE: EOF is interpreted as a read error because we know a priori
!           exactly how many data items we need to read: N.
!
!     Begin decoding the encoded data items read from the input file
!     in blocks of size NITMS. Each time the number of data items
!     NITMS is reached, decode the encoded numbers into the data array.
!
      do while ( itmbeg <= n )
!
!        The number of items is either the size of the workspace, or
!        the number of data items which remain to be processed, which
!        should always be less than or equal to the size of the
!        workspace.
!
         nitms = min ( wrksiz, n - itmbeg + 1 )
!
!        Read in a block of data items to be decoded.
!
         read (unit,iostat=iostat,fmt=*) ( work(i), i = 1, nitms )
!
!        Check to see if we got a read error: IOSTAT .NE. 0. If we did,
!        then signal an error. EOF is considered to be a read error,
!        since we know exactly how many data items we expect to read.
!
         if ( iostat /= 0 ) then

            call ctx%setmsg ( 'Error reading from logical unit #,'  // &
                          ' IOSTAT = #. One possible cause is'  // &
                          ' an incomplete text transfer file.'   )
            call ctx%errint ( '#', unit                              )
            call ctx%errint ( '#', iostat                            )
            call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                )
            return

         end if
!
!        Begin to decode the data items into the data array. Signal an
!        error if we cannot decode a data item.
!
         do i = 1, nitms

            call hx2dp ( ctx, work(i), data(itmbeg + i - 1), error, errmsg )
            if ( error ) then

               call ctx%setmsg ( 'Decoding error occurred while'     // &
                          ' attempting to decode item #: #. #'    )
               call ctx%errint ( '#', i                               )
               call ctx%errch  ( '#', work(i)                         )
               call ctx%errch  ( '#', errmsg                          )
               call ctx%sigerr ( 'SPICE(DECODINGERROR)'               )
               return

            end if

         end do
!
!        Position the data item pointer at the next location to begin
!        placing the decoded items in the array DATA, and continue
!        processing the until done.
!
         itmbeg = itmbeg + nitms

      end do

      return
      end subroutine rdencd

      !> Open a new text file for subsequent write access.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of the new text file to be opened.
      !>
      !>## Detailed Output
      !>
      !>     UNIT     is the logical unit connected to the opened file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the file cannot be opened, the error SPICE(FILEOPENFAILED)
      !>         is signaled.
      !>
      !>     2)  If FNAME is a blank string, the error SPICE(BLANKFILENAME) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     See FNAME and UNIT above.
      !>
      !>## Particulars
      !>
      !>     In SPICELIB, a text file is formatted and sequential and may
      !>     contain only printable ASCII characters and blanks (ASCII 32-127).
      !>     When printing a text file, records are single spaced; the first
      !>     character will not be interpreted as a carriage control character.
      !>
      !>     TXTOPN opens a new text file and makes use of the SPICELIB
      !>     mechanism for coordinating the use of logical units.
      !>
      !>     System Dependencies
      !>     ===================
      !>
      !>     The open statement will include the following keyword = value
      !>     pairs:
      !>
      !>            UNIT   =  UNIT
      !>            FILE   =  FNAME
      !>            FORM   = 'FORMATTED'
      !>            ACCESS = 'SEQUENTIAL'
      !>            STATUS = 'NEW'
      !>            IOSTAT =  IOSTAT
      !>
      !>     In addition, the statement will include
      !>
      !>            CARRIAGECONTROL = 'LIST'
      !>
      !>     for the Vax and Macintosh.
      !>
      !>## Examples
      !>
      !>     The following example reads a line from an input file,
      !>     'INPUT.TXT', and writes it to an output file, 'OUTPUT.TXT'.
      !>
      !>        CALL TXTOPR ( 'INPUT.TXT',  IN  )
      !>        CALL TXTOPN ( 'OUTPUT.TXT', OUT )
      !>
      !>        READ  ( IN,  FMT='(A)' ) LINE
      !>        WRITE ( OUT, FMT='(A)' ) LINE
      !>
      !>        CLOSE ( IN  )
      !>        CLOSE ( OUT )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     [1]  "Language Reference Manual", Absoft Fortran V3.2, page 7-12
      !>          (for the NeXT), 1993.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.E. McLean        (JPL)
      !>     H.A. Neilan        (JPL)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.27.0, 28-NOV-2021 (BVS)
      !>
      !>        Updated for MAC-OSX-M1-64BIT-CLANG_C.
      !>
      !>-    SPICELIB Version 2.26.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
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
      !>-    SPICELIB Version 2.0.5, 17-JUL-2002 (BVS)
      !>
      !>        Added MAC-OSX environments.
      !>
      !>-    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 2.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 2.0.0, 07-APR-1998 (NJB)
      !>
      !>        Module was updated for the PC-LINUX platform.
      !>
      !>-    SPICELIB Version 1.2.0, 11-NOV-1993 (HAN)
      !>
      !>        Module was updated for the Silicon Graphics, DEC Alpha-OSF/1,
      !>        and NeXT platforms.
      !>
      !>-    SPICELIB Version 1.1.0, 12-OCT-1992 (HAN)
      !>
      !>        The code was reformatted so that a utility program can
      !>        create the source file for a specific environment given a
      !>        master source file.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 07-APR-1998 (NJB)
      !>
      !>        Module was updated for the PC-LINUX platform.
      !>
      !>-    SPICELIB Version 1.2.0, 11-NOV-1993 (HAN)
      !>
      !>        Module was updated for the Silicon Graphics, DEC Alpha-OSF/1,
      !>        and NeXT platforms.
      !>
      !>-    SPICELIB Version 1.1.0, 12-OCT-1992 (HAN)
      !>
      !>        The code was reformatted so that a utility program can
      !>        create the source file for a specific environment given a
      !>        master source file.
      module subroutine txtopn ( ctx, fname, unit )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of file.
      integer(ip), intent(out) :: unit  !! Logical unit.

      integer(ip) :: iostat

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      if ( fname == ' ' ) then
         call ctx%setmsg ( 'A blank string is unacceptable as a file name' )
         call ctx%sigerr ( 'SPICE(BLANKFILENAME)'                          )
         return
      end if

!     PEPPER: the unit comes from NEWUNIT= in the OPEN below.

      open ( newunit            =  unit, &
             file            =  fname, &
             form            = 'FORMATTED', &
             access          = 'SEQUENTIAL', &
             status          = 'NEW', &
             iostat          =  iostat      )

      if ( iostat /= 0 ) then
         call ctx%setmsg ( 'Could not open file #. IOSTAT was #. ' )
         call ctx%errch  ( '#', fname                              )
         call ctx%errint ( '#', iostat                             )
         call ctx%sigerr ( 'SPICE(FILEOPENFAILED)'                 )
         return
      end if

      return
      end subroutine txtopn

      !> Open a text file for read access.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of the text file to be opened.
      !>
      !>## Detailed Output
      !>
      !>     UNIT     is the logical unit connected to the opened file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If FNAME is a blank string, the error SPICE(BLANKFILENAME) is
      !>         signaled.
      !>
      !>     2)  If the file cannot be opened, the error SPICE(FILEOPENFAILED)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     See FNAME and UNIT above.
      !>
      !>## Particulars
      !>
      !>     In SPICELIB, a text file is formatted and sequential and may
      !>     contain only printable ASCII characters and blanks (ASCII 32-127).
      !>     When printing a text file, records are single spaced; the first
      !>     character will not be interpreted as a carriage control character.
      !>
      !>     TXTOPR opens an existing text file for read access and makes use
      !>     of the SPICELIB mechanism for coordinating use of logical units.
      !>
      !>     System Dependencies
      !>     ===================
      !>
      !>     The open statement will include the following keyword = value
      !>     pairs:
      !>
      !>            UNIT   =  UNIT
      !>            FILE   =  FNAME
      !>            FORM   = 'FORMATTED'
      !>            ACCESS = 'SEQUENTIAL'
      !>            STATUS = 'OLD'
      !>            IOSTAT =  IOSTAT
      !>
      !>     In addition, the statement will include
      !>
      !>            CARRIAGECONTROL = 'LIST'
      !>            READONLY
      !>
      !>     for the Vax and the OS X Absoft compiler, or
      !>
      !>            MODE            = 'READ'
      !>
      !>     for the IBM pc.
      !>
      !>## Examples
      !>
      !>     The following example reads the first line from an input file,
      !>     'INPUT.TXT', and writes it to an output file, 'OUTPUT.TXT'.
      !>
      !>        CALL TXTOPR ( 'INPUT.TXT',  IN  )
      !>        CALL TXTOPN ( 'OUTPUT.TXT', OUT )
      !>
      !>        READ  ( IN,  FMT='(A)' ) LINE
      !>        WRITE ( OUT, FMT='(A)' ) LINE
      !>
      !>        CLOSE ( IN  )
      !>        CLOSE ( OUT )
      !>
      !>## Restrictions
      !>
      !>     1)  The file, FNAME, must exist prior to calling TXTOPR.
      !>
      !>## Literature References
      !>
      !>     [1]  "Lahey F77L EM/32 FORTRAN Language Reference Manual", p.145.
      !>
      !>     [2]  "Language Reference Manual", Absoft Fortran V3.2, page 7-12
      !>          (for the NeXT), 1993.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.E. McLean        (JPL)
      !>     H.A. Neilan        (JPL)
      !>     B.V. Semenov       (JPL)
      !>     M.J. Spencer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.27.0, 28-NOV-2021 (BVS)
      !>
      !>        Updated for MAC-OSX-M1-64BIT-CLANG_C.
      !>
      !>-    SPICELIB Version 2.26.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
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
      !>-    SPICELIB Version 2.0.6, 24-APR-2003 (EDW)
      !>
      !>        Added MAC-OSX-F77 to the list of platforms
      !>        that require READONLY to read write protected
      !>        kernels.
      !>
      !>-    SPICELIB Version 2.0.5, 17-JUL-2002 (BVS)
      !>
      !>        Added MAC-OSX environments.
      !>
      !>-    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 2.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
      !>
      !>        References to the PC-LINUX environment were added.
      !>
      !>-    SPICELIB Version 1.3.0, 11-NOV-1993 (HAN)
      !>
      !>        Module was updated for the Silicon Graphics, DEC Alpha-OSF/1,
      !>        and NeXT platforms.
      !>
      !>-    SPICELIB Version 1.2.0, 12-OCT-1992 (HAN)
      !>
      !>        The code was also reformatted so that a utility program can
      !>        create the source file for a specific environment given a
      !>        master source file.
      !>
      !>-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.0, 13-NOV-1991 (MJS)
      !>
      !>        Module updated to allow portability to the Lahey F77L EM/32
      !>        FORTRAN V 4.0 environment.
      !>
      !>-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
      !>
      !>        References to the PC-LINUX environment were added.
      !>
      !>-    SPICELIB Version 1.3.0, 11-NOV-1993 (HAN)
      !>
      !>        Module was updated for the Silicon Graphics, DEC Alpha-OSF/1,
      !>        and NeXT platforms.
      !>
      !>-    SPICELIB Version 1.2.0, 12-OCT-1992 (HAN)
      !>
      !>        The code was also reformatted so that a utility program can
      !>        create the source file for a specific environment given a
      !>        master source file.
      !>
      !>-    SPICELIB Version 1.1.0, 13-NOV-1991 (MJS)
      !>
      !>        Module updated to allow portability to the Lahey F77L EM/32
      !>        FORTRAN V 4.0 environment.
      module subroutine txtopr ( ctx, fname, unit )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of file.
      integer(ip), intent(out) :: unit  !! Logical unit.

      integer(ip) :: iostat

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      if ( fname == ' ' ) then
         call ctx%setmsg ( 'A blank string is unacceptable as a file name' )
         call ctx%sigerr ( 'SPICE(BLANKFILENAME)'                          )
         return
      end if

!
!     PEPPER: the unit comes from NEWUNIT=, and ACTION='READ' (standard)
!     replaces Intel's MODE='READ' on Windows.
!
      open ( newunit         =  unit, &
             file            =  fname, &
             form            = 'FORMATTED', &
             access          = 'SEQUENTIAL', &
             status          = 'OLD', &
             action          = 'READ', &
             iostat          =  iostat      )

      if ( iostat /= 0 ) then
         call ctx%setmsg ( 'Could not open file #. IOSTAT was #. ' )
         call ctx%errch  ( '#', fname                              )
         call ctx%errint ( '#', iostat                             )
         call ctx%sigerr ( 'SPICE(FILEOPENFAILED)'                 )
         return
      end if

      return
      end subroutine txtopr

      !> Encode and write integers to a text file.
      !>
      !>## Detailed Input
      !>
      !>     UNIT     is the Fortran unit number for a previously opened text
      !>              file. All writing will begin at the CURRENT POSITION
      !>              in the text file.
      !>
      !>     N        is the number of integers to be encoded and written to
      !>              the text file attached to UNIT.
      !>
      !>     DATA     is the list of integers to be encoded and written to the
      !>              text file attached to UNIT.
      !>
      !>## Detailed Output
      !>
      !>     See the $Particulars section for a description of the effect of
      !>     this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If N, the number of data items, is not positive, the error
      !>         SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>     2)  If an error occurs while writing to the text file attached
      !>         to UNIT, the error SPICE(FILEWRITEFAILED) is signaled.
      !>
      !>     3)  If the Fortran logical unit UNIT is not defined, the results
      !>         of this routine are unpredictable.
      !>
      !>## Files
      !>
      !>     See the description of UNIT in the $Detailed_Input section.
      !>
      !>## Particulars
      !>
      !>     This routine will accept a list of one or more integers which
      !>     it will encode into equivalent text strings and write to the
      !>     current position in a text file. The current position in a file
      !>     is defined to be the text line immediately following the last
      !>     text line that was written or read. The encoded integers are
      !>     written to the output text file as quoted character strings so
      !>     that a Fortran list directed read may be used to read the
      !>     encoded values, rather than a formatted read with the format
      !>     specifier FMT = '(A)'.
      !>
      !>     This routine is one of a pair of routines which are used to
      !>     encode and decode integers:
      !>
      !>           WRENCI -- Encode and write integers to a file.
      !>           RDENCI -- Read and decode integers from a file.
      !>
      !>     The encoding/decoding of integers is performed to provide a
      !>     portable means for transferring data values.
      !>
      !>     Currently the text string produced will be a signed hexadecimal
      !>     number See INT2HX.FOR and HX2INT.FOR for details.
      !>
      !>## Examples
      !>
      !>     Please note that the output format in the examples is not
      !>     intended to be exactly identical with the output format of this
      !>     routine in actual use. The output format used in the examples is
      !>     intended to aid in the understanding of how this routine works.
      !>     It is NOT intended to be a specification of the output format for
      !>     this routine.
      !>
      !>     Let
      !>
      !>        UNIT     be the Fortran logical unit of a previously opened
      !>                 text file.
      !>
      !>        N        = 100
      !>
      !>        DATA(I)  = I, I = 1, N
      !>
      !>     Then, the subroutine call
      !>
      !>           CALL WRENCI( UNIT, N, DATA )
      !>
      !>     will write the first 100 integers, encoded, to the output text
      !>     file attached to UNIT, beginning at the current position in the
      !>     output file, which is marked by an arrow, '-->'. The resulting
      !>     output will look something like the following:
      !>
      !>        -->'1' '2' '3' '4' '5' '6' '7' '8' '9' 'A' 'B' 'C' 'D' 'E'
      !>           'F' '10' '11' '12' '13' '14' '15' '16' '17' '18' '19'
      !>           '1A' '1B' '1C' '1D' '1E' '1F' '20' '21' '22' '23' '24'
      !>           '25' '26' '27' '28' '29' '2A' '2B' '2C' '2D' '2E' '2F'
      !>           '30' '31' '32' '33' '34' '35' '36' '37' '38' '39' '3A'
      !>           '3B' '3C' '3D' '3E' '3F' '40'
      !>           '41' '42' '43' '44' '45' '46' '47' '48' '49' '4A' '4B'
      !>           '4C' '4D' '4E' '4F' '50' '51' '52' '53' '54' '55' '56'
      !>           '57' '58' '59' '5A' '5B' '5C' '5D' '5E' '5F' '60' '61'
      !>           '62' '63' '64'
      !>        -->
      !>
      !>     At this point, the arrow marks the position of the file pointer
      !>     immediately after the call to WRENCI.
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
      !>-    SPICELIB Version 1.3.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.0, 09-SEP-1993 (KRG)
      !>
      !>        The list directed write was changed to a formatted write using
      !>        the  specifier FMT='(A)'. This was done in order to prevent a
      !>        space from appearing as the first character on each line of the
      !>        file for certain computer platforms.
      !>
      !>-    SPICELIB Version 1.1.0, 21-JUN-1993 (KRG)
      !>
      !>        This routine was modified to avoid the creation of long output
      !>        lines on some of the supported systems, such as the NeXT with
      !>        Absoft Fortran 3.2.
      !>
      !>        A disclaimer was added to the $Examples section concerning
      !>        the output format used. The disclaimer simply states that the
      !>        output format used in the example is not necessarily the
      !>        output format actually used by the routine.
      !>
      !>-    SPICELIB Version 1.0.0, 19-OCT-1992 (KRG)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.2.0, 09-SEP-1993 (KRG)
      !>
      !>        The list directed write was changed to a formatted write using
      !>        the  specifier FMT='(A)'. This was done in order to prevent a
      !>        space from appearing as the first character on each line of the
      !>        file for certain computer platforms.
      !>
      !>-    SPICELIB Version 1.1.0, 21-JUN-1993 (KRG)
      !>
      !>        This routine was modified to avoid the creation of long output
      !>        lines on some of the supported systems, such as the NeXT with
      !>        Absoft Fortran 3.2.
      !>
      !>        On some of the supported computers this routine would produce
      !>        very long (greater than 1000 characters) output lines due to
      !>        the implicit DO loop used in the WRITE statement:
      !>
      !>            WRITE (UNIT,IOSTAT=IOSTAT,FMT=*)
      !>           .   ( QUOTE//WORK(I)(1:LENGTH(I))//QUOTE//' ', I=1,NITMS )
      !>
      !>        This problem was fixed by removing the implicit DO loop from
      !>        the WRITE statement and placing an equivalent DO loop around
      !>        the WRITE statement:
      !>
      !>            DO I = 1, NITMS
      !>               WRITE (UNIT,IOSTAT=IOSTAT,FMT=*)
      !>           .       QUOTE//WORK(I)(1:LENGTH(I))//QUOTE
      !>            END DO
      !>
      !>        The net effect of this will be that only a single datum will
      !>        be written on each line of output.
      !>
      !>        A disclaimer was added to the $Examples section concerning
      !>        the output format used. The disclaimer simply states that the
      !>        output format used in the example is not necessarily the
      !>        output format actually used by the routine.
      !>
      !>-    SPICELIB Version 1.0.0, 19-OCT-1992 (KRG)
      module subroutine wrenci ( ctx, unit, n, data )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: unit  !! Fortran unit number of output text file.
      integer(ip), intent(in) :: n  !! Number of integers to encode and write.
      integer(ip), intent(in) :: data(*)  !! List of integers to be encoded and written.



      character(maxenc) :: work(wrksiz)

      integer(ip) :: i
      integer(ip) :: iostat
      integer(ip) :: itmbeg
      integer(ip) :: length(wrksiz)
      integer(ip) :: nitms

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     Check to see if the number of data items is less than or equal
!     to zero. If it is, signal an error.
!
      if ( n < 1 ) then

         call ctx%setmsg ( 'The number of data items to be written was' // &
                       ' not positive: #.'                           )
         call ctx%errint ( '#', n                                        )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)'                      )
         return

      end if
!
!     Initialize the beginning location for the data items to be
!     encoded.
!
      itmbeg = 1
!
!     Begin encoding the input data items in blocks of size NITMS.
!     Each time the number of data items NITMS is reached, write
!     out the encoded items in the workspace.
!
      do while ( itmbeg <= n )
!
!        The number of items is either the size of the workspace, or
!        the number of data items which remain to be processed, which
!        should always be less than or equal to the size of the
!        workspace.
!
         nitms = min ( wrksiz, n - itmbeg + 1 )
!
!        Encode each of the numbers into an equivalent character string.
!
         do i = 1, nitms

            call int2hx ( ctx, data(itmbeg + i - 1), work(i), length(i) )

         end do
!
!        Write out the current workspace, placing single quotes around
!        each of the character strings so that they may be read using
!        Fortran list directed read statements rather than the format
!        specifier FMT = '(A)'.
!
         do i = 1, nitms

            write ( unit,fmt='(A)',iostat=iostat ) &
               quote//work(i)(1:length(i))//quote
!
!           Check to see if we got a write error, IOSTAT .NE. 0.
!
            if ( iostat /= 0 ) then

               call ctx%setmsg ( 'Error writing to logical unit #,'  // &
                             ' IOSTAT = #.'                       )
               call ctx%errint ( '#', unit                            )
               call ctx%errint ( '#', iostat                          )
               call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'             )
               return

            end if

         end do
!
!        Position the data item pointer at the next location to begin
!        encoding the items in the array DATA, and continue processing
!        the data items until done.
!
         itmbeg = itmbeg + nitms

      end do

      return
      end subroutine wrenci

      !> Convert an integer to an equivalent signed hexadecimal string.
      !>
      !>## Detailed Input
      !>
      !>     NUMBER   is the integer to be converted.
      !>
      !>## Detailed Output
      !>
      !>     STRING   is the signed hexadecimal string representing the integer
      !>              NUMBER.
      !>
      !>              The following table describes the character set used
      !>              to represent the hexadecimal digits and their
      !>              corresponding values.
      !>
      !>              Character    Value           Character    Value
      !>              ---------    -----           ---------    -----
      !>                '0'          0                '8'          8
      !>                '1'          1                '9'          9
      !>                '2'          2                'A'         10
      !>                '3'          3                'B'         11
      !>                '4'          4                'C'         12
      !>                '5'          5                'D'         13
      !>                '6'          6                'E'         14
      !>                '7'          7                'F'         15
      !>
      !>              In order to obtain the entire signed hexadecimal number,
      !>              the output character string should be at least N
      !>              characters long, where
      !>
      !>                              # of bits per integer + 3
      !>                    N = 1 + ---------------------------- .
      !>                                         4
      !>
      !>              There should be 1 character position for the sign, and
      !>              one character position for each hexadecimal digit that
      !>              could be produced from any integer which can be
      !>              represented by a particular computer system.
      !>
      !>              The following table contains minimum output string
      !>              lengths necessary to obtain the complete hexadecimal
      !>              string for various integer sizes.
      !>
      !>                 Integer size in bits      Minimum output length
      !>                 --------------------      ---------------------
      !>                 8                         3
      !>                 16                        5
      !>                 32                        9
      !>                 36 (really,it exists)     10
      !>                 64                        17
      !>                 etc.
      !>
      !>              The hexadecimal character string produced by this
      !>              routine will be left justified and consist of a
      !>              contiguous sequence of hexadecimal digits, or in the
      !>              case of a negative number, a contiguous sequence of
      !>              hexadecimal digits immediately preceded by a minus
      !>              sign, '-', e.g.:
      !>
      !>                 (1)   h h ... h
      !>                        1 2     n
      !>
      !>                 (2)   -h h ... h
      !>                         1 2     n
      !>
      !>              where h  represents an hexadecimal digit.
      !>                     i
      !>
      !>              The character string produced will be blank padded on
      !>              the right if LENGTH < LEN( STRING ).
      !>
      !>     LENGTH   is the length of the hexadecimal character string
      !>              produced by the conversion.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the output character string is not long enough to
      !>         contain the entire hexadecimal string that was produced,
      !>         the hexadecimal string will be truncated on the right.
      !>
      !>     2)  If LEN( STRING ) > LENGTH, the output character string will
      !>         be blank padded on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine will convert a signed integer into an equivalent
      !>     signed hexadecimal character string. This provides a machine
      !>     independent mechanism for storing or porting integer values.
      !>     This routine is used by the routine DP2HX which converts a
      !>     double precision value into an equivalent character string.
      !>
      !>     This routine is one of a pair of routines which are used to
      !>     perform conversions between integers and equivalent signed
      !>     hexadecimal character strings:
      !>
      !>           INT2HX -- Convert an integer into a signed hexadecimal
      !>                     character string.
      !>
      !>           HX2INT -- Convert a signed hexadecimal character string
      !>                     into an integer.
      !>
      !>## Examples
      !>
      !>     All of the values shown are for a two's complement representation
      !>     for signed integers.
      !>
      !>     The following input and output argument values illustrate the
      !>     action of INT2HX for various input values of NUMBER.
      !>
      !>         NUMBER       STRING           LENGTH
      !>         -----------  ---------------  ------
      !>          1           '1'              1
      !>         -1           '-1'             2
      !>          223         'DF'             2
      !>         -32          '-20'            3
      !>          0           '0'              1
      !>
      !>          2147483647  '7FFFFFFF'       8
      !>          (Maximum 32 bit integer)
      !>
      !>         -2147483647  '-7FFFFFFF'      9
      !>          (Minimum 32 bit integer + 1)
      !>
      !>         -2147483648  '-80000000'      9
      !>          (Minimum 32 bit integer)
      !>
      !>## Restrictions
      !>
      !>     1)  The maximum number of characters permitted in the output
      !>         string is specified by the local parameter STRLEN.
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
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 22-OCT-1992 (KRG)
      module subroutine int2hx ( ctx, number, string, length )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: number  !! Integer to be converted.
      character(*), intent(out) :: string  !! Equivalent hexadecimal string, left justified.
      integer(ip), intent(out) :: length  !! The length of the hexadecimal string produced.

      integer(ip), parameter :: base = 16


      character(1), parameter :: digits(0:maxdig-1) = &
         [ character(1) :: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', &
           'C', 'D', 'E', 'F' ]
      character(strlen) :: tmpstr

      integer(ip) :: begin
      integer(ip) :: itemp
      integer(ip) :: remndr
      integer(ip) :: result
!     Local variables
!

!
!     The hexadecimal digits in the integer are found by repeated
!     applications of the "modulus" and division operations. We fill
!     the string in reverse order so that the digits are in the
!     correct order when we have finished building the string. We then
!     left justify the resulting string and set the value for its
!     length before returning.
!
!     Make a copy of the input so that it will not be changed by this
!     routine.
!
      itemp = number
!
!     We need to do different things for the cases where the integer to
!     be converted is positive, negative, or zero. ( Actually, the
!     positive case and the zero case are the same, but since we can
!     test for integer zero exactly it will save a few arithmetic
!     operations if we treat it as a special case. ) The case for a
!     negative integer is the only one which truly might cause problems,
!     because ABS(minimum integer) may equal ABS(maximum integer) + 1,
!     on some machines. For example, on many machines with 32 bit
!     integers, INTMIN = -2147483648 and INTMAX = 2147483647.
!
!     Set the beginning position of the hexadecimal number to be
!     one past the end of the character string that will hold the
!     hexadecimal representation of the input number. Before each
!     digit of the hexadecimal number is inserted into the character
!     string, the beginning position is decremented, so we always know
!     exactly where the hexadecimal string begins. This simplifies the
!     calculation of the length of the hexadecimal character string at
!     the end of the routine.
!
      begin = strlen + 1

      if ( itemp < 0 ) then
!
!        Collect all of the digits in the string. We know we're done
!        when the value of ITEMP is equal to zero, thanks to the fact
!        that integer arithmetic operations are exact.
!
         do while ( itemp /= 0 )

            begin               = begin - 1
            result              = itemp / base
            remndr              = result * base - itemp
            itemp               = result
            tmpstr(begin:begin) = digits(remndr)

         end do
!
!        Put the minus sign in place.
!
         begin               = begin - 1
         tmpstr(begin:begin) = '-'

      else if ( itemp > 0 ) then
!
!        Collect all of the digits in the string. We know we're done
!        when the value of ITEMP is equal to zero, thanks to the fact
!        that integer arithmetic operations are exact.
!
         do while ( itemp /= 0 )

            begin               = begin - 1
            result              = itemp / base
            remndr              = itemp - result * base
            itemp               = result
            tmpstr(begin:begin) = digits(remndr)

         end do

      else
!
!        Treat zero as a special case, because it's easier.
!
         begin               = begin - 1
         tmpstr(begin:begin) = digits(0)

      end if
!
!     Set the value of the output string before returning. Let the
!     Fortran string assignment deal with the left justification, and
!     the truncation on the right if the output string STRING is not
!     long enough to contain all of the characters in the string
!     that was produced.
!
      string = tmpstr(begin:)
!
!     Also, set the value for the length of the hexadecimal string
!     before returning.
!
      length = strlen - begin + 1

      return
      end subroutine int2hx

      !> Convert a double precision number to an equivalent character
      !> string using a base 16 "scientific notation."
      !>
      !>## Detailed Input
      !>
      !>     NUMBER   is the double precision number to be converted to a
      !>              character string representation.
      !>
      !>## Detailed Output
      !>
      !>     HXSTR    is the character string produced by this routine that
      !>              represents NUMBER in a base 16 "scientific notation,"
      !>              e.g.:
      !>
      !>                 672.0 = '2A^3' = ( 2/16 + 10/( 16**2 ) ) * 16**3
      !>
      !>              and
      !>
      !>                 -11.0 = '-B^1' = - ( 11/16 ) * 16**1.
      !>
      !>              The following table describes the character set used to
      !>              represent the hexadecimal digits and their corresponding
      !>              values.
      !>
      !>                 Character    Value         Character    Value
      !>                 ---------    ------        ---------    ------
      !>                   '0'         0.0D0          '8'         8.0D0
      !>                   '1'         1.0D0          '9'         9.0D0
      !>                   '2'         2.0D0          'A'        10.0D0
      !>                   '3'         3.0D0          'B'        11.0D0
      !>                   '4'         4.0D0          'C'        12.0D0
      !>                   '5'         5.0D0          'D'        13.0D0
      !>                   '6'         6.0D0          'E'        14.0D0
      !>                   '7'         7.0D0          'F'        15.0D0
      !>
      !>              The caret, or hat, character, '^', is used to
      !>              distinguish the exponent.
      !>
      !>              The plus sign, '+', and the minus sign, '-', are used,
      !>              and they have their usual meanings.
      !>
      !>              In order to obtain the entire character string produced
      !>              by this routine, the output character string should be
      !>              at least N characters long, where
      !>
      !>                          # of bits per double precision mantissa + 3
      !>                 N = 3 + ---------------------------------------------
      !>                                               4
      !>
      !>                          # of bits per double precision exponent + 3
      !>                       + ---------------------------------------------
      !>                                               4
      !>
      !>              There should be one character position for the sign of
      !>              the mantissa, one for the sign of the exponent, one for
      !>              the exponentiation character, and one for each
      !>              hexadecimal digit that could be produced from a mantissa
      !>              and an exponent.
      !>
      !>              The following table contains minimum output string
      !>              lengths necessary to obtain the complete character
      !>              string produced by this routine for some typical
      !>              implementations of double precision numbers.
      !>
      !>                 Double precision number
      !>                 Size Mantissa Exponent   Minimum output string
      !>                 bits   bits     bits     length
      !>                 ---- -------- --------   ----------------------
      !>                 64   48       15         3 + 12 + 4 = 19
      !>                 64   55+1     8          3 + 14 + 2 = 19 (VAX)
      !>                 64   52       11         3 + 13 + 3 = 19 (IEEE)
      !>
      !>              The base 16 "scientific notation" character string
      !>              produced by this routine will be left justified and
      !>              consist of a contiguous sequence of characters with one
      !>              of the following formats:
      !>
      !>                 (1)   h h h h  ... h ^H H  ... H
      !>                        1 2 3 4      n  1 2      m
      !>
      !>                 (2)   -h h h h  ... h ^H H  ... H
      !>                         1 2 3 4      n  1 2      m
      !>
      !>                 (3)   h h h h  ... h ^-H H  ... H
      !>                        1 2 3 4      n   1 2      m
      !>
      !>                 (4)   -h h h h  ... h ^-H H  ... H
      !>                         1 2 3 4      n   1 2      m
      !>
      !>              where
      !>
      !>                 h   and  H   denote hexadecimal digits
      !>                  i        j
      !>
      !>                 '^'          denotes exponentiation ( base 16 )
      !>
      !>              and
      !>
      !>                 '+' and '-'  have their usual interpretations.
      !>
      !>              The character string produced will be blank padded on
      !>              the right if HXSSIZ < LEN( HXSTR ).
      !>
      !>     HXSSIZ   is the length of the base 16 "scientific notation"
      !>              character string produced by this routine.
      !>
      !>## Parameters
      !>
      !>     STRLEN   is the maximum number of characters permitted in the
      !>              output string. The value of STRLEN is 255.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If the output character string is not long enough to
      !>         contain the entire character string that was produced,
      !>         the string will be truncated on the right.
      !>
      !>     2)  If LEN( HXSTR ) > HXSSIZ, the output character string will
      !>         be blank padded on the right.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine converts a double precision number into an equivalent
      !>     character string using a base 16 "scientific notation." This
      !>     representation allows the full precision of a number to be placed
      !>     in a format that is suitable for porting or archival storage.
      !>
      !>     This routine is one of a pair of routines which are used to
      !>     perform conversions between double precision numbers and
      !>     an equivalent base 16 "scientific notation" character string
      !>     representation:
      !>
      !>        DP2HX  -- Convert a double precision number into a base 16
      !>                  "scientific notation" character string.
      !>
      !>        HX2DP  -- Convert a base 16 "scientific notation"
      !>                  character string into a double precision number.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Convert a set of double precision numbers to their equivalent
      !>        character string using a base 16 "scientific notation."
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DP2HX_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER               HXSLEN
      !>      PARAMETER           ( HXSLEN = 40 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(HXSLEN)    STRVAL
      !>
      !>      DOUBLE PRECISION      NUMBER (8)
      !>
      !>      INTEGER               I
      !>      INTEGER               SVALLN
      !>
      !>C
      !>C     Assign an array of double precision numbers.
      !>C
      !>      DATA                  NUMBER /    2.0D-9,       1.0D0,
      !>     .                                  -1.0D0,    1024.0D0,
      !>     .                               -1024.0D0,  521707.0D0,
      !>     .                                  27.0D0,       0.0D0 /
      !>
      !>C
      !>C     Loop over the NUMBER array, call DP2HX for each
      !>C     element of NUMBER.
      !>C
      !>      WRITE(*,*) 'number       string             length'
      !>      WRITE(*,*) '-----------  -----------------  ------'
      !>
      !>      DO I= 1, 8
      !>
      !>         CALL DP2HX ( NUMBER(I), STRVAL, SVALLN )
      !>         WRITE(*,'(E12.4,2X,A17,2X,I5)') NUMBER(I), STRVAL,
      !>     .                                              SVALLN
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         number       string             length
      !>         -----------  -----------------  ------
      !>          0.2000E-08  89705F4136B4A8^-7     17
      !>          0.1000E+01  1^1                    3
      !>         -0.1000E+01  -1^1                   4
      !>          0.1024E+04  4^3                    3
      !>         -0.1024E+04  -4^3                   4
      !>          0.5217E+06  7F5EB^5                7
      !>          0.2700E+02  1B^2                   4
      !>          0.0000E+00  0^0                    3
      !>
      !>        Note: the hat or caret, '^', signals an exponent.
      !>
      !>## Restrictions
      !>
      !>     1)  The maximum number of characters permitted in the output
      !>         string is specified by the parameter STRLEN.
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Changed the argument names "STRING" and "LENGTH" to "HXSTR"
      !>        and "HXSSIZ" for consistency with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        The declaration of STRLEN has been promoted to the
      !>        $Declarations section.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete example code.
      !>
      !>        Updated $Brief_I/O, $Parameters and $Restrictions sections to
      !>        properly describe STRLEN.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1994 (KRG)
      !>
      !>        Fixed a typo in the description of the input argument STRING.
      !>        The example showing the expansion of 160 into hexadecimal
      !>        was incorrect. 160 was replaced with 672 which makes the
      !>        example correct.
      !>
      !>-    SPICELIB Version 1.0.0, 26-OCT-1992 (KRG)
      module subroutine dp2hx ( ctx, number, hxstr, hxssiz )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: number  !! D.p. number to be converted.
      character(*), intent(out) :: hxstr  !! Equivalent character string, left justified.
      integer(ip), intent(out) :: hxssiz  !! Length of the character string produced.





      integer(ip), parameter :: expinb = 1

      real(wp), parameter :: factr1 = 4294967296.0_wp

      real(wp), parameter :: invfc1 = 1.0_wp/factr1

      integer(ip), parameter :: expin1 = 8

      real(wp), parameter :: factr2 = 65536.0_wp

      real(wp), parameter :: invfc2 = 1.0_wp/factr2

      integer(ip), parameter :: expin2 = 4


      character(1), parameter :: digits(0:maxdig-1) = &
         [ character(1) :: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', &
           'C', 'D', 'E', 'F' ]
      character(strlen) :: expstr
      character(strlen) :: tmpstr

      real(wp) :: remndr
      real(wp) :: tmpnum

      integer(ip) :: explen
      integer(ip) :: intexp
      integer(ip) :: positn
      integer(ip) :: result

      logical :: negtiv
      logical :: postiv
!     Initial values
!

!
!     Make a copy of the input so that it will not be changed by this
!     routine. Also, assume that we do not know the sign of the number.
!
      tmpnum = number
      negtiv = .false.
      postiv = .false.
!
!     Check to see what the sign of the number is, because we treat
!     negative numbers, positive numbers and zero separately. This
!     simplifies the testing in the loop boundaries a bit, and removes
!     calls to DABS() that would otherwise have been necessary.
!
!     Set the appropriate logical flag for the sign of the input number.
!
      if ( tmpnum < 0.0_wp ) then

         negtiv = .true.

      else if ( tmpnum > 0.0_wp ) then

         postiv = .true.

      end if
!
!     If nonzero, a double precision number is first normalized,
!     so that it has a value between 1.0D0/BASE and 1.0D0 or -1.0D0
!     and -1/BASE. The hexadecimal digits in the mantissa  are found
!     by repeated applications of multiplication and truncation
!     operations. The hexadecimal digits will be in the correct order
!     when finished. The string will be left justified, and its length
!     will be set before returning.
!
!     Calculate the exponent of the number using multiple scaling
!     levels. The different scale factors, 16**8, 16**4, and 16,
!     provide a significant speed improvement for the normalization
!     process.
!
      intexp = 0
      if ( negtiv ) then

         if ( tmpnum > -1.0_wp ) then
!
!           ABS(TMPNUM) .LT. 1.0
!
            do while ( factr1 * tmpnum > -1.0_wp )
!
!              Scale the number and decrement the exponent.
!
               tmpnum = tmpnum * factr1
               intexp = intexp - expin1

            end do

            do while ( factr2 * tmpnum > -1.0_wp )
!
!              Scale the number and decrement the exponent.
!
               tmpnum = tmpnum * factr2
               intexp = intexp - expin2

            end do

            do while ( ( base * tmpnum ) > -1.0_wp )
!
!              Scale the number and decrement the exponent.
!
               tmpnum = tmpnum * base
               intexp = intexp - expinb

            end do
!
!           At this point, -1 < TMPNUM <= -1/BASE.
!
         else
!
!           ABS(TMPNUM) .GE. 1.0
!
            do while ( invfc1 * tmpnum <= -1.0_wp )
!
!              Scale the number and increment the exponent.
!
               tmpnum = tmpnum * invfc1
               intexp = intexp + expin1

            end do

            do while ( invfc2 * tmpnum <= -1.0_wp )
!
!              Scale the number and increment the exponent.
!
               tmpnum = tmpnum * invfc2
               intexp = intexp + expin2

            end do

            do while ( tmpnum <= -1.0_wp )
!
!              Scale the number and increment the exponent.
!
               tmpnum = tmpnum * invbas
               intexp = intexp + expinb

            end do
!
!           At this point, -1 < TMPNUM <= -1/BASE.
!
         end if

      else if ( postiv ) then

         if ( tmpnum < 1.0_wp ) then
!
!           ABS(TMPNUM) .LT. 1.0
!
            do while ( factr1 * tmpnum < 1.0_wp )
!
!              Scale the number and decrement the exponent.
!
               tmpnum = tmpnum * factr1
               intexp = intexp - expin1

            end do

            do while ( factr2 * tmpnum < 1.0_wp )
!
!              Scale the number and decrement the exponent.
!
               tmpnum = tmpnum * factr2
               intexp = intexp - expin2

            end do

            do while ( ( base * tmpnum ) < 1.0_wp )
!
!              Scale the number and decrement the exponent.
!
               tmpnum = tmpnum * base
               intexp = intexp - expinb

            end do
!
!           At this point, 1/BASE <= TMPNUM < 1
!
         else
!
!           ABS(TMPNUM) .GE. 1.0
!
            do while ( invfc1 * tmpnum >= 1.0_wp )
!
!              Scale the number and increment the exponent.
!
               tmpnum = tmpnum * invfc1
               intexp = intexp + expin1

            end do

            do while ( invfc2 * tmpnum >= 1.0_wp )
!
!              Scale the number and increment the exponent.
!
               tmpnum = tmpnum * invfc2
               intexp = intexp + expin2

            end do

            do while ( tmpnum >= 1.0_wp )
!
!              Scale the number and increment the exponent.
!
               tmpnum = tmpnum * invbas
               intexp = intexp + expinb

            end do
!
!           At this point, 1/BASE <= TMPNUM < 1
!
         end if

      end if
!
!     We do different things for the cases where the number to be
!     converted is positive, negative, or zero.
!
      if ( negtiv ) then
!
!        Set the beginning position.
!
         positn = 1
!
!        Put the minus sign in place.
!
         tmpstr(positn:positn) = '-'
!
!        Start with the remainder equal to the normalized value of the
!        original number.
!
         remndr = tmpnum
!
!        Collect all of the digits in the string.
!
!        This stopping test works because the base is a power of
!        2 and the mantissa is composed of a sum of powers of 2.
!
         do while ( remndr /= 0.0_wp )
!
!           -1 < REMNDR <= -1/BASE
!
            positn                 = positn + 1
            tmpnum                 = remndr * base
            result                 = int( tmpnum )
            remndr                 = tmpnum - real( result, wp )
            tmpstr (positn:positn) = digits(-result)

         end do
!
!        Put the exponent on the end of the number and update the
!        position.
!
         call int2hx( ctx, intexp, expstr, explen )
         tmpstr(positn+1:) = expchr//expstr(1:explen)
         positn            = positn + explen + 1

      else if ( postiv ) then
!
!        Set the beginning position.
!
         positn = 0
!
!        Start with the remainder equal to the normalized value of the
!        original number.
!
         remndr = tmpnum
!
!        Collect all of the digits in the string.
!
!        This stopping test works because the base is a power of
!        2 and the mantissa is composed of a sum of powers of 2.
!
         do while ( remndr /= 0.0_wp )
!
!           1/BASE <= REMNDR < 1
!
            positn                 = positn + 1
            tmpnum                 = remndr * base
            result                 = int( tmpnum )
            remndr                 = tmpnum - real( result, wp )
            tmpstr (positn:positn) = digits(result)

         end do
!
!        Put the exponent on the end of the number and update the
!        position.
!
         call int2hx( ctx, intexp, expstr, explen )
         tmpstr(positn+1:) = expchr//expstr(1:explen)
         positn            = positn + explen + 1

      else
!
!        Treat zero as a special case, because it's easier.
!
         positn      = 3
         tmpstr(1:3) = '0^0'

      end if
!
!     Set the value for the length of the character string produced
!     before returning.
!
      hxssiz = positn
!
!     Set the value of the output string before returning. Let the
!     Fortran string assignment deal with the left justification, and
!     the truncation on the right if HXSTR is not long enough to
!     contain all of the characters produced.
!
      hxstr = tmpstr(:hxssiz)

      return
      end subroutine dp2hx

      !> Encode and write d.p. numbers to a text file.
      !>
      !>## Detailed Input
      !>
      !>     UNIT     is the Fortran unit number for a previously opened text
      !>              file. All writing will begin at the CURRENT POSITION
      !>              in the text file.
      !>
      !>     N        is the number of double precision numbers to be encoded
      !>              and written to the text file attached to UNIT.
      !>
      !>     DATA     is the list of double precision numbers to be encoded and
      !>              written to the text file attached to UNIT.
      !>
      !>## Detailed Output
      !>
      !>     See the $Particulars section for a description of the effect of
      !>     this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If N, the number of data items, is not positive, the error
      !>         SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>     2)  If an error occurs while writing to the text file attached
      !>         to UNIT, the error SPICE(FILEWRITEFAILED) is signaled.
      !>
      !>     3)  If the Fortran logical unit UNIT is not defined, the results
      !>         of this routine are unpredictable.
      !>
      !>## Files
      !>
      !>     See the description of UNIT in the $Detailed_Input section.
      !>
      !>## Particulars
      !>
      !>     This routine will accept a list of one or more double precision
      !>     numbers which it will encode into equivalent text strings and
      !>     write to the current position in a text file. The current
      !>     position in a file is defined to be the text line immediately
      !>     following the last text line that was written or read. The
      !>     encoded d.p. numbers are written to the output text file as
      !>     quoted character strings so that a Fortran list directed read may
      !>     be used to read the encoded values, rather than a formatted read
      !>     with the format specifier FMT = '(A)'.
      !>
      !>     This routine is one of a pair of routines which are used to
      !>     encode and decode d.p. numbers:
      !>
      !>           WRENCD -- Encode and write d.p. numbers to a file.
      !>           RDENCD -- Read and decode d.p. numbers from a file.
      !>
      !>     The encoding/decoding of d.p.numbers is performed to provide a
      !>     portable means for transferring data values.
      !>
      !>     Currently the text string produced will be in a base 16
      !>     ``scientific notation.'' This format retains the full precision
      !>     available for d.p. numbers on any given computer architecture.
      !>     See DP2HX.FOR and HX2DP.FOR for details.
      !>
      !>## Examples
      !>
      !>     Please note that the output format in the examples is not
      !>     intended to be exactly identical with the output format of this
      !>     routine in actual use. The output format used in the examples is
      !>     intended to aid in the understanding of how this routine works.
      !>     It is NOT intended to be a specification of the output format for
      !>     this routine.
      !>
      !>     Let
      !>
      !>        UNIT     be the Fortran logical unit of a previously opened
      !>                 text file.
      !>
      !>        N        = 100
      !>
      !>        DATA(I)  = DBLE(I), I = 1,N
      !>
      !>     Then, the subroutine call
      !>
      !>           CALL WRENCD( UNIT, N, DATA )
      !>
      !>     will write the first 100 integers as encoded d.p. numbers to the
      !>     output text file attached to UNIT, beginning at the current
      !>     position in the output file, which is marked by an arrow, '-->'.
      !>     The resulting output will look something like the following:
      !>
      !>        -->'1^1' '2^1' '3^1' '4^1' '5^1' '6^1' '7^1' '8^1' '9^1'
      !>           'A^1' 'B^1' 'C^1' 'D^1' 'E^1' 'F^1' '1^2' '11^2' '12^2'
      !>           '13^2' '14^2' '15^2' '16^2' '17^2' '18^2' '19^2' '1A^2'
      !>           '1B^2' '1C^2' '1D^2' '1E^2' '1F^2' '2^2' '21^2' '22^2'
      !>           '23^2' '24^2' '25^2' '26^2' '27^2' '28^2' '29^2' '2A^2'
      !>           '2B^2' '2C^2' '2D^2' '2E^2' '2F^2' '3^2' '31^2' '32^2'
      !>           '33^2' '34^2' '35^2' '36^2' '37^2' '38^2' '39^2' '3A^2'
      !>           '3B^2' '3C^2' '3D^2' '3E^2' '3F^2' '4^2'
      !>           '41^2' '42^2' '43^2' '44^2' '45^2' '46^2' '47^2' '48^2'
      !>           '49^2' '4A^2' '4B^2' '4C^2' '4D^2' '4E^2' '4F^2' '5^2'
      !>           '51^2' '52^2' '53^2' '54^2' '55^2' '56^2' '57^2' '58^2'
      !>           '59^2' '5A^2' '5B^2' '5C^2' '5D^2' '5E^2' '5F^2' '6^2'
      !>           '61^2' '62^2' '63^2' '64^2'
      !>        -->
      !>
      !>     At this point, the arrow marks the position of the file pointer
      !>     immediately after the call to WRENCD.
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
      !>-    SPICELIB Version 1.3.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.0, 09-SEP-1993 (KRG)
      !>
      !>        The list directed write was changed to a formatted write using
      !>        the specifier FMT='(A)'. This was done in order to prevent a
      !>        space from appearing as the first character on each line of the
      !>        file for certain computer platforms.
      !>
      !>-    SPICELIB Version 1.1.0, 21-JUN-1993 (KRG)
      !>
      !>        This routine was modified to avoid the creation of long output
      !>        lines on some of the supported systems, such as the NeXT with
      !>        Absoft Fortran 3.2.
      !>
      !>        A disclaimer was added to the $Examples section concerning
      !>        the output format used. The disclaimer simply states that the
      !>        output format used in the example is not necessarily the
      !>        output format actually used by the routine.
      !>
      !>-    SPICELIB Version 1.0.0, 20-OCT-1992 (KRG)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.2.0, 09-SEP-1993 (KRG)
      !>
      !>        The list directed write was changed to a formatted write using
      !>        the  specifier FMT='(A)'. This was done in order to prevent a
      !>        space from appearing as the first character on each line of the
      !>        file for certain computer platforms.
      !>
      !>-    SPICELIB Version 1.1.0, 21-JUN-1993 (KRG)
      !>
      !>        This routine was modified to avoid the creation of long output
      !>        lines on some of the supported systems, such as the NeXT with
      !>        Absoft Fortran 3.2.
      !>
      !>        On some of the supported computers this routine would produce
      !>        very long (greater than 1000 characters) output lines due to
      !>        the implicit DO loop used in the WRITE statement:
      !>
      !>            WRITE (UNIT,IOSTAT=IOSTAT,FMT=*)
      !>           .   ( QUOTE//WORK(I)(1:LENGTH(I))//QUOTE//' ', I=1,NITMS )
      !>
      !>        This problem was fixed by removing the implicit DO loop from
      !>        the WRITE statement and placing an equivalent DO loop around
      !>        the WRITE statement:
      !>
      !>            DO I = 1, NITMS
      !>               WRITE (UNIT,IOSTAT=IOSTAT,FMT=*)
      !>           .       QUOTE//WORK(I)(1:LENGTH(I))//QUOTE
      !>            END DO
      !>
      !>        The net effect of this will be that only a single datum will
      !>        be written on each line of output.
      !>
      !>        A disclaimer was added to the $Examples section concerning
      !>        the output format used. The disclaimer simply states that the
      !>        output format used in the example is not necessarily the
      !>        output format actually used by the routine.
      !>
      !>-    SPICELIB Version 1.0.0, 20-OCT-1992 (KRG)
      module subroutine wrencd ( ctx, unit, n, data )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: unit  !! Fortran unit number of output text file.
      integer(ip), intent(in) :: n  !! Number of d.p. numbers to encode and write.
      real(wp), intent(in) :: data(*)  !! List of d.p. numbers to encode and write.

      character(maxenc) :: work(wrksiz)

      integer(ip) :: i
      integer(ip) :: iostat
      integer(ip) :: itmbeg
      integer(ip) :: nitms
      integer(ip) :: length(wrksiz)

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     Check to see if the number of data items is less than or equal
!     to zero. If it is, signal an error.
!
      if ( n < 1 ) then

         call ctx%setmsg ( 'The number of data items to be written was' // &
                       ' not positive: #.'                           )
         call ctx%errint ( '#', n                                        )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)'                      )
         return

      end if
!
!     Initialize the beginning location for the data items to be
!     encoded.
!
      itmbeg = 1
!
!     Begin encoding the input data items in blocks of size NITMS.
!     Each time the number of data items NITMS is reached, write
!     out the encoded items in the workspace.
!
      do while ( itmbeg <= n )
!
!        The number of items is either the size of the workspace, or
!        the number of data items which remain to be processed, which
!        should always be less than or equal to the size of the
!        workspace.
!
         nitms = min ( wrksiz, n - itmbeg + 1 )
!
!        Encode each of the numbers into an equivalent character string.
!
         do i = 1, nitms

            call dp2hx ( ctx, data(itmbeg + i - 1), work(i), length(i) )

         end do
!
!        Write out the current workspace, placing single quotes around
!        each of the character strings so that they may be read using
!        Fortran list directed read statements rather than the format
!        specifier FMT = '(A)'.
!
         do i = 1, nitms

            write ( unit,fmt='(A)',iostat=iostat ) &
               quote//work(i)(1:length(i))//quote
!
!           Check to see if we got a write error, IOSTAT .NE. 0.
!
            if ( iostat /= 0 ) then

               call ctx%setmsg ( 'Error writing to logical unit #,'  // &
                             ' IOSTAT = #.'                       )
               call ctx%errint ( '#', unit                            )
               call ctx%errint ( '#', iostat                          )
               call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'             )
               return

            end if

         end do
!
!        Position the data item pointer at the next location to begin
!        encoding the items in the array DATA, and continue processing
!        the data items until done.
!
         itmbeg = itmbeg + nitms

      end do

      return
      end subroutine wrencd

      !> Read the contents of the file record of a DAF.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the handle associated with the DAF.
      !>
      !>## Detailed Output
      !>
      !>     IDWORD     is a character string identifying the architecture
      !>                and type of a SPICE binary kernel.  In this case
      !>                it will be a string identifying the type of DAF.
      !>
      !>     ND,
      !>     NI         are the number of double precision and integer
      !>                components, respectively, in each array summary in
      !>                the specified file.
      !>
      !>     IFNAME     is the internal file name stored in the first
      !>                (or file) record of the specified file.
      !>
      !>     FWARD      is the forward list pointer. This points to the
      !>                first summary record in the file. (Records between
      !>                the first record and the first summary record are
      !>                reserved when the file is created, and are invisible
      !>                to DAF routines.)
      !>
      !>     BWARD      is the backward list pointer. This points
      !>                to the final summary record in the file.
      !>
      !>     FREE       is the free address pointer. This contains the
      !>                first free address in the file. (That is, the
      !>                initial address of the next array to be added
      !>                to the file.)
      !>
      !>     FOUND      is TRUE when the file record is found, and is
      !>                FALSE otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) SPICE(HANDLENOTFOUND) is signaled if HANDLE can not be
      !>        found in the set of loaded handles.  The output arguments
      !>        are unmodified when this error occurs.
      !>
      !>     2) Routines in the call tree of this routine may trap and
      !>        signal errors.  The output arguments are unmodified in
      !>        these cases.
      !>
      !>## Files
      !>
      !>     This routine reads data from the DAF associated with HANDLE.
      !>     This action may result in connecting a logical unit to the
      !>     file, if the handle manager has rotated the file out of the
      !>     unit table.
      !>
      !>## Particulars
      !>
      !>     This routine reads the publicly available components of
      !>     file records from native and supported non-native DAFs.
      !>
      !>     The size of the character buffer and the number of records
      !>     read may have to change to support new environments.  As of
      !>     the original release of this routine, all systems currently
      !>     supported have a 1 kilobyte record length.
      !>
      !>## Examples
      !>
      !>     See DAFRFR for sample usage.
      !>
      !>## Restrictions
      !>
      !>     1) Numeric data when read as characters from a file preserves
      !>        the bit patterns present in the file in memory.
      !>
      !>     2) A record of double precision data is at most 1024 characters
      !>        in length.
      !>
      !>     3) Future updates to this module must preserve the fact that
      !>        FOUND is returned as FALSE whenever an error occurs.  An
      !>        incompletely translated or extracted file record is NOT
      !>        FOUND.
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
      !>-    SPICELIB Version 1.0.2, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    SPICELIB Version 1.0.1, 24-JUL-2014 (NJB)
      !>
      !>        Corrected typo in Procedure line of header.
      !>        Re-ordered header sections.
      !>
      !>-    SPICELIB Version 1.0.0, 12-NOV-2001 (FST)
      module subroutine zzdafgfr ( ctx, handle, idword, nd, ni, ifname, fward, bward, free, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of the DAF.
      character(*), intent(out) :: idword  !! DAF ID Word that indicates file type.
      integer(ip), intent(out) :: nd  !! Number of double precision components in summaries.
      integer(ip), intent(out) :: ni  !! Number of integer components in summaries.
      character(*), intent(out) :: ifname  !! Internal file name.
      integer(ip), intent(out) :: fward  !! Forward list pointer.
      integer(ip), intent(out) :: bward  !! Backward list pointer.
      integer(ip), intent(out) :: free  !! Free address pointer.
      logical, intent(out) :: found  !! Logical indicating whether the record was found.
!
!     Local Parameters
!
!     Record Number of the file record in a DAF.
!
      integer(ip), parameter :: frlocr = 1
!
!     Starting location in bytes of the internal filename in the
!     file record.
!
      integer(ip), parameter :: ifnstr = 17
!
!     Size of an integer in bytes.
!
      integer(ip), parameter :: inlen = 4
!
!     Local Variables
!
!     PEPPER: ZZXLATEI returns an array (also of one element).
!
      character(cbfsiz) :: chrbuf
      character(1) :: fname  ! PEPPER: the name is not used (ZZDDHNFO cuts it)
      character(idwlen) :: locidw
      character(ifnlen) :: locifn
      character(strsiz) :: tmpstr
      integer(ip) :: cindex
      integer(ip) :: i
      integer(ip) :: iamh
      integer(ip) :: iarch
      integer(ip) :: ibff
      integer(ip) :: iostat
      integer(ip) :: locbwd
      integer(ip) :: locfre
      integer(ip) :: locfwd
      integer(ip) :: locnd
      integer(ip) :: ival ( 1 )
      integer(ip) :: locni
      integer(ip) :: lun
      logical :: locfnd
      if ( .not. allocated ( ctx%zzdafgfr_state ) ) allocate ( ctx%zzdafgfr_state )
      associate ( first => ctx%zzdafgfr_state%first, &
                  natbff => ctx%zzdafgfr_state%natbff, &
                  strbff => ctx%zzdafgfr_state%strbff )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Perform some initialization tasks.
!
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
!        Do not perform initialization tasks again.
!
         first = .false.

      end if

!
!     Assume the data record will not be found, until it has been read
!     from the file, and if necessary, successfully translated.
!
      found = .false.

!
!     Retrieve information regarding the file from the handle manager.
!     The value of IARCH is not a concern, since this is a DAF routine
!     all values passed into handle manager entry points will have
!     'DAF' as their architecture arguments.
!
      call zzddhnfo ( ctx, handle, fname, iarch, ibff, iamh, locfnd )

      if ( .not. locfnd ) then
         call ctx%setmsg ( 'Unable to locate file associated with ' &
         //            'HANDLE, #.  The most likely cause of this ' &
         //            'is the file that you are trying to read ' &
         //            'has been closed.'                           )
         call ctx%errint ( '#', handle                                  )
         call ctx%sigerr ( 'SPICE(HANDLENOTFOUND)'                      )
         return
      end if

!
!     PEPPER: a DAF built in memory has no file: its file record is in
!     its image.
!
      i = daf_synthetic_slot ( ctx, handle )
      if ( i > 0 ) then
         call daf_image_file_record ( ctx%spice%zzddhman%files(i)%image, &
                                      idword, nd, ni, ifname, &
                                      fward, bward, free )
         found = .true.
         return
      end if

!
!     PEPPER: in-memory binary DAF (furnsh_memory): decode the file record
!     directly from raw_records without opening a logical unit.
!
      i = daf_raw_slot ( ctx, handle )
      if ( i > 0 ) then
         if ( frlocr < 1 .or. frlocr > size(ctx%spice%zzddhman%files(i)%raw_records) ) return
         chrbuf = ctx%spice%zzddhman%files(i)%raw_records(frlocr)
         locidw = chrbuf( 1      : idwlen            )
         locifn = chrbuf( ifnstr : (ifnstr+ifnlen-1) )
         if ( ibff == natbff ) then
            cindex = idwlen + 1
            locnd  = transfer ( chrbuf(cindex:cindex+inlen-1), 0_ip )
            cindex = cindex + inlen
            locni  = transfer ( chrbuf(cindex:cindex+inlen-1), 0_ip )
            cindex = ifnstr + ifnlen
            locfwd = transfer ( chrbuf(cindex:cindex+inlen-1), 0_ip )
            cindex = cindex + inlen
            locbwd = transfer ( chrbuf(cindex:cindex+inlen-1), 0_ip )
            cindex = cindex + inlen
            locfre = transfer ( chrbuf(cindex:cindex+inlen-1), 0_ip )
         else
            cindex = idwlen + 1
            call zzxlatei ( ctx, ibff, chrbuf(cindex:cindex+inlen-1), 1, ival )
            locnd = ival(1)
            cindex = cindex + inlen
            call zzxlatei ( ctx, ibff, chrbuf(cindex:cindex+inlen-1), 1, ival )
            locni = ival(1)
            cindex = ifnstr + ifnlen
            call zzxlatei ( ctx, ibff, chrbuf(cindex:cindex+inlen-1), 1, ival )
            locfwd = ival(1)
            cindex = cindex + inlen
            call zzxlatei ( ctx, ibff, chrbuf(cindex:cindex+inlen-1), 1, ival )
            locbwd = ival(1)
            cindex = cindex + inlen
            call zzxlatei ( ctx, ibff, chrbuf(cindex:cindex+inlen-1), 1, ival )
            locfre = ival(1)
            if ( ctx%failed() ) return
         end if
         found  = .true.
         idword = locidw
         nd     = locnd
         ni     = locni
         ifname = locifn
         fward  = locfwd
         bward  = locbwd
         free   = locfre
         return
      end if

!
!     Now get a logical unit for the handle.  Check FAILED() in
!     case an error occurs.
!
      call zzddhhlu ( ctx, handle, 'DAF', .false., lun )

      if ( ctx%failed() ) return

!
!     Branch based on whether the binary file format is native
!     or not.  Only supported formats can be opened by ZZDDHOPN,
!     so no check of IBFF is required.
!
      if ( ibff == natbff ) then

!
!        In the native case, just read the components of the file
!        record from the file.
!
         read ( unit   = lun, &
                rec    = frlocr, &
                iostat = iostat  ) locidw, locnd,  locni, locifn, &
                                   locfwd, locbwd, locfre

!
!        Since this routine does not signal any IOSTAT based
!        errors, return if a non-zero value is assigned to IOSTAT.
!
         if ( iostat /= 0 ) return

!
!     Process the non-native binary file format case.
!
      else

!
!        Read the data record as characters.
!
         read ( unit   = lun, &
                rec    = frlocr, &
                iostat = iostat  ) chrbuf

!
!        Again, since this routine does not signal any IOSTAT
!        based errors, return if one occurs.
!
         if ( iostat /= 0 ) return

!
!        Assign the character components of the file record.
!
         locidw = chrbuf( 1      : idwlen            )
         locifn = chrbuf( ifnstr : (ifnstr+ifnlen-1) )

!
!        Convert the integer components.
!
         cindex = idwlen + 1
         call zzxlatei ( ctx, ibff, chrbuf(cindex:cindex+inlen-1), &
                         1,    ival )
         locnd = ival(1)

         cindex = cindex + inlen
         call zzxlatei ( ctx, ibff, chrbuf(cindex:cindex+inlen-1), &
                         1,    ival )
         locni = ival(1)

         cindex = ifnstr + ifnlen
         call zzxlatei ( ctx, ibff, chrbuf(cindex:cindex+inlen-1), &
                         1,    ival )
         locfwd = ival(1)

         cindex = cindex + inlen
         call zzxlatei ( ctx, ibff, chrbuf(cindex:cindex+inlen-1), &
                         1,    ival )
         locbwd = ival(1)

         cindex = cindex + inlen
         call zzxlatei ( ctx, ibff, chrbuf(cindex:cindex+inlen-1), &
                         1,    ival )
         locfre = ival(1)

         if ( ctx%failed() ) return

      end if

!
!     Transfer the contents of the record to the output arguments
!     and return to the caller.
!
      found = .true.

      idword = locidw
      nd     = locnd
      ni     = locni
      ifname = locifn
      fward  = locfwd
      bward  = locbwd
      free   = locfre

      return
      end associate
      end subroutine zzdafgfr

      !> Write the file record to a new DAF file.
      !>
      !>## Detailed Input
      !>
      !>     LUN        is a logical unit number of a DAF whose first record is
      !>                to be created with a DAF file record bearing the
      !>                attributes specified by the other arguments.
      !>
      !>     IDWORD     is the 'ID word' contained in the first eight
      !>                characters of the file record.
      !>
      !>     ND,        are the number of double precision and integer
      !>     NI         components, respectively, in each array summary
      !>                in the specified file.
      !>
      !>     IFNAME     is the internal filename to be stored in the file
      !>                record for identification purposes.
      !>
      !>     FWARD,     are the record numbers of the first and last
      !>     BWARD      descriptor records in the DAF file, respectively.
      !>                FWARD is greater than 2 whenever reserved records
      !>                are present.
      !>
      !>     FREE       is the first free address pointer.  This integer
      !>                stores the first free DAF address for writing the
      !>                next array to be appended to the file.
      !>
      !>     FORMAT     is a character string that indicates what the numeric
      !>                binary format the DAF is utilizing.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     See include file zzftprms.inc.
      !>
      !>## Exceptions
      !>
      !>     1) If any errors occur from the WRITE to the logical unit LUN,
      !>        the error SPICE(DAFWRITEFAIL) is signaled. Before returning
      !>        to the caller, the file attached to LUN is closed and deleted.
      !>
      !>## Files
      !>
      !>     This routine writes to the first record of the DAF whose
      !>     logical unit is LUN.
      !>
      !>## Particulars
      !>
      !>     This routine assembles the file record and writes it to the
      !>     first record in a DAF.  Its purpose is to write new file
      !>     records only.  For updates, use DAFWFR.
      !>
      !>     Make certain the caller checks FAILED() after this returns, since
      !>     on error it closes and deletes the file.
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     1) An individual character must occupy 1 byte of space and
      !>        conform to the ASCII standard.
      !>
      !>     2) The word size for the machine should be at least 32 bits,
      !>        else the computations to null pad the gaps in the file
      !>        record may overstep record boundaries.
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
      !>-    SPICELIB Version 1.0.2, 02-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments. Reordered header sections.
      !>
      !>-    SPICELIB Version 1.0.1, 11-DEC-2001 (FST)
      !>
      !>        Corrected the omission of IDWORD from the Brief_I/O and
      !>        Detailed_Input sections of the module header.
      !>
      !>-    SPICELIB Version 1.0.0, 02-MAR-1999 (FST)
      module subroutine zzdafnfr ( ctx, lun, idword, nd, ni, ifname, fward, bward, free, format )
      use pepper_kinds, only: ip
      use pepper_strings, only: rtrim
      use pepper_inc_zzftprms
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: lun  !! Logical unit number of an open DAF file.
      character(*), intent(in) :: idword  !! DAF ID word.
      integer(ip), intent(in) :: nd  !! Number of double precision components in a summary.
      integer(ip), intent(in) :: ni  !! Number of integer components in a summary.
      character(*), intent(in) :: ifname  !! Internal filename.
      integer(ip), intent(in) :: fward  !! First descriptor record.
      integer(ip), intent(in) :: bward  !! Last descriptor record.
      integer(ip), intent(in) :: free  !! First free address.
      character(*), intent(in) :: format  !! File binary format identifier string.
!
!     Local Parameters
!
!     Amount of space measured in characters necessary to
!     null pad between the last character of FORMAT and the
!     first character of FTPSTR to keep FTPSTR at character
!     700 in a 1024 byte record.
!
      integer(ip), parameter :: preftp = 603
!
!     Amount of space measured in characters necessary to
!     null pad from the last character of FTPSTR to the
!     end of the file record. Note: This value assumes the
!     length of the file record is 1024 bytes.  The DAF
!     specification only requires the presence of 1000
!     characters, so this may require modification for
!     non-standard platforms.
!
      integer(ip), parameter :: pstftp = 297
!
!     Local Variables
!
      character(1) :: nullch
      character(fmtlen) :: locfmt
      character(idwlen) :: locidw
      character(ifnlen) :: locifn
      character(sizdlm) :: delim
      character(sizend) :: lftbkt
      character(sizend) :: rgtbkt
      character(sizstr) :: tststr
      integer(ip) :: i
      integer(ip) :: iostat
      if ( .not. allocated ( ctx%zzdafnfr_state ) ) allocate ( ctx%zzdafnfr_state )
      associate ( first => ctx%zzdafnfr_state%first, &
                  ftpstr => ctx%zzdafnfr_state%ftpstr, &
                  prenul => ctx%zzdafnfr_state%prenul, &
                  pstnul => ctx%zzdafnfr_state%pstnul )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     On the first pass, format the PRENUL and PSTNUL strings,
!     and build FTPSTR from its components.
!
      if ( first ) then

!
!        Store NULL into NULLCH.
!
         nullch = char(0)

!
!        Set all of the characters of PRENUL to nulls.
!
         do i = 1, preftp

            prenul (i:i) = nullch

         end do

!
!        Set all of the characters of PSTNUL to nulls.
!
         do i = 1, pstftp

            pstnul (i:i) = nullch

         end do

!
!        Build FTPSTR from its components that come back from
!        ZZFTPSTR.  This private SPICE routine returns the
!        following components:
!7
!           TSTSTR - The test component of the FTP string
!           LFTBKT - The left bracketing, printable, component of
!                    the FTP string.
!           RGTBKT - The right bracketing, printable, component of
!                    the FTP string.
!           DELIM  - The printable delimiter that separates the
!                    individual test character blocks in TSTSTR.
!
!        which are assembled into the FTP string as it appears in
!        the DAF file record.
!
         call zzftpstr ( ctx, tststr, lftbkt, rgtbkt, delim )

         ftpstr = lftbkt( 1:rtrim(lftbkt) )                          // &
                  tststr( 1:rtrim(tststr) )                          // &
                  rgtbkt( 1:rtrim(rgtbkt) )

!
!        Stop this block from executing except on the first pass.
!
         first = .false.

      end if

!
!     Make local copies of each of the string arguments.  This way we
!     maintain the proper sizes for each of the string objects, in
!     the event larger or smaller strings are passed in.
!
      locidw = idword
      locifn = ifname
      locfmt = format

!
!     Write the file record components out to the first record of the
!     file.
!
      write ( unit = lun, rec=1, iostat=iostat ) locidw, &
                                                 nd, &
                                                 ni, &
                                                 locifn, &
                                                 fward, &
                                                 bward, &
                                                 free, &
                                                 locfmt, &
                                                 prenul, &
                                                 ftpstr, &
                                                 pstnul
      flush ( lun )
      call files_written ( ctx )
!
!     Check IOSTAT for errors.
!
      if ( iostat /= 0 ) then

!
!        Since we are unable to write to the file record, make
!        certain the output file is destroyed.
!
         call ctx%setmsg ( 'Attempt to write file ''#'' failed. '   // &
                       'Value of IOSTAT was #. The file has '   // &
                       'been deleted.'                           )
         call ctx%errfnm ( '#', lun                                  )
         call ctx%errint ( '#', iostat                               )

         close ( unit = lun, status = 'DELETE' )

         call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)' )
         return

      end if

      return
      end associate
      end subroutine zzdafnfr

      !> Read a data record from a DAF.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the handle associated with the DAF.
      !>
      !>     RECNO      is the record number of a particular double precision
      !>                record within the DAF, whose contents are to be read.
      !>
      !>## Detailed Output
      !>
      !>     DPREC      contains the contents of the specified record from
      !>                the DAF associated with HANDLE.
      !>
      !>     FOUND      is TRUE when the specified record is found, and is
      !>                FALSE otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Files
      !>
      !>     This routine reads data from the DAF associated with HANDLE.
      !>     This action may result in connecting a logical unit to the
      !>     file, if the handle manager has rotated the file out of the
      !>     unit table.
      !>
      !>## Exceptions
      !>
      !>     1) SPICE(HANDLENOTFOUND) is signaled if HANDLE can not be
      !>        found in the set of loaded handles.
      !>
      !>     2) Routines in the call tree of this routine may trap and
      !>        signal errors.
      !>
      !>## Particulars
      !>
      !>     This routine reads records of double precision numbers
      !>     from native and supported non-native DAFs.
      !>
      !>     The size of the character buffer and the number of records
      !>     read may have to change to support new environments.  As of
      !>     the original release of this routine, all systems currently
      !>     supported have a 1 kilobyte record length.
      !>
      !>## Examples
      !>
      !>     See DAFGDR for sample usage.
      !>
      !>## Restrictions
      !>
      !>     1) Numeric data when read as characters from a file preserves
      !>        the bit patterns present in the file in memory.
      !>
      !>     2) A record of double precision data is at most 1024 characters
      !>        in length.
      !>
      !>     3) DPREC has enough space to store 128 double precision numbers.
      !>
      !>     4) RECNO points to a record that contains double precision data
      !>        in the file to which HANDLE refers.
      !>
      !>     5) ZZXLATED will translate all possible inputs (garbage or not)
      !>        coming from the file associated with HANDLE.
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
      !>-    SPICELIB Version 1.0.0, 12-NOV-2001 (FST)
      module subroutine zzdafgdr ( ctx, handle, recno, dprec, found )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of the DAF.
      integer(ip), intent(in) :: recno  !! Record number.
      real(wp), intent(out) :: dprec  ( * )  !! Contents of the record.
      logical, intent(out) :: found  !! Logical indicating whether the record was found.
!
!     Local Variables
!
      character(cbfsiz) :: chrbuf
      character(1) :: fname  ! PEPPER: the name is not used (ZZDDHNFO cuts it)
      character(strsiz) :: tmpstr
      real(wp) :: dpbuf  ( 128 )
      integer(ip) :: i
      integer(ip) :: iamh
      integer(ip) :: iarch
      integer(ip) :: ibff
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: locfnd
      if ( .not. allocated ( ctx%zzdafgdr_state ) ) allocate ( ctx%zzdafgdr_state )
      associate ( first => ctx%zzdafgdr_state%first, &
                  natbff => ctx%zzdafgdr_state%natbff, &
                  strbff => ctx%zzdafgdr_state%strbff )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Perform some initialization tasks.
!
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
!        Do not perform initialization tasks again.
!
         first = .false.

      end if

!
!     Assume the data record will not be found, until it has been read
!     from the file, and if necessary, successfully translated.
!
      found = .false.

!
!     Retrieve information regarding the file from the handle manager.
!     The value of IARCH is not a concern, since this is a DAF routine
!     all values passed into handle manager entry points will have
!     'DAF' as their architecture arguments.
!
      call zzddhnfo ( ctx, handle, fname, iarch, ibff, iamh, locfnd )

      if ( .not. locfnd ) then
         call ctx%setmsg ( 'Unable to locate file associated with ' &
         //            'HANDLE, #.  The most likely cause of this ' &
         //            'is the file that you are trying to read ' &
         //            'has been closed.'                           )
         call ctx%errint ( '#', handle                                  )
         call ctx%sigerr ( 'SPICE(HANDLENOTFOUND)'                      )
         return
      end if

!
!     PEPPER: a file loaded in memory (FURNSH's in_memory) is not read;
!     its copy is native.
!
      i = daf_image_slot ( ctx, handle )
      if ( i > 0 ) then
         associate ( image => ctx%spice%zzddhman%files(i)%image )
            if ( recno >= 1 .and. recno <= size(image)/128 ) then
               dprec(1:128) = image(128*(recno-1)+1:128*recno)
               found = .true.
               return
            end if
         end associate
      end if

!
!     Now get a logical unit for the handle.  Check FAILED() in
!     case an error occurs.
!
      call zzddhhlu ( ctx, handle, 'DAF', .false., lun )

      if ( ctx%failed() ) return

!
!     Branch based on whether the binary file format is native
!     or not.  Only supported formats can be opened by ZZDDHOPN,
!     so no check of IBFF is required.
!
      if ( ibff == natbff ) then

!
!        In the native case, just read the double precision
!        numbers from the file.
!
         read ( unit   = lun, &
                rec    = recno, &
                iostat = iostat ) ( dpbuf(i), i = 1, 128 )

!
!        Since this routine does not signal any IOSTAT based
!        errors, return if a non-zero value is assigned to IOSTAT.
!
         if ( iostat /= 0 ) return

!
!     Process the non-native binary file format case.
!
      else

!
!        Read the data record as characters.
!
         read ( unit   = lun, &
                rec    = recno, &
                iostat = iostat ) chrbuf

!
!        Again, since this routine does not signal any IOSTAT
!        based errors, return if one occurs.
!
         if ( iostat /= 0 ) return

!
!        Translate the data record.  Assume (improperly in the
!        general case) that ZZXLATED will translate the contents
!        of the entire record without signaling an error.  This
!        is appropriate at this stage since ZZXLATED simply swaps
!        bytes between BIG-IEEE and LTL-IEEE environments.  In
!        the future, updates may be necessary to prevent
!        processing of garbage data.
!
         call zzxlated ( ctx, ibff, chrbuf, 128, dpbuf )

         if ( ctx%failed() ) return

      end if

!
!     Transfer the DPs to the output argument and return
!     to the caller.
!
      found = .true.
      dprec(1:128) = dpbuf

      return
      end associate
      end subroutine zzdafgdr

      !> Read a summary/descriptor record from a DAF.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE     is the handle associated with the DAF.
      !>
      !>     RECNO      is the record number of a particular summary record
      !>                within the DAF, whose contents are to be read.
      !>     ND,
      !>     NI         are the number of double precision and integer
      !>                components, respectively, in each array summary
      !>                in the specified file.
      !>
      !>## Detailed Output
      !>
      !>     DPREC      contains the contents of the specified record from
      !>                the DAF associated with HANDLE, properly translated
      !>                for use on the native environment.
      !>
      !>     FOUND      is TRUE when the specified record is found, and is
      !>                FALSE otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) SPICE(HANDLENOTFOUND) is signaled if HANDLE can not be
      !>        found in the set of loaded handles.
      !>
      !>     2) Routines in the call tree of this routine may trap and
      !>        signal errors.
      !>
      !>## Files
      !>
      !>     This routine reads data from the DAF associated with HANDLE.
      !>     This action may result in connecting a logical unit to the
      !>     file, if the handle manager has rotated the file out of the
      !>     unit table.
      !>
      !>## Particulars
      !>
      !>     This routine reads summary records of double precision
      !>     numbers which contain integers packed through an EQUIVALENCE
      !>     statement from native and supported non-native DAFs.
      !>
      !>     The size of the character buffer and the number of records
      !>     read may have to change to support new environments.  As of
      !>     the original release of this routine, all systems currently
      !>     supported have a 1 kilobyte record length.
      !>
      !>## Examples
      !>
      !>     See DAFGSR for sample usage.
      !>
      !>## Restrictions
      !>
      !>     1) Numeric data when read as characters from a file preserves
      !>        the bit patterns present in the file in memory.
      !>
      !>     2) A record of double precision data is at most 1024 characters
      !>        in length.
      !>
      !>     3) DPREC has enough space to store 128 double precision numbers.
      !>
      !>     4) Characters a byte-sized, 8 characters constitute a double
      !>        precision number, and 4 characters an integer.
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
      !>        Corrected typo in comments. Reordered header sections.
      !>
      !>-    SPICELIB Version 1.0.0, 12-NOV-2001 (FST)
      module subroutine zzdafgsr ( ctx, handle, recno, nd, ni, dprec, found )
      use pepper_kinds, only: ip, wp
      use pepper_packing, only: pack_integers
      use pepper_strings, only: ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of the DAF.
      integer(ip), intent(in) :: recno  !! Record number.
      integer(ip), intent(in) :: nd  !! Number of double precision components in a summary.
      integer(ip), intent(in) :: ni  !! Number of integer components in a summary.
      real(wp), intent(out) :: dprec  ( * )  !! Contents of the record.
      logical, intent(out) :: found  !! Logical indicating whether the record was found.
!
!     Local Parameters
!
!     Length in bytes of double precision numbers and integers.
!
      integer(ip), parameter :: dplen = 8
      integer(ip), parameter :: inlen = dplen/2
!
!     Local Variables
!
      character(cbfsiz) :: chrbuf
      character(1) :: fname  ! PEPPER: the name is not used (ZZDDHNFO cuts it)
      character(strsiz) :: tmpstr
      real(wp) :: dpbuf  ( 128 )
      integer(ip) :: cindex
      integer(ip) :: dindex
      integer(ip) :: i
      integer(ip) :: iamh
      integer(ip) :: iarch
      integer(ip) :: ibff
      integer(ip) :: inbuf  ( 256 )  !! a summary's integers, translated (PEPPER: not equivalenced to DPBUF)
      integer(ip) :: iostat
      integer(ip) :: left
      integer(ip) :: lun
      integer(ip) :: nsum
      integer(ip) :: sumsiz
      logical :: locfnd
      if ( .not. allocated ( ctx%zzdafgsr_state ) ) allocate ( ctx%zzdafgsr_state )
      associate ( first => ctx%zzdafgsr_state%first, &
                  natbff => ctx%zzdafgsr_state%natbff, &
                  strbff => ctx%zzdafgsr_state%strbff )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Perform some initialization tasks.
!
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
!        Do not perform initialization tasks again.
!
         first = .false.

      end if

!
!     Assume the data record will not be found, until it has been read
!     from the file, and if necessary, successfully translated.
!
      found = .false.

!
!     Retrieve information regarding the file from the handle manager.
!     The value of IARCH is not a concern, since this is a DAF routine
!     all values passed into handle manager entry points will have
!     'DAF' as their architecture arguments.
!
      call zzddhnfo ( ctx, handle, fname, iarch, ibff, iamh, locfnd )

      if ( .not. locfnd ) then
         call ctx%setmsg ( 'Unable to locate file associated with ' &
         //            'HANDLE, #.  The most likely cause of this ' &
         //            'is the file that you are trying to read ' &
         //            'has been closed.'                           )
         call ctx%errint ( '#', handle                                  )
         call ctx%sigerr ( 'SPICE(HANDLENOTFOUND)'                      )
         return
      end if

!
!     PEPPER: a DAF built in memory has no file: the summary record is
!     in its image (native).
!
      i = daf_synthetic_slot ( ctx, handle )
      if ( i > 0 ) then
         associate ( image => ctx%spice%zzddhman%files(i)%image )
            found = recno >= 1 .and. recno <= size(image)/128
            if ( found ) dprec(1:128) = image(128*(recno-1)+1:128*recno)
         end associate
         return
      end if

!
!     PEPPER: in-memory binary DAF (furnsh_memory): read summary records
!     from image / raw_records without opening a logical unit.
!
      i = daf_raw_slot ( ctx, handle )
      if ( i > 0 ) then
         if ( recno < 1 .or. recno > size(ctx%spice%zzddhman%files(i)%raw_records) ) return
         if ( ibff == natbff ) then
            dprec(1:128) = ctx%spice%zzddhman%files(i)%image(128*(recno-1)+1 : 128*recno)
            found = .true.
            return
         end if
         chrbuf = ctx%spice%zzddhman%files(i)%raw_records(recno)
      else
!
!     Now get a logical unit for the handle.  Check FAILED()
!     in case an error occurs.
!
         call zzddhhlu ( ctx, handle, 'DAF', .false., lun )

         if ( ctx%failed() ) then
            found = .false.
            return
         end if
      end if

!
!     Branch based on whether the binary file format is native
!     or not.  Only supported formats can be opened by ZZDDHOPN,
!     so no check of IBFF is required.
!
      if ( ibff == natbff ) then

!
!        In the native case, just read the array of double precision
!        numbers from the file.  The packed integers will be
!        processed properly by the READ.
!
         read ( unit   = lun, &
                rec    = recno, &
                iostat = iostat ) ( dpbuf(i), i = 1, 128 )

!
!        Since this routine does not signal any IOSTAT based
!        errors, return if a non-zero value is assigned to IOSTAT.
!
         if ( iostat /= 0 ) return

!
!     Process the non-native binary file format case.
!
      else

         if ( daf_raw_slot ( ctx, handle ) == 0 ) then
!
!        Read the record as characters.
!
            read ( unit   = lun, &
                   rec    = recno, &
                   iostat = iostat ) chrbuf

!
!        Again, since this routine does not signal any IOSTAT
!        based errors, return if one occurs.
!
            if ( iostat /= 0 ) return
         end if

!
!        Translate the summary record.  First extract the leading
!        3 double precision numbers from the summary record as these
!        respectively are NEXT, PREV, and NSUM.
!
         call zzxlated ( ctx, ibff, chrbuf(1:3*dplen), 128, dpbuf )

!
!        Check FAILED() in case the translation process fails for
!        any reason.
!
         if ( ctx%failed() ) return

!
!        Convert NSUM to an integer, and compute the number of
!        double precision numbers required to store each individual
!        summary in the record.
!
         nsum   = int ( dpbuf(3) )
         sumsiz = ( nd + (ni+1)/2 )

!
!        Convert each of the summaries one at a time.
!
         do i = 1, nsum

!
!           Set the start index into the double precision array
!           to receive the components.  Also set the character
!           substring index to the start location for this summary.
!           In the diagram below, each box represents a double
!           precision number.  The figure assumes SUMSIZ is 5
!           double precision numbers:
!
!                 |--- 1 ---|--- 2 ---|--- 3 ---|   |- (I-1) -|
!           -------------------------------------   -------------
!           | | | | | | | | | | | | | | | | | | |...| | | | | | |...
!           -------------------------------------   -------------
!           |-----|                                            ^
!              ^                                               |
!              |                                            Summary
!           NEXT, PREV, NSUM                                 Start
!
            dindex = (i-1)*sumsiz + 4
            cindex = dplen*(dindex-1) + 1

!
!           First, check to see if there are any double precision
!           numbers to translate.  If so, translate, and then
!           increment DINDEX and CINDEX accordingly.
!
            if ( nd > 0 ) then

!
!              DPBUF has room for 128 double precision numbers
!              total.  Compute the amount of space left in the
!              buffer.
!
               left = 128 - (i-1)*sumsiz - 3

               call zzxlated ( ctx, ibff, &
                               chrbuf(cindex:cindex+nd*dplen-1), &
                               left, &
                               dpbuf(dindex)                     )

!
!              If the translation routine fails for any reason,
!              return.
!
               if ( ctx%failed() ) return

               dindex = dindex + nd
               cindex = cindex + dplen*nd

            end if

!
!           At this point DINDEX and CINDEX are pointing at the
!           locations for the packed integers in the record.
!           Use DINDEX to compute the index into INBUF, the
!           equivalenced integer buffer and translate. (PEPPER: the
!           integers are translated into INBUF, then packed into
!           DPBUF(DINDEX...) by PACK_INTEGERS.)
!
            if ( ni > 0 ) then

!
!              INBUF has room for 256 integers total.  Compute
!              the amount of space left in the buffer.  Since
!              it is equivalenced to DPBUF, account for the
!              double precision numbers that were just added.
!              (PEPPER: INBUF is separate, and holds one summary's
!              integers; the space left in the record still bounds
!              them.)
!
               left = 256 - 2*(i-1)*sumsiz - nd*2 - 6

               call zzxlatei ( ctx, ibff, &
                               chrbuf(cindex:cindex+ni*inlen-1), &
                               left, &
                               inbuf                             )

!
!              If the translation routine fails for any reason,
!              return.
!
               if ( ctx%failed() ) return

!
!              Now check to see if NI is odd.  If so, then zero
!              the last integer occupied by the newly translated
!              summary.  This is necessary to purge any garbage
!              present in memory.
!
               if ( mod(ni,2) == 1 ) then
                  inbuf(ni+1) = 0
               end if

!
!              PEPPER: the integers packed into the summary's double
!              precision words by PACK_INTEGERS (SPICELIB translated
!              them into the words through an EQUIVALENCE).
!
               call pack_integers ( inbuf(1:ni), ni, dpbuf(dindex:dindex+(ni+1)/2-1) )

            end if

         end do

!
!        Translating garbage is a bad idea in general, so set
!        the any remaining double precision numbers in the summary
!        record to 0.
!
         dindex = nsum*sumsiz + 4

         do i = dindex, 128
            dpbuf(i) = 0
         end do

      end if

!
!     Transfer the DPs to the output argument and return to the
!     caller.
!
      found = .true.
      dprec(1:128) = dpbuf

      return
      end associate
      end subroutine zzdafgsr

      !> Open a DAF for subsequent read requests.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the file name of a DAF to be opened for read
      !>              access.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the file handle associated with the file. This
      !>              handle is used to identify the file in subsequent
      !>              calls to other DAF routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified file has already been opened for read
      !>         access, the handle already associated with the file is
      !>         returned.
      !>
      !>     2)  If the specified file has already been opened for write
      !>         access, an error is signaled by a routine in the call
      !>         tree of this routine.
      !>
      !>     3)  If the specified file has already been opened by a non-DAF
      !>         routine, an error is signaled by a routine in the call
      !>         tree of this routine.
      !>
      !>     4)  If the specified file cannot be opened without exceeding
      !>         the maximum number of files, the error SPICE(DAFFTFULL)
      !>         is signaled.
      !>
      !>     5)  If the attempt to read the file's file record fails,
      !>         the error SPICE(FILEREADFAILED) is signaled.
      !>
      !>     6)  If the specified file is not a DAF file, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     7)  If no logical units are available, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     8)  If the file does not exist, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     9)  If an I/O error occurs in the process of opening the file,
      !>         the error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     10) If the file name is blank or otherwise inappropriate,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     11) If the file was transferred improperly via FTP, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     12) If the file utilizes a binary file format that is not
      !>         currently supported on this platform, an error is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument FNAME.
      !>
      !>## Particulars
      !>
      !>     Most DAFs require only read access. If you do not need to
      !>     change the contents of a file, you should open it with DAFOPR.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) In the following code fragment, DAFOPR is used to open a file,
      !>        which is then searched for DAFs containing data for a
      !>        particular object.
      !>
      !>           CALL DAFOPR ( FNAME, HANDLE )
      !>           CALL DAFBFS ( HANDLE )
      !>           CALL DAFFNA ( FOUND  )
      !>
      !>           DO WHILE ( FOUND )
      !>              CALL DAFGS ( SUM )
      !>              CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>              IF ( IC(1) .EQ. TARGET_OBJECT ) THEN
      !>               .
      !>               .
      !>
      !>              END IF
      !>
      !>              CALL DAFFNA ( FOUND )
      !>           END DO
      !>
      !>     2) Use a simple routine to output the double precision and integer
      !>        values stored in an SPK's segments descriptors. This function
      !>        opens a DAF for read, performs a forwards search for the DAF
      !>        arrays, prints segments description for each array found, then
      !>        closes the DAF.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           de421.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFOPR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Define the summary parameters appropriate
      !>C     for an SPK file.
      !>C
      !>      INTEGER               MAXSUM
      !>      PARAMETER           ( MAXSUM = 125 )
      !>
      !>      INTEGER               ND
      !>      PARAMETER           ( ND = 2 )
      !>
      !>      INTEGER               NI
      !>      PARAMETER           ( NI = 6 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(32)        KERNEL
      !>
      !>      DOUBLE PRECISION      DC     ( ND     )
      !>      DOUBLE PRECISION      SUM    ( MAXSUM )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               IC     ( NI     )
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Open a DAF for read. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      KERNEL = 'de421.bsp'
      !>      CALL DAFOPR ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Begin a forward search on the file.
      !>C
      !>      CALL DAFBFS ( HANDLE )
      !>
      !>C
      !>C     Search until a DAF array is found.
      !>C
      !>      CALL DAFFNA ( FOUND )
      !>
      !>C
      !>C     Loop while the search finds subsequent DAF arrays.
      !>C
      !>      DO WHILE ( FOUND )
      !>
      !>         CALL DAFGS ( SUM )
      !>         CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>         WRITE(*,*)                'Doubles:', DC(1:ND)
      !>         WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
      !>
      !>C
      !>C        Check for another segment.
      !>C
      !>         CALL DAFFNA ( FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        1        0        1        2      641   310404
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        2        0        1        2   310405   423048
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        3        0        1        2   423049   567372
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        4        0        1        2   567373   628976
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        5        0        1        2   628977   674740
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        6        0        1        2   674741   715224
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        7        0        1        2   715225   750428
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        8        0        1        2   750429   785632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        9        0        1        2   785633   820836
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:       10        0        1        2   820837   944040
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      301        3        1        2   944041  1521324
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      399        3        1        2  1521325  2098608
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      199        1        1        2  2098609  2098620
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      299        2        1        2  2098621  2098632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      499        4        1        2  2098633  2098644
      !>
      !>        Note, the final entries in the integer array contain the
      !>        segment start/end indexes. The output indicates the search
      !>        proceeded from the start of the file (low value index) towards
      !>        the end (high value index).
      !>
      !>## Restrictions
      !>
      !>     1)  Files opened using this routine must be closed with DAFCLS.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.1.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Updated code example with IMPLICIT NONE, and declarations of
      !>        SUM variable and MAXSUM parameter.
      !>
      !>        Corrected minor typos in header.
      !>
      !>-    SPICELIB Version 8.1.1, 10-OCT-2012 (EDW)
      !>
      !>        Added a functional code example to the $Examples section.
      !>
      !>        Removed the unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>        Corrected ordering of header section.
      !>
      !>-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
      !>
      !>        This routine was updated to accommodate changes to the
      !>        handle manager interface. See DAFAH's Revision section
      !>        for details.
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 5.0.0, 03-MAR-1999 (FST)
      !>
      !>        This entry point now attempts to locate and validate the
      !>        FTP validation string contained in the file record.
      !>
      !>        See the $Revisions section under DAFAH for a discussion
      !>        of the impact of the changes made for this version.
      !>
      !>-    SPICELIB Version 4.0.0, 27-SEP-1993 (KRG)
      !>
      !>        This routine was modified to use a subroutine to obtain the
      !>        architecture of the file rather than using hard coded values
      !>        for comparison with the file ID word. This was done in order to
      !>        isolate the code which checks to determine a file architecture
      !>        and to make the identification of file types easier through a
      !>        change to the file ID word.
      !>
      !>        In particular, the changes to this routine support the change
      !>        of the file ID word from 'NAIF/DAF' or 'NAIF/NIP' to 'DAF/xxxx'
      !>        where 'xxxx' represents a four character mnemonic code for the
      !>        type of data in the file.
      !>
      !>        Removed the error SPICE(DAFNOIDWORD) as it was no longer
      !>        relevant.
      !>
      !>        Added the error SPICE(NOTADAFFILE) if this routine is called
      !>        with a file that does not contain an ID word identifying the
      !>        file as a DAF file.
      !>
      !>        Changed the long error message when the error
      !>        SPICE(NOTADAFFILE) is signaled to suggest that a common error
      !>        is attempting to load a text version of the desired file rather
      !>        than the binary version.
      !>
      !>-    SPICELIB Version 3.0.0, 25-FEB-1993 (JML)
      !>
      !>        The INQUIRE statement that checks if the file is already open
      !>        now also checks that the file exists.
      !>
      !>        A new variable LUN is used for the logical unit number
      !>        returned by GETLUN.
      !>
      !>        The IF-THEN statements were reorganized to improve readability.
      !>
      !>        A long error message is now set when the DAF id word is not
      !>        recognized. Also, the file is closed when this error is
      !>        signaled.
      !>
      !>        IOSTAT is checked after the file record is read.
      !>
      !>        The file name is checked to see if it is blank.
      !>
      !>        The file name string that is passed to the FORTRAN OPEN and
      !>        INQUIRE statements has been chopped at the last non-blank
      !>        character.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 03-SEP-1991 (NJB) (WLT)
      !>
      !>        This routine was updated so that it now keeps current the set
      !>        of DAF handles returned by DAFHOF.
      !>
      !>        Some error messages were changed so that they specify
      !>        names of relevant DAFs.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafopr ( ctx, fname, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of DAF to be opened.
      integer(ip), intent(out) :: handle  !! Handle assigned to DAF.
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: found
      associate ( ft => ctx%spice%dafah, &
                  nft => ctx%spice%dafah%nft )
      if ( ctx%failed() ) return

!
!     Initialize the handle list, if necessary.
!
!     Attempt to open the file; perform any appropriate checks.
!
      call zzddhopn ( ctx, fname, 'READ', 'DAF', handle )

!
!     Check FAILED(); return if an error has occurred.
!
      if ( ctx%failed() ) return

!
!     See if this file is already present in the file table.  If it
!     is simply increment its link count by one, check out and
!     return.
!
      findex = ft%find ( handle )

      if ( findex /= 0 ) then
         ft%files(findex)%links = ft%files(findex)%links + 1
         return
      end if

!
!     Retrieve ND and NI from the file record.
!
      call zzdafgfr ( ctx, handle, &
                      idword, &
                      fnd, &
                      fni, &
                      ifn, &
                      fward, &
                      bward, &
                      free, &
                      found   )

      if ( .not. found ) then

         call zzddhcls ( ctx, handle, 'DAF', .false.                      )
         call ctx%setmsg   ( 'Error reading the file record from the'  // &
                         ' binary DAF file ''#''.'                   )
         call ctx%errch    ( '#', fname                                  )
         call ctx%sigerr   ( 'SPICE(FILEREADFAILED)'                     )
         return

      end if

!
!     At this point, we know that we have a valid DAF file, and we're
!     set up to read from it, so ...
!
!     Update the file table to include information about our newly
!     opened DAF.
!
      call ft%add ( daf_file ( handle = handle, links = 1, nd = fnd, ni = fni ) )

      return
      end associate
      end subroutine dafopr

      !> Open a DAF for subsequent write requests.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of a DAF to be opened with write access.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the file handle associated with the file. This handle
      !>              is used to identify the file in subsequent calls to other
      !>              DAF routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified file has already been opened, either by the
      !>         DAF routines or by other code, an error is signaled by a
      !>         routine in the call tree of this routine. Note that this
      !>         response is not paralleled by DAFOPR, which allows you to open
      !>         a DAF for reading even if it is already open for reading.
      !>
      !>     2)  If the specified file cannot be opened without exceeding the
      !>         maximum number of files, the error SPICE(DAFFTFULL) is
      !>         signaled.
      !>
      !>     3)  If the attempt to read the file's file record fails, the error
      !>         SPICE(FILEREADFAILED) is signaled.
      !>
      !>     4)  If the specified file is not a DAF file, an error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>     5)  If no logical units are available, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     6)  If the file does not exist, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     7)  If an I/O error occurs in the process of opening the file, the
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     8)  If the file name is blank or otherwise inappropriate, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     9)  If the file was transferred improperly via FTP, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     10) If the file utilizes a non-native binary file format, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     See argument FNAME.
      !>
      !>## Particulars
      !>
      !>     Most DAFs require only read access. If you do not need to
      !>     change the contents of a file, you should open it with DAFOPR.
      !>     Use DAFOPW when you need to
      !>
      !>        -- change (update) one or more summaries, names, or
      !>           arrays within a file; or
      !>
      !>        -- add new arrays to a file.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Delete the entire comment area of a DAF file. Note that this
      !>        action should only be performed if fresh new comments are to
      !>        be placed within the DAF file.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           earthstns_itrf93_201023.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFOPW_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               RTRIM
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         KERNEL
      !>      PARAMETER           ( KERNEL =
      !>     .                         'earthstns_itrf93_201023.bsp' )
      !>
      !>      INTEGER               BUFFSZ
      !>      PARAMETER           ( BUFFSZ = 10 )
      !>
      !>      INTEGER               LINLEN
      !>      PARAMETER           ( LINLEN = 1000 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(LINLEN)    BUFFER ( BUFFSZ )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               N
      !>
      !>      LOGICAL               DONE
      !>
      !>C
      !>C     Open a DAF for write. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      CALL DAFOPW ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Print the first 10 lines of comments from the DAF file.
      !>C
      !>      WRITE(*,'(A)') 'Comment area of input DAF file '
      !>     .            // '(max. 10 lines): '
      !>      WRITE(*,'(A)') '---------------------------------------'
      !>     .            // '-----------------------'
      !>
      !>      CALL DAFEC  ( HANDLE, BUFFSZ, N, BUFFER, DONE )
      !>
      !>      DO I = 1, N
      !>
      !>         WRITE (*,*) BUFFER(I)(:RTRIM(BUFFER(I)))
      !>
      !>      END DO
      !>
      !>      WRITE(*,'(A)') '---------------------------------------'
      !>     .            // '-----------------------'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Deleting entire comment area...'
      !>
      !>C
      !>C     Delete all the comments from the DAF file.
      !>C
      !>      CALL DAFDC ( HANDLE )
      !>
      !>C
      !>C     Close the DAF file and re-open it for read
      !>C     access to work around the DAFEC restriction
      !>C     on comments not to be modified while they are
      !>C     being extracted.
      !>C
      !>      CALL DAFCLS( HANDLE  )
      !>
      !>      CALL DAFOPR( KERNEL, HANDLE  )
      !>
      !>C
      !>C     Check if the comments have indeed been deleted.
      !>C
      !>      CALL DAFEC  ( HANDLE, BUFFSZ, N, BUFFER, DONE )
      !>
      !>      IF ( DONE .AND. N .EQ. 0 ) THEN
      !>
      !>         WRITE(*,*) ' '
      !>         WRITE(*,*) '   Successful operation.'
      !>
      !>      ELSE
      !>
      !>         WRITE(*,*) ' '
      !>         WRITE(*,*) '   Operation failed.'
      !>
      !>      END IF
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Comment area of input DAF file (max. 10 lines):
      !>        --------------------------------------------------------------
      !>
      !>            SPK for DSN Station Locations
      !>            ========================================================***
      !>
      !>            Original file name:                   earthstns_itrf93_2***
      !>            Creation date:                        2020 October 28 12:30
      !>            Created by:                           Nat Bachman  (NAIF***
      !>
      !>            Introduction
      !>        --------------------------------------------------------------
      !>
      !>         Deleting entire comment area...
      !>
      !>            Successful operation.
      !>
      !>        Warning: incomplete output. 3 lines extended past the right
      !>        margin of the header and have been truncated. These lines are
      !>        marked by "***" at the end of each line.
      !>
      !>## Restrictions
      !>
      !>     1)  Only files of the native binary file format may be opened
      !>         with this routine.
      !>
      !>     2)  Files opened using this routine must be closed with DAFCLS.
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
      !>     J.E. McLean        (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.1.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example. Updated $Index_Entries.
      !>
      !>-    SPICELIB Version 8.1.1, 10-OCT-2012 (EDW)
      !>
      !>        Corrected ordering of header section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
      !>
      !>        This routine was updated to accommodate changes to the
      !>        handle manager interface. See DAFAH's Revision section
      !>        for details.
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 6.0.0, 03-MAR-1999 (FST)
      !>
      !>        This entry point now attempts to locate and validate the
      !>        FTP validation string contained in the file record.
      !>
      !>-    SPICELIB Version 5.0.0, 27-SEP-1993 (KRG)
      !>
      !>        This routine was modified to use a subroutine to obtain the
      !>        architecture of the file rather than using hard coded values
      !>        for comparing to the file ID word. This was done in order to
      !>        isolate the code which checks to determine a file architecture,
      !>        and to make the identification of file types easier through a
      !>        change to the file ID word.
      !>
      !>        In particular, the changes to this routine support the change
      !>        of the file ID word from 'NAIF/DAF' or 'NAIF/NIP' to 'DAF/xxxx'
      !>        where 'xxxx' represents a four character mnemonic code for the
      !>        type of data in the file.
      !>
      !>        Removed the error SPICE(DAFNOIDWORD) as it was no longer
      !>        relevant.
      !>
      !>        Added the error SPICE(NOTADAFFILE) if this routine is called
      !>        with a file that does not contain an ID word identifying the
      !>        file as a DAF file.
      !>
      !>        Changed the long error message when the error
      !>        SPICE(NOTADAFFILE) is signaled to suggest that a common error
      !>        is attempting to load a text version of the desired file rather
      !>        than the binary version.
      !>
      !>-    SPICELIB Version 4.0.0, 25-FEB-1993 (JML)
      !>
      !>        The INQUIRE statement that checks if the file is already open
      !>        now also checks that the file exists.
      !>
      !>        A new variable LUN is used for the logical unit number
      !>        returned by GETLUN.
      !>
      !>        The IF-THEN statements were reorganized to improve readability.
      !>
      !>        A long error message is now set when the DAF id word is not
      !>        recognized. Also, the file is closed when this error is
      !>        signaled.
      !>
      !>        IOSTAT is now checked after the file record is read.
      !>
      !>        The file name is checked to see if it is blank.
      !>
      !>        The file name string that is passed to the FORTRAN OPEN and
      !>        INQUIRE statements has been chopped at the last non-blank
      !>        character.
      !>
      !>-    SPICELIB Version 3.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 3.0.0, 03-SEP-1991 (NJB) (WLT)
      !>
      !>        DAFOPW now allows multiple files to be open for writing.
      !>
      !>        This routine was updated so that it now keeps current the set
      !>        of DAF handles returned by DAFHOF.
      !>
      !>-    SPICELIB Version 2.0.0, 24-JAN-1991 (JEM)
      !>
      !>        DAFOPW now accepts the ID word 'NAIF/NIP' as well 'NAIF/DAF'.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 6.0.0, 03-MAR-1999 (FST)
      !>
      !>        See the $Revisions section under DAFAH for a discussion
      !>        of the impact of the changes made for this version.
      !>
      !>-    SPICELIB Version 2.0.0, 24-JAN-1991 (NJB) (WLT)
      !>
      !>        DAFOPW now allows multiple files to be open for writing.
      !>
      !>        This routine was updated so that it now keeps current the set
      !>        of DAF handles returned by DAFHOF.
      !>
      !>        Some error messages were changed so that they specify
      !>        names of relevant DAFs.
      module subroutine dafopw ( ctx, fname, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of DAF to be opened.
      integer(ip), intent(out) :: handle  !! Handle assigned to DAF.
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: found
      associate ( ft => ctx%spice%dafah, &
                  nft => ctx%spice%dafah%nft )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Initialize the handle list, if necessary.
!
!     Check to see if there is room in the file table.
!
!     PEPPER: the file table grows as needed; SPICELIB signaled
!     SPICE(DAFFTFULL) here when it had FTSIZE (5000) DAFs.
!

!
!     Attempt to open the file; perform any appropriate checks.
!
      call zzddhopn ( ctx, fname, 'WRITE', 'DAF', handle )

!
!     Check FAILED(); return if an error has occurred.
!
      if ( ctx%failed() ) return

!
!     Retrieve ND and NI from the file record.
!
      call zzdafgfr ( ctx, handle, &
                      idword, &
                      fnd, &
                      fni, &
                      ifn, &
                      fward, &
                      bward, &
                      free, &
                      found   )

      if ( .not. found ) then

         call zzddhcls ( ctx, handle, 'DAF', .false.                      )
         call ctx%setmsg   ( 'Error reading the file record from the'  // &
                         ' binary DAF file ''#''.'                   )
         call ctx%errch    ( '#', fname                                  )
         call ctx%errint   ( '#', iostat                                 )
         call ctx%sigerr   ( 'SPICE(FILEREADFAILED)'                     )
         return

      end if

!
!     At this point, we know that we have a valid DAF file, and we're
!     set up to write to it or read from it, so ...
!
!     Update the file table to include information about our
!     newly opened DAF.
!
      call ft%add ( daf_file ( handle = handle, links = 1, nd = fnd, ni = fni ) )

      return
      end associate
      end subroutine dafopw

      !> Open a new DAF for subsequent write requests.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of a new DAF to be created (and
      !>              consequently opened for write access).
      !>
      !>     FTYPE    is a code for type of data placed into a DAF file.
      !>              The first nonblank character and the three (3)
      !>              characters immediately following it, giving four (4)
      !>              characters, are used to represent the type of the data
      !>              placed in the DAF file. This is provided as a
      !>              convenience for higher level software. It is an error
      !>              if this string is blank. When written to the DAF file,
      !>              the value for the type IS case sensitive; what you put
      !>              in is what you get out, so be careful.
      !>
      !>              NAIF has reserved for its own use file types
      !>              consisting of the upper case letters (A-Z) and the
      !>              digits 0-9. NAIF recommends lower case or mixed case
      !>              file types be used by all others in order to avoid
      !>              any conflicts with NAIF file types.
      !>
      !>     ND       is the number of double precision components
      !>              in each array summary of the new file.
      !>
      !>     NI       is the number of integer components in each
      !>              array summary in the new file.
      !>
      !>     IFNAME   is the internal file name (containing as many as 60
      !>              characters) for the new file. This should uniquely
      !>              identify the file.
      !>
      !>     RESV     is the number of records in the new file to be
      !>              reserved; these records will not be used to store any
      !>              data belonging to DAF arrays subsequently written to
      !>              the file. The user may reserve records 2 through (2 +
      !>              RESV - 1) in the file. SPICE kernels based on the DAF
      !>              format use the reserved record area to store optional
      !>              textual information; for these kernels, the reserved
      !>              records contain the file's "comment area."
      !>
      !>              When RESV is non-zero, this routine writes an
      !>              end-of-comments character into the first byte of
      !>              record 2, and fills the rest of the allocated records
      !>              will null (ASCII code 0) characters.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the file handle associated with the file. This
      !>              handle is used to identify the file in subsequent
      !>              calls to other DAF routines.
      !>
      !>## Parameters
      !>
      !>     INTEOC   is the ASCII decimal integer code of the character
      !>              recognized by SPICE as representing the end of the
      !>              comment data in the reserved record area.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified file cannot be opened without exceeding
      !>         the maximum number of files, the error SPICE(DAFFTFULL)
      !>         is signaled.
      !>
      !>     2)  If the input argument ND is out of the range [0, 124]
      !>         or if NI is out of the range [2, 250], the error
      !>         SPICE(DAFINVALIDPARAMS) is signaled.
      !>
      !>     3)  If
      !>
      !>            ND + ( NI + 1 ) / 2   >  125
      !>
      !>         the error SPICE(DAFINVALIDPARAMS) is signaled.
      !>
      !>     4)  If the number of records to be reserved is not zero or
      !>         positive, the error SPICE(DAFNORESV) is signaled.
      !>
      !>     5)  If an I/O error occurs in the process of opening the file,
      !>         the error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     6)  If (for some reason) the initial records in the file cannot
      !>         be written, the error SPICE(DAFWRITEFAIL) is signaled.
      !>
      !>     7)  If no logical units are available, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     8)  If the file name is blank or otherwise inappropriate,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     9)  If the file type is blank, the error SPICE(BLANKFILETYPE)
      !>         is signaled.
      !>
      !>     10) If the file type contains nonprinting characters, decimal
      !>         0-31 and 127-255, the error SPICE(ILLEGALCHARACTER) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     See argument FNAME.
      !>
      !>## Particulars
      !>
      !>     This routine supersedes DAFOPN as the method for opening a new DAF
      !>     file. It includes a data type identifier as part of the ID word of
      !>     a DAF file it creates.
      !>
      !>     The DAFs created by DAFONW have initialized file records but
      !>     do not yet contain any arrays. See the DAF Required Reading
      !>     for a discussion of file records.
      !>
      !>## Examples
      !>
      !>     In the following code fragment, DAFONW is used to open a file,
      !>     to which a new array is then added. This file will have the data
      !>     type 'TEST' which may be used to distinguish production data from
      !>     test data at a user subroutine level.
      !>
      !>        FNAME = 'test.bin'
      !>        FTYPE = 'TEST'
      !>
      !>        CALL DAFONW   ( FNAME, FTYPE,  ND,  NI,  IFNAME, 0, HANDLE )
      !>
      !>        CALL DAFBNA   ( HANDLE, SUM, NAME  )
      !>        CALL GET_DATA ( DATA,   N,   FOUND )
      !>
      !>        DO WHILE ( FOUND )
      !>
      !>           CALL DAFADA   ( DATA, N        )
      !>           CALL GET_DATA ( DATA, N, FOUND )
      !>
      !>        END DO
      !>
      !>        CALL DAFENA
      !>
      !>## Restrictions
      !>
      !>     1)  Files opened using this routine must be closed with DAFCLS.
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
      !>-    SPICELIB Version 9.0.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 9.0.1, 10-OCT-2012 (EDW)
      !>
      !>        Corrected ordering of header section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 9.0.0, 09-NOV-2006 (NJB)
      !>
      !>        DAFONW now writes a EOC character to the first byte
      !>        of the second record when NRESV > 0.
      !>
      !>-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
      !>
      !>        This routine was updated to accommodate changes to the
      !>        handle manager interface. See DAFAH's Revision section
      !>        for details.
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 2.0.0, 03-MAR-1999 (FST)
      !>
      !>        The entry point was modified to insert the FTP validation
      !>        string, as well as the binary file format into the file record.
      !>
      !>-    SPICELIB Version 1.1.0, 08-MAR-1996 (KRG)
      !>
      !>        The modifications support the notion of a DAF comment area,
      !>        and involve writing NULL filled reserved records when the
      !>        number of reserved records is greater than zero (0).
      !>
      !>        Some nested IF...THEN...ELSE IF...THEN...END IF constructs
      !>        were expanded to be independent IF...THEN...END IF tests.
      !>        The tests were for IOSTAT errors on cascading write statements
      !>        nested in the IF...ELSE IF... statements, and this was
      !>        confusing. These tests were restructured so that IOSTAT is
      !>        tested after each write statement which is equivalent to the
      !>        original intent and easier to read.
      !>
      !>-    SPICELIB Version 1.0.0, 29-SEP-1993 (KRG)
      !>
      !>        This routine implements the notion of a file type for DAF
      !>        files. It allows type information to be added to the file ID
      !>        word.
      !>
      !>        This routine is a modified version of DAFOPN. See the revision
      !>        history of that entry point for details of changes before the
      !>        creation of this entry point.
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 9.0.0, 09-NOV-2006 (NJB)
      !>
      !>        DAFONW now writes a EOC character to the first byte
      !>        of the second record when NRESV > 0.
      !>
      !>-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
      !>
      !>        This routine was updated to accommodate changes to the
      !>        handle manager interface. See DAFAH's Revision section
      !>        for details.
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 2.0.0, 03-MAR-1999 (FST)
      !>
      !>        The entry point was modified to insert the FTP validation
      !>        string, as well as the binary file format into the file record.
      !>
      !>-    SPICELIB Version 1.1.0, 08-MAR-1996 (KRG)
      !>
      !>        The modifications support the notion of a DAF comment area,
      !>        and involve writing NULL filled reserved records when the
      !>        number of reserved records is greater than zero (0).
      !>
      !>        Some nested IF...THEN...ELSE IF...THEN...END IF constructs
      !>        were expanded to be independent IF...THEN...END IF tests.
      !>        The tests were for IOSTAT errors on cascading write statements
      !>        nested in the IF...ELSE IF... statements, and this was
      !>        confusing. These tests were restructured so that IOSTAT is
      !>        tested after each write statement which is equivalent to the
      !>        original intent and easier to read.
      !>
      !>-    SPICELIB Version 2.0.0, 03-MAR-1999 (FST)
      !>
      !>        See the $Revisions section under DAFAH for a discussion
      !>        of the impact of the changes made for this version.
      module subroutine dafonw ( ctx, fname, ftype, nd, ni, ifname, resv, handle )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: ltrim, rtrim
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of DAF to be opened.
      character(*), intent(in) :: ftype  !! Mnemonic code for type of data in the DAF file.
      integer(ip), intent(in) :: nd  !! Number of double precision components in summaries.
      integer(ip), intent(in) :: ni  !! Number of integer components in summaries.
      character(*), intent(in) :: ifname  !! Internal file name.
      integer(ip), intent(in) :: resv  !! Number of records to reserve.
      integer(ip), intent(out) :: handle  !! Handle assigned to DAF.
      integer(ip), parameter :: maxpc = 126
      integer(ip), parameter :: minpc = 32
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: found
      associate ( ft => ctx%spice%dafah, &
                  nft => ctx%spice%dafah%nft )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Initialize the handle list, if necessary.
!
!     Check to see if there is room in the file table.
!
!     PEPPER: the file table grows as needed; SPICELIB signaled
!     SPICE(DAFFTFULL) here when it had FTSIZE (5000) DAFs.
!

!
!     Check if the file type is blank.
!
      if ( ftype == ' ' ) then

         call ctx%setmsg ( 'The file type is blank.' )
         call ctx%sigerr ( 'SPICE(BLANKFILETYPE)'    )
         return

      end if

!
!     Check for nonprinting characters in the file type.
!
      fnb = ltrim ( ftype )

      do i = fnb, rtrim ( ftype )

         if ( ( ichar ( ftype(i:i) ) > maxpc )   .or. &
              ( ichar ( ftype(i:i) ) < minpc ) ) then

            call ctx%setmsg ( 'The file type contains nonprinting' // &
                          ' characters.'                        )
            call ctx%sigerr ( 'SPICE(ILLEGALCHARACTER)'             )
            return

         end if

      end do

!
!     Set the value the file type in a temporary variable to be sure of
!     its length and then set the value of the ID word. Only 4
!     characters are allowed for the file type, and they are the first
!     nonblank character and its three (3), or fewer, immediate
!     successors in the input string FTYPE.
!
      ttype  = ftype(fnb:)
      idword = 'DAF/' // ttype

!
!     Make sure ND and NI are in range.
!
      if (  ( nd < 0 )  .or.  ( nd > maxnd )  ) then

         call ctx%setmsg ( 'ND was #, should be in range [0,#].' )
         call ctx%errint ( '#', nd                               )
         call ctx%errint ( '#', maxnd                            )
         call ctx%sigerr ( 'SPICE(DAFINVALIDPARAMS)'             )
         return

      end if

      if (  ( ni < 2 )  .or.  ( ni > maxni )  ) then

         call ctx%setmsg ( 'NI was #, should be in range [2,#].' )
         call ctx%errint ( '#', ni                               )
         call ctx%errint ( '#', maxni                            )
         call ctx%sigerr ( 'SPICE(DAFINVALIDPARAMS)'             )
         return

      end if

      if (  ( nd + (ni+1)/2 )  >  maxsum  ) then

         call ctx%setmsg ( 'Summary size was #, should not exceed #.' )
         call ctx%errint ( '#', nd + (ni+1)/2                         )
         call ctx%errint ( '#', maxsum                                )
         call ctx%sigerr ( 'SPICE(DAFINVALIDPARAMS)'                  )
         return

      end if

!
!     The user must reserve some non-negative number of records.
!
      if ( resv < 0 ) then

         call ctx%setmsg ( 'An attempt was made to reserve a negative '   // &
                       'number (#) of records.'                        )
         call ctx%errint ( '#', resv                                       )
         call ctx%sigerr ( 'SPICE(DAFNORESV)'                              )
         return

      end if

!
!     Attempt to create the file; perform any appropriate checks.
!
      call zzddhopn ( ctx, fname, 'NEW', 'DAF', handle )

!
!     Check FAILED(); return if an error has occurred.
!
      if ( ctx%failed() ) return

      ifn    = ifname
      fnd    = nd
      fni    = ni
      fward  = resv + 2
      bward  = fward
      crec   = ' '

      drec(1:128) = 0.0_wp
      call dafrwa ( ctx, fward+2, 1, free )

!
!     Fetch a logical unit for HANDLE.
!
      call zzddhhlu ( ctx, handle, 'DAF', .false., lun )

!
!     Check FAILED(); return if an error has occurred.
!
      if ( ctx%failed() ) return

!
!     Fetch the system file format.
!
      call zzplatfm ( ctx, 'FILE_FORMAT', format )

!
!     Write the new file record to the logical unit, LUN.
!
      call zzdafnfr ( ctx, lun, &
                      idword, &
                      fnd, &
                      fni, &
                      ifn, &
                      fward, &
                      bward, &
                      free, &
                      format  )

!
!     Check to see whether or not ZZDAFNFR generated an error writing
!     the file record to the logical unit.  In the event an error
!     occurs, return.
!
      if ( ctx%failed() ) return

!
!     Write NULL filled reserved records.
!
      if ( resv > 0 ) then

         do i = 1, crlen
            crec(i:i) = char(0)
         end do

         do i = 2, resv + 1
!
!            Place an end-of-comments marker in the first byte
!            of the first record.
!
            if ( i == 2 ) then
               crec(1:1) = char(inteoc)
            else
               crec(1:1) = char(0)
            end if

            write ( lun, rec=i, iostat=iostat ) crec
            flush ( lun )
            call files_written ( ctx )

            if ( iostat /= 0 ) then

               call zzddhcls ( ctx, handle, 'DAF', .true. )
               call ctx%setmsg ( 'Attempt to write file ''#'' failed. '   // &
                             'Value of IOSTAT was #.'                  )
               call ctx%errch  ( '#', fname                                )
               call ctx%errint ( '#', iostat                               )
               call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                     )
               return

            end if

         end do

      end if

      write ( lun, rec=fward, iostat=iostat ) drec
      flush ( lun )
      call files_written ( ctx )

      if ( iostat /= 0 ) then

         call zzddhcls ( ctx, handle, 'DAF', .true.                     )
         call ctx%setmsg   ( 'Attempt to write file ''#'' failed. '   // &
                         'Value of IOSTAT was #.'                  )
         call ctx%errch    ( '#', fname                                )
         call ctx%errint   ( '#', iostat                               )
         call ctx%sigerr   ( 'SPICE(DAFWRITEFAIL)'                     )
         return

      end if

      write ( lun, rec=fward+1, iostat=iostat ) crec
      flush ( lun )
      call files_written ( ctx )

      if ( iostat /= 0 ) then

         call zzddhcls ( ctx, handle, 'DAF', .true.                     )
         call ctx%setmsg   ( 'Attempt to write file ''#'' failed. '   // &
                         'Value of IOSTAT was #.'                  )
         call ctx%errch    ( '#', fname                                )
         call ctx%errint   ( '#', iostat                               )
         call ctx%sigerr   ( 'SPICE(DAFWRITEFAIL)'                     )
         return

      end if

!
!     Update the file table to include information about our newly
!     opened DAF.
!
      call ft%add ( daf_file ( handle = handle, links = 1, nd = fnd, ni = fni ) )

      return
      end associate
      end subroutine dafonw

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine [[dafonw]]. NAIF supports this routine only to provide
      !> backward compatibility.
      !>
      !> Open a new DAF for subsequent write requests.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of a new DAF to be created (and
      !>              consequently open for write access).
      !>
      !>     ND       is the number of double precision components
      !>              in each array summary of the new file.
      !>
      !>     NI       is the number of integer components in each
      !>              array summary in the new file.
      !>
      !>     IFNAME   is the internal file name (containing as many as 60
      !>              characters) for the new file. This should uniquely
      !>              identify the file.
      !>
      !>     RESV     is the number of records in the new file to be
      !>              reserved for non-DAF use. The user may reserve
      !>              records 2 through (2 + RESV - 1) in the file.
      !>              These records are not used to store DAF data,
      !>              and are in fact invisible to all DAF routines.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the file handle associated with the file. This
      !>              handle is used to identify the file in subsequent
      !>              calls to other DAF routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified file cannot be opened without exceeding
      !>         the maximum number of files, the error SPICE(DAFFTFULL)
      !>         is signaled.
      !>
      !>     2)  If the input argument ND is out of the range [0, 124]
      !>         or if NI is out of the range [2, 250], the error
      !>         SPICE(DAFINVALIDPARAMS) is signaled.
      !>
      !>     3)  If
      !>
      !>            ND + ( NI + 1 ) / 2   >  125
      !>
      !>         the error SPICE(DAFINVALIDPARAMS) is signaled.
      !>
      !>     4)  If the number of records to be reserved is not zero or
      !>         positive, the error SPICE(DAFNORESV) is signaled.
      !>
      !>     5)  If an I/O error occurs in the process of opening the file,
      !>         the error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     6)  If (for some reason) the initial records in the file cannot
      !>         be written, the error SPICE(DAFWRITEFAIL) is signaled.
      !>
      !>     7)  If no logical units are available, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     8)  If the file name is blank or otherwise inappropriate,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See argument FNAME.
      !>
      !>## Particulars
      !>
      !>     The DAFs created by DAFOPN have initialized file records but
      !>     do not yet contain any arrays. See the DAF Required Reading
      !>     for a discussion of file records.
      !>
      !>     This entry point has been made obsolete by the entry point DAFONW.
      !>     It is supported for reasons of backward compatibility only. New
      !>     software development should use the entry point DAFONW.
      !>
      !>## Examples
      !>
      !>     In the following code fragment, DAFOPN is used to open a file,
      !>     to which a new array is then added.
      !>
      !>        CALL DAFOPN   ( FNAME,  ND,  NI,  IFNAME, 0, HANDLE )
      !>
      !>        CALL DAFBNA   ( HANDLE, SUM, NAME  )
      !>        CALL GET_DATA ( DATA,   N,   FOUND )
      !>
      !>        DO WHILE ( FOUND )
      !>
      !>           CALL DAFADA   ( DATA, N        )
      !>           CALL GET_DATA ( DATA, N, FOUND )
      !>
      !>        END DO
      !>
      !>        CALL DAFENA
      !>
      !>## Restrictions
      !>
      !>     1)  Files opened using this routine must be closed with DAFCLS.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.1.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.1.1, 10-OCT-2012 (EDW)
      !>
      !>        Edited $Abstract section to use "Deprecated" keyword
      !>        and state replacement routine.
      !>
      !>        Corrected ordering of header section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
      !>
      !>        This routine was updated to accommodate changes to the
      !>        handle manager interface. See DAFAH's Revision section
      !>        for details.
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 4.0.0, 03-MAR-1999 (FST)
      !>
      !>        The entry point was modified to insert the FTP validation
      !>        string, as well as the binary file format into the file record.
      !>
      !>-    SPICELIB Version 3.1.0, 08-MAR-1996 (KRG)
      !>
      !>        The modifications support the notion of a DAF comment area,
      !>        and involve writing NULL filled reserved records when the
      !>        number of reserved records is greater than zero (0).
      !>
      !>        Some nested IF...THEN...ELSE IF...THEN...END IF constructs
      !>        were expanded to be independent IF...THEN...END IF tests.
      !>        The tests were for IOSTAT errors on cascading write statements
      !>        nested in the IF...ELSE IF... statements, and this was
      !>        confusing. These tests were restructured so that IOSTAT is
      !>        tested after each write statement which is equivalent to the
      !>        original intent and easier to read.
      !>
      !>-    SPICELIB Version 3.0.0, 29-SEP-1993 (KRG)
      !>
      !>        Modified the logical structure of some
      !>           IF ... THEN ... ELSE IF... END IF
      !>        statements which were testing different items in each ELSE IF
      !>        clause for failure into separate IF ... END IF statements. This
      !>        improved the readability and supportability of the code.
      !>
      !>-    SPICELIB Version 2.1.0, 25-FEB-1993 (JML)
      !>
      !>        A new variable LUN is used for the logical unit number
      !>        returned by GETLUN.
      !>
      !>        The file name is checked to see if it is blank.
      !>
      !>        The file name string that is passed to the FORTRAN OPEN and
      !>        INQUIRE statements has been chopped at the last non-blank
      !>        character.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 03-SEP-1991 (NJB) (HAN) (WLT)
      !>
      !>        Updated to allow multiple DAFs to be open for write
      !>        access simultaneously. An error in a calling sequence
      !>        shown in the $Examples section was corrected.
      !>
      !>        This routine was updated so that it now keeps current the set
      !>        of DAF handles returned by DAFHOF.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.0.0, 03-MAR-1999 (FST)
      !>
      !>        See the $Revisions section under DAFAH for a discussion
      !>        of the impact of the changes made for this version.
      !>
      !>-    SPICELIB Version 2.0.0, 03-SEP-1991 (NJB) (HAN) (WLT)
      !>
      !>        Updated to allow multiple DAFs to be open for write
      !>        access simultaneously.
      !>
      !>        This routine was updated so that it now keeps current the set
      !>        of DAF handles returned by DAFHOF.
      !>
      !>        Invalid values of ND and NI are now screened; two new
      !>        exceptions were added to the $Exceptions header section.
      !>
      !>        The calling sequence of DAFADA shown in the first example
      !>        in the $Examples section was reversed; this was fixed.
      !>
      !>        Some error messages were changed so that they specify
      !>        names of relevant DAFs.
      module subroutine dafopn ( ctx, fname, nd, ni, ifname, resv, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of DAF to be opened.
      integer(ip), intent(in) :: nd  !! Number of double precision components in summaries.
      integer(ip), intent(in) :: ni  !! Number of integer components in summaries.
      character(*), intent(in) :: ifname  !! Internal file name.
      integer(ip), intent(in) :: resv  !! Number of records to reserve.
      integer(ip), intent(out) :: handle  !! Handle assigned to DAF.
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: found
      associate ( ft => ctx%spice%dafah, &
                  nft => ctx%spice%dafah%nft )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Initialize the handle list, if necessary.
!
!     Check to see if there is room in the file table.
!
!     PEPPER: the file table grows as needed; SPICELIB signaled
!     SPICE(DAFFTFULL) here when it had FTSIZE (5000) DAFs.
!

!
!     Make sure ND and NI are in range.
!
      if (  ( nd < 0 )  .or.  ( nd > maxnd )  ) then

         call ctx%setmsg ( 'ND was #, should be in range [0,#].' )
         call ctx%errint ( '#', nd                               )
         call ctx%errint ( '#', maxnd                            )
         call ctx%sigerr ( 'SPICE(DAFINVALIDPARAMS)'             )
         return

      end if

      if (  ( ni < 2 )  .or.  ( ni > maxni )  ) then

         call ctx%setmsg ( 'NI was #, should be in range [2,#].' )
         call ctx%errint ( '#', ni                               )
         call ctx%errint ( '#', maxni                            )
         call ctx%sigerr ( 'SPICE(DAFINVALIDPARAMS)'             )
         return

      end if

      if (  ( nd + (ni+1)/2 )  >  maxsum  ) then

         call ctx%setmsg ( 'Summary size was #, should not exceed #.' )
         call ctx%errint ( '#', nd + (ni+1)/2                         )
         call ctx%errint ( '#', maxsum                                )
         call ctx%sigerr ( 'SPICE(DAFINVALIDPARAMS)'                  )
         return

      end if

!
!     The user must reserve some non-negative number of records.
!
      if ( resv < 0 ) then

         call ctx%setmsg ( 'An attempt was made to reserve a negative '   // &
                       'number (#) of records.'                        )
         call ctx%errint ( '#', resv                                       )
         call ctx%sigerr ( 'SPICE(DAFNORESV)'                              )
         return

      end if

!
!     Attempt to create the file; perform any appropriate checks.
!
      call zzddhopn ( ctx, fname, 'NEW', 'DAF', handle )

!
!     Check FAILED(); return if an error has occurred.
!
      if ( ctx%failed() ) return

      ifn    = ifname
      fnd    = nd
      fni    = ni
      fward  = resv + 2
      bward  = fward
      crec   = ' '

      drec(1:128) = 0.0_wp
      call dafrwa ( ctx, fward+2, 1, free )

!
!     Fetch a logical unit for HANDLE.
!
      call zzddhhlu ( ctx, handle, 'DAF', .false., lun )

!
!     Check FAILED(); return if an error has occurred.
!
      if ( ctx%failed() ) return

!
!     Fetch the system file format.
!
      call zzplatfm ( ctx, 'FILE_FORMAT', format )

!
!     Write the new file record to the logical unit, LUN.
!
      call zzdafnfr ( ctx, lun, &
                      'NAIF/DAF', &
                      fnd, &
                      fni, &
                      ifn, &
                      fward, &
                      bward, &
                      free, &
                      format  )
!
!     Check to see whether or not ZZDAFNFR generated an error writing
!     the file record to the logical unit.  In the event an error
!     occurs, return.
!
      if ( ctx%failed() ) return

!
!     Write NULL filled reserved records.
!
      if ( resv > 0 ) then

         do i = 1, crlen
            crec(i:i) = char(0)
         end do

         do i = 2, resv + 1

            write ( lun, rec=i, iostat=iostat ) crec
            flush ( lun )
            call files_written ( ctx )

            if ( iostat /= 0 ) then

               call zzddhcls ( ctx, handle, 'DAF', .true. )

               call ctx%setmsg ( 'Attempt to write file ''#'' failed. '   // &
                             'Value of IOSTAT was #.'                  )
               call ctx%errch  ( '#', fname                                )
               call ctx%errint ( '#', iostat                               )
               call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                     )
               return

            end if

         end do

      end if

      write ( lun, rec=fward, iostat=iostat ) drec
      flush ( lun )
      call files_written ( ctx )

      if ( iostat /= 0 ) then

         call zzddhcls ( ctx, handle, 'DAF', .true. )

         call ctx%setmsg ( 'Attempt to write file ''#'' failed. '   // &
                       'Value of IOSTAT was #.'                  )
         call ctx%errch  ( '#', fname                                )
         call ctx%errint ( '#', iostat                               )
         call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                     )
         return

      end if

      write ( lun, rec=fward+1, iostat=iostat ) crec
      flush ( lun )
      call files_written ( ctx )

      if ( iostat /= 0 ) then

         call zzddhcls ( ctx, handle, 'DAF', .true. )

         call ctx%setmsg ( 'Attempt to write file ''#'' failed. '   // &
                       'Value of IOSTAT was #.'                  )
         call ctx%errch  ( '#', fname                                )
         call ctx%errint ( '#', iostat                               )
         call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                     )
         return

      end if

!
!     Update the file table to include information about
!     our newly opened DAF.
!
      call ft%add ( daf_file ( handle = handle, links = 1, nd = fnd, ni = fni ) )

      return
      end associate
      end subroutine dafopn

      !> Close the DAF associated with a given handle.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the file handle of a previously opened DAF file.
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
      !>     1)  If the specified handle does not belong to a DAF
      !>         that is currently open, nothing happens.
      !>
      !>     2)  If this routine is used to close a HANDLE not associated
      !>         with a DAF, an error is signaled by a routine in the call tree
      !>         of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Because DAFAH and its entry points must keep track of what
      !>     files are open at any given time, it is important that DAF
      !>     files be closed only with DAFCLS, to prevent the remaining
      !>     DAF routines from failing, sometimes mysteriously.
      !>
      !>     Note that when a file is opened more than once for read access,
      !>     DAFOPR returns the same handle each time it is re-opened.
      !>     Each time the file is closed, DAFCLS checks to see if any other
      !>     claims on the file are still active before physically closing
      !>     the file.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) In the following code fragment, the arrays in a file are
      !>        examined in order to determine whether the file contains
      !>        any arrays whose names begin with the word TEST.
      !>        The complete names for these arrays are printed to
      !>        the screen. The file is closed at the end of the search.
      !>
      !>           CALL DAFOPR ( FNAME, HANDLE )
      !>           CALL DAFBFS ( HANDLE )
      !>           CALL DAFFNA ( FOUND  )
      !>
      !>           DO WHILE ( FOUND )
      !>              CALL DAFGN ( NAME )
      !>
      !>              IF ( NAME(1:5) .EQ. 'TEST ' ) THEN
      !>                 WRITE (*,*) NAME
      !>              END IF
      !>
      !>              CALL DAFFNA ( FOUND )
      !>           END DO
      !>
      !>           CALL DAFCLS ( HANDLE )
      !>
      !>        Note that if the file has been opened already by a DAF routine
      !>        at some other place in the calling program, it remains open.
      !>        This makes it possible to examine files that have been opened
      !>        for use by other modules without interfering with the operation
      !>        of those routines.
      !>
      !>     2) Use a simple routine to output the double precision and integer
      !>        values stored in an SPK's segments descriptors. This function
      !>        opens a DAF for read, performs a forwards search for the DAF
      !>        arrays, prints segments description for each array found, then
      !>        closes the DAF.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           de421.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFCLS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Define the summary parameters appropriate
      !>C     for an SPK file.
      !>C
      !>      INTEGER               MAXSUM
      !>      PARAMETER           ( MAXSUM = 125 )
      !>
      !>      INTEGER               ND
      !>      PARAMETER           ( ND = 2 )
      !>
      !>      INTEGER               NI
      !>      PARAMETER           ( NI = 6 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(32)        KERNEL
      !>
      !>      DOUBLE PRECISION      DC     ( ND     )
      !>      DOUBLE PRECISION      SUM    ( MAXSUM )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               IC     ( NI     )
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Open a DAF for read. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      KERNEL = 'de421.bsp'
      !>      CALL DAFOPR ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Begin a forward search on the file.
      !>C
      !>      CALL DAFBFS ( HANDLE )
      !>
      !>C
      !>C     Search until a DAF array is found.
      !>C
      !>      CALL DAFFNA ( FOUND )
      !>
      !>C
      !>C     Loop while the search finds subsequent DAF arrays.
      !>C
      !>      DO WHILE ( FOUND )
      !>
      !>         CALL DAFGS ( SUM )
      !>         CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>         WRITE(*,*)                'Doubles:', DC(1:ND)
      !>         WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
      !>
      !>C
      !>C        Check for another segment.
      !>C
      !>         CALL DAFFNA ( FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        1        0        1        2      641   310404
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        2        0        1        2   310405   423048
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        3        0        1        2   423049   567372
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        4        0        1        2   567373   628976
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        5        0        1        2   628977   674740
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        6        0        1        2   674741   715224
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        7        0        1        2   715225   750428
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        8        0        1        2   750429   785632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        9        0        1        2   785633   820836
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:       10        0        1        2   820837   944040
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      301        3        1        2   944041  1521324
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      399        3        1        2  1521325  2098608
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      199        1        1        2  2098609  2098620
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      299        2        1        2  2098621  2098632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      499        4        1        2  2098633  2098644
      !>
      !>        Note, the final entries in the integer array contain the
      !>        segment start/end indexes. The output indicates the search
      !>        proceeded from the start of the file (low value index) towards
      !>        the end (high value index).
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.1.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Updated code example with IMPLICIT NONE, and declarations of
      !>        SUM variable and MAXSUM parameter.
      !>
      !>        Corrected minor typos in header.
      !>
      !>-    SPICELIB Version 8.1.1, 10-OCT-2012 (EDW)
      !>
      !>        Added a functional code example to the $Examples section.
      !>
      !>        Removed the unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>        Corrected ordering of header section.
      !>
      !>-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
      !>
      !>        This routine was updated to accommodate changes to the
      !>        handle manager interface. See DAFAH's Revision section
      !>        for details.
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 2.0.3, 29-SEP-1993 (KRG)
      !>
      !>        Removed references to specific DAF file open routines in the
      !>        $Detailed_Input section of the header. This was done in order
      !>        to minimize documentation changes if these open routines ever
      !>        change.
      !>
      !>-    SPICELIB Version 2.0.2, 25-FEB-1993 (JML)
      !>
      !>        A minor error in the $Particulars section of the header was
      !>        corrected. It formerly stated that a file could be open more
      !>        than once for read or write access instead of just read access.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 03-SEP-1991 (NJB) (WLT)
      !>
      !>        This routine was updated so that it now keeps current the set
      !>        of DAF handles returned by DAFHOF.
      !>
      !>        Upgraded to support file handle checking routines
      !>        DAFHOF and DAFSIH. DAFCLS now initializes the file
      !>        handle list if necessary, and removes from the list
      !>        the handles of files it closes.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafcls ( ctx, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF to be closed.
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: found
      associate ( ft => ctx%spice%dafah, &
                  nft => ctx%spice%dafah%nft )
      if ( ctx%failed() ) return

!
!     Initialize the handle list, if necessary.
!
!     Is this file even open? If so, decrement the number of links
!     to the file. If the number of links drops to zero, physically
!     close the file and remove it from the file buffer.
!
!     If the file is not open: no harm, no foul.
!
      findex = ft%find ( handle )

      if ( findex > 0 ) then

         ft%files(findex)%links = ft%files(findex)%links - 1

         if ( ft%files(findex)%links == 0 ) then

            call zzddhcls ( ctx, handle, 'DAF', .false. )

            call ft%remove ( findex )

         end if

      end if

      return
      end associate
      end subroutine dafcls

      !> Return the summary format associated with a handle.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a previously opened
      !>              DAF file.
      !>
      !>## Detailed Output
      !>
      !>     ND,
      !>     NI       are the numbers of double precision and integer
      !>              components, respectively, in each array summary
      !>              in the specified file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified handle does not belong to any file that is
      !>         currently known to be open, the error SPICE(DAFNOSUCHHANDLE)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The summary format must be known in order to pack or unpack
      !>     an array summary. See the DAF Required Reading for a discussion
      !>     of summary formats.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the number of d.p. `words' in a DAF having an
      !>        arbitrary summary format.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           de421.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFHSF_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Count the number of d.p. words of data in a
      !>C     DAF.  Exclude array summaries, reserved records,
      !>C     the file record, and character records.
      !>C
      !>      INTEGER               FILEN
      !>      PARAMETER           ( FILEN  = 128 )
      !>
      !>      INTEGER               MAXND
      !>      PARAMETER           ( MAXND  = 124 )
      !>
      !>      INTEGER               MAXNI
      !>      PARAMETER           ( MAXNI  = 250 )
      !>
      !>      INTEGER               MAXSUM
      !>      PARAMETER           ( MAXSUM = 125 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(FILEN)     DAF
      !>
      !>      DOUBLE PRECISION      DC    ( MAXND  )
      !>      DOUBLE PRECISION      SUM   ( MAXSUM )
      !>
      !>      INTEGER               FA
      !>      INTEGER               HANDLE
      !>      INTEGER               IA
      !>      INTEGER               IC    ( MAXNI )
      !>      INTEGER               N
      !>      INTEGER               ND
      !>      INTEGER               NI
      !>
      !>      LOGICAL               FOUND
      !>
      !>      DAF = 'de421.bsp'
      !>
      !>C
      !>C     Open the DAF and find the summary format.
      !>C
      !>      CALL DAFOPR ( DAF,    HANDLE )
      !>      CALL DAFHSF ( HANDLE, ND, NI )
      !>
      !>C
      !>C     Start a forward search and examine each array in
      !>C     turn.
      !>C
      !>      CALL DAFBFS ( HANDLE )
      !>      CALL DAFFNA ( FOUND  )
      !>
      !>      N = 0
      !>      DO WHILE ( FOUND )
      !>
      !>C
      !>C        Obtain the array summary, unpack it, and get
      !>C        the initial and final array addresses from
      !>C        the integer descriptor component.
      !>C
      !>         CALL DAFGS ( SUM )
      !>         CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>         IA  =  IC ( NI - 1 )
      !>         FA  =  IC ( NI     )
      !>
      !>         N   =  FA - IA + 1 + N
      !>
      !>         CALL DAFFNA ( FOUND )
      !>
      !>      END DO
      !>
      !>      WRITE (*,*) 'Number of d.p. words is ', N
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Number of d.p. words is      2098004
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.0.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Changed example
      !>        code to hardcode the DAF file used as input.
      !>
      !>-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
      !>
      !>        Corrected ordering of header section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 1.0.4, 29-SEP-1993 (KRG)
      !>
      !>        Removed references to specific DAF file open routines in the
      !>        $Detailed_Input section of the header. This was done in order
      !>        to minimize documentation changes if these open routines ever
      !>        change.
      !>
      !>-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.2, 03-SEP-1990 (NJB)
      !>
      !>        Example added to the $Examples section.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafhsf ( ctx, handle, nd, ni )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: nd  !! Number of double precision components in summaries.
      integer(ip), intent(out) :: ni  !! Number of integer components in summaries.
      integer(ip), intent(in) :: handle  !! Handle of a DAF file.
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: found
      associate ( ft => ctx%spice%dafah, &
                  nft => ctx%spice%dafah%nft )
      if ( ctx%failed() ) return

      findex = ft%find ( handle )

      if ( findex > 0 ) then

         nd = ft%files(findex)%nd
         ni = ft%files(findex)%ni

      else

         call ctx%setmsg ( 'There is no DAF open with handle = #' )
         call ctx%errint ( '#', handle                            )
         call ctx%sigerr ( 'SPICE(DAFNOSUCHHANDLE)'               )

      end if

      return
      end associate
      end subroutine dafhsf

      !> Return the logical unit associated with a handle.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a previously opened
      !>              DAF file.
      !>
      !>## Detailed Output
      !>
      !>     UNIT     is the Fortran logical unit to which the file is
      !>              connected.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any error occurs while attempting to fetch a logical unit,
      !>         the error is signaled by a routine in the call tree of this
      !>         routine. The value of UNIT in this case is undefined.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The best reason for knowing the logical unit to which a DAF
      !>     is connected is to read or write from the records reserved in a
      !>     file. Since these records are by definition invisible to the DAF
      !>     routines, you must read and write them directly.
      !>
      !>## Examples
      !>
      !>     In the following code fragment, the first reserved record in
      !>     a newly created DAF is used to store the name and address
      !>     of the person who created it.
      !>
      !>        FTYPE = 'TEST'
      !>        CALL DAFONW ( FNAME, FTYPE, 3, 6, IFNAME, 5, HANDLE )
      !>        CALL DAFHLU ( HANDLE, UNIT )
      !>
      !>        WRITE (UNIT,REC=2) 'Ellis Wyatt, JPL ',
      !>       .                   '4800 Oak Grove Drive ',
      !>       .                   'Room 301-125A ',
      !>       .                   'Pasadena, CA 91109'
      !>
      !>## Restrictions
      !>
      !>     1)  This routine may only be used to retrieve logical units
      !>         for DAFs loaded or created using the interfaces available
      !>         in this entry point umbrella. Using this entry point to
      !>         retrieve units for files not loaded through these interfaces
      !>         may result in unexpected behavior.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.0.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
      !>
      !>        Corrected ordering of header section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 1.0.3, 29-SEP-1993 (KRG)
      !>
      !>        Removed references to specific DAF file open routines in the
      !>        $Detailed_Input section of the header. This was done in order
      !>        to minimize documentation changes if these open routines ever
      !>        change.
      !>
      !>        Changed the example to use the new entry point DAFONW.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        Successfully invoking this module has the side effect of
      !>        locking UNIT to HANDLE. This 'lock' guarantees until
      !>        HANDLE is closed (or unlocked) that the file associated
      !>        with HANDLE is always open and attached to logical unit
      !>        UNIT. To unlock a handle without closing the file, use
      !>        ZZDDHUNL, an entry point in the handle manager umbrella,
      !>        ZZDDHMAN.
      !>
      !>        The system can lock at most UTSIZE-SCRUNT-RSVUNT
      !>        simultaneously (see the include file 'zzddhman.inc' for
      !>        specific values of these parameters), but unnecessarily
      !>        locking handles to their logical units may cause performance
      !>        degradation. The handle manager will have less logical
      !>        units to utilize when disconnecting and reconnecting
      !>        loaded files.
      !>
      !>-    Beta Version 1.1.0, 1-NOV-1989 (RET)
      !>
      !>        DAFHLU now only checks in and checks out if the one exception
      !>        occurs. The purpose of this change was to help speed up a
      !>        routine that gets called constantly by higher level DAF
      !>        routines.
      module subroutine dafhlu ( ctx, handle, unit )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a DAF file.
      integer(ip), intent(out) :: unit  !! Corresponding logical unit.
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: found
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      call zzddhhlu ( ctx, handle, 'DAF', .true., unit )

      return
      end subroutine dafhlu

      !> Return the handle associated with a logical unit.
      !>
      !>## Detailed Input
      !>
      !>     UNIT     is the logical unit to which a DAF has been
      !>              connected after it has been opened.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the handle associated with the file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified unit is not connected to any file that is
      !>         currently loaded as a DAF, the error SPICE(DAFNOSUCHUNIT)
      !>         is signaled. The value of HANDLE returned is undefined in
      !>         this case.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     It is unlikely, but possible, that a calling program would know
      !>     the logical unit to which a file is connected without knowing the
      !>     handle associated with the file. DAFLUH is provided mostly for
      !>     completeness.
      !>
      !>## Examples
      !>
      !>     In the following code fragment, the handle associated with
      !>     a DAF is retrieved using the logical unit to which the
      !>     file is connected. The handle is then used to determine the
      !>     name of the file.
      !>
      !>        CALL DAFLUH ( UNIT,   HANDLE )
      !>        CALL DAFHFN ( HANDLE, FNAME )
      !>
      !>## Restrictions
      !>
      !>     1)  This routine may only be used to retrieve handles for logical
      !>         units connected to DAFs loaded or created using the interfaces
      !>         available in this entry point umbrella. Using this entry point
      !>         to retrieve handles for files not loaded through these
      !>         interfaces may result in unexpected behavior.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.0.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
      !>
      !>        Corrected ordering of header section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 1.0.3, 29-SEP-1993 (KRG)
      !>
      !>        Removed references to specific DAF file open routines in the
      !>        $Detailed_Input section of the header. This was done in order
      !>        to minimize documentation changes if these open routines ever
      !>        change.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafluh ( ctx, unit, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: handle  !! Corresponding DAF file handle.
      integer(ip), intent(in) :: unit  !! Logical unit connected to a DAF.
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(1) :: dafnam  ! PEPPER: the name is not used (ZZDDHNFO cuts it)
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: found
      if ( ctx%failed() ) return

      call zzddhluh ( ctx, unit, handle, found )

      if ( .not. found ) then
         handle = 0
         call ctx%setmsg ( 'There is no file open with unit = #' )
         call ctx%errint ( '#', unit                             )
         call ctx%sigerr ( 'SPICE(DAFNOSUCHUNIT)'                )
         return
      end if

!
!     Now make certain that the HANDLE is associated with a DAF.
!
      call zzddhnfo ( ctx, handle, dafnam, iarc, ibff, iamh, found )

      if ( iarc /= daf ) then
         handle = 0
         call ctx%setmsg ( 'The file, ''#'', connected to unit # ' &
         //            'is not a DAF.'                         )
         call ctx%errfnm ( '#', unit                               )
         call ctx%errint ( '#', unit                               )
         call ctx%sigerr ( 'SPICE(DAFNOSUCHUNIT)'                  )
         return
      end if

      return
      end subroutine dafluh

      !> Return the name of the file associated with a handle.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a previously opened
      !>              DAF file.
      !>
      !>## Detailed Output
      !>
      !>     FNAME    is the name of the file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified handle does not belong to any file that is
      !>         currently known to be loaded as a DAF, the error
      !>         SPICE(DAFNOSUCHHANDLE) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     It may be desirable to recover the names of one or more DAF
      !>     files in a different part of the program from the one in which
      !>     they were opened. Note that the names returned by DAFHFN may
      !>     not be identical to the names used to open the files. Under
      !>     most operating systems, a particular file can be accessed using
      !>     many different names. DAFHFN returns one of them.
      !>
      !>## Examples
      !>
      !>     In the following code fragment, the name of a DAF is
      !>     recovered using the handle associated with the file.
      !>
      !>        CALL DAFOPR ( 'sample.DAF', HANDLE )
      !>         .
      !>         .
      !>
      !>        CALL DAFHFN ( HANDLE, FNAME )
      !>
      !>     Depending on the circumstances (operating system, compiler,
      !>     default directory) the value of FNAME might resemble any of
      !>     the following:
      !>
      !>        'USER$DISK:[WYATT.IMAGES]SAMPLE.DAF;4'
      !>
      !>        '/wyatt/images/sample.DAF'
      !>
      !>        'A:\IMAGES\SAMPLE.DAF'
      !>
      !>     On the other hand, it might not.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine may only be used to retrieve the names of DAFs
      !>         loaded or created using the interfaces available in this entry
      !>         point umbrella. Using this entry point to retrieve names for
      !>         files not loaded through these interfaces may result in
      !>         unexpected behavior.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.0.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
      !>
      !>        Corrected ordering of header section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 1.1.1, 29-SEP-1993 (KRG)
      !>
      !>        Removed references to specific DAF file open routines in the
      !>        $Detailed_Input section of the header. This was done in order
      !>        to minimize documentation changes if these open routines ever
      !>        change.
      !>
      !>-    SPICELIB Version 1.1.0, 25-FEB-1993 (JML)
      !>
      !>        IOSTAT is checked after the INQUIRE statement.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafhfn ( ctx, handle, fname )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: fname  !! Corresponding file name.
      integer(ip), intent(in) :: handle  !! Handle of a DAF file.
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      character(1) :: dafnam  ! PEPPER: the name is taken whole below
      logical :: found
      if ( ctx%failed() ) return

      call zzddhnfo ( ctx, handle, dafnam, iarc, ibff, iamh, found )

      if ( ( .not. found ) .or. ( iarc /= daf ) ) then
         call ctx%setmsg ( 'There is no file open with handle = #' )
         call ctx%errint ( '#', handle                             )
         call ctx%sigerr ( 'SPICE(DAFNOSUCHHANDLE)'                )
         return
      end if
      fname = file_name ( ctx%spice, handle )
      return
      end subroutine dafhfn

      !> Return handle associated with a file name.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of a previously opened DAF file.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the handle associated with the file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the specified name does not specify any file currently
      !>         known to be loaded as a DAF, the error SPICE(DAFNOSUCHFILE) is
      !>         signaled. The value of HANDLE is undefined in this case.
      !>
      !>     2)  If the file does not exist, an error is signaled by a routine
      !>         in the call tree of this routine. The value of HANDLE is
      !>         undefined in this case.
      !>
      !>     3)  If an I/O error is generated in the process of connecting the
      !>         specified name with a handle, the error is signaled by a
      !>         routine in the call tree of this routine. The value of
      !>         HANDLE is undefined in this case.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     It is sometimes easier to work with file names (which are
      !>     meaningful, and often predictable) than with file handles
      !>     (which are neither), especially in interactive situations.
      !>     However, nearly every DAF routines requires that you use file
      !>     handles to refer to files. DAFFNH is provided to bridge the gap
      !>     between the two representations.
      !>
      !>## Examples
      !>
      !>     In the following code fragment, the handle associated with a
      !>     DAF is recovered using the name of the file.
      !>
      !>        CALL DAFOPR ( 'sample.DAF', HANDLE )
      !>         .
      !>         .
      !>
      !>        CALL DAFFNH ( 'sample.DAF', HANDLE )
      !>
      !>## Restrictions
      !>
      !>     1)  Only file names of DAFs loaded with interfaces present in
      !>         this entry point umbrella should be passed into this routine.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.1.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.1.1, 10-OCT-2012 (EDW)
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>        Corrected ordering of header section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 8.1.0, 02-APR-2002 (FST)
      !>
      !>        Fixed a bug, where an error was signaled but the call to
      !>        CHKOUT and the RETURN statement were omitted.
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>        In previous version of DAFAH, this module simply
      !>        performed an INQUIRE on FNAME and looked in the
      !>        file table for the logical unit returned.
      !>
      !>        The integration of the new handle manager interfaces
      !>        into this entry point has the possibility of increasing
      !>        the complexity of this routine when more than UTSIZE
      !>        files are loaded. Essentially, when given an arbitrary
      !>        name, a total of FTSIZE INQUIRE statements may be executed
      !>        to accurately connect FNAME with HANDLE. See ZZDDHFNH and
      !>        ZZDDHF2H for details.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 2.0.1, 29-SEP-1993 (KRG)
      !>
      !>        Removed references to specific DAF file open routines in the
      !>        $Detailed_Input section of the header. This was done in order
      !>        to minimize documentation changes if these open routines ever
      !>        change.
      !>
      !>-    SPICELIB Version 2.0.0, 25-FEB-1993 (JML)
      !>
      !>        The INQUIRE statement that checks if the file is open now also
      !>        checks that the file exists. Two new exceptions were added as
      !>        a result of this change.
      !>
      !>        A RETURN statement was added after the error signaled when
      !>        the file is not open.
      !>
      !>        The file name is checked to see if it is blank.
      !>
      !>        The file name string that is passed to the FORTRAN INQUIRE
      !>        statement has been chopped at the last non-blank character.
      !>
      !>-    SPICELIB Version 1.1.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.1.1, 18-SEP-1991 (HAN)
      !>
      !>        The $Revisions section was incorrectly named $Version. This has
      !>        been fixed.
      !>
      !>-    SPICELIB Version 1.1.0, 05-NOV-1990 (HAN)
      !>
      !>        Call to CHKIN was corrected. The module was checking in
      !>        as 'DAFFHN'.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine daffnh ( ctx, fname, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of a DAF file.
      integer(ip), intent(out) :: handle  !! Corresponding DAF file handle.
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(1) :: dafnam  ! PEPPER: the name is not used (ZZDDHNFO cuts it)
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: found
      if ( ctx%failed() ) return

      call zzddhfnh ( ctx, fname, handle, found )

      if ( ( .not. found ) ) then
         handle = 0
         call ctx%setmsg ( 'There is no file in the DAF table with '  // &
                       'file name = ''#'''                          )
         call ctx%errch  ( '#', fname                                   )
         call ctx%sigerr ( 'SPICE(DAFNOSUCHFILE)'                       )
         return
      end if

!
!     Now make certain that HANDLE is associated with a DAF.
!
      call zzddhnfo ( ctx, handle, dafnam, iarc, ibff, iamh, found )

      if ( iarc /= daf ) then
         handle = 0
         call ctx%setmsg ( 'The file, ''#'', is not a DAF.' )
         call ctx%errch  ( '#', fname                       )
         call ctx%sigerr ( 'SPICE(DAFNOSUCHFILE)'           )
         return
      end if

      return
      end subroutine daffnh

      !> Return a SPICE set containing the handles of all currently
      !> open DAFS.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     FHSET    is a SPICE set containing the file handles of
      !>              all currently open DAFs.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the set FHSET is not initialized, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If the set FHSET is too small to accommodate the set of
      !>         handles to be returned, an error is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine allows subroutines to test file handles for
      !>     validity before performing operations on them, such as
      !>     finding the name of the file designated by a handle. Many
      !>     DAF operations on handles cause errors to be signaled if
      !>     the handles are invalid.
      !>
      !>## Examples
      !>
      !>     1)  Find out how may DAFs are open for writing.
      !>
      !>            C
      !>            C    Find out which DAFs are open.
      !>            C
      !>                 CALL DAFHOF  ( FHSET )
      !>
      !>            C
      !>            C    Count the ones open for writing. These have
      !>            C    negative file handles.
      !>            C
      !>                 COUNT = 0
      !>
      !>                 DO I = 1, CARDC(FHSET)
      !>
      !>                    IF ( FHSET(I) .LT. 0 ) THEN
      !>                       COUNT = COUNT + 1
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
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.0.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
      !>
      !>        Corrected ordering of header section.
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 03-SEP-1991 (NJB) (WLT)
      module subroutine dafhof ( ctx, fhset )
      use pepper_arrays, only: shelli
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip), parameter :: lbcell = -5
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: fhset ( lbcell : * )  !! A set containing handles of
                                                          !! currently
                                                        !! open DAFS.
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: found
      associate ( ft => ctx%spice%dafah )
      if ( ctx%failed() ) return

!
!     Just stuff our local list into the set. (PEPPER: the set is built
!     from the file table, SPICELIB kept it as FHLIST; COPYI signals
!     SPICE(CELLTOOSMALL) as before if FHSET has no room.)
!
      block
         integer(ip), allocatable :: set(:)
         integer(ip) :: k
         allocate ( set(lbcell:max(1_ip, ft%nft)) )
         call ssizei ( ctx, max(1_ip, ft%nft), set )
         do k = 1, ft%nft
            set(k) = ft%files(k)%handle
         end do
         call shelli ( ft%nft, set(1:) )
         call scardi ( ctx, ft%nft, set )
         call copyi ( ctx, set, fhset )
      end block

      return
      end associate
      end subroutine dafhof

      !> Signal an error if a DAF file handle does not designate a DAF
      !> that is open for a specified type of access.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is a DAF handle to validate. For HANDLE to be
      !>              considered valid, it must specify a DAF that is
      !>              open for the type of access specified by the input
      !>              argument ACCESS.
      !>
      !>     ACCESS   is a string indicating the type of access that
      !>              the DAF specified by the input argument HANDLE
      !>              must be open for. The values of ACCESS may be
      !>
      !>                 'READ'      File must be open for read access
      !>                             by DAF routines. All open DAFs
      !>                             may be read.
      !>
      !>                 'WRITE'     File must be open for write access
      !>                             by DAF routines.
      !>
      !>                             Note that files open for write
      !>                             access may be read as well as
      !>                             written.
      !>
      !>              Leading and trailing blanks in ACCESS are ignored,
      !>              and case is not significant.
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
      !>     1)  If the input argument ACCESS has an unrecognized value,
      !>         the error SPICE(INVALIDOPTION) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine signals the error SPICE(DAFINVALIDACCESS) if the
      !>     DAF designated by the input argument HANDLE is not open
      !>     for the specified type of access. If HANDLE does not designate
      !>     an open DAF, the error SPICE(DAFNOSUCHHANDLE) is signaled.
      !>
      !>     This routine allows subroutines to test file handles for
      !>     validity before performing operations on them, such as
      !>     finding the name of the file designated by a handle. Many
      !>     DAF operations on handles may cause unpredictable program
      !>     behavior if the handles are invalid. This routine should
      !>     be used in situations where the appropriate action to take upon
      !>     determining that a handle is invalid is to signal an error.
      !>     DAFSIH centralizes the error response for this type of error in a
      !>     single routine.
      !>
      !>     In cases where it is necessary to determine the validity of a
      !>     file handle, but it is not an error for the handle to refer
      !>     to a closed file, the entry point DAFHOF should be used instead
      !>     of DAFSIH.
      !>
      !>## Examples
      !>
      !>     1)  Add data to a DAF specified by a file handle. Signal an
      !>         error if the file is not open for writing. Check the
      !>         SPICELIB error status function FAILED after calling
      !>         DAFSIH, so that the routine will return if DAFSIH
      !>         signaled an error (we're presuming that this code
      !>         fragment would be used in a subroutine).
      !>
      !>            C
      !>            C     Check that HANDLE is valid, then add data to the
      !>            C     file specified by HANDLE.
      !>            C
      !>                  CALL DAFSIH  (  HANDLE, 'WRITE' )
      !>
      !>                  IF ( FAILED() ) THEN
      !>                     RETURN
      !>                  END IF
      !>
      !>                  CALL DAFBNA (  HANDLE,  SUM,    NAME )
      !>                  CALL DAFADA (  DATA,    N            )
      !>                  CALL DAFENA
      !>
      !>     2)  Find the size of an array in a DAF specified by a file
      !>         handle. Signal an error if the file is not open for reading.
      !>
      !>            C
      !>            C     Check that HANDLE is valid, then obtain the
      !>            C     current array summary and compute the size of
      !>            C     the current array.
      !>            C
      !>                  CALL DAFSIH  ( HANDLE, 'READ' )
      !>
      !>                  IF ( FAILED() ) THEN
      !>                     RETURN
      !>                  END IF
      !>
      !>            C
      !>            C     Obtain the summary format, then the integer and d.p.
      !>            C     components of the summary. Finally, compute the
      !>            C     array length.
      !>            C
      !>                  CALL DAFHSF (  HANDLE, ND, NI          )
      !>                  CALL DAFGS  (  SUMMRY                  )
      !>                  CALL DAFUS  (  SUMMRY, ND, NI, DC, IC  )
      !>
      !>                  IA      =  IC( NI - 1 )
      !>                  FA      =  IC( NI     )
      !>                  LENGTH  =  FA  -  IA  +  1
      !>
      !>     3)  Make sure that a file handle designates an open DAF. Signal
      !>         an error if it does not.
      !>
      !>         Note that if a DAF is open at all, read access is allowed.
      !>
      !>                  CALL DAFSIH ( HANDLE, 'READ' )
      !>
      !>                  IF ( FAILED() ) THEN
      !>                     RETURN
      !>                  END IF
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
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 8.0.2, 25-NOV-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 8.0.1, 10-OCT-2012 (EDW)
      !>
      !>        Corrected ordering of header section.
      !>
      !>-    SPICELIB Version 8.0.0, 13-NOV-2001 (FST)
      !>
      !>        This routine was updated to utilize the new handle manager
      !>        software to manage binary file formats and consolidated
      !>        I/O code.
      !>
      !>-    SPICELIB Version 7.0.4, 08-OCT-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are WIN-NT
      !>
      !>-    SPICELIB Version 7.0.3, 16-SEP-1999 (NJB)
      !>
      !>        CSPICE environments were added. Some typos were corrected.
      !>
      !>-    SPICELIB Version 7.0.2, 28-JUL-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. New
      !>        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
      !>
      !>-    SPICELIB Version 7.0.1, 17-MAR-1999 (WLT)
      !>
      !>        The environment lines were expanded so that the supported
      !>        environments are now explicitly given. Previously,
      !>        environments such as SUN-SUNOS and SUN-SOLARIS were implied
      !>        by the environment label SUN.
      !>
      !>-    SPICELIB Version 1.2.1, 29-SEP-1993 (KRG)
      !>
      !>        Removed references to specific DAF file open routines in the
      !>        $Detailed_Input section of the header. This was done in order
      !>        to minimize documentation changes if these open routines ever
      !>        change.
      !>
      !>-    SPICELIB Version 1.2.0, 25-FEB-1993 (JML)
      !>
      !>        IOSTAT is now checked after the INQUIRE statement.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 03-SEP-1991 (NJB) (WLT)
      module subroutine dafsih ( ctx, handle, access )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! HANDLE to be validated.
      character(*), intent(in) :: access  !! String indicating access type.
      integer(ip) :: findex
      character(acclen) :: acc
      character(crlen) :: crec
      character(1) :: dafnam  ! PEPPER: the name is not used (ZZDDHNFO cuts it)
      character(fmtlen) :: format
      character(idlen) :: idword
      character(ifnlen) :: ifn
      character(typlen) :: ttype
      real(wp) :: drec   ( dprsiz )
      integer(ip) :: bward
      integer(ip) :: fnd
      integer(ip) :: fni
      integer(ip) :: free
      integer(ip) :: fnb
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: iarc
      integer(ip) :: ibff
      integer(ip) :: iamh
      integer(ip) :: iostat
      integer(ip) :: lun
      logical :: found
      associate ( ft => ctx%spice%dafah )
      if ( ctx%failed() ) return

!
!     Initialize the handle list, if necessary.
!
!     Get an upper case, left-justified copy of ACCESS.
!
      acc = adjustl(access)
      call ucase_inplace ( acc )

!
!     Make sure we recognize the access type specified by the caller.
!
      if (  ( acc /= 'READ' ) .and. ( acc /= 'WRITE' )  ) then

         call ctx%setmsg ( 'Unrecognized access type.  Type was #. ' )
         call ctx%errch  ( '#', access                               )
         call ctx%sigerr ( 'SPICE(INVALIDOPTION)'                    )
         return

      end if

!
!     Retrieve information about this HANDLE.
!
      call zzddhnfo ( ctx, handle, dafnam, iarc, ibff, iamh, found )

!
!     See whether the input handle is in our list at all.  It's
!     unlawful for the handle to be absent.  All open DAFs are
!     readable, so in the case that ACC is 'READ', we're done if
!     the DAF is open.
!
      if ( ( .not. found ) .or. ( ft%find ( handle ) == 0 ) ) then

         call ctx%setmsg ( 'There is no file open with handle = #' )
         call ctx%errint ( '#', handle                             )
         call ctx%sigerr ( 'SPICE(DAFNOSUCHHANDLE)'                )
         return

!
!     If the access type is 'WRITE', the DAF must be open for writing.
!     This is not the case if the value of IAMH returned from the handle
!     manager is not READ.
!
      else if ( ( acc == 'WRITE' ) .and. ( iamh == read ) ) then

         call ctx%setmsg ( 'DAF not open for write.  Handle = #, ' // &
                          'file = ''#'''                         )
         call ctx%errint ( '#', handle                               )
         call ctx%errch  ( '#', dafnam                               )
         call ctx%sigerr ( 'SPICE(DAFINVALIDACCESS)'                 )
         return

      end if

!
!     The DAF's handle is o.k.
!
      return
      end associate
      end subroutine dafsih

      !> Convert a record/word pair to its equivalent address within
      !> a DAF.
      !>
      !>## Detailed Input
      !>
      !>     RECNO,
      !>     WORDNO   are the record and word numbers of an arbitrary
      !>              location within a DAF.
      !>
      !>## Detailed Output
      !>
      !>     ADDR     is the corresponding address within the DAF.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If either RECNO or WORDNO is zero or negative, the error
      !>         SPICE(DAFNOSUCHADDR) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     To the user, the data in a DAF appear to be a contiguous
      !>     collection of double precision numbers, each of which has an
      !>     address. To the DAF software, however, the data appear to be
      !>     a collection of records, each containing 128 double precision
      !>     words. The routines DAFARW and DAFRWA translate between these
      !>     two representations.
      !>
      !>## Examples
      !>
      !>     Routines DAFRDA and DAFWDA illustrate the use of DAFARW and
      !>     DAFRWA.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved DAF
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafrwa ( ctx, recno, wordno, addr )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: recno  !! Record, word numbers of a location within DAF.
      integer(ip), intent(in) :: wordno  !! Record, word numbers of a location within DAF.
      integer(ip), intent(out) :: addr  !! Corresponding address.
      if ( ctx%failed() ) then
         return

      else if ( recno <= 0  .or.  wordno <= 0 ) then
         call ctx%setmsg ( 'No address for record #, word #.' )
         call ctx%errint ( '#', recno                         )
         call ctx%errint ( '#', wordno                        )
         call ctx%sigerr ( 'SPICE(DAFNOSUCHADDR)'             )
         return
      end if

!
!     If the record and word numbers are legal, the computation is
!     straightforward.
!
      addr = wordno + ( recno - 1 ) * 128

      return
      end subroutine dafrwa

      !> Convert an address within a DAF to its equivalent
      !> record/word representation.
      !>
      !>## Detailed Input
      !>
      !>     ADDR     is an arbitrary address within a DAF.
      !>
      !>## Detailed Output
      !>
      !>     RECNO,
      !>     WORDNO   are the corresponding record and word numbers
      !>              within the DAF.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If ADDR is zero or negative, the error SPICE(DAFNOSUCHADDR)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     To the user, the data in a DAF appear to be a contiguous
      !>     collection of double precision numbers, each of which has an
      !>     address. To the DAF software, however, the data appear to be
      !>     a collection of records, each containing 128 double precision
      !>     words. The routines DAFARW and DAFRWA translate between these
      !>     two representations.
      !>
      !>## Examples
      !>
      !>     Routines DAFRDA and DAFWDA illustrate the use of DAFARW and
      !>     DAFRWA.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved DAF
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafarw ( ctx, addr, recno, wordno )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: recno  !! Corresponding record, word numbers.
      integer(ip), intent(out) :: wordno  !! Corresponding record, word numbers.
      integer(ip), intent(in) :: addr  !! Address within DAF.
      if ( ctx%failed() ) then
         return

      else if ( addr <= 0 ) then
         call ctx%setmsg ( 'No record, word for address #.' )
         call ctx%errint ( '#', addr                        )
         call ctx%sigerr ( 'SPICE(DAFNOSUCHADDR)'           )
         return
      end if

!
!     If the address is legal, the computation is straightforward.
!
      recno  =        ( addr  - 1 ) / 128 + 1
      wordno = addr - ( recno - 1 ) * 128

      return
      end subroutine dafarw

      !> Pack (assemble) an array summary from its double precision and
      !> integer components.
      !>
      !>## Detailed Input
      !>
      !>     ND       is the number of double precision components in
      !>              the summary to be packed.
      !>
      !>     NI       is the number of integer components in the summary.
      !>
      !>     DC       are the double precision components of the summary.
      !>
      !>     IC       are the integer components of the summary.
      !>
      !>## Detailed Output
      !>
      !>     SUM      is an array summary containing the components in DC
      !>              and IC. This identifies the contents and location of
      !>              a single array within a DAF.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If ND is zero or negative, no DP components are stored.
      !>
      !>     2)  If NI is zero or negative, no integer components are stored.
      !>
      !>     3)  If the total size of the summary is greater than 125 double
      !>         precision words, some components may not be stored. See
      !>         $Particulars for details.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The components of array summaries are packed into double
      !>     precision arrays for reasons outlined in [1]. Two routines,
      !>     DAFPS (pack summary) and DAFUS (unpack summary) are provided
      !>     for packing and unpacking summaries.
      !>
      !>     The total size of the summary is
      !>
      !>             (NI - 1)
      !>        ND + -------- + 1
      !>                 2
      !>
      !>     double precision words (where ND, NI are nonnegative).
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Replace the body ID code 301 (Moon) with a test body ID,
      !>        e.g. -999, in every descriptor of an SPK file.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFPS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions.
      !>C
      !>      INTEGER               CARDI
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               LBCELL
      !>      PARAMETER           ( LBCELL = -5   )
      !>
      !>      INTEGER               DSCSIZ
      !>      PARAMETER           ( DSCSIZ = 5    )
      !>
      !>      INTEGER               FILSIZ
      !>      PARAMETER           ( FILSIZ = 256  )
      !>
      !>      INTEGER               MAXOBJ
      !>      PARAMETER           ( MAXOBJ = 1000 )
      !>
      !>      INTEGER               ND
      !>      PARAMETER           ( ND     = 2    )
      !>
      !>      INTEGER               NI
      !>      PARAMETER           ( NI     = 6    )
      !>
      !>      INTEGER               NEWCOD
      !>      PARAMETER           ( NEWCOD = -999 )
      !>
      !>      INTEGER               OLDCOD
      !>      PARAMETER           ( OLDCOD =  301 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(FILSIZ)    FNAME
      !>
      !>      DOUBLE PRECISION      DC     ( ND )
      !>      DOUBLE PRECISION      SUM    ( DSCSIZ )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               IC     ( NI )
      !>      INTEGER               IDS    ( LBCELL : MAXOBJ )
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Get the SPK file name.
      !>C
      !>      CALL PROMPT ( 'Enter name of the SPK file > ', FNAME )
      !>
      !>C
      !>C     Initialize the set IDS.
      !>C
      !>      CALL SSIZEI ( MAXOBJ, IDS )
      !>
      !>C
      !>C     Open for writing the SPK file.
      !>C
      !>      CALL DAFOPW ( FNAME, HANDLE )
      !>
      !>C
      !>C     Search the file in forward order.
      !>C
      !>      CALL DAFBFS ( HANDLE )
      !>      CALL DAFFNA ( FOUND )
      !>
      !>      DO WHILE ( FOUND )
      !>
      !>C
      !>C        Fetch and unpack the descriptor (aka summary)
      !>C        of the current segment.
      !>C
      !>         CALL DAFGS ( SUM )
      !>         CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>C
      !>C        Replace ID codes if necessary.
      !>C
      !>         IF ( IC(1) .EQ. OLDCOD ) THEN
      !>
      !>            IC(1) = NEWCOD
      !>
      !>         END IF
      !>
      !>         IF ( IC(2) .EQ. OLDCOD ) THEN
      !>
      !>            IC(2) = NEWCOD
      !>
      !>         END IF
      !>
      !>C
      !>C        Re-pack the descriptor; replace the descriptor
      !>C        in the file.
      !>C
      !>         CALL DAFPS ( ND, NI, DC, IC, SUM )
      !>         CALL DAFRS ( SUM )
      !>
      !>C
      !>C        Find the next segment.
      !>C
      !>         CALL DAFFNA ( FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the file.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>C
      !>C     Find the set of objects in the SPK file.
      !>C
      !>      CALL SPKOBJ ( FNAME, IDS )
      !>
      !>      WRITE(*,'(A)') 'Objects in the DAF file:'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,'(20I4)') ( IDS(I), I= 1, CARDI ( IDS ) )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, using the SPK file named de430.bsp, the output was:
      !>
      !>        Enter name of the SPK file > de430.bsp
      !>        Objects in the DAF file:
      !>
      !>        -999   1   2   3   4   5   6   7   8   9  10 199 299 399
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>-    SPICELIB Version 1.0.3, 10-OCT-2012 (EDW)
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>        Corrected ordering of header section.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafps ( ctx, nd, ni, dc, ic, sum )
      use pepper_kinds, only: ip, wp
      use pepper_packing, only: pack_integers
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nd  !! Number of double precision components.
      integer(ip), intent(in) :: ni  !! Number of integer components.
      real(wp), intent(in) :: dc       ( * )  !! Double precision components.
      integer(ip), intent(in) :: ic       ( * )  !! Integer components.
      real(wp), intent(out) :: sum      ( * )  !! Array summary.
      integer(ip) :: n
      integer(ip) :: m
      n = min ( 125, max ( 0, nd ) )

      sum(1:n) = dc(1:n)

!
!     PEPPER: the integers are packed two to a double precision word by
!     PACK_INTEGERS (SPICELIB: an EQUIVALENCE); the second half of the
!     last word is zero when NI is odd.
!
      m = min ( 250 - 2 * n, max ( 0, ni ) )

      call pack_integers ( ic(1:m), m, sum(n+1:n+(m+1)/2) )

      return
      end subroutine dafps

      !> Unpack an array summary into its double precision and integer
      !> components.
      !>
      !>## Detailed Input
      !>
      !>     SUM      is an array summary. This identifies the contents and
      !>              location of a single array within a DAF.
      !>
      !>     ND       is the number of double precision components in
      !>              the summary.
      !>
      !>     NI       is the number of integer components in the summary.
      !>
      !>## Detailed Output
      !>
      !>     DC       are the double precision components of the summary.
      !>
      !>     IC       are the integer components of the summary.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  If ND is zero or negative, no double precision components
      !>         are returned.
      !>
      !>     2)  If NI is zero or negative, no integer components are returned.
      !>
      !>     3)  If the total size of the summary is greater than 125 double
      !>         precision words, some components may not be returned.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The components of array summaries are packed into double
      !>     precision arrays for reasons outlined in [1]. Two routines,
      !>     DAFPS (pack summary) and DAFUS (unpack summary) are provided
      !>     for packing and unpacking summaries.
      !>
      !>     The total size of the summary is
      !>
      !>             (NI - 1)
      !>        ND + -------- + 1
      !>                 2
      !>
      !>     double precision words (where ND, NI are nonnegative).
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Use a simple routine to output the double precision and
      !>        integer values stored in an SPK's segments descriptors. This
      !>        function opens a DAF for read, performs a forwards search for
      !>        the DAF arrays, prints segments description for each array
      !>        found, then closes the DAF.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           de421.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFUS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Define the summary parameters appropriate
      !>C     for an SPK file.
      !>C
      !>      INTEGER               MAXSUM
      !>      PARAMETER           ( MAXSUM = 125 )
      !>
      !>      INTEGER               ND
      !>      PARAMETER           ( ND = 2       )
      !>
      !>      INTEGER               NI
      !>      PARAMETER           ( NI = 6       )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(32)        KERNEL
      !>
      !>      DOUBLE PRECISION      DC     ( ND     )
      !>      DOUBLE PRECISION      SUM    ( MAXSUM )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               IC     ( NI )
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Open a DAF for read. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      KERNEL = 'de421.bsp'
      !>      CALL DAFOPR ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Begin a forward search on the file.
      !>C
      !>      CALL DAFBFS ( HANDLE )
      !>
      !>C
      !>C     Search until a DAF array is found.
      !>C
      !>      CALL DAFFNA ( FOUND )
      !>
      !>C
      !>C     Loop while the search finds subsequent DAF arrays.
      !>C
      !>      DO WHILE ( FOUND )
      !>
      !>         CALL DAFGS ( SUM )
      !>         CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>         WRITE(*,*)                'Doubles:', DC(1:ND)
      !>         WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
      !>
      !>C
      !>C        Check for another segment.
      !>C
      !>         CALL DAFFNA ( FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        1        0        1        2      641   310404
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        2        0        1        2   310405   423048
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        3        0        1        2   423049   567372
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        4        0        1        2   567373   628976
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        5        0        1        2   628977   674740
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        6        0        1        2   674741   715224
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        7        0        1        2   715225   750428
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        8        0        1        2   750429   785632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        9        0        1        2   785633   820836
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:       10        0        1        2   820837   944040
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      301        3        1        2   944041  1521324
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      399        3        1        2  1521325  2098608
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      199        1        1        2  2098609  2098620
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      299        2        1        2  2098621  2098632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      499        4        1        2  2098633  2098644
      !>
      !>        Note, the final entries in the integer array contains the
      !>        segment start/end indexes. The output indicates the search
      !>        proceeded from the start of the file (low value index) towards
      !>        the end (high value index).
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
      !>-    SPICELIB Version 1.1.0, 04-AUG-2020 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard..
      !>        Added undeclared variables to code example.
      !>
      !>-    SPICELIB Version 1.0.3, 10-OCT-2012 (EDW)
      !>
      !>        Added a functional code example to the $Examples section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>        Corrected ordering of header section.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 04-AUG-2020 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      module subroutine dafus ( ctx, sum, nd, ni, dc, ic )
      use pepper_kinds, only: ip, wp
      use pepper_packing, only: unpack_integers
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nd  !! Number of double precision components.
      integer(ip), intent(in) :: ni  !! Number of integer components.
      real(wp), intent(out) :: dc       ( * )  !! Double precision components.
      integer(ip), intent(out) :: ic       ( * )  !! Integer components.
      real(wp), intent(in) :: sum      ( * )  !! Array summary.
      integer(ip) :: n
      integer(ip) :: m
!
!     Just undo whatever DAFPS did.
!
      n = min ( 125, max ( 0, nd ) )

      dc(1:n) = sum(1:n)

!
!     PEPPER: the integers are unpacked from the double precision words
!     by UNPACK_INTEGERS (SPICELIB: an EQUIVALENCE).
!
      m = min ( 250 - 2 * n, max ( 0, ni ) )

      call unpack_integers ( sum(n+1:n+(m+1)/2), m, ic(1:m) )

      return
      end subroutine dafus

      !> Read a portion of the contents of a double precision record in a
      !> DAF file.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a DAF.
      !>
      !>     RECNO    is the record number of a particular double precision
      !>              record within the DAF, whose contents are to be read.
      !>
      !>     BEGIN    is the first word in the specified record to be
      !>              returned.
      !>
      !>     END      is the final word in the specified record to be
      !>              returned.
      !>
      !>## Detailed Output
      !>
      !>     DATA     contains the specified portion (from BEGIN to END,
      !>              inclusive) of the specified record from the specified
      !>              file, specifically.
      !>
      !>     FOUND    is .TRUE. when the specified record is found, and is
      !>              .FALSE. otherwise.
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
      !>     DAFGDR checks the record buffer to see if the requested
      !>     record can be returned without actually reading it from
      !>     external storage. If not, it reads the record and stores
      !>     it in the buffer, typically removing another record from
      !>     the buffer as a result.
      !>
      !>     Once in the buffer, the specified portion of the record is
      !>     returned, using the following control loop.
      !>
      !>        J = 1
      !>        DO I = MAX( 1, BEGIN ), MIN( 128, END )
      !>           DATA( J ) = Buffered record ( I )
      !>           J = J + 1
      !>        END DO
      !>
      !>     Therefore bad values for BEGIN and END (BEGIN < 1, END < BEGIN,
      !>     etc.) are not signaled as errors, but result in the actions
      !>     implied by the above.
      !>
      !>## Examples
      !>
      !>     The following code fragment illustrates one way that DAFGDR
      !>     and DAFWDR can be used to update part of a double precision
      !>     record. If the record does not yet exist, we can assume that
      !>     it is filled with zeros.
      !>
      !>        CALL DAFGDR ( HANDLE, RECNO, 1, 128, DREC, FOUND )
      !>
      !>        IF ( .NOT. FOUND ) THEN
      !>           CALL MOVED ( 0.D0, 128, DREC )
      !>        END IF
      !>
      !>        DO I = FIRST, LAST
      !>           DREC(I) = NEW_VALUE(I)
      !>        END DO
      !>
      !>        CALL DAFWDR ( HANDLE, RECNO, DREC )
      !>
      !>     Note that since only entire records may be written using DAFWDR,
      !>     the entire record needs to be read also.
      !>
      !>## Restrictions
      !>
      !>     1)  Bad values for BEGIN and END ( BEGIN < 1, END > 128,
      !>         END < BEGIN ) are not signaled as errors. The effects of
      !>         such assignments on the returned data are defined by the
      !>         following control structure:
      !>
      !>            J = 1
      !>            DO I = MAX( 1, BEGIN ), MIN( 128, END )
      !>               DATA( J ) = Buffered record ( I )
      !>               J = J + 1
      !>            END DO
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     F.S. Turner        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 12-AUG-2021 (NJB) (JDR)
      !>
      !>        Bug fix: now NREAD is not incremented once it reaches
      !>        INTMAX(), if it does.
      !>
      !>        Re-ordered header sections. Corrected typos in comments.
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Removed DAF required reading from $Literature_References.
      !>
      !>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
      module subroutine dafgdr ( ctx, handle, recno, begin, end, data, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF.
      integer(ip), intent(in) :: recno  !! Record number.
      integer(ip), intent(in) :: begin  !! First word to read from record.
      integer(ip), intent(in) :: end  !! Last word to read from record.
      real(wp), intent(out) :: data     (  *  )  !! Contents of record.
      logical, intent(out) :: found  !! .TRUE. if record is found.
      integer(ip) :: unit
      integer(ip) :: iostat
      integer(ip) :: bufloc
      logical :: done
      logical :: stored
      integer(ip) :: b
      integer(ip) :: count
      integer(ip) :: e
      integer(ip) :: minval
      integer(ip) :: nd
      integer(ip) :: ni
      logical :: locfnd
      logical :: native
      call files_check ( ctx )
      if ( .not. allocated ( ctx%dafrwd ) ) then
         allocate ( ctx%dafrwd )
         call init_dafrwd_state ( ctx%dafrwd )
      end if
      associate ( rbhan => ctx%dafrwd%rbhan, &
                  rbrec => ctx%dafrwd%rbrec, &
                  rbreq => ctx%dafrwd%rbreq, &
                  rbdat => ctx%dafrwd%rbdat, &
                  rbnbr => ctx%dafrwd%rbnbr, &
                  nread => ctx%dafrwd%nread, &
                  nreq => ctx%dafrwd%nreq )
      if ( ctx%failed() ) return

!
!     Assume that the record will be found until proven otherwise.
!
      found  = .true.

!
!     First, find the record.
!
!     If the specified handle and record number match those of
!     a buffered record, determine the location of that record
!     within the buffer.
!
      bufloc = 0
      done   = .false.
      stored = .false.

      do while ( .not. done )
         bufloc = bufloc + 1

         stored = (       handle == rbhan(bufloc) &
                    .and. recno  == rbrec(bufloc) )

         done   = (       stored &
                    .or.  bufloc == rbnbr )
      end do

!
!     If not, determine the location of the least recently requested
!     record (the one with the smallest request number). Get the unit
!     number for the file, and read the record into this location.
!
!     If an error occurs while reading the record, clear the entire
!     buffer entry in case the entry was corrupted by a partial read.
!     Otherwise, increment the number of reads performed so far.
!
      if ( .not. stored ) then

         if ( rbnbr > 0 ) then
            bufloc = minloc(rbreq(1:rbnbr), dim=1)
            minval = rbreq(bufloc)
         else
            bufloc = 0
         end if

         call zzdafgdr ( ctx, handle, recno, rbdat(1,bufloc), locfnd )

!
!        If the call to ZZDAFGDR failed, or the record was not found,
!        then clean up.
!
         if ( ( ctx%failed() ) .or. ( .not. locfnd ) ) then
            found         = .false.
            rbhan(bufloc) = 0
            rbrec(bufloc) = 0
            rbreq(bufloc) = 0
         else

            if ( nread < intmax ) then
               nread = nread + 1
            end if

            rbhan(bufloc) = handle
            rbrec(bufloc) = recno

            if ( rbnbr < rbsize ) then
               rbnbr = rbnbr + 1
            end if

         end if
      end if

!
!     Whether previously stored or just read, the record is now in
!     the buffer. Return the specified portion directly, and increment
!     the corresponding request number.
!
      if ( found ) then

         b     = max(1,begin)
         e     = min(128,end)
         count = e-b+1

         data(1:count) = rbdat(b:b+count-1,bufloc)

!
!        Increment the request counter in such a way that integer
!        overflow will not occur.  This private module from the
!        handle manager halves RBREQ if adding 1 to NREQ would
!        cause its value to exceed INTMAX.
!
         call zzddhrcm ( ctx, rbnbr, rbreq, nreq )
         rbreq(bufloc) = nreq

      end if

      return
      end associate
      end subroutine dafgdr

      !> Read a portion of the contents of a summary record in a DAF file.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a DAF.
      !>
      !>     RECNO    is the record number of a particular double precision
      !>              record within the DAF, whose contents are to be read. DAF
      !>              record numbers start at 1.
      !>
      !>     BEGIN    is the first word in the specified record to be returned.
      !>              Word numbers range from 1 to 128.
      !>
      !>     END      is the final word in the specified record to be returned.
      !>              Word numbers range from 1 to 128.
      !>
      !>## Detailed Output
      !>
      !>     DATA     contains the specified portion (from BEGIN to END,
      !>              inclusive) of the specified record from the specified
      !>              file.
      !>
      !>     FOUND    is .TRUE. when the specified record is found, and is
      !>              .FALSE. otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE does not belong to any file that is currently open,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If an error occurs while reading the record, the error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     3)  Bad values for BEGIN and END ( BEGIN < 1, END > 128,
      !>         END < BEGIN ) are not diagnosed. See $Particulars and
      !>         $Restrictions for the effect of this routine in this case.
      !>
      !>## Files
      !>
      !>     The input HANDLE must refer to a DAF file that is open for read
      !>     or write access.
      !>
      !>## Particulars
      !>
      !>     DAFGSR checks the DAF record buffer to see if the requested
      !>     record can be returned without actually reading it from
      !>     external storage. If not, it reads the record and stores
      !>     it in the buffer, typically removing another record from
      !>     the buffer as a result.
      !>
      !>     Once in the buffer, the specified portion of the record is
      !>     returned, using the following control loop.
      !>
      !>        J = 1
      !>        DO I = MAX( 1, BEGIN ), MIN( 128, END )
      !>           DATA( J ) = Buffered record ( I )
      !>           J = J + 1
      !>        END DO
      !>
      !>     Therefore bad values for BEGIN and END (BEGIN < 1, END < BEGIN,
      !>     etc.) are not signaled as errors, but result in the actions
      !>     implied by the above.
      !>
      !>## Examples
      !>
      !>     The following code fragment illustrates one way that DAFGSR
      !>     and DAFWDR can be used to update part of a summary record.
      !>     If the record does not yet exist, we can assume that it is
      !>     filled with zeros.
      !>
      !>        CALL DAFGSR ( HANDLE, RECNO, 1, 128, DREC, FOUND )
      !>
      !>        IF ( .NOT. FOUND ) THEN
      !>           CALL MOVED ( 0.D0, 128, DREC )
      !>        END IF
      !>
      !>        DO I = FIRST, LAST
      !>           DREC(I) = NEW_VALUE(I)
      !>        END DO
      !>
      !>        CALL DAFWDR ( HANDLE, RECNO, DREC )
      !>
      !>     Note that since only entire records may be written using DAFWDR,
      !>     the entire record needs to be read also.
      !>
      !>## Restrictions
      !>
      !>     1)  Bad values for BEGIN and END ( BEGIN < 1, END > 128,
      !>         END < BEGIN ) are not signaled as errors. The effects of
      !>         such assignments on the returned data are defined by the
      !>         following control structure:
      !>
      !>            J = 1
      !>            DO I = MAX( 1, BEGIN ), MIN( 128, END )
      !>               DATA( J ) = Buffered record ( I )
      !>               J = J + 1
      !>            END DO
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     F.S. Turner        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 09-AUG-2021 (NJB) (JDR)
      !>
      !>        Bug fix: now NREAD is not incremented once it reaches
      !>        INTMAX(), if it does.
      !>
      !>        Corrected typos in comments. Edited the header to comply with
      !>        NAIF standard. Added $Exceptions and $Files sections and
      !>        extended $Detailed_Input.
      !>
      !>        Removed DAF required reading from $Literature_References.
      !>
      !>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
      module subroutine dafgsr ( ctx, handle, recno, begin, end, data, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF.
      integer(ip), intent(in) :: recno  !! Record number.
      integer(ip), intent(in) :: begin  !! First word to read from record.
      integer(ip), intent(in) :: end  !! Last word to read from record.
      real(wp), intent(out) :: data     (  *  )  !! Contents of record.
      logical, intent(out) :: found  !! .TRUE. if record is found.
      integer(ip) :: unit
      integer(ip) :: iostat
      integer(ip) :: bufloc
      logical :: done
      logical :: stored
      integer(ip) :: b
      integer(ip) :: count
      integer(ip) :: e
      integer(ip) :: minval
      integer(ip) :: nd
      integer(ip) :: ni
      logical :: locfnd
      logical :: native
      call files_check ( ctx )
      if ( .not. allocated ( ctx%dafrwd ) ) then
         allocate ( ctx%dafrwd )
         call init_dafrwd_state ( ctx%dafrwd )
      end if
      associate ( rbhan => ctx%dafrwd%rbhan, &
                  rbrec => ctx%dafrwd%rbrec, &
                  rbreq => ctx%dafrwd%rbreq, &
                  rbdat => ctx%dafrwd%rbdat, &
                  rbnbr => ctx%dafrwd%rbnbr, &
                  nread => ctx%dafrwd%nread, &
                  nreq => ctx%dafrwd%nreq )
      if ( ctx%failed() ) return

!
!     Assume that the record will be found until proven otherwise.
!
      found  = .true.

!
!     First, find the record.
!
!     If the specified handle and record number match those of
!     a buffered record, determine the location of that record
!     within the buffer.
!
      bufloc = 0
      done   = .false.
      stored = .false.

      do while ( .not. done )
         bufloc = bufloc + 1

         stored = (       handle == rbhan(bufloc) &
                    .and. recno  == rbrec(bufloc) )

         done   = (       stored &
                    .or.  bufloc == rbnbr )
      end do

!
!     If not, determine the location of the least recently requested
!     record (the one with the smallest request number). Get the unit
!     number for the file, and read the record into this location.
!
!     If an error occurs while reading the record, clear the entire
!     buffer entry in case the entry was corrupted by a partial read.
!     Otherwise, increment the number of reads performed so far.
!
      if ( .not. stored ) then

         if ( rbnbr > 0 ) then
            bufloc = minloc(rbreq(1:rbnbr), dim=1)
            minval = rbreq(bufloc)
         else
            bufloc = 0
         end if

         call dafhsf ( ctx, handle, nd, ni )

         call zzdafgsr ( ctx, handle,          recno, nd, ni, &
                         rbdat(1,bufloc), locfnd         )

!
!        If the call to ZZDAFGSR or DAFHSF failed, or the record
!        was not found, then clean up.
!
         if ( ( ctx%failed() ) .or. ( .not. locfnd ) ) then
            found         = .false.
            rbhan(bufloc) = 0
            rbrec(bufloc) = 0
            rbreq(bufloc) = 0
         else

            if ( nread < intmax ) then
               nread = nread + 1
            end if

            rbhan(bufloc) = handle
            rbrec(bufloc) = recno

            if ( rbnbr < rbsize ) then
               rbnbr = rbnbr + 1
            end if

         end if
      end if

!
!     Whether previously stored or just read, the record is now in
!     the buffer. Return the specified portion directly, and increment
!     the corresponding request number.
!
      if ( found ) then

         b     = max(1,begin)
         e     = min(128,end)
         count = e-b+1

         data(1:count) = rbdat(b:b+count-1,bufloc)

!
!        Increment the request counter in such a way that integer
!        overflow will not occur.  This private module from the
!        handle manager halves RBREQ if adding 1 to NREQ would
!        cause its value to exceed INTMAX.
!
         call zzddhrcm ( ctx, rbnbr, rbreq, nreq )
         rbreq(bufloc) = nreq

      end if

      return
      end associate
      end subroutine dafgsr

      !> Read a portion of the contents of a double precision record in a
      !> DAF file.
      !> **Obsolete:** This routine has been superseded by [[dafgdr]], and it is
      !> supported for purposes of backwards compatibility only.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a DAF.
      !>
      !>     RECNO    is the record number of a particular double precision
      !>              record within the DAF, whose contents are to be read.
      !>
      !>     BEGIN    is the first word in the specified record to be
      !>              returned.
      !>
      !>     END      is the final word in the specified record to be
      !>              returned.
      !>
      !>## Detailed Output
      !>
      !>     DATA     contains the specified portion (from BEGIN to END,
      !>              inclusive) of the specified record from the specified
      !>              file, specifically.
      !>
      !>     FOUND    is .TRUE. when the specified record is found, and is
      !>              .FALSE. otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the file associated with HANDLE is not of the native
      !>         binary file format, the error SPICE(UNSUPPORTEDBFF) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     DAFRDR checks the record buffer to see if the requested
      !>     record can be returned without actually reading it from
      !>     external storage. If not, it reads the record and stores
      !>     it in the buffer, typically removing another record from
      !>     the buffer as a result.
      !>
      !>     Once in the buffer, the specified portion of the record is
      !>     returned, using the following control loop.
      !>
      !>        J = 1
      !>        DO I = MAX( 1, BEGIN ), MIN( 128, END )
      !>           DATA( J ) = Buffered record ( I )
      !>           J = J + 1
      !>        END DO
      !>
      !>     Therefore bad values for BEGIN and END (BEGIN < 1, END < BEGIN,
      !>     etc.) are not signaled as errors, but result in the actions
      !>     implied by the above.
      !>
      !>     This routine has been made obsolete by the routine DAFGDR,
      !>     and it is supported for reasons of backwards compatibility
      !>     only. New software development should utilize DAFGDA.
      !>
      !>## Examples
      !>
      !>     The following code fragment illustrates one way that DAFRDR
      !>     and DAFWDR can be used to update part of a double precision
      !>     record. If the record does not yet exist, we can assume that
      !>     it is filled with zeros.
      !>
      !>        CALL DAFRDR ( HANDLE, RECNO, 1, 128, DREC, FOUND )
      !>
      !>        IF ( .NOT. FOUND ) THEN
      !>           CALL MOVED ( 0.D0, 128, DREC )
      !>        END IF
      !>
      !>        DO I = FIRST, LAST
      !>           DREC(I) = NEW_VALUE(I)
      !>        END DO
      !>
      !>        CALL DAFWDR ( HANDLE, RECNO, DREC )
      !>
      !>     Note that since only entire records may be written using DAFWDR,
      !>     the entire record needs to be read also.
      !>
      !>## Restrictions
      !>
      !>     1)  An integer overflow may occur if the number of requests
      !>         by a single program exceeds the maximum number that can
      !>         be stored in an integer variable.
      !>
      !>     2)  Bad values for BEGIN and END ( BEGIN < 1, END > 128,
      !>         END < BEGIN ) are not signaled as errors. The effects of
      !>         such assignments on the returned data are defined by the
      !>         following control structure:
      !>
      !>            J = 1
      !>            DO I = MAX( 1, BEGIN ), MIN( 128, END )
      !>               DATA( J ) = Buffered record ( I )
      !>               J = J + 1
      !>            END DO
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 12-AUG-2021 (NJB) (JDR)
      !>
      !>        Bug fix: now NREAD is not incremented once it reaches
      !>        INTMAX(), if it does.
      !>
      !>        Tweaked one line of code to suppress ftnchek warning.
      !>
      !>        Re-ordered header sections. Corrected typos in comments.
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Removed DAF required reading from $Literature_References.
      !>
      !>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
      !>
      !>        Added SPICE(UNSUPPORTEDBFF) exception to the routine.
      !>
      !>-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
      !>
      !>        The loop in DAFRDR that moved buffered d.p.s into the output
      !>        array DATA was modified to use the routine MOVED.
      !>
      !>-    SPICELIB Version 1.2.0, 01-AUG-1997 (NJB)
      !>
      !>        Unnecessary CHKIN and CHKOUT calls were removed from entry
      !>        point DAFRDR.
      !>
      !>-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
      !>
      !>        1) In DAFRDR, the found flag is now set to false if the
      !>           call to DAFHLU fails.
      !>
      !>        2) In the example code fragment in DAFRDR and DAFWDR, the
      !>           calling sequence to MOVED was corrected.
      !>
      !>        3) In the call to MINAI the argument for the minimum value
      !>           was changed from I to MINVAL.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
      !>
      !>        The exception SPICE(UNSUPPORTEDBFF) was added to guarantee
      !>        this routine's functionality remains unchanged as a result
      !>        of the updates to the underlying DAF software's utilization of
      !>        the handle manager. In versions of the toolkit prior to this,
      !>        all DAFs loaded were of the native binary file format.
      !>        Previously, this routine was used to read the contents of
      !>        summary records in addition to the usual data records.
      !>        The non-native to native translation process for these two
      !>        different types of records in general are not the same.
      !>        Rather than attempt to interpret the caller's intent, this
      !>        routine is obsolete and restricted to functioning only on
      !>        DAFs of the native binary file format.
      !>
      !>-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
      !>
      !>        The loop in DAFRDR that moved buffered d.p.s into the output
      !>        array DATA was modified to use the routine MOVED.
      !>
      !>-    SPICELIB Version 1.2.0, 01-AUG-1997 (NJB)
      !>
      !>        Unnecessary CHKIN and CHKOUT calls were removed from entry
      !>        point DAFRDR. These calls were placed together prior to
      !>        a RETURN statement. It's unclear why they were there in the
      !>        first place.
      !>
      !>-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
      !>
      !>        1) In DAFRDR, the found flag is now set to false if the
      !>           call to DAFHLU fails.
      !>
      !>        2) In the example code fragment in DAFRDR and DAFWDR, the
      !>           calling sequence to MOVED was corrected.
      !>
      !>        3) In the call to MINAI the argument for the minimum value
      !>           was changed from I to MINVAL.
      !>
      !>-     Beta Version 2.0.0, 1-NOV-1989 (RET)
      !>
      !>        The function of DAFRDR was changed so that it returns only
      !>        a specified portion of the record. The calling sequence there-
      !>        fore changed from
      !>
      !>           DAFRDR ( HANDLE, RECNO, DREC, FOUND ) to
      !>           DAFRDR ( HANDLE, RECNO, BEGIN, END, DATA, FOUND )
      !>
      !>        The change was made to cut down on the shuffling of unneeded
      !>        data.
      !>
      !>        Also, DAFRDR now only checks in and checks out if DAFHLU has
      !>        failed (the only routine called by DAFRDR that could possibly
      !>        signal an error). The purpose of this change was to help
      !>        speed up a routine that gets called constantly by higher level
      !>        DAF routines.
      module subroutine dafrdr ( ctx, handle, recno, begin, end, data, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF.
      integer(ip), intent(in) :: recno  !! Record number.
      integer(ip), intent(in) :: begin  !! First word to read from record.
      integer(ip), intent(in) :: end  !! Last word to read from record.
      real(wp), intent(out) :: data     (  *  )  !! Contents of record.
      logical, intent(out) :: found  !! .TRUE. if record is found.
      integer(ip) :: unit
      integer(ip) :: iostat
      integer(ip) :: bufloc
      logical :: done
      logical :: stored
      integer(ip) :: b
      integer(ip) :: count
      integer(ip) :: e
      integer(ip) :: minval
      integer(ip) :: nd
      integer(ip) :: ni
      logical :: locfnd
      logical :: native
      call files_check ( ctx )
      if ( .not. allocated ( ctx%dafrwd ) ) then
         allocate ( ctx%dafrwd )
         call init_dafrwd_state ( ctx%dafrwd )
      end if
      associate ( rbhan => ctx%dafrwd%rbhan, &
                  rbrec => ctx%dafrwd%rbrec, &
                  rbreq => ctx%dafrwd%rbreq, &
                  rbdat => ctx%dafrwd%rbdat, &
                  rbnbr => ctx%dafrwd%rbnbr, &
                  nread => ctx%dafrwd%nread, &
                  nreq => ctx%dafrwd%nreq )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Assume that the record will be found until proven otherwise.
!
      found  = .true.

!
!     First check to see if HANDLE is associated with a DAF of the
!     native binary file format.
!
      call zzddhisn ( ctx, handle, native, locfnd )

      if ( locfnd .and. ( .not. native ) ) then

         found = .false.

         call ctx%setmsg ( 'The binary file format for file ''#'' ' &
         //            'is not native. This routine operates ' &
         //            'only on files of the native format.'    )
         call ctx%errhan ( '#', handle                              )
         call ctx%sigerr ( 'SPICE(UNSUPPORTEDBFF)'                  )
         return
      end if

!
!     Now, find the record.
!
!     If the specified handle and record number match those of
!     a buffered record, determine the location of that record
!     within the buffer.
!
      bufloc = 0
      done   = .false.
      stored = .false.

      do while ( .not. done )
         bufloc = bufloc + 1

         stored = (       handle == rbhan(bufloc) &
                    .and. recno  == rbrec(bufloc) )

         done   = (       stored &
                    .or.  bufloc == rbnbr )
      end do

!
!     If not, determine the location of the least recently requested
!     record (the one with the smallest request number). Get the unit
!     number for the file, and read the record into this location.
!
!     If an error occurs while reading the record, clear the entire
!     buffer entry in case the entry was corrupted by a partial read.
!     Otherwise, increment the number of reads performed so far.
!
      if ( .not. stored ) then

         if ( rbnbr > 0 ) then
            bufloc = minloc(rbreq(1:rbnbr), dim=1)
            minval = rbreq(bufloc)
         else
            bufloc = 0
         end if

         call zzdafgdr ( ctx, handle, recno, rbdat(1, bufloc), locfnd )

!
!        If the call to ZZDAFGDR failed, or the record was not found,
!        then clean up.
!
         if ( ( ctx%failed() ) .or. ( .not. locfnd ) ) then
            found         = .false.
            rbhan(bufloc) = 0
            rbrec(bufloc) = 0
            rbreq(bufloc) = 0
         else

            if ( nread < intmax ) then
               nread = nread + 1
            end if

            rbhan(bufloc) = handle
            rbrec(bufloc) = recno

            if ( rbnbr < rbsize ) then
               rbnbr = rbnbr + 1
            end if

         end if
      end if

!
!     Whether previously stored or just read, the record is now in
!     the buffer. Return the specified portion directly, and increment
!     the corresponding request number.
!
      if ( found ) then

         b     = max(1,begin)
         e     = min(128,end)
         count = e-b+1

         data(1:count) = rbdat(b:b+count-1,bufloc)

!
!        Increment the request counter in such a way that integer
!        overflow will not occur.  This private module from the
!        handle manager halves RBREQ if adding 1 to NREQ would
!        cause its value to exceed INTMAX.
!
         call zzddhrcm ( ctx, rbnbr, rbreq, nreq )
         rbreq(bufloc) = nreq

      end if

      return
      end associate
      end subroutine dafrdr

      !> Write or rewrite the contents of a double precision record in
      !> a DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a DAF.
      !>
      !>     RECNO    is the record number of a particular double
      !>              precision record within the file, whose
      !>              contents are to be written (if the record does
      !>              not yet exist) or overwritten (if it does).
      !>
      !>     DREC     contains the new contents of the record.
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
      !>     1)  If the file is not open for write access, the error
      !>         SPICE(DAFILLEGWRITE) is signaled.
      !>
      !>     2)  If (for some reason) the record cannot be written, the
      !>         error SPICE(DAFDPWRITEFAIL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Like DAFRDR, DAFWDR checks the record buffer to see if the
      !>     requested record is in the buffer. If so, the buffer is
      !>     updated along with the file. This prevents the buffer from
      !>     becoming outdated.
      !>
      !>## Examples
      !>
      !>     The following code fragment illustrates one way that DAFRDR
      !>     and DAFWDR can be used to update part of a double precision
      !>     record. If the record does not yet exist, we can assume that
      !>     it is filled with zeros.
      !>
      !>        CALL DAFRDR ( HANDLE, RECNO, DREC, FOUND )
      !>
      !>        IF ( .NOT. FOUND ) THEN
      !>           CALL MOVED ( 0.D0, 128, DREC )
      !>        END IF
      !>
      !>        DO I = FIRST, LAST
      !>           DREC(I) = NEW_VALUE(I)
      !>        END DO
      !>
      !>        CALL DAFWDR ( HANDLE, RECNO, DREC )
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.1, 09-AUG-2021 (NJB) (JDR)
      !>
      !>        Re-ordered header sections. Corrected typos in comments.
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Removed DAF required reading from $Literature_References.
      !>
      !>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
      !>
      !>        Replaced the call to DAFHLU to ZZDDHHLU. This prevents
      !>        DAFWDR from tying up resources in the handle manager.
      !>
      !>-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
      !>
      !>        The loop in DAFRDR that moved buffered d.p.s into the output
      !>        array DATA was modified to use the routine MOVED.
      !>
      !>-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
      !>
      !>        In the example code fragment in DAFRDR and DAFWDR, the
      !>        calling sequence to MOVED was corrected.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafwdr ( ctx, handle, recno, drec )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF.
      integer(ip), intent(in) :: recno  !! Record number.
      real(wp), intent(in) :: drec     ( 128 )  !! Contents of record.
      integer(ip) :: unit
      integer(ip) :: iostat
      integer(ip) :: bufloc
      logical :: done
      logical :: stored
      integer(ip) :: b
      integer(ip) :: count
      integer(ip) :: e
      integer(ip) :: minval
      integer(ip) :: nd
      integer(ip) :: ni
      logical :: locfnd
      logical :: native
      call files_check ( ctx )
      if ( .not. allocated ( ctx%dafrwd ) ) then
         allocate ( ctx%dafrwd )
         call init_dafrwd_state ( ctx%dafrwd )
      end if
      associate ( rbhan => ctx%dafrwd%rbhan, &
                  rbrec => ctx%dafrwd%rbrec, &
                  rbreq => ctx%dafrwd%rbreq, &
                  rbdat => ctx%dafrwd%rbdat )
      if ( ctx%failed() ) return

!
!     No fair writing to a read-only file!
!
      if ( handle >= 0 ) then
         call ctx%setmsg ( 'Attempt was made to write to a read-only file.')
         call ctx%sigerr ( 'SPICE(DAFILLEGWRITE)' )

         return
      end if

!
!     If the specified handle and record number match those of
!     a buffered record, determine the location of that record
!     within the buffer.
!
      bufloc = 0
      done   = .false.
      stored = .false.

      do while ( .not. done )
         bufloc = bufloc + 1

         stored = (       handle == rbhan(bufloc) &
                    .and. recno  == rbrec(bufloc) )

         done   = (       stored &
                    .or.  bufloc == rbsize )
      end do

!
!     Get the unit number for the file, and write the record.
!
      call zzddhhlu ( ctx, handle, 'DAF', .false., unit )

      write (unit, &
             rec=recno, &
             iostat=iostat) drec
      flush ( unit )
      call files_written ( ctx )

!
!     If the record was buffered, replace it---with the input
!     record if the write was successful, or with zeros if it
!     was not.
!
      if ( stored ) then
         if ( iostat == 0 ) then
            rbdat(:,bufloc) = drec
         else
            rbhan(bufloc) = 0
            rbrec(bufloc) = 0
            rbreq(bufloc) = 0
         end if
      end if

!
!     Declare an error if the write failed.
!
      if ( iostat /= 0 ) then
         call ctx%setmsg ( 'Double precision write failed. Value of '     // &
                       'IOSTAT was #' )
         call ctx%errint ( '#', iostat )
         call ctx%sigerr ( 'SPICE(DAFDPWRITEFAIL)' )
      end if

      return
      end associate
      end subroutine dafwdr

      !> Return the number of reads and requests fielded by [[dafrdr]].
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     READS,
      !>     REQS     are the number of physical reads and the number
      !>              of requests processed by DAFRDR during the current
      !>              execution of the calling program.
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
      !>     The ratio of reads to requests tells you something about
      !>     the effectiveness with which the record buffer is preventing
      !>     unwanted disk access. In the ideal case, most of the records
      !>     needed by the calling program can be returned directly from
      !>     the buffer, and the ratio of reads to requests approaches zero.
      !>     More realistically, it should be be somewhere between 1/10
      !>     and 1/2.
      !>
      !>     If the ratio is greater than 1/2, you should consider increasing
      !>     the size of the record buffer (which is controlled by parameter
      !>     RBSIZE) in order to improve the performance of the DAF package,
      !>     unless your application is strapped for space.
      !>
      !>## Examples
      !>
      !>     In the following code fragment, the ratio of reads to requests
      !>     is determined following a series of calls to the reader DAFEZ.
      !>
      !>        DO I = 1, N
      !>           CALL DAFEZ ( ..., STATES(1,I), ... )
      !>        END DO
      !>
      !>        CALL DAFNRR ( READS, REQS )
      !>
      !>        WRITE (*,*) 'Reads/requests = ', FLOAT( READS ) / FLOAT( REQS )
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
      !>     J.M. Lynch         (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.1, 09-AUG-2021 (NJB)
      !>
      !>        Re-ordered header sections. Corrected typos in comments.
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Removed DAF required reading from $Literature_References.
      !>
      !>-    SPICELIB Version 1.3.0, 24-MAR-2000 (WLT)
      !>
      !>        The loop in DAFRDR that moved buffered d.p.s into the output
      !>        array DATA was modified to use the routine MOVED.
      !>
      !>-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
      !>
      !>        A cut and paste error in the literature references
      !>        section of the header was fixed.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafnrr ( ctx, reads, reqs )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: reads  !! Reads, requests in this execution.
      integer(ip), intent(out) :: reqs  !! Reads, requests in this execution.
      logical :: return
      integer(ip) :: unit
      integer(ip) :: iostat
      integer(ip) :: bufloc
      logical :: done
      logical :: stored
      integer(ip) :: b
      integer(ip) :: count
      integer(ip) :: e
      integer(ip) :: minval
      integer(ip) :: nd
      integer(ip) :: ni
      logical :: locfnd
      logical :: native
      call files_check ( ctx )
      if ( .not. allocated ( ctx%dafrwd ) ) then
         allocate ( ctx%dafrwd )
         call init_dafrwd_state ( ctx%dafrwd )
      end if
      associate ( nread => ctx%dafrwd%nread, &
                  nreq => ctx%dafrwd%nreq )
      reads = nread
      reqs  = nreq

      return
      end associate
      end subroutine dafnrr

      !> Read the double precision data bounded by two addresses within
      !> a DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF.
      !>
      !>     BADDR,
      !>     EADDR    are the initial and final addresses of a contiguous
      !>              set of double precision numbers within a DAF.
      !>              Presumably, these make up all or part of a particular
      !>              array.
      !>
      !>## Detailed Output
      !>
      !>     DATA     are the double precision data contained between
      !>              the specified addresses within the specified file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If BADDR is zero or negative, the error SPICE(DAFNEGADDR)
      !>         is signaled.
      !>
      !>     2)  If BADDR > EADDR, the error SPICE(DAFBEGGTEND) is signaled.
      !>
      !>     3)  If HANDLE is invalid, an error is signaled by a routine in the
      !>         call tree of this routine.
      !>
      !>     4)  If the range of addresses covered between BADDR and EADDR
      !>         includes records that do not contain strictly double
      !>         precision data, then the values returned in DATA are
      !>         undefined. See the $Restrictions section below for details.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The principal reason that DAFs are so easy to use is that
      !>     the data in each DAF are considered to be one long contiguous
      !>     set of double precision numbers. You can grab data from anywhere
      !>     within a DAF without knowing (or caring) about the physical
      !>     records in which they are stored.
      !>
      !>     This routine replaces DAFRDA as the principle mechanism for
      !>     reading the contents of DAF arrays.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Open a type 8 SPK for read access, retrieve the data for
      !>        the first segment and identify the beginning and end addresses,
      !>        the number of data elements within, the size of the data
      !>        array, and print the first two records.
      !>
      !>        Use the SPK kernel below as input type 8 SPK file for the
      !>        example.
      !>
      !>           mer1_ls_040128_iau2000_v1.bsp
      !>
      !>        Each segment contains only two records which provide the start
      !>        and end position for the MER-1 rover landing site in the
      !>        IAU_MARS frame. Since the landing site does not change over
      !>        time, it is expected that both records are equal.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFGDA_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      CHARACTER*(*)         FMT
      !>      PARAMETER           ( FMT = '(6F10.3)' )
      !>
      !>      INTEGER               MAXDAT
      !>      PARAMETER           ( MAXDAT = 1000 )
      !>
      !>      INTEGER               MAXSUM
      !>      PARAMETER           ( MAXSUM = 125  )
      !>
      !>      INTEGER               ND
      !>      PARAMETER           ( ND     = 2    )
      !>
      !>      INTEGER               NI
      !>      PARAMETER           ( NI     = 6    )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      DAFSUM ( MAXSUM )
      !>      DOUBLE PRECISION      DATA   ( MAXDAT )
      !>      DOUBLE PRECISION      DC     ( ND )
      !>
      !>      INTEGER               BADDR
      !>      INTEGER               EADDR
      !>      INTEGER               HANDLE
      !>      INTEGER               IC     ( NI )
      !>      INTEGER               SIZE
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Open the type 8 SPK for read access, then read the
      !>C     data from the first segment.
      !>C
      !>      CALL DAFOPR ( 'mer1_ls_040128_iau2000_v1.bsp', HANDLE )
      !>
      !>C
      !>C     Begin a forward search; find the first segment; read the
      !>C     segment summary.
      !>C
      !>      CALL DAFBFS ( HANDLE )
      !>      CALL DAFFNA ( FOUND  )
      !>      CALL DAFGS  ( DAFSUM )
      !>      CALL DAFUS  ( DAFSUM, ND, NI, DC, IC )
      !>
      !>C
      !>C     Retrieve the data begin and end addresses.
      !>C
      !>      BADDR = IC(5)
      !>      EADDR = IC(6)
      !>
      !>      WRITE(*,'(A,I4)') 'Beginning address       : ', BADDR
      !>      WRITE(*,'(A,I4)') 'Ending address          : ', EADDR
      !>      WRITE(*,'(A,I4)') 'Number of data elements : ',
      !>     .                                    EADDR - BADDR + 1
      !>
      !>C
      !>C     Extract all data bounded by the begin and end addresses.
      !>C
      !>      CALL DAFGDA ( HANDLE, BADDR, EADDR, DATA )
      !>
      !>C
      !>C     Check the data.
      !>C
      !>      WRITE(*,'(A)') 'The first and second states '
      !>     .            // 'stored in the segment:'
      !>      WRITE(*,FMT) DATA(1),  DATA(2),  DATA(3),
      !>     .             DATA(4),  DATA(5),  DATA(6)
      !>      WRITE(*,FMT) DATA(7),  DATA(8),  DATA(9),
      !>     .             DATA(10), DATA(11), DATA(12)
      !>
      !>C
      !>C     Safely close the file
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Beginning address       :  897
      !>        Ending address          :  912
      !>        Number of data elements :   16
      !>        The first and second states stored in the segment:
      !>          3376.422  -326.649  -115.392     0.000     0.000     0.000
      !>          3376.422  -326.649  -115.392     0.000     0.000     0.000
      !>
      !>## Restrictions
      !>
      !>     1)  There are several types of records in a DAF. This routine
      !>         is only to be used to read double precision data bounded
      !>         between two DAF addresses. The range of addresses input
      !>         may not cross data and summary record boundaries.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     F.S. Turner        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Changed the input argument names BEGIN and END to BADDR to
      !>        EADDR for consistency with other routines.
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added
      !>        complete code example. Removed unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.0, 16-NOV-2001 (FST)
      module subroutine dafgda ( ctx, handle, baddr, eaddr, data )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a DAF.
      integer(ip), intent(in) :: baddr  !! Initial, final address within file.
      integer(ip), intent(in) :: eaddr  !! Initial, final address within file.
      real(wp), intent(out) :: data     ( * )  !! Data contained between BADDR and EADDR.


      integer(ip) :: begr
      integer(ip) :: begw
      integer(ip) :: endr
      integer(ip) :: endw

      integer(ip) :: recno
      integer(ip) :: first
      integer(ip) :: last
      integer(ip) :: next

      integer(ip) :: k
      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Bad addresses?
!
      if ( baddr <= 0 ) then
         call ctx%setmsg ( 'Negative value for BADDR address: #' )
         call ctx%errint ( '#', baddr )
         call ctx%sigerr ( 'SPICE(DAFNEGADDR)' )
         return

      else if ( baddr > eaddr ) then
         call ctx%setmsg ( 'Beginning address (#) greater than ending '   // &
                       'address (#).' )
         call ctx%errint ( '#', baddr )
         call ctx%errint ( '#', eaddr   )
         call ctx%sigerr ( 'SPICE(DAFBEGGTEND)' )
         return
      end if

!
!     PEPPER: a file loaded in memory (FURNSH's in_memory): one copy, no
!     record buffer. Addresses past the end of the copy take the path
!     below (as for the file, whose missing records give zeros).
!
      k = daf_image_slot ( ctx, handle )
      if ( k > 0 ) then
         associate ( image => ctx%spice%zzddhman%files(k)%image )
            if ( eaddr <= size(image) ) then
               data(1:eaddr-baddr+1) = image(baddr:eaddr)
               return
            end if
         end associate
      end if

!
!     Convert raw addresses to record/word representations.
!
      call dafarw ( ctx, baddr, begr, begw )
      call dafarw ( ctx, eaddr, endr, endw )

!
!     Get as many records as needed. Return the last part of the
!     first record, the first part of the last record, and all of
!     every record in between. Any record not found is assumed to
!     be filled with zeros.
!
      next = 1

      do recno = begr, endr

         if ( begr == endr ) then
            first = begw
            last  = endw

         else if ( recno == begr ) then
            first = begw
            last  = bsize

         else if ( recno == endr ) then
            first = 1
            last  = endw

         else
            first = 1
            last  = bsize
         end if

         call dafgdr ( ctx, handle, recno, first, last, data( next ), found )

         if ( .not. found ) then
            data(next:next+last-first) = 0.0_wp
         end if

         next = next + ( last-first+1 )

      end do

      return
      end subroutine dafgda

      !> Convert double precision values from one binary file format
      !> to another.
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
      !>     INPUT      is a string containing a group of d.p. values read
      !>                from a file as a character string.  The length of
      !>                this string must be a multiple of the number of
      !>                bytes used to store a d.p. value in a file utilizing
      !>                INBFF.
      !>
      !>     SPACE      is the number of d.p. values that OUTPUT has room to
      !>                store.
      !>
      !>## Detailed Output
      !>
      !>     OUTPUT     is an array of double precision values containing
      !>                the translated values from INPUT into the native
      !>                binary format.
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
      !>     This routine translates double precision values from a non-native
      !>     binary format read from a file as a sequence of characters to the
      !>     native format.
      !>
      !>## Examples
      !>
      !>     See ZZDAFGSR or ZZDAFGDR.
      !>
      !>## Restrictions
      !>
      !>     1) Numeric data when read as characters from a file preserves
      !>        the bit patterns present in the file in memory.
      !>
      !>     2) The intrinsic ICHAR preserves the bit pattern of the character
      !>        byte read from a file.  Namely if one examines the integer
      !>        created the 8 least significant bits will be precisely those
      !>        found in the character.
      !>
      !>     3) The size of double precision values on the target environment
      !>        are a multiple of some number of bytes.
      !>
      !>     4) The length of the INPUT string is a multiple of the number
      !>        of bytes for a double precision value in the INBFF format.
      !>
      !>     5) INBFF is supported for reading on this platform, and not
      !>        equivalent to NATBFF on this platform.
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
      !>        Recast J = 8*(-1+I) + 1 as J = -7+8*I to accommodate
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
      module subroutine zzxlated ( ctx, inbff, input, space, output )
      use pepper_kinds, only: ip, wp
      use pepper_packing, only: pack_integers
      use pepper_constants, only: intmin
      use pepper_strings, only: ucase_inplace
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: inbff  !! Binary file format of d.p. values in INPUT.
      character(*), intent(in) :: input  !! String containing d.p. values read as characters.
      integer(ip), intent(in) :: space  !! Number of d.p. values that can be placed in OUTPUT.
      real(wp), intent(out) :: output ( * )  !! Translated d.p. values.
!
!     Length of the double precision and integer buffers that
!     are equivalenced. (PEPPER: the integer buffer only; its
!     integers are copied to OUTPUT as doubles with TRANSFER.)
!
      integer(ip), parameter :: dpblen = 128
      integer(ip), parameter :: inblen = 2 * dpblen
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
      integer(ip) :: inbufr ( inblen )
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: lenipt
      integer(ip) :: numdp
      integer(ip) :: osign
      integer(ip) :: outpos
      integer(ip) :: value
!
!     Statement Function Definitions
!
!     This function controls the conversion of characters to integers.
!     On some supported environments, ICHAR is not sufficient to
!     produce the desired results.  This, however, is not the case
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
      if ( .not. allocated ( ctx%zzxlated_state ) ) allocate ( ctx%zzxlated_state )
      associate ( bigint => ctx%zzxlated_state%bigint, &
                  smlint => ctx%zzxlated_state%smlint, &
                  strbff => ctx%zzxlated_state%strbff, &
                  natbff => ctx%zzxlated_state%natbff, &
                  first => ctx%zzxlated_state%first )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Perform some initialization tasks.
!
      if ( first ) then

!
!        Populate STRBFF.
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
!     Check to see if INBFF makes sense.
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
!     Retrieve the length of the input string, and set the position
!     into the output buffer to the beginning.
!
      lenipt = len(input)
      outpos = 1

!
!     Now branch based on NATBFF.
!
      if ( natbff == bigi3e ) then

         if ( inbff == ltli3e ) then

!
!           Check to see that the length of the input string is
!           appropriate.  Since this is a string containing LTL-IEEE
!           d.p. values, and this is a BIG-IEEE machine characters
!           are 1-byte and d.p. values are 8-bytes.  So the length
!           of INPUT must be a multiple of 8.
!
            numdp = lenipt / 8

            if ( lenipt - ( numdp*8 ) /= 0 ) then

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
            if ( numdp > space ) then

               call ctx%setmsg ( 'The caller specified that # double ' &
               //            'precision numbers are to be ' &
               //            'translated from binary format # to #. ' &
               //            ' However there is only room to hold # ' &
               //            'integers in the output array.  This ' &
               //            'error should never occur.'              )
               call ctx%errint ( '#', numdp                               )
               call ctx%errch  ( '#', strbff(inbff)                       )
               call ctx%errch  ( '#', strbff(natbff)                      )
               call ctx%errint ( '#', space                               )
               call ctx%sigerr ( 'SPICE(BUG)'                             )
               return

            end if

!
!           The remainder of this branch is devoted to translating
!           and copying blocks of DPBLEN double precision numbers
!           into OUTPUT.  Initialize K, the integer index into the
!           buffer equivalenced to DPBUFR. (PEPPER: INBUFR, whose
!           pairs of integers are copied to OUTPUT as doubles.)
!
            k = 1

!
!           Start looping over each 8 character package in INPUT and
!           converting it to double precision numbers.
!
            do i = 1, numdp

!
!              Compute the substring index of the first character
!              in INPUT for this integer.
!
               j = -7 + 8*i

!
!              Now arrange the bytes properly.  Since these characters
!              were read from a file utilizing LTL-IEEE:
!
!                      .
!                      .
!                      .
!                   -------
!                  |   J   |  - Least Significant Byte of Mantissa
!                   -------
!                  |  J+1  |  - Sixth Most Significant Mantissa Byte
!                   -------
!                  |  J+2  |  - Fifth Most Significant Mantissa Byte
!                   -------
!                  |  J+3  |  - Fourth Most Significant Mantissa Byte
!                   -------
!                  |  J+4  |  - Third Most Significant Mantissa Byte
!                   -------
!                  |  J+5  |  - Second Most Significant Mantissa Byte
!                   -------
!                  |  J+6  |  - Tail of Exponent, Most Significant
!                   -------     Bits of the Mantissa
!                  |  J+7  |  - Sign Bit, Head of Exponent
!                   -------
!                      .
!                      .
!                      .
!
!              Now rearrange the bytes to place them in the
!              proper order for d.p. values on BIG-IEEE machines.
!              This is accomplished in the following manner:
!
!                     INPUT(J+4:J+4)
!                     INPUT(J+5:J+5)*SHFT8
!                     INPUT(J+6:J+6)*SHFT16
!                  +  INPUT(J+7:J+7)*SHFT24
!                  -------------------------
!                     INBUFR(K)
!
!                     INPUT(J:J)
!                     INPUT(J+1:J+1)*SHFT8
!                     INPUT(J+2:J+2)*SHFT16
!                  +  INPUT(J+3:J+3)*SHFT24
!                  -------------------------
!                     INBUFR(K+1)
!
!              Perform the necessary computations.  What is outlined
!              above is implemented below using arithmetic operations.
!              The last "shifted 24 bits to the MSb" is handled
!              in a special way, since the sign bit can not be shifted
!              into place through simple multiplication.
!
               value       = zzichr( input((j+4):(j+4)) )
               inbufr(k)   = value

               value       = zzichr( input((j+5):(j+5)) )
               value       = value*shft8
               inbufr(k)   = inbufr(k)+value

               value       = zzichr( input((j+6):(j+6)) )
               value       = value*shft16
               inbufr(k)   = inbufr(k)+value

               value       = zzichr( input((j+7):(j+7)) )

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
!              positions and combine them with INBUFR(K).
!
               value     = value*shft24
               inbufr(k) = inbufr(k)+value

!
!              Install the sign bit.  At the moment in INBUFR(K)
!              the bits are precisely as they need to be arranged.
!              Perform the following computations:
!
!                 INBUFR(K) = (2**31-1) - INBUFR(K) + 1
!
!              Break this up into steps since 2**31 is not
!              representable with 32 bit integers that utilize
!              2's complement.
!
!              First, negate the result:
!
!                 INBUFR(K) = -INBUFR(K)
!
!              But this negation is effectively:
!
!                 INBUFR(K) = 2**32 - INBUFR(K)
!
!              Which yields:
!
!                 2**32 - (2**31) + INBUFR(K)
!
!              or
!
!                 2**31 + INBUFR(K)
!
!              which is the desired quantity.  Note, 0 must be
!              treated as a special case.
!
               if ( osign == 1 ) then
                  if ( inbufr(k) == 0 ) then
                     inbufr(k) = smlint
                  else
                     inbufr(k) = bigint-inbufr(k)
                     inbufr(k) = inbufr(k) + 1
                     inbufr(k) = -inbufr(k)
                  end if
               end if

               value       = zzichr( input(j:j) )
               inbufr(k+1) = value

               value       = zzichr( input((j+1):(j+1)) )
               value       = value*shft8
               inbufr(k+1) = inbufr(k+1)+value

               value       = zzichr( input((j+2):(j+2)) )
               value       = value*shft16
               inbufr(k+1) = inbufr(k+1)+value

               value       = zzichr( input((j+3):(j+3)) )

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
!              positions and combine them with INBUFR(K).
!
               value       = value*shft24
               inbufr(k+1) = inbufr(k+1)+value

!
!              Install the sign bit.  At the moment in INBUFR(K+1)
!              we have the bits precisely as they need to be
!              arranged.  Perform the computations as with the
!              previous integer.
!
               if ( osign == 1 ) then
                  if ( inbufr(k+1) == 0 ) then
                     inbufr(k+1) = smlint
                  else
                     inbufr(k+1) = bigint-inbufr(k+1)
                     inbufr(k+1) = inbufr(k+1) + 1
                     inbufr(k+1) = -inbufr(k+1)
                  end if
               end if

!
!              Check to see if the local buffer is full and the
!              double precision numbers need to be moved into the
!              next block of OUTPUT.
!
               if ( k == inblen-1 ) then

                  call pack_integers ( inbufr, size(inbufr), output(outpos:outpos+dpblen-1) )
                  outpos = outpos + dpblen
                  k = 1

!
!              Otherwise, increment K.
!
               else
                  k = k + 2
               end if

            end do

!
!           Copy any remaining double precision numbers from DPBUFR
!           into OUTPUT. (PEPPER: from INBUFR, by PACK_INTEGERS.)
!
            if ( k /= 1 ) then
               call pack_integers ( inbufr(1:k-1), k-1, output(outpos:outpos+k/2-1) )
            end if

         else

            call ctx%setmsg ( 'Unable to translate double precision ' &
            //            'values from binary file format # to #. ' &
            //            'This error should never occur and is ' &
            //            'indicative of a bug.  Contact NAIF.'     )
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
!           d.p. values, and this is a LTL-IEEE machine characters
!           are 1-byte and d.p. values are 8-bytes.  So the length
!           of INPUT must be a multiple of 8.
!
            numdp = lenipt / 8

            if ( lenipt - ( numdp*8 ) /= 0 ) then

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
            if ( numdp > space ) then

               call ctx%setmsg ( 'The caller specified that # double ' &
               //            'precision numbers are to be ' &
               //            'translated from binary format # to #. ' &
               //            ' However there is only room to hold # ' &
               //            'integers in the output array.  This ' &
               //            'error should never occur.'              )
               call ctx%errint ( '#', numdp                               )
               call ctx%errch  ( '#', strbff(inbff)                       )
               call ctx%errch  ( '#', strbff(natbff)                      )
               call ctx%errint ( '#', space                               )
               call ctx%sigerr ( 'SPICE(BUG)'                             )
               return

            end if

!
!           The remainder of this branch is devoted to translating
!           and copying blocks of DPBLEN double precision numbers
!           into OUTPUT.  Initialize K, the integer index into the
!           buffer equivalenced to DPBUFR. (PEPPER: INBUFR, whose
!           pairs of integers are copied to OUTPUT as doubles.)
!
            k = 1

!
!           Start looping over each 8 character package in INPUT and
!           converting them to double precision numbers.
!
            do i = 1, numdp

!
!              Compute the substring index of the first character
!              in INPUT for this integer.
!
               j = -7 + 8*i

!
!              Now arrange the bytes properly.  Since these characters
!              were read from a file utilizing BIG-IEEE:
!
!                      .
!                      .
!                      .
!                   -------
!                  |   J   |  - Sign Bit, Head of Exponent
!                   -------
!                  |  J+1  |  - Tail of Exponent, Most Significant
!                   -------     Bits of the Mantissa
!                  |  J+2  |  - Second Most Significant Mantissa Byte
!                   -------
!                  |  J+3  |  - Third Most Significant Mantissa Byte
!                   -------
!                  |  J+4  |  - Fourth Most Significant Mantissa Byte
!                   -------
!                  |  J+5  |  - Fifth Most Significant Mantissa Byte
!                   -------
!                  |  J+6  |  - Sixth Most Significant Mantissa Byte
!                   -------
!                  |  J+7  |  - Least Significant Byte of Mantissa
!                   -------
!                      .
!                      .
!                      .
!
!              Now rearrange the bytes to place them in the
!              proper order for d.p. values on LTL-IEEE machines.
!              This is accomplished in the following manner:
!
!                     INPUT(J+7:J+7)
!                     INPUT(J+6:J+6)*SHFT8
!                     INPUT(J+5:J+5)*SHFT16
!                  +  INPUT(J+4:J+4)*SHFT24
!                  -------------------------
!                     INBUFR(K)
!
!                     INPUT(J+3:J+3)
!                     INPUT(J+2:J+2)*SHFT8
!                     INPUT(J+1:J+1)*SHFT16
!                  +  INPUT(J:J)*SHFT24
!                  -------------------------
!                     INBUFR(K+1)
!
!              Perform the necessary computations. What is outlined
!              above is implemented below using arithmetic operations.
!              The last "shifted 24 bits to the MSb" is handled
!              in a special way, since the sign bit can not be shifted
!              into place through simple multiplication.
!
               value       = zzichr( input((j+7):(j+7)) )
               inbufr(k)   = value

               value       = zzichr( input((j+6):(j+6)) )
               value       = value*shft8
               inbufr(k)   = inbufr(k)+value

               value       = zzichr( input((j+5):(j+5)) )
               value       = value*shft16
               inbufr(k)   = inbufr(k)+value

               value       = zzichr( input((j+4):(j+4)) )

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
!              positions and combine them with INBUFR(K).
!
               value     = value*shft24
               inbufr(k) = inbufr(k)+value

!
!              Install the sign bit.  At the moment in INBUFR(K)
!              the bits are precisely as they need to be arranged.
!              Perform the following computations:
!
!                 INBUFR(K) = (2**31-1) - INBUFR(K) + 1
!
!              Break this up into steps since 2**31 is not
!              representable with 32 bit integers that utilize
!              2's complement.
!
!              First, negate the result:
!
!                 INBUFR(K) = -INBUFR(K)
!
!              But this negation is effectively:
!
!                 INBUFR(K) = 2**32 - INBUFR(K)
!
!              Which yields:
!
!                 2**32 - (2**31) + INBUFR(K)
!
!              or
!
!                 2**31 + INBUFR(K)
!
!              which is the desired quantity.  Note, 0 must be
!              treated as a special case.
!
               if ( osign == 1 ) then
                  if ( inbufr(k) == 0 ) then
                     inbufr(k) = smlint
                  else
                     inbufr(k) = bigint-inbufr(k)
                     inbufr(k) = inbufr(k) + 1
                     inbufr(k) = -inbufr(k)
                  end if
               end if

               value       = zzichr( input((j+3):(j+3)) )
               inbufr(k+1) = value

               value       = zzichr( input((j+2):(j+2)) )
               value       = value*shft8
               inbufr(k+1) = inbufr(k+1)+value

               value       = zzichr( input((j+1):(j+1)) )
               value       = value*shft16
               inbufr(k+1) = inbufr(k+1)+value

               value       = zzichr( input(j:j) )

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
!              positions and combine them with INBUFR(K).
!
               value       = value*shft24
               inbufr(k+1) = inbufr(k+1)+value

!
!              Install the sign bit.  At the moment in INBUFR(K+1)
!              we have the bits precisely as they need to be
!              arranged.  Perform the computations as with the
!              previous integer.
!
               if ( osign == 1 ) then
                  if ( inbufr(k+1) == 0 ) then
                     inbufr(k+1) = smlint
                  else
                     inbufr(k+1) = bigint-inbufr(k+1)
                     inbufr(k+1) = inbufr(k+1) + 1
                     inbufr(k+1) = -inbufr(k+1)
                  end if
               end if

!
!              Check to see if the local buffer is full and the
!              double precision numbers need to be moved into the
!              next block of OUTPUT.
!
               if ( k == inblen-1 ) then

                  call pack_integers ( inbufr, size(inbufr), output(outpos:outpos+dpblen-1) )
                  outpos = outpos + dpblen
                  k = 1

!
!              Otherwise, increment K.
!
               else
                  k = k + 2
               end if

            end do

!
!           Copy any remaining double precision numbers from DPBUFR
!           into OUTPUT. (PEPPER: from INBUFR, by PACK_INTEGERS.)
!
            if ( k /= 1 ) then
               call pack_integers ( inbufr(1:k-1), k-1, output(outpos:outpos+k/2-1) )
            end if

         else

            call ctx%setmsg ( 'Unable to translate double precision ' &
            //            'values from binary file format # to #. ' &
            //            'This error should never occur and is ' &
            //            'indicative of a bug.  Contact NAIF.'     )
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
         //            'for translation of double precision numbers ' &
         //            'from non-native formats.'                      )
         call ctx%errch  ( '#', strbff(natbff)                             )
         call ctx%sigerr ( 'SPICE(BUG)'                                    )
         return

      end if

      return
      end associate
      end subroutine zzxlated

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routines [[dafgda]] and [[dafgsr]]. This routine is supported for purposes
      !> of backward compatibility only.
      !>
      !> Read the double precision data bounded by two addresses within
      !> a DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF.
      !>
      !>     BEGIN,
      !>     END      are the initial and final addresses of a contiguous
      !>              set of double precision numbers within a DAF.
      !>              Presumably, these make up all or part of a particular
      !>              array.
      !>
      !>## Detailed Output
      !>
      !>     DATA     are the double precision data contained between
      !>              the specified addresses within the specified file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If BEGIN is zero or negative, the error SPICE(DAFNEGADDR)
      !>         is signaled.
      !>
      !>     2)  If BEGIN > END, the error SPICE(DAFBEGGTEND) is signaled.
      !>
      !>     3)  If the file associated with HANDLE is not of the native
      !>         binary file format, the error SPICE(UNSUPPORTEDBFF) is
      !>         signaled.
      !>
      !>     4)  If HANDLE is invalid, an error is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The principal reason that DAFs are so easy to use is that
      !>     the data in each DAF are considered to be one long contiguous
      !>     set of double precision numbers. You can grab data from anywhere
      !>     within a DAF without knowing (or caring) about the physical
      !>     records in which they are stored.
      !>
      !>     This routine has been made obsolete by the routines DAFGDA and
      !>     DAFGSR. This routine is supported for reasons of backward
      !>     compatibility only. New software development should utilize
      !>     DAFGDA or DAFGSR.
      !>
      !>## Examples
      !>
      !>     The following code fragment illustrates the use of DAFRDA
      !>     to read data from an imaginary array. The array begins with a
      !>     directory containing 11 epochs. Each pair of epochs bounds
      !>     an interval, and each interval is covered by a set of eight
      !>     osculating elements.
      !>
      !>        CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>        BEGIN = IC(5)
      !>        END   = IC(6)
      !>
      !>        CALL DAFRDA ( HANDLE, BEGIN, BEGIN+10, EPOCHS )
      !>
      !>        DO I = 1, 10
      !>           IF ( ET .GE. EPOCHS(I)  .AND.  ET .LE. EPOCHS(I+1) ) THEN
      !>              OFFSET = IC(5) + 11 + (I - 1) * 8
      !>
      !>              CALL DAFRDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS )
      !>              RETURN
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
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     H.A. Neilan        (JPL)
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.0.2, 18-MAY-2010 (BVS)
      !>
      !>        Index line now states that this routine is deprecated.
      !>
      !>-    SPICELIB Version 2.0.1, 27-OCT-2003 (NJB)
      !>
      !>        The header now states that this routine is deprecated.
      !>        The $Exceptions header section has been extended.
      !>        Minor additional header updates were made.
      !>
      !>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
      !>
      !>        Added SPICE(UNSUPPORTEDBFF) exception to the routine.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
      !>
      !>        The exception SPICE(UNSUPPORTEDBFF) was added to guarantee
      !>        this routine's functionality remains unchanged as a result
      !>        of the updates to the underlying DAF software's utilization of
      !>        the handle manager. In versions of the Toolkit prior to this,
      !>        all DAFs loaded were of the native binary file format.
      !>        While rather unlikely, this routine could be used to read
      !>        the contents of summary records in addition to the usual
      !>        data records. The non-native to native translation process
      !>        for these two different types of records in general are not
      !>        the same. Rather than attempt to interpret the caller's
      !>        intent, this routine is deprecated and restricted to
      !>        functioning only on DAFs of the native binary file format.
      !>
      !>-    Beta Version 1.1.0, 1-NOV-1989 (RET)
      !>
      !>        DAFRDA now only checks in and checks out if one of the two
      !>        possible exceptions occurs. The purpose of this change was to
      !>        help speed up a routine that gets called constantly by higher
      !>        level DAF routines.
      module subroutine dafrda ( ctx, handle, begin, end, data )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a DAF.
      integer(ip), intent(in) :: begin  !! Initial, final address within file.
      integer(ip), intent(in) :: end  !! Initial, final address within file.
      real(wp), intent(out) :: data     ( * )  !! Data contained between BEGIN and END.


      integer(ip) :: begr
      integer(ip) :: begw
      integer(ip) :: endr
      integer(ip) :: endw

      integer(ip) :: recno
      integer(ip) :: first
      integer(ip) :: last
      integer(ip) :: next

      logical :: found
      logical :: native

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to see if HANDLE is associated with a DAF of the native
!     binary file format.
!
      call zzddhisn ( ctx, handle, native, found )

!
!     If the HANDLE was located, then check whether the binary file
!     format is native.  Otherwise, defer diagnosing the missing
!     handle to DAFRDR.
!
      if ( ( found ) .and. ( .not. native ) ) then
         call ctx%setmsg ( 'The binary file format for file ''#'' ' &
         //            'is not native. This routine operates ' &
         //            'only on files of the native format.'    )
         call ctx%errhan ( '#', handle                              )
         call ctx%sigerr ( 'SPICE(UNSUPPORTEDBFF)'                  )
         return
      end if

!
!     Bad addresses?
!
      if ( begin <= 0 ) then
         call ctx%setmsg ( 'Negative value for BEGIN address: #' )
         call ctx%errint ( '#', begin )
         call ctx%sigerr ( 'SPICE(DAFNEGADDR)' )
         return

      else if ( begin > end ) then
         call ctx%setmsg ( 'Beginning address (#) greater than ending '   // &
                       'address (#).' )
         call ctx%errint ( '#', begin )
         call ctx%errint ( '#', end   )
         call ctx%sigerr ( 'SPICE(DAFBEGGTEND)' )
         return
      end if

!
!     Convert raw addresses to record/word representations.
!
      call dafarw ( ctx, begin, begr, begw )
      call dafarw ( ctx, end,   endr, endw )

!
!     Get as many records as needed. Return the last part of the
!     first record, the first part of the last record, and all of
!     every record in between. Any record not found is assumed to
!     be filled with zeros.
!

      next = 1

      do recno = begr, endr

         if ( begr == endr ) then
            first = begw
            last  = endw

         else if ( recno == begr ) then
            first = begw
            last  = bsize

         else if ( recno == endr ) then
            first = 1
            last  = endw

         else
            first = 1
            last  = bsize
         end if

         call dafrdr ( ctx, handle, recno, first, last, data( next ), found )

         if ( .not. found ) then
            data(next:next+last-first) = 0.0_wp
         end if

         next = next + ( last-first+1 )

      end do

      return
      end subroutine dafrda

      !> Read the contents of the file record of a DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle assigned to a DAF file opened for
      !>              reading.
      !>
      !>## Detailed Output
      !>
      !>     ND,
      !>     NI       are the numbers of double precision and integer
      !>              components, respectively, in each array summary in
      !>              the specified file.
      !>
      !>     IFNAME   is the internal file name stored in the first
      !>              (or file) record of the specified file.
      !>
      !>     FWARD    is the forward list pointer. This points to the
      !>              first summary record in the file. (Records between
      !>              the first record and the first summary record are
      !>              reserved when the file is created, and are invisible
      !>              to DAF routines.)
      !>
      !>     BWARD    is the backward list pointer. This points
      !>              to the final summary record in the file.
      !>
      !>     FREE     is the free address pointer. This contains the
      !>              first free address in the file. (That is, the
      !>              initial address of the next array to be added
      !>              to the file.)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the handle passed to this routine is not the handle of an
      !>         open DAF file, an error is signaled by a routine in the call
      !>         tree of this routine.
      !>
      !>     2)  If the specified DAF file is not open for read access, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     3)  If the specified record cannot (for some reason) be read,
      !>         the error SPICE(DAFFRNOTFOUND) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The file record of a DAF is the only record that contains
      !>     any global information about the file. This record is created
      !>     when the file is created, and is updated only when new arrays
      !>     are added.
      !>
      !>     Like character records, file records are not buffered.
      !>
      !>## Examples
      !>
      !>     In the following example, the value of the forward list
      !>     pointer is examined in order to determine the number of
      !>     reserved records in the DAF. These records are then read
      !>     and the contents printed to the screen.
      !>
      !>        CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
      !>        CALL DAFHLU ( HANDLE, UNIT )
      !>
      !>        DO I = 2, FWARD - 1
      !>           READ  (UNIT,REC=I) PRIVATE(1:1000)
      !>           WRITE (*,*)        PRIVATE(1:1000)
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
      !>     K.R. Gehringer     (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.2.0, 02-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.1.0, 30-DEC-2009 (EDW)
      !>
      !>        Expanded DAFFRNOTFOUND error message to identify the file
      !>        handle corresponding to the error condition.
      !>
      !>        Reordered header sections to conform to SPICE format.
      !>        Merged the $Revisions sections, now deleted, with $Version.
      !>
      !>-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
      !>
      !>        Updated this routine to utilize interfaces built on
      !>        the new handle manager to perform I/O operations.
      !>
      !>        This routine now utilizes ZZDAFGFR to retrieve information
      !>        from the file record. As this private interface takes a
      !>        handle and performs the necessary logical unit to handle
      !>        mapping, the call to DAFHLU was removed. The DAFSIH call
      !>        remains, since this insures that HANDLE is known to DAFAH.
      !>
      !>-    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG)
      !>
      !>        The error SPICE(DAFNOIDWORD) is no longer signaled by this
      !>        routine. The reason for this is that if DAFSIH returns OK then
      !>        the handle passed to this routine is indeed a valid DAF file
      !>        handle, otherwise the error is diagnosed by DAFSIH.
      !>
      !>        Added a call to DAFSIH to signal an invalid handle and a test
      !>        of FAILED () after it. This is to make sure that the DAF file
      !>        is open for reading. If this call succeeds, we know that we
      !>        have a valid DAF handle, so there is no need to check FAILED
      !>        after the call to DAFHLU.
      !>
      !>        The variable name DAFWRD was changed to IDWORD.
      !>
      !>        Added two new exceptions to the $Exceptions section: 1 and 2.
      !>        The remaining exception (3) was already present. The exceptions
      !>        that were added are not new, but are being documented for the
      !>        first time.
      !>
      !>-    SPICELIB Version 1.0.3, 06-OCT-1992 (HAN)
      !>
      !>        Corrected a typo in the $Brief_I/O section. ND was listed
      !>        twice as an input, and NI was not listed.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of an open DAF file.
      integer(ip), intent(out) :: nd  !! Number of double precision components in summaries.
      integer(ip), intent(out) :: ni  !! Number of integer components in summaries.
      character(*), intent(out) :: ifname  !! Internal file name.
      integer(ip), intent(out) :: fward  !! Forward list pointer.
      integer(ip), intent(out) :: bward  !! Backward list pointer.
      integer(ip), intent(out) :: free  !! Free address pointer.



      character(idwlen) :: idword

      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Do some initializations
!
      idword = ' '

!
!     Check to be sure that HANDLE is attached to a file that is open
!     with read access. If the call fails, return.
!
      call dafsih ( ctx, handle, 'READ' )

      if ( ctx%failed() ) return

!
!     Retrieve all but the internal file name directly from the
!     file record.  Read the internal file name into a temporary
!     string, to be sure of the length. Check FOUND.
!
      call zzdafgfr ( ctx, handle, &
                      idword, &
                      nd, &
                      ni, &
                      ifname, &
                      fward, &
                      bward, &
                      free, &
                      found   )

      if ( .not. found ) then

         call ctx%setmsg ( 'File record not found for file handle #1. ' &
                   //  'Check if program code uses ' &
                   //  'handle #2 for a read or write operation.')
         call ctx%errint ( '#1', handle              )
         call ctx%errint ( '#2', handle              )
         call ctx%sigerr ( 'SPICE(DAFFRNOTFOUND)'   )
         return

      end if

      return
      end subroutine dafrfr

      !> Write or rewrite the contents of the file record of a DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a DAF file opened for
      !>              writing.
      !>
      !>     ND,
      !>     NI       are the numbers of double precision and integer
      !>              components, respectively, in each array summary
      !>              in the specified file.
      !>
      !>     IFNAME   is the internal file name to be stored in the first
      !>              (or file) record of the specified file.
      !>
      !>     FWARD    is the forward list pointer. This points to the
      !>              first summary record in the file.
      !>
      !>     BWARD    is the backward list pointer. This points to the
      !>              final summary record in the file.
      !>
      !>     FREE     is the free address pointer. This contains the
      !>              first free address in the file.
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
      !>     1)  If the handle passed to this routine is not the handle of an
      !>         open DAF file, an error is signaled by a routine in the call
      !>         tree of this routine.
      !>
      !>     2)  If the specified DAF file is not open for write access, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     3)  If the file record cannot (for some reason) be written,
      !>         the error SPICE(DAFWRITEFAIL) is signaled.
      !>
      !>     4)  If the attempt to read the file record fails, the error
      !>         SPICE(DAFREADFAIL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The file record of a DAF is the only record that contains
      !>     any global information about the file. This record is created
      !>     when the file is created, and is updated only when new arrays
      !>     are added.
      !>
      !>        DO NOT CHANGE THE CONTENTS OF THE FILE RECORD UNLESS
      !>        YOU ARE ABSOLUTELY SURE YOU KNOW WHAT YOU ARE DOING.
      !>
      !>     Like character records, file records are not buffered.
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
      !>     K.R. Gehringer     (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.1.0, 06-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Moved DAF required reading from $Literature_References to
      !>        $Required_Reading section. Fixed typo in $Brief_I/O: ND
      !>        argument was listed twice. Second should be NI. Removed
      !>        unnecessary entries in $Revisions section.
      !>
      !>-    SPICELIB Version 4.0.0, 27-NOV-2001 (FST)
      !>
      !>        Updated this routine to utilize new handle manager
      !>        interfaces. Comments were expanded and clarified.
      !>
      !>-    SPICELIB Version 3.0.0, 21-MAR-1999 (FST)
      !>
      !>        This routine was modified to accommodate the preservation
      !>        of the FTP validation and binary file format strings that
      !>        are now part of the DAF file record.
      !>
      !>-    SPICELIB Version 2.0.0, 05-OCT-1993 (KRG)
      !>
      !>        The error SPICE(DAFNOIDWORD) is no longer signaled by this
      !>        routine. The reason for this is that if DAFSIH returns OK then
      !>        the handle passed to this routine is indeed a valid DAF file
      !>        handle, otherwise the error is diagnosed by DAFSIH.
      !>
      !>        Added two new exceptions to the $Exceptions section: 1 and 4.
      !>        The remaining exceptions (2 and 3) were already present. The
      !>        exceptions that were added are not new, but are being
      !>        documented for the first time.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.0.0, 27-NOV-2001 (FST)
      !>
      !>        The call to DAFHLU has been replaced with a call to
      !>        ZZDDHHLU, the handle manager interface for retrieving
      !>        a logical unit. DAFHLU is no longer used, since it
      !>        locks the unit returned to its HANDLE, tying up resources
      !>        in the handle manager.
      !>
      !>-    SPICELIB Version 3.0.0, 21-MAR-1999 (FST)
      !>
      !>        In order to preserve the additional information that
      !>        now resides in the file record, this routine reads
      !>        the entire record into local buffers, including the
      !>        TAILEN characters that follow the actual data content.
      !>        The contents of the local buffers that correspond to
      !>        information brought in from the call sequence of the
      !>        routine are ignored when the record is rewritten.
      !>        However, the ID word, the file format string, and the
      !>        trailing TAILEN characters that contain the FTP validation
      !>        string are rewritten along with the input values.
      !>
      !>        This routine does not simply replace the FTP validation
      !>        string with the components from ZZFTPSTR, since that
      !>        would possibly validate a corrupt file created using a newer
      !>        Toolkit.
      !>
      !>-    SPICELIB Version 2.0.0, 05-OCT-1993 (KRG)
      !>
      !>        The error SPICE(DAFNOIDWORD) is no longer signaled by this
      !>        routine. The reason for this is that if DAFSIH returns OK then
      !>        the handle passed to this routine is indeed a valid DAF file
      !>        handle, otherwise the error is diagnosed by DAFSIH.
      !>
      !>        Added a call to DAFSIH to signal an invalid handle and a test
      !>        of FAILED () after it. This is to make sure that the DAF file
      !>        is open for writing. If this call succeeds, we know that we
      !>        have a valid DAF handle, so there is no need to check FAILED
      !>        after the call to DAFHLU.
      !>
      !>        Added code to read the file ID word so that it could be
      !>        preserved when the file record is written. This supports the ID
      !>        word format that contains type information.
      !>
      !>        Added variable IDWORD to the routine, as well as the parameters
      !>        IDWLEN and IFNLEN.
      !>
      !>        Added two new exceptions to the $Exceptions section: 1 and 4.
      !>        The remaining exceptions (2 and 3) were already present. The
      !>        exceptions that were added are not new, but are being
      !>        documented for the first time.
      !>
      !>        Removed code that tested the sign of HANDLE to see if the file
      !>        was open for write access, HANDLE < 0. This test was no longer
      !>        necessary, as the call to DASSIH performs this test as well. No
      !>        sense doing it twice.
      module subroutine dafwfr ( ctx, handle, nd, ni, ifname, fward, bward, free )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of an open DAF file.
      integer(ip), intent(in) :: nd  !! Number of double precision components in summaries.
      integer(ip), intent(in) :: ni  !! Number of integer components in summaries.
      character(*), intent(in) :: ifname  !! Internal filename.
      integer(ip), intent(in) :: fward  !! Forward list pointer.
      integer(ip), intent(in) :: bward  !! Backward list pointer.
      integer(ip), intent(in) :: free  !! Free address pointer.




!
!     The parameter TAILEN determines the tail length of a DAF file
!     record.  This is the number of bytes (characters) that
!     occupy the portion of the file record that follows the
!     integer holding the first free address.  For environments
!     with a 32 bit word length, 1 byte characters, and DAF
!     record sizes of 1024 bytes, we have:
!
!           8 bytes - IDWORD
!           4 bytes - ND     (32 bit integer)
!           4 bytes - NI     (32 bit integer)
!          60 bytes - IFNAME
!           4 bytes - FWARD  (32 bit integer)
!           4 bytes - BWARD  (32 bit integer)
!         + 4 bytes - FREE   (32 bit integer)
!          ---------
!          88 bytes - (All file records utilize this space.)
!
!     So the size of the remaining portion (or tail) of the DAF
!     file record for computing environments as described above
!     would be:
!
!        1024 bytes - DAF record size
!      -    8 bytes - DAF Binary File Format Word
!      -   88 bytes - (from above)
!       ------------
!         928 bytes - DAF file record tail length
!
!     Note: environments that do not have a 32 bit word length,
!     1 byte characters, and a DAF record size of 1024 bytes, will
!     require the adjustment of this parameter.
!
      integer(ip), parameter :: tailen = 928

      character(fmtlen) :: format
      character(idwlen) :: idword
      character(ifnlen) :: ifn
      character(ifnlen) :: locifn
      character(tailen) :: tail

      integer(ip) :: iostat
      integer(ip) :: locfdr
      integer(ip) :: locffa
      integer(ip) :: locldr
      integer(ip) :: locnd
      integer(ip) :: locni
      integer(ip) :: unit

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Do some initializations
!
      idword = ' '

!
!     Check to be sure that HANDLE is attached to a file that is open
!     with write access. If the call fails, return.
!
      call dafsih ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Get the logical unit for the file, as we know we have a valid DAF
!     handle with the correct access method.
!
      call zzddhhlu ( ctx, handle, 'DAF', .false., unit )

      if ( ctx%failed() ) return

!
!     In order to maintain the integrity of the file ID word, the
!     file FORMAT, and the FTP string if present, we need to
!     read the entire file record into the appropriate sized local
!     buffers. The values of the LOCxxx variables are simply
!     ignored, since the caller passes new values in for updates.
!
      read ( unit, rec=1, iostat=iostat) idword, &
                                         locnd, &
                                         locni, &
                                         locifn, &
                                         locfdr, &
                                         locldr, &
                                         locffa, &
                                         format, &
                                         tail

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'Attempt to read the file record failed for'   // &
                       ' file ''#''. IOSTAT = #'                      )
         call ctx%errfnm ( '#', unit                                      )
         call ctx%errint ( '#', iostat                                    )
         call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                           )
         return

      end if

!
!     Set the value of the internal filename before writing. This is to
!     guarantee that its length is ok.
!
      ifn = ifname

      write (unit, &
             rec=1, &
             iostat=iostat) idword, &
                            nd, &
                            ni, &
                            ifn, &
                            fward, &
                            bward, &
                            free, &
                            format, &
                            tail
      flush ( unit )
      call files_written ( ctx )

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'File record write failed. Value of IOSTAT ' // &
                       'was #' )
         call ctx%errint ( '#', iostat )
         call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)' )
         return

      end if

      return
      end subroutine dafwfr

      !> Write or rewrite the double precision data bounded by two
      !> addresses within a DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF.
      !>
      !>     BEGIN,
      !>     END      are the initial and final addresses of a contiguous
      !>              set of double precision numbers within a DAF.
      !>              Presumably, these make up all or part of a
      !>              particular array.
      !>
      !>     DATA     are the double precision data to be stored between
      !>              the specified addresses within the specified file.
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
      !>     1)  If BEGIN is zero or negative, the error SPICE(DAFNEGADDR)
      !>         is signaled.
      !>
      !>     2)  If the BEGIN > END, the error SPICE(DAFBEGGTEND)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The principal reason that DAFs are so easy to use is that
      !>     the data in each DAF are considered to be one long contiguous
      !>     set of double precision numbers. You can store data anywhere
      !>     within a DAF without knowing (or caring) about the physical
      !>     records in which they are stored.
      !>
      !>     Of course, if you are merely adding arrays to a DAF,
      !>     you should not use DAFWDA directly, but should use DAFANA
      !>     (add new array) and its entry points, since these update
      !>     the appropriate bookkeeping records automatically.
      !>
      !>## Examples
      !>
      !>     The following code fragment illustrates the use of DAFWDA
      !>     to update an imaginary array. The array begins with a directory
      !>     containing 11 epochs. Each pair of epochs bounds an
      !>     interval, and each interval is covered by a set of eight
      !>     osculating elements.
      !>
      !>     By accident, the elements were written with the wrong value for
      !>     the GM of the central body (the last element in each set). Each
      !>     set must be retrieved, updated,and rewritten.
      !>
      !>        CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>        BEGIN = IC(5)
      !>
      !>        DO I = 1, 10
      !>           OFFSET = BEGIN + 11 + (I - 1) * 8
      !>
      !>           CALL DAFRDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS )
      !>           ELEMENTS(8) = NEW_GM
      !>
      !>           CALL DAFWDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS )
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 27-OCT-2021 (JDR) (NJB)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Local variable BUFFER is now initialized and saved.
      !>
      !>        Edited the header to comply with NAIF standard. Moved DAF
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafwda ( ctx, handle, begin, end, data )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a DAF.
      integer(ip), intent(in) :: begin  !! Initial, final address within file.
      integer(ip), intent(in) :: end  !! Initial, final address within file.
      real(wp), intent(in) :: data     ( * )  !! Data to be stored between BEGIN and END.
      integer(ip) :: begr
      integer(ip) :: begw
      integer(ip) :: endr
      integer(ip) :: endw
      integer(ip) :: recno
      integer(ip) :: first
      integer(ip) :: n
      integer(ip) :: next
      logical :: found
      if ( .not. allocated ( ctx%dafwda_state ) ) allocate ( ctx%dafwda_state )
      associate ( buffer => ctx%dafwda_state%buffer )
      if ( ctx%failed() ) return

!
!     Bad addresses?
!
      if ( begin <= 0 ) then
         call ctx%setmsg ( 'Negative beginning address: #' )
         call ctx%errint ( '#', begin )
         call ctx%sigerr ( 'SPICE(DAFNEGADDR)' )
         return

      else if ( begin > end ) then
         call ctx%setmsg ( 'Beginning address (#) greater than ending '   // &
                       'address (#)' )
         call ctx%errint ( '#', begin )
         call ctx%errint ( '#', end   )
         call ctx%sigerr ( 'SPICE(DAFBEGGTEND)' )
         return
      end if

!
!     Convert raw addresses to record/word representations.
!
      call dafarw ( ctx, begin, begr, begw )
      call dafarw ( ctx, end,   endr, endw )

!
!     The first and last records may have to be read, updated, and
!     rewritten. Any records in between may be written directly.
!
      next = 1

      do recno = begr, endr

         if ( recno == begr  .or.  recno == endr ) then
            call dafrdr ( ctx, handle, recno, 1, bsize, buffer, found )

            if ( .not. found ) then
               buffer = 0.0_wp
            end if
         end if

         if ( begr == endr ) then
            first = begw
            n     = endw - begw + 1

         else if ( recno == begr ) then
            first = begw
            n     = bsize - begw + 1

         else if ( recno == endr ) then
            first = 1
            n     = endw

         else
            first = 1
            n     = bsize
         end if

         buffer(first:first+n-1) = data(next:next+n-1)
         next = next + n

         call dafwdr ( ctx, handle, recno, buffer )

      end do

      return
      end associate
      end subroutine dafwda

      !> Add a specified number of reserved records to a Double Precision
      !> Array File (DAF).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a DAF file that has
      !>              been opened with write access.
      !>
      !>     RESV     is the number of reserved records to be added
      !>              to the specified file.
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
      !>     1)  If RESV is less than one, the file is not changed.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     Normally, the reserved records in an array file are reserved
      !>     when the file is created. However, it may occasionally become
      !>     necessary to add reserved records---when the contents of one
      !>     file are appended to another, for example. (In this case, any
      !>     information in the reserved records of either file should
      !>     be included in the resulting file.)
      !>
      !>     The new reserved records are appended to the old ones. The new
      !>     reserved records are also NULL filled.
      !>
      !>## Examples
      !>
      !>     In the following call to DAFARR, assume that HANDLE is the file
      !>     handle for a DAF file that has been opened for write access, and
      !>     that the DAF file already contains 12 reserved records (located in
      !>     records 2-13 of the physical file).
      !>
      !>        CALL DAFARR ( HANDLE, 7 )
      !>
      !>     After this call, the DAF file attached to HANDLE will contain 19
      !>     reserved records. The new reserved records are located in
      !>     records 14-20 of the physical file.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine will only add reserved records to DAFs open for
      !>         write. These files are implicitly of the native binary file
      !>         format.
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.6.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.5.0, 16-NOV-2001 (FST)
      !>
      !>        Added a call to DAFSIH to prevent this routine from
      !>        attempting to write to non-native binary file formats.
      !>        This will provide a more useful error diagnostic with
      !>        little impact on performance.
      !>
      !>-    SPICELIB Version 1.4.0, 08-MAR-1996 (KRG)
      !>
      !>        Added code to write NULL filled records to the file for the
      !>        new reserved records.
      !>
      !>-    SPICELIB Version 1.3.0, 12-MAY-1994 (KRG)
      !>
      !>        Added a missing call to CHKOUT before the RETURN statement in
      !>        the test
      !>
      !>              IF ( RESV .LT. 1 ) THEN
      !>                 RETURN
      !>              END IF
      !>
      !>-    SPICELIB Version 1.2.0, 30-SEP-1993 (KRG)
      !>
      !>        $Detailed_Input and $Examples section of the header were
      !>        modified.
      !>
      !>        Added calls to the FORTRAN intrinsic functions INT and
      !>        DBLE in the code that updates the summary record.
      !>
      !>        Modified an IF loop to make logic clearer.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 17-JUL-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.4.0, 08-MAR-1996 (KRG)
      !>
      !>        Added code to write NULL filled records to the file for the
      !>        new reserved records.
      !>
      !>-    SPICELIB Version 1.3.0, 12-MAY-1994 (KRG)
      !>
      !>        Added a missing call to CHKOUT before the RETURN statement in
      !>        the test
      !>
      !>              IF ( RESV .LT. 1 ) THEN
      !>                 RETURN
      !>              END IF
      !>
      !>-    SPICELIB Version 1.2.0, 30-SEP-1993 (KRG)
      !>
      !>        $Detailed_Input section was modified. References to any
      !>        specific routines by name as a method for opening a DAF file
      !>        for write access were removed. The assumption is that a person
      !>        using DAF files would already know something about opening and
      !>        closing the files.
      !>
      !>        $Examples section was modified. References to any specific
      !>        routines by name as a method for opening a DAF file for writing
      !>        were removed, and the example was reworded in such a way that
      !>        the use of the subroutine remained clear.
      !>
      !>        Added calls to the INT intrinsic function to convert a DP
      !>        number to an integer before assigning it to NEXT, which is an
      !>        integer variable. Also added calls to INT in IF statements
      !>        where comparisons were made between DP numbers and INTEGERs,
      !>        when integral values were actually being compared.
      !>
      !>        Added calls to the intrinsic function DBLE to convert an
      !>        integer, RESV, into a DP number when doing some arithmetic.
      !>
      !>        Took an ELSE IF clause out of the initial IF return  ELSE
      !>        check in END IF at the beginning of the routine. Replaced the
      !>        code:
      !>
      !>              IF ( RETURN () ) THEN
      !>                 RETURN
      !>
      !>              ELSE IF ( RESV .LT. 1 ) THEN
      !>                 RETURN
      !>
      !>              ELSE
      !>                 CALL CHKIN ( 'DAFARR' )
      !>              END IF
      !>
      !>        with the equivalent code:
      !>
      !>              IF ( RETURN () ) THEN
      !>                 RETURN
      !>              ELSE
      !>                 CALL CHKIN ( 'DAFARR' )
      !>              END IF
      !>
      !>        C
      !>        C     Check to see if the number of records to be reserved is
      !>        C     less than one. If so, just return without changing
      !>        C     anything.
      !>        C
      !>              IF ( RESV .LT. 1 ) THEN
      !>                 RETURN
      !>              END IF
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 17-JUL-1990 (IMU)
      module subroutine dafarr ( ctx, handle, resv )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a DAF file opened for writing.
      integer(ip), intent(in) :: resv  !! Number of records to reserve.

      character(maxc) :: crec
      character(ifnlen) :: ifname

      real(wp) :: dc      ( maxd )
      real(wp) :: drec    ( wpr  )
      real(wp) :: sum     ( maxd )

      integer(ip) :: begblk
      integer(ip) :: bward
      integer(ip) :: endblk
      integer(ip) :: free
      integer(ip) :: fward
      integer(ip) :: i
      integer(ip) :: incr
      integer(ip) :: ic      ( maxi )
      integer(ip) :: nd
      integer(ip) :: next
      integer(ip) :: ni
      integer(ip) :: recno
      integer(ip) :: word

      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     Check to see if the number of records to be reserved is less than
!     one. If so, just return without changing anything.
!
      if ( resv < 1 ) return

!
!     Before proceeding any further, check that the DAF associated
!     with HANDLE is available for write access.
!
      call dafsih ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Get the contents of the file record. If it fails, then just check
!     out and return, as an appropriate error message should have
!     already been set.
!
      call dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )

      if ( ctx%failed() ) return

!
!     Okay, here's the plan. We are just going to move records
!     in the direction of the end of the file, starting
!     with the last record in the file and ending with the first
!     summary record.
!
!     After everything has been moved, the initial and final
!     addresses of all the arrays have to be incremented by the
!     same amount: the number of words per record (128) times
!     the number of new records.
!
      incr = wpr * resv

!
!     Before we do that, however, we should write some bogus records
!     to the end of the file, to make sure we don't run out of space
!     later on. If this doesn't work, we will leave the logical
!     contents of the file uncorrupted (although it may get larger).
!
      call dafarw ( ctx, free, recno, word )

      do i = 1, resv

         call dafwdr ( ctx, handle, recno + i, drec )

      end do

      if ( ctx%failed() ) return

!
!     Records will be moved in `blocks', where each block contains
!
!        -- a summary record
!
!        -- a name record
!
!        -- one or more data records
!
!     The first block to be moved (that is, the last block in
!     the file) lies between the final summary record (BWARD) and
!     whatever record contains the first free address in the file.
!
      begblk = bward
      call dafarw ( ctx, free, endblk, word )

      do while ( ( begblk > 0 )  .and.  ( .not. ctx%failed() ) )

!
!        Move the data records first.
!
         do recno = endblk,  begblk + 2,  -1

            call dafrdr ( ctx, handle, recno,        1, wpr, drec, found )
            call dafwdr ( ctx, handle, recno + resv,         drec        )

         end do

!
!        Then the name record.
!
         recno = begblk + 1
         call dafrcr ( ctx, handle, recno,        crec )
         call dafwcr ( ctx, handle, recno + resv, crec )

!
!        Finally, the summary record.
!
!        To find the beginning of the next block, look at the backward
!        pointer from the summary record of the current block.
!
!        Be sure to adjust the forward and backward pointers;
!        otherwise, we won't be able to find the summaries again.
!
         recno = begblk
         call dafrdr ( ctx, handle, recno, 1, wpr, drec, found )

         next = int ( drec(2) )

         if ( int ( drec(1) ) > 0 ) then
            drec(1) = drec(1) + real( resv, wp )
         end if

         if ( int ( drec(2) ) > 0 ) then
            drec(2) = drec(2) + real( resv, wp )
         end if

         call dafwdr ( ctx, handle, recno + resv, drec )

!
!        The next block ends just before the current block begins.
!
         endblk = begblk  - 1
         begblk = next

      end do

!
!     Rewrite the file record, to reflect the new organization of
!     the file.
!
      fward = fward + resv
      bward = bward + resv
      free  = free  + incr

      call dafwfr ( ctx, handle, nd, ni, ifname, fward, bward, free )

!
!     Get the summary for each array, increment the addresses (stored
!     in the final two integer components), and replace the summary.
!
      call dafbfs ( ctx, handle )
      call daffna ( ctx, found  )

      do while ( found  .and.  ( .not. ctx%failed() ) )

         call dafgs  ( ctx, sum                 )
         call dafus  ( ctx, sum, nd, ni, dc, ic )

         ic(ni-1) = ic(ni-1) + incr
         ic(ni  ) = ic(ni  ) + incr

         call dafps ( ctx, nd, ni, dc, ic, sum )
         call dafws ( ctx,                 sum )

         call daffna ( ctx, found )

      end do
!
!     Write NULL filled records to the reserved record area.
!
      do i = 1, maxc
         crec(i:i) = char(0)
      end do

      i = fward-resv
      do recno = i, i + resv - 1
         call dafwcr ( ctx, handle, recno, crec )
      end do

      return
      end subroutine dafarr

      !> Read the contents of a character record from a DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a DAF.
      !>
      !>     RECNO    is the record number of a character record within
      !>              the file.
      !>
      !>## Detailed Output
      !>
      !>     CREC     contains the first 1000 characters of the specified
      !>              record from the specified file.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the declared length of CREC is not 1000 characters,
      !>         the error SPICE(DAFBADRECLEN) is signaled.
      !>
      !>     2)  If the specified record cannot (for some reason) be read,
      !>         the error SPICE(DAFCRNOTFOUND) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Unlike double precision records, character records are
      !>     not buffered. Also, while failing to find a specific double
      !>     precision record is indicated through the calling sequence,
      !>     failing to find a character record results in an error.
      !>
      !>## Examples
      !>
      !>     In the following example, matching summary and name records are
      !>     read from a DAF:
      !>
      !>        CALL DAFGDR ( HANDLE, NEXT,   DREC, FOUND )
      !>        CALL DAFRCR ( HANDLE, NEXT+1, CREC        )
      !>
      !>     Note that a character record always immediately follows a summary
      !>     record.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine is only used to read records on environments
      !>         whose characters are a single byte in size. Updates
      !>         to this routine and routines in its call tree may be
      !>         required to properly handle other cases.
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
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 17-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved DAF
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
      !>
      !>        Updated this routine to make proper use of the new
      !>        handle manager functionality installed underneath
      !>        DAF.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
      !>
      !>        This routine now makes use of the handle manager
      !>        code. A call to DAFSIH was inserted just after
      !>        the standard SPICE error handling code at the
      !>        head of the module. This was done to insure that
      !>        the caller is referring to a legitimately loaded
      !>        DAF. The penalty for performing this check is
      !>        a binary search on the number of loaded files,
      !>        which should be small compared to the actual READ
      !>        performed below.
      !>
      !>        The call to DAFHLU has been replaced with ZZDDHHLU,
      !>        since calls to DAFHLU locks handles to their logical
      !>        units.
      module subroutine dafrcr ( ctx, handle, recno, crec )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF.
      integer(ip), intent(in) :: recno  !! Record number of character record.
      character(*), intent(out) :: crec  !! Character record.

      integer(ip) :: unit
      integer(ip) :: iostat
      integer(ip) :: k

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to be sure that HANDLE is attached to a file that is open
!     with read access.  If the call fails, return.
!
      call dafsih ( ctx, handle, 'READ' )

      if ( ctx%failed() ) return

!
!     Now make certain that the string to receive the contents of
!     the character record is the appropriate length.
!
      if ( len ( crec ) /= 1000 ) then
         call ctx%setmsg ( 'Expected length of character record is 1000. '// &
                       'Passed string has length #' )
         call ctx%errint ( '#', len( crec ) )
         call ctx%sigerr ( 'SPICE(DAFBADCRECLEN)' )

      else

!
!        PEPPER: in-memory binary DAF (furnsh_memory): copy character
!        record directly from raw_records without opening a logical unit.
!
         unit = daf_raw_slot ( ctx, handle )
         if ( unit > 0 ) then
            if ( recno < 1 .or. recno > size(ctx%spice%zzddhman%files(unit)%raw_records) ) then
               call ctx%setmsg ( 'Could not read record #. IOSTAT was #.' )
               call ctx%errint ( '#', recno                               )
               call ctx%errint ( '#', -1_ip                               )
               call ctx%sigerr ( 'SPICE(DAFCRNOTFOUND)'                   )
            else
               crec = ctx%spice%zzddhman%files(unit)%raw_records(recno)(1:1000)
            end if
            return
         end if

!
!        Retrieve a logical unit for this handle.  This has the
!        side-effect of locking this UNIT to HANDLE.
!
!        PEPPER: a DAF built in memory has no file: the record is in its
!        image.
!
         k = daf_synthetic_slot ( ctx, handle )
         if ( k > 0 ) then
            associate ( image => ctx%spice%zzddhman%files(k)%image )
               if ( recno >= 1 .and. recno <= size(image)/128 ) then
                  crec = daf_image_chars ( image, recno )
               else
                  call ctx%setmsg ( 'Could not read record #. IOSTAT was #.' )
                  call ctx%errint ( '#', recno                               )
                  call ctx%errint ( '#', -1_ip                               )
                  call ctx%sigerr ( 'SPICE(DAFCRNOTFOUND)'                   )
               end if
            end associate
            return
         end if

         call zzddhhlu ( ctx, handle, 'DAF', .false., unit )

         if ( ctx%failed() ) return

         read (unit, &
               rec=recno, &
               iostat=iostat) crec

         if ( iostat /= 0 ) then
            call ctx%setmsg ( 'Could not read record #. IOSTAT was #.' )
            call ctx%errint ( '#', recno                               )
            call ctx%errint ( '#', iostat                              )
            call ctx%sigerr ( 'SPICE(DAFCRNOTFOUND)'                   )
         end if

      end if

      return
      end subroutine dafrcr

      !> Write or rewrite the contents of a character record to
      !> a DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a DAF.
      !>
      !>     RECNO    is the record number of a character record within
      !>              the file. If the record does not already exist, it
      !>              is created. Otherwise its contents are overwritten.
      !>
      !>     CREC     contains the first 1000 characters of the specified
      !>              record.
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
      !>     1)  If the specified file is not open for write access, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     2)  If the declared length of CREC is not 1000 characters,
      !>         the error SPICE(DAFBADRECLEN) is signaled.
      !>
      !>     3)  If the specified record cannot (for some reason) be written,
      !>         the error SPICE(DAFWRITEFAIL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Unlike double precision records, character records are
      !>     not buffered.
      !>
      !>## Examples
      !>
      !>     In the following example, matching summary and name records are
      !>     written to a DAF:
      !>
      !>        CALL DAFWDR ( HANDLE, NEXT,   DREC )
      !>        CALL DAFWCR ( HANDLE, NEXT+1, CREC )
      !>
      !>     Note that a character record always immediately follows a summary
      !>     record.
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
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 14-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved DAF
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 2.0.0, 27-NOV-2001 (FST)
      !>
      !>        Updated this routine to utilize new handle manager
      !>        interfaces. Replaced the check of the input handle's
      !>        sign with the appropriate call to DAFSIH.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 27-NOV-2001 (FST)
      !>
      !>        The call to DAFHLU has been replaced with a call to
      !>        ZZDDHHLU, the handle manager interface for retrieving
      !>        a logical unit. DAFHLU is no longer used, since it
      !>        locks the unit returned to its HANDLE, tying up resources
      !>        in the handle manager. A call to DAFSIH was inserted to
      !>        make certain that HANDLE is present in DAFAH's file table,
      !>        rather than simply checking the sign of HANDLE.
      module subroutine dafwcr ( ctx, handle, recno, crec )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF.
      integer(ip), intent(in) :: recno  !! Record number of character record.
      character(*), intent(in) :: crec  !! Character record.

      integer(ip) :: unit
      integer(ip) :: iostat

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      call zzddhhlu ( ctx, handle, 'DAF', .false., unit )

!
!     Look out for
!
!       -- Writing to a file that is open for read-only.
!
!       -- Trying to write a record that doesn't have length 1000.
!
!       -- Failed write.
!
      call dafsih ( ctx, handle, 'WRITE' )

      if ( len ( crec ) /= 1000 ) then

         call ctx%setmsg ( 'Expected length of character record is 1000.' // &
                       ' Length of passed record is #' )
         call ctx%errint ( '#', len ( crec ) )
         call ctx%sigerr ( 'SPICE(DAFBADCRECLEN)' )

      else

         write (unit, &
                rec=recno, &
                iostat=iostat) crec
         flush ( unit )
         call files_written ( ctx )

         if ( iostat /= 0 ) then
            call ctx%setmsg ( 'Character record write failed. Value of '  // &
                          'IOSTAT was #' )
            call ctx%errint ( '#', iostat )
            call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)' )
         end if
      end if

      return
      end subroutine dafwcr

      !> Reorder the arrays in a DAF according to a given order vector.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF that has been opened for
      !>              write access. Use DAFOPW, for example, to open
      !>              an existing file and get its handle.
      !>
      !>     IORDER   is the order vector to be used to re-order the
      !>              arrays stored in the DAF specified by HANDLE.
      !>
      !>              An integer order vector is an array of length
      !>              N whose elements are the integers 1 through N.
      !>
      !>              The first element of IORDER is the index of the
      !>              first array in the re-ordered file, and so on.
      !>
      !>     N        is the number of elements in the order vector.
      !>              This may be less than the number of arrays in
      !>              the file.
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
      !>     1)  If IORDER is not an order vector (that is, if it does
      !>         not contain every integer between 1 and N), the error
      !>         SPICE(DISORDER) is signaled.
      !>
      !>     2)  If N is greater than the number of arrays in the file,
      !>         the error SPICE(DISARRAY) is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     DAFRA does not actually move the elements of the double
      !>     precision arrays; it works by rearranging the contents
      !>     of the summary and name records in the file. The result
      !>     is that the search routines (BFS, FNA, BBS, FPA) will
      !>     return the arrays in the indicated order.
      !>
      !>     After re-ordering, array IORDER(1) of the input file is the
      !>     first array of the output file, array IORDER(2) of the input
      !>     file is the second array of the output file, and so on.
      !>
      !>     The order vector used by DAFRA is typically created for
      !>     a related array by one of the ORDER routines, as shown in
      !>     the example below.
      !>
      !>## Examples
      !>
      !>     The following code fragment sorts the arrays in a DAF by name.
      !>
      !>        C
      !>        C     Collect the names of the arrays in the file.
      !>        C
      !>              CALL DAFOPW ( FILE, HANDLE )
      !>
      !>              N = 0
      !>              CALL DAFBFS ( HANDLE )
      !>              CALL DAFFNA ( FOUND  )
      !>
      !>              DO WHILE ( FOUND )
      !>                 N = N + 1
      !>                 CALL DAFGN  ( NAMES(I) )
      !>                 CALL DAFFNA ( FOUND    )
      !>              END DO
      !>
      !>        C
      !>        C     Sort the names.
      !>        C
      !>              CALL ORDERC ( NAMES, N, IORDER )
      !>
      !>        C
      !>        C     Re-order the arrays.
      !>        C
      !>              CALL DARFA  ( HANDLE, IORDER, N )
      !>              CALL DAFCLS ( HANDLE            )
      !>
      !>     Afterward, a forward search like the one shown below
      !>
      !>        CALL DAFBFS ( HANDLE )
      !>        CALL DAFFNA ( FOUND  )
      !>
      !>        DO WHILE ( FOUND )
      !>           CALL DAFGN ( NAME )
      !>           WRITE (*,*) NAME
      !>
      !>           CALL DAFFNA ( FOUND )
      !>        END DO
      !>
      !>     produces an ordered list of the names in the sorted file.
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
      !>-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 28-MAR-1991 (IMU)
      module subroutine dafra ( ctx, handle, iorder, n )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: isordv
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF.
      integer(ip), intent(inout) :: iorder ( * )  !! Order vector.
      integer(ip), intent(in) :: n  !! Dimension of IORDER.

      integer(ip), parameter :: namlen = 1000

      integer(ip), parameter :: sumlen = 128

      character(namlen) :: holdn
      character(namlen) :: tempn

      real(wp) :: holds    ( sumlen )
      real(wp) :: temps    ( sumlen )

      integer(ip) :: hold
      integer(ip) :: i
      integer(ip) :: index
      integer(ip) :: start
      integer(ip) :: total

      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     If the order vector has fewer than two elements, don't bother.
!
      if ( n < 2 ) return

!
!     If IORDER is not an order vector, complain.
!
      if ( .not. isordv ( iorder, n ) ) then
         call ctx%setmsg ( 'Sorry, IORDER is not an order vector.' )
         call ctx%sigerr ( 'SPICE(DISORDER)' )
         return
      end if

!
!     If the number of arrays to be moved exceeds the number of
!     arrays in the file, complain.
!
      total = 0

      call dafbfs ( ctx, handle )
      call daffna ( ctx, found  )

      do while ( found  .and.  ( .not. ctx%failed() ) )
         total = total + 1
         call daffna ( ctx, found )
      end do

      if ( ctx%failed() ) then
         return

      else if ( total < n ) then
         call ctx%setmsg ( 'N (#) exceeds number of arrays (#).' )
         call ctx%errint ( '#', n )
         call ctx%errint ( '#', total )
         call ctx%sigerr ( 'SPICE(DISARRAY)' )
         return
      end if

!
!     Not surprisingly, this routine is patterned closely after the
!     (original) REORDx routines in SPICELIB. The only differences
!     are that
!
!        1) This routine is not error free---it checks to make
!           sure that IORDER is in fact an order vector, and that
!           every element in IORDER refers to an existing array.
!
!        2) Instead of moving elements of an array in and out of
!           a temporary location, it moves summaries and names.
!           This means that two sets of temporary storage locations
!           are needed: one to hold the summary and name of the
!           guy who began the current cycle; and one to hold the guy
!           being moved from location HOLD to location INDEX.
!
      start = 1

      do while ( ( start < n )  .and.  ( .not. ctx%failed() ) )

!
!        Start the cycle. One guy (pair of summary and name record)
!        has to sit out (in HOLDS and HOLDN) until the end of the cycle
!        is reached.
!
         index = start
         hold  = iorder(index)

         call dafbfs ( ctx, handle )

         do i = 1, index
            call daffna ( ctx, found )
         end do

         call dafgs ( ctx, holds )
         call dafgn ( ctx, holdn )

!
!        Move guys from HOLD to INDEX; then update HOLD (to point
!        to the next guy to be moved) and INDEX (to point at the
!        space just vacated).
!
!        Keep going until HOLD points to the first guy moved during
!        the current cycle. This ends the cycle.
!
         do while ( hold /= start )

!
!           Get the guy in position HOLD.
!
            call dafbfs ( ctx, handle )

            do i = 1, hold
               call daffna ( ctx, found )
            end do

            call dafgs ( ctx, temps )
            call dafgn ( ctx, tempn )

!
!           Move him to position INDEX. (Note that DAFWS is used to
!           update the summary instead of DAFRS, because the addresses
!           are actually being changed.)
!
            call dafbfs ( ctx, handle )

            do i = 1, index
               call daffna ( ctx, found )
            end do

            call dafws ( ctx, temps )
            call dafrn ( ctx, tempn )

!
!           Update HOLD and INDEX.
!
            index              =  hold
            hold               =  iorder(hold)
            iorder(index)      = -iorder(index)

         end do

!
!        The last element in the cycle is restored from TEMP.
!
         call dafbfs ( ctx, handle )

         do i = 1, index
            call daffna ( ctx, found )
         end do

         call dafws ( ctx, holds )
         call dafrn ( ctx, holdn )

         iorder(hold) = -iorder(hold)

!
!        Begin the next cycle at the next element in the order
!        vector with a positive sign. (That is, the next one
!        that hasn't been moved.)
!
         do while ( iorder(start) < 0  .and.  start < n )
            start = start + 1
         end do

      end do

!
!     Restore the original signs of the elements of the order
!     vector, for the next go around.
!
      do index = 1, n
        iorder(index) = iabs ( iorder(index) )
      end do

      return
      end subroutine dafra

      !> Remove a specified number of reserved records from a Double
      !> Precision Array File (DAF).
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle associated with a DAF that has been
      !>              opened with write access.
      !>
      !>     RESV     is the number of reserved records to be removed
      !>              from the specified file.
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
      !>     1)  If RESV is less than one, the file is not changed.
      !>
      !>     2)  If RESV is greater than the number of reserved records in the
      !>         file, all of the reserved records are removed.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     Normally, the reserved records in an array file are reserved
      !>     when the file is created. However, it may occasionally become
      !>     desirable to remove reserved records---when their contents are
      !>     significantly reduced, for example.
      !>
      !>     The records nearest the end of the file are removed. Note
      !>     that the physical size of the file is not reduced when reserved
      !>     records are removed.
      !>
      !>## Examples
      !>
      !>     For the following call to DAFRRR, assume that HANDLE is the file
      !>     handle for a DAF file that has been opened for write access, and
      !>     that the DAF file already contains 12 reserved records (located in
      !>     records 2-13 of the physical file).
      !>
      !>        CALL DAFRRR ( HANDLE, 7 )
      !>
      !>     After this call to DAFRRR, the number of reserved records has been
      !>     decreased by 7, leaving only the first five of the original
      !>     reserved records, physical records 2-6.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine will only remove reserve records from DAFs open
      !>         for write. These files are implicitly of the native binary
      !>         file format.
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 02-JUN-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.2.0, 16-NOV-2001 (FST)
      !>
      !>        Added a call to DAFSIH to prevent this routine from
      !>        attempting to write to non-native binary file formats.
      !>        This will provide a more useful error diagnostic with
      !>        little impact on performance.
      !>
      !>-    SPICELIB Version 1.1.0, 30-SEP-1993 (KRG)
      !>
      !>        $Detailed_Input and $Examples section of the header were
      !>        modified.
      !>
      !>        Added calls to the FORTRAN intrinsic functions INT and
      !>        DBLE in the code that updates the summary record.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 18-JUL-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 30-SEP-1993 (KRG)
      !>
      !>        $Detailed_Input section was modified. References to any
      !>        specific routines by name as a method for opening a DAF file
      !>        for write access were removed. The assumption is that a person
      !>        using DAF files would already know something about opening and
      !>        closing the files.
      !>
      !>        $Examples section was modified. References to any specific
      !>        routines by name as a method for opening a DAF file for writing
      !>        were removed, and the example was reworded in such a way that
      !>        the use of the subroutine remained clear.
      !>
      !>        Added calls to the INT intrinsic function to convert a DP
      !>        number to an integer before assigning it to NEXT or ENDBLK,
      !>        both of which are integer variables. Also added calls to INT
      !>        in IF statements where comparisons were made between DP numbers
      !>        and INTEGERs, when integral values were actually being
      !>        compared.
      !>
      !>        Added calls to the intrinsic function DBLE to convert an
      !>        integer, REMOVE, into a DP number when doing some arithmetic.
      module subroutine dafrrr ( ctx, handle, resv )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! DAF, opened for writing.
      integer(ip), intent(in) :: resv  !! Number of records to remove.

      character(maxc) :: crec
      character(ifnlen) :: ifname

      real(wp) :: dc      ( maxd )
      real(wp) :: drec    ( wpr  )
      real(wp) :: sum     ( maxd )

      integer(ip) :: begblk
      integer(ip) :: bward
      integer(ip) :: decr
      integer(ip) :: endblk
      integer(ip) :: free
      integer(ip) :: fward
      integer(ip) :: ic      ( maxi )
      integer(ip) :: nd
      integer(ip) :: next
      integer(ip) :: ni
      integer(ip) :: recno
      integer(ip) :: remove
      integer(ip) :: word

      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Before proceeding any further, check that the DAF associated
!     with HANDLE is available for write access.
!
      call dafsih ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Get the contents of the file record. If it fails, then just check
!     out and return, as an appropriate error message should have
!     already been set.
!
      call dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )

      if ( ctx%failed() ) return

!
!     Don't remove more than the current number of reserved records!
!     If there are none, check out.
!
      remove = min ( resv, fward - 2 )

      if ( remove < 1 ) return

!
!     Okay, here's the plan. We are just going to move records
!     forward, starting with the first summary record in the file
!     and ending with the last data record.
!
!     After everything has been moved, the initial and final
!     addresses of all the arrays have to be decremented by the
!     same amount: the number of words per record (128) times
!     the number of records removed.
!
      decr = wpr * remove

!
!     Records will be moved in `blocks', where each block contains
!
!        -- a summary record
!
!        -- a name record
!
!        -- one or more data records
!
!     Most blocks lie between one summary record and the next.
!     The final block lies between the final summary record and
!     whatever data record contains the first free address.
!
!     BEGBLK is initially the first summary record location.
!
      begblk = fward

      do while ( ( begblk > 0 )  .and.  ( .not. ctx%failed() ) )

!
!        Move the summary record first. The location of the next
!        summary record determines the end of this block, and the
!        beginning of the next.
!
!        Be sure to adjust the forward and backward pointers;
!        otherwise, we won't be able to find the summaries again.
!
         recno = begblk
         call dafrdr ( ctx, handle, recno, 1, wpr, drec, found )

         if ( int ( drec(1) ) > 0 ) then
            endblk = int ( drec(1) ) - 1
            next   = int ( drec(1) )
         else
            call dafarw ( ctx, free, endblk, word )
            next = 0
         end if

         if ( int ( drec(1) ) > 0 ) then
            drec(1) = drec(1) - real( remove, wp )
         end if

         if ( int ( drec(2) ) > 0 ) then
            drec(2) = drec(2) - real( remove, wp )
         end if

         call dafwdr ( ctx, handle, recno - remove, drec )

!
!        Then the name record.
!
         recno = begblk + 1
         call dafrcr ( ctx, handle, recno,          crec )
         call dafwcr ( ctx, handle, recno - remove, crec )

!
!        Finally, the data records.
!
         do recno = begblk + 2, endblk
            call dafrdr ( ctx, handle, recno,          1, wpr, drec, found )
            call dafwdr ( ctx, handle, recno - remove,         drec        )
         end do

!
!        Start the next block, if one exists.
!
         begblk = next

      end do

!
!     Rewrite the file record, to reflect the new organization of
!     the file.
!
      fward = fward - remove
      bward = bward - remove
      free  = free  - decr

      call dafwfr ( ctx, handle, nd, ni, ifname, fward, bward, free )

!
!     Get the summary for each array, decrement the addresses (stored
!     in the final two integer components), and replace the summary.
!
      call dafbfs ( ctx, handle )
      call daffna ( ctx, found  )

      do while ( found  .and.  ( .not. ctx%failed() ) )

         call dafgs  ( ctx, sum                 )
         call dafus  ( ctx, sum, nd, ni, dc, ic )

         ic(ni-1) = ic(ni-1) - decr
         ic(ni  ) = ic(ni  ) - decr

         call dafps ( ctx, nd, ni, dc, ic, sum )
         call dafws ( ctx,                 sum )

         call daffna ( ctx, found )

      end do

      return
      end subroutine dafrrr

      !> Add comments from a buffer of character strings to the comment
      !> area of a binary DAF file, appending them to any comments which
      !> are already present in the file's comment area.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the file handle of a binary DAF which has been opened
      !>              with write access.
      !>
      !>     N        is the number of comments in BUFFER that are to be added
      !>              to the comment area of the binary DAF attached to HANDLE.
      !>
      !>     BUFFER   is a buffer containing comments which are to be added
      !>              to the comment area of the binary DAF attached to HANDLE.
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
      !>     1)  If the number of comments to be added is not positive, the
      !>         error SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>     2)  If a non printing ASCII character is encountered in the
      !>         comments, the error SPICE(ILLEGALCHARACTER) is signaled.
      !>
      !>     3)  If the binary DAF file attached to HANDLE is not open with
      !>         write access, an error is signaled by a routine in the call
      !>         tree of this routine.
      !>
      !>     4)  If the end of the comments cannot be found, i.e., the end of
      !>         comments marker is missing on the last comment record, the
      !>         error SPICE(BADCOMMENTAREA) is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE in $Detailed_Input.
      !>
      !>## Particulars
      !>
      !>     A binary DAF contains a data area which is reserved for storing
      !>     annotations or descriptive textual information about the data
      !>     contained in a file. This area is referred to as the ``comment
      !>     area'' of the file. The comment area of a DAF is a line oriented
      !>     medium for storing textual information. The comment area
      !>     preserves leading or embedded white space in the line(s) of text
      !>     which are stored so that the appearance of the information will
      !>     be unchanged when it is retrieved (extracted) at some other time.
      !>     Trailing blanks, however, are NOT preserved, due to the way that
      !>     character strings are represented in standard Fortran 77.
      !>
      !>     This routine will take a buffer of text lines and add (append)
      !>     them to the comment area of a binary DAF. If there are no
      !>     comments in the comment area of the file, then space will be
      !>     allocated and the text lines in BUFFER will be placed into the
      !>     comment area. The text lines may contain only printable ASCII
      !>     characters (decimal values 32 - 126).
      !>
      !>     There is NO maximum length imposed on the significant portion
      !>     of a text line that may be placed into the comment area of a
      !>     DAF. The maximum length of a line stored in the comment area
      !>     should be reasonable, however, so that they may be easily
      !>     extracted. A good maximum value for this would be 255 characters,
      !>     as this can easily accommodate ``screen width'' lines as well as
      !>     long lines which may contain some other form of information.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) This example demonstrates how to append new comments to the
      !>        comment area of a DAF file.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           earthstns_itrf93_201023.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFAC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               RTRIM
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         KERNEL
      !>      PARAMETER           ( KERNEL =
      !>     .                         'earthstns_itrf93_201023.bsp' )
      !>
      !>      INTEGER               BUFSIZ
      !>      PARAMETER           ( BUFSIZ = 25 )
      !>
      !>      INTEGER               CMTSIZ
      !>      PARAMETER           ( CMTSIZ = 7  )
      !>
      !>      INTEGER               LINLEN
      !>      PARAMETER           ( LINLEN = 1000 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(LINLEN)    BUFFER ( BUFSIZ )
      !>      CHARACTER*(LINLEN)    NEWCMT ( CMTSIZ )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               N
      !>
      !>      LOGICAL               DONE
      !>
      !>C
      !>C     Set the new comments to be added to the DAF file.
      !>C
      !>      DATA                  NEWCMT /
      !>     .  '================== NEW COMMENTS ==================',
      !>     .  '',
      !>     .  '   New comments can be appended to the end of the',
      !>     .  '   comment area of a DAF file, with a single',
      !>     .  '   operation.',
      !>     .  '',
      !>     .  '================ END NEW COMMENTS ================' /
      !>
      !>C
      !>C     Open a DAF for write. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      CALL DAFOPW ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Print the end of comment area from the DAF file.
      !>C     (Maximum 15 lines.)
      !>C
      !>      DONE = .FALSE.
      !>
      !>      DO WHILE ( .NOT. DONE )
      !>
      !>         CALL DAFEC  ( HANDLE, 15, N, BUFFER, DONE )
      !>
      !>         IF ( DONE ) THEN
      !>
      !>            WRITE(*,'(A)') 'End of comment area of input '
      !>     .                  // 'DAF file (max. 15 lines): '
      !>            WRITE(*,'(A)') '-------------------------------'
      !>     .                  // '-------------------------------'
      !>
      !>            DO I = 1, N
      !>
      !>               WRITE (*,*) BUFFER(I)(:RTRIM(BUFFER(I)))
      !>
      !>            END DO
      !>
      !>            WRITE(*,'(A)') '-------------------------------'
      !>     .                  // '-------------------------------'
      !>
      !>         END IF
      !>
      !>      END DO
      !>
      !>C
      !>C     Append the new comments to the DAF file.
      !>C
      !>      CALL DAFAC ( HANDLE, CMTSIZ, NEWCMT )
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>C
      !>C     Check if the comments have indeed appended.
      !>C
      !>C     Open a DAF for read.
      !>C
      !>      CALL DAFOPR ( KERNEL, HANDLE )
      !>      DONE = .FALSE.
      !>
      !>      DO WHILE ( .NOT. DONE )
      !>
      !>         CALL DAFEC  ( HANDLE, BUFSIZ, N, BUFFER, DONE )
      !>
      !>         IF ( DONE ) THEN
      !>
      !>            WRITE(*,'(A)') 'End of comment area of input '
      !>     .                  // 'DAF file (max. 25 lines): '
      !>            WRITE(*,'(A)') '-------------------------------'
      !>     .                  // '-------------------------------'
      !>
      !>            DO I = 1, N
      !>
      !>               WRITE (*,*) BUFFER(I)(:RTRIM(BUFFER(I)))
      !>
      !>            END DO
      !>
      !>            WRITE(*,'(A)') '-------------------------------'
      !>     .                  // '-------------------------------'
      !>
      !>         END IF
      !>
      !>      END DO
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        End of comment area of input DAF file (max. 15 lines):
      !>        --------------------------------------------------------------
      !>            DSS-65_DXYZ       =    (    -0.0100          0.0242     ***
      !>            DSS-65_TOPO_EPOCH =       @2020-OCT-23/00:00
      !>            DSS-65_UP         =       'Z'
      !>            DSS-65_NORTH      =       'X'
      !>
      !>         \begintext
      !>        --------------------------------------------------------------
      !>        End of comment area of input DAF file (max. 25 lines):
      !>        --------------------------------------------------------------
      !>            DSS-65_DXYZ       =    (    -0.0100          0.0242     ***
      !>            DSS-65_TOPO_EPOCH =       @2020-OCT-23/00:00
      !>            DSS-65_UP         =       'Z'
      !>            DSS-65_NORTH      =       'X'
      !>
      !>         \begintext
      !>         ================== NEW COMMENTS ==================
      !>
      !>            New comments can be appended to the end of the
      !>            comment area of a DAF file, with a single
      !>            operation.
      !>
      !>         ================ END NEW COMMENTS ================
      !>        --------------------------------------------------------------
      !>
      !>        Warning: incomplete output. 2 lines extended past the right
      !>        margin of the header and have been truncated. These lines are
      !>        marked by "***" at the end of each line.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine uses constants that are specific to the ASCII
      !>         character sequence. The results of using this routine with
      !>         a different character sequence are unpredictable.
      !>
      !>     2)  This routine is only used to extract records on environments
      !>         whose characters are a single byte in size. Updates to this
      !>         routine and routines in its call tree may be required to
      !>         properly handle other cases.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     F.S. Turner        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 25-NOV-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code examples from existing code fragments.
      !>
      !>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
      !>
      !>        Updated this routine to utilize the new handle manager
      !>        interfaces.
      !>
      !>-    SPICELIB Version 1.0.0, 26-JUL-1994 (KRG)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
      !>
      !>        The call to DAFHLU has been replaced with a call to ZZDDHHLU,
      !>        the handle manager interface for retrieving a logical unit.
      !>        DAFHLU is no longer used, since it locks the unit returned to
      !>        its HANDLE, tying up resources in the handle manager.
      module subroutine dafac ( ctx, handle, n, buffer )
      use pepper_kinds, only: ip
      use pepper_strings, only: cpos, ncpos
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of a DAF opened with write access.
      integer(ip), intent(in) :: n  !! Number of comments to put into the comment area.
      character(*), intent(in) :: buffer(*)  !! Buffer of comments to put into the comment area.

!
!     Maximum and minimum decimal values for the printable ASCII
!     characters.
!
      integer(ip), parameter :: maxpch = 126

      integer(ip), parameter :: minpch = 32
      character(mxcrec) :: crecrd
      character(ifnlen) :: ifname

      integer(ip) :: curpos
      integer(ip) :: daflun
      integer(ip) :: eocpos
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: length
      integer(ip) :: nchars
      integer(ip) :: ncomc
      integer(ip) :: ncomr
      integer(ip) :: nelpos
      integer(ip) :: newrec
      integer(ip) :: notusd
      integer(ip) :: recno
      integer(ip) :: rinuse
      integer(ip) :: space

      integer(ip) :: iostat
      integer(ip) :: bward
      integer(ip) :: fward
      integer(ip) :: free
      integer(ip) :: nd
      integer(ip) :: ni

      logical :: empty
      logical :: found
!
!     Constants (SPICELIB computed them on the first call).
!
      character(1), parameter :: eocmrk = char ( inteoc )
      character(1), parameter :: eolmrk = char ( inteol )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     The lines of text in BUFFER will be ``packed'' into DAF comment
!     records: the significant portion of each comment line from BUFFER
!     will be terminated using the special character EOLMRK to indicate
!     the end of the line. When a comment record is full or all of the
!     comments have been added, the comment record will be written to
!     the comment area of the binary DAF file.
!
!     If this is the first time that this routine has been called,
!     we need to initialize the character value for the end-of-line
!     marker and the character value for the end of comments marker.
!
!     Verify that the DAF file attached to HANDLE is opened with write
!     access.
!
      call dafsih ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return
!
!     Convert the DAF file handle to its corresponding Fortran logical
!     unit number for reading and writing comment records.
!
      call zzddhhlu ( ctx, handle, 'DAF', .false., daflun )

      if ( ctx%failed() ) return
!
!     Check for a nonpositive number of lines in the buffer.
!
      if ( n <= 0 ) then

         call ctx%setmsg ( 'The number of comment lines to be added'  // &
                       ' to the binary DAF file ''#'' was not'    // &
                       ' positive: #.'                             )
         call ctx%errfnm ( '#', daflun                                 )
         call ctx%errint ( '#', n                                      )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)'                    )
         return

      end if
!
!     Count the number of characters in the buffer ignoring trailing
!     blanks on nonblank lines and blank lines. The count will be
!     modified to include the contribution of blank lines later. This
!     count is used to determine the number of character records to be
!     added to the binary DAF file attached to HANDLE.
!
      nchars = 0

      do i = 1, n
!
!        Get the length of the significant portion of a comment line.
!
         length = len_trim(buffer(i))
!
!        Scan the comment line for non printing characters.
!
         do j = 1, length
!
!           Check to see that the characters in the buffer are all
!           printing ASCII characters. The bounds for printing ASCII
!           characters are given by MINPCH and MAXPCH, which are
!           defined in the $ Local Parameters section of the header.
!
            if ( ( ichar ( buffer(i)(j:j) ) > maxpch ) .or. &
                 ( ichar ( buffer(i)(j:j) ) < minpch )      ) then

                  call ctx%setmsg ( 'A nonprinting character was'      // &
                                ' encountered in the comment'      // &
                                ' buffer. Value: #'                 )
                  call ctx%errint ( '#', ichar( buffer(i)(j:j) )        )
                  call ctx%sigerr ( 'SPICE(ILLEGALCHARACTER)'           )
                  return

            end if

         end do
!
!        Increment the number of characters by the length of the
!        significant portion of the current line in the buffer.
!
         nchars = nchars + length

      end do
!
!     We need to include the number of end of line markers in the
!     number of characters, so add the number of comment lines to
!     be added, N, to the number of characters, NCHARS. This is where
!     the contribution of any blank lines gets added to the character
!     count. We also need to have space for the end of comments marker.
!
      nchars = nchars + n + 1
!
!     Get the current number of comment records and comment characters
!     from the DAF file attached to HANDLE. We will also get back some
!     extra stuff that we do not use.
!
      call dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )

      if ( ctx%failed() ) return
!
!     Compute the number of comment records and the number of comment
!     characters. In order to perform these calculations, we assume
!     that we have a valid comment area in the DAF file attached to
!     HANDLE.
!
      ncomr = fward - 2

      if ( ncomr > 0 ) then
!
!        The starting record number is the number of comment records + 1
!        where the 1 skips the file record.
!
         empty  = .true.
         found  = .false.
         notusd = 0

         do while ( ( ncomr > 0 ) .and. ( .not. found ) .and. empty )

            recno = ncomr + 1

            read ( daflun, rec=recno, iostat=iostat ) crecrd

            if ( iostat /= 0 ) then

               call ctx%setmsg ( 'Error reading comment area of binary' // &
                             ' file named ''#''.  IOSTAT = #.'       )
               call ctx%errfnm ( '#', daflun                             )
               call ctx%errint ( '#', iostat                             )
               call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                 )
               return

            end if
!
!           Scan the comment record looking for the end of comments
!           marker.
!
            eocpos = cpos ( crecrd(1:mxcrec), eocmrk, 1 )

            if ( eocpos > 0 ) then

               found = .true.

            else

               nelpos = ncpos ( crecrd(1:mxcrec), eolmrk, 1 )

               if ( nelpos /= 0 ) then

                  empty = .false.

               else

                  ncomr  = ncomr  - 1
                  notusd = notusd + 1

               end if

            end if

         end do
!
!        If we do not find the end of comments marker and the comment
!        area is not empty, then it is an error.
!
         if ( ( .not. found ) .and. ( .not. empty ) ) then

            call ctx%setmsg ( 'The comment area in the DAF file'   // &
                          ' ''#'' may be damaged. The end of'  // &
                          ' the comments could not be found.'   )
            call ctx%errfnm ( '#', daflun                           )
            call ctx%sigerr ( 'SPICE(BADCOMMENTAREA)'               )
            return

         else if ( found ) then

            ncomc = mxcrec * ( ncomr - 1 ) + eocpos - 1

         else if ( empty ) then

            ncomc = 0

         end if

      else

         ncomc  = 0
         notusd = 0

      end if
!
!     Determine the amount of free space in the comment area. If
!     there are some comment records allocated, the space available
!     is the number of comment records allocated times the length of
!     a comment record, minus the number of comment characters already
!     used. Otherwise, the space available is zero.
!
      if ( ncomr + notusd > 0 ) then

         space = notusd * mxcrec + ncomr * mxcrec - ncomc

      else

         space = 0

      end if
!
!     Determine the number of new comment records which are necessary
!     to store all of the comments from the buffer.
!
      if ( nchars > space ) then
!
!        If there are more characters to store than available space
!        we need at least one new record.
!
         newrec = 1  +  (  ( nchars - space )  - 1  ) / mxcrec

      else
!
!        Otherwise, we do not need any new records.
!
         newrec = 0

      end if
!
!     Now add the necessary number of comment records to the file,
!     if we need to add any.
!
      if ( newrec > 0 ) then

         call dafarr ( ctx, handle, newrec )

         if ( ctx%failed() ) return

      end if
!
!     At this point, we know that we have enough space to write all of
!     the comments in BUFFER to the comment area. Either there was
!     enough space already there, or we calculated how many new comment
!     records were needed, and we added them to the file. So, now we
!     begin ``packing'' the comments into DAF comment records and
!     writing them to the file.
!
!     We begin initializing the appropriate variables.
!
      if ( ncomc == 0 ) then
!
!        If there are no comments in the comment area, then we need
!        to skip the file record. The first available comment record
!        is the record immediately after the file record, so we set
!        RECNO accordingly. We also initialize the current position in
!        the comment record, and the comment record itself.
!
         recno  = 2
         curpos = 1
         crecrd = ' '

      else
!
!        If there are comments in the comment area, then we need to
!        skip the file record and any comment records which have been
!        filled. The first comment record with space available is the
!        record immediately following the last completely filled
!        comment record. So calculate the number of comment records
!        in use, and set RECNO appropriately. Finally calculate the
!        initial position.
!
         rinuse = 1 + ncomc / mxcrec
         recno  = 1 + rinuse
         curpos = ncomc - mxcrec * ( rinuse - 1 ) + 1

      end if
!
!     Begin ``packing'' the comments from the input buffer into the
!     comment records, writing the comment records to the file as they
!     become filled.
!
      do i = 1, n
!
!        Get the length of the significant portion of comment line I.
!
         length = len_trim(buffer(i))
!
!        Process the comment line.
!
         do j = 1, length
!
!           If we have filled the comment record while processing
!           comment line BUFFER(I), write out the comment record,
!           increment the record number, RECNO, and reset the values
!           of the current position and the comment record.
!
            if ( curpos > mxcrec ) then

               write ( daflun, rec=recno, iostat=iostat ) crecrd
               flush ( daflun )
               call files_written ( ctx )

               if ( iostat /= 0 ) then

                  call ctx%setmsg ( 'Error writing to record # of the'  // &
                                ' binary file named ''#''. IOSTAT'  // &
                                ' = #.'                              )
                  call ctx%errint ( '#', recno                           )
                  call ctx%errfnm ( '#', daflun                          )
                  call ctx%errint ( '#', iostat                          )
                  call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'             )
                  return

               end if

               recno  = recno + 1
               curpos = 1
               crecrd = ' '

            end if

            crecrd(curpos:curpos) = buffer(i)(j:j)
            curpos                = curpos + 1

         end do
!
!        Check to see if we happened to exactly fill the comment record
!        when we finished processing comment line BUFFER(I). If we
!        did, CURPOS will be 1 greater than MXCREC, and we will need
!        to write the comment record to the file, increment the record
!        number, RECNO, and reset the values of the current position
!        and the comment record.
!
         if ( curpos > mxcrec ) then

               write ( daflun, rec=recno, iostat=iostat ) crecrd
               flush ( daflun )
               call files_written ( ctx )

               if ( iostat /= 0 ) then

                  call ctx%setmsg ( 'Error writing to record # of the'  // &
                                ' binary file named ''#''. IOSTAT'  // &
                                ' = #.'                              )
                  call ctx%errint ( '#', recno                           )
                  call ctx%errfnm ( '#', daflun                          )
                  call ctx%errint ( '#', iostat                          )
                  call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'             )
                  return

               end if

            recno  = recno + 1
            curpos = 1
            crecrd = ' '

         end if
!
!        Append the end-of-line marker to the comment line that we just
!        placed into the comment record.
!
         crecrd(curpos:curpos) = eolmrk
         curpos                = curpos + 1

      end do
!
!     We have now finished processing all of the comment lines in
!     BUFFER, so we need write the current record to the file. This
!     record will always contain something, so we always need to write
!     it.
!
      if ( curpos > mxcrec ) then
!
!        If we have completely filled the comment record, the last
!        character of the last line n the buffer coincides with the
!        last character in the comment record, then we need to write
!        the record and get set up to add the end of comments mark on
!        the next record.
!
         write ( daflun, rec=recno, iostat=iostat ) crecrd
         flush ( daflun )
         call files_written ( ctx )

         if ( iostat /= 0 ) then

            call ctx%setmsg ( 'Error writing to record # of the'    // &
                          ' binary file named ''#''. IOSTAT'    // &
                          ' = #.'                                )
            call ctx%errint ( '#', recno                             )
            call ctx%errfnm ( '#', daflun                            )
            call ctx%errint ( '#', iostat                            )
            call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'               )
            return

         end if

         recno  = recno + 1
         curpos = 1
         crecrd = ' '

      end if
!
!     Add the end of comments mark to the final comment record and
!     write it to the file.
!
      crecrd(curpos:curpos) = eocmrk

      write ( daflun, rec=recno, iostat=iostat ) crecrd
      flush ( daflun )
      call files_written ( ctx )

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'Error writing to record # of the'    // &
                       ' binary file named ''#''. IOSTAT'    // &
                       ' = #.'                                )
         call ctx%errint ( '#', recno                             )
         call ctx%errfnm ( '#', daflun                            )
         call ctx%errint ( '#', iostat                            )
         call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'               )
         return

      end if
!
!     Check out and leave DAFAC.
!
      return
      end subroutine dafac

      !> Delete the entire comment area of a previously opened binary
      !> DAF attached to HANDLE.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a binary DAF that is to have its entire
      !>              comment area deleted. The DAF must have been opened
      !>              with write access.
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
      !>     1)  If the binary DAF attached to HANDLE is not open with write
      !>         access, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE in $Detailed_Input.
      !>
      !>## Particulars
      !>
      !>     A binary DAF contains an area which is reserved for storing
      !>     annotations or descriptive textual information about the data
      !>     contained in a file. This area is referred to as the ``comment
      !>     area'' of the file. The comment area of a DAF is a line
      !>     oriented medium for storing textual information. The comment
      !>     area preserves any leading or embedded white space in the line(s)
      !>     of text which are stored, so that the appearance of the of
      !>     information will be unchanged when it is retrieved (extracted) at
      !>     some other time. Trailing blanks, however, are NOT preserved,
      !>     due to the way that character strings are represented in
      !>     standard Fortran 77.
      !>
      !>     This routine will delete the entire comment area from the binary
      !>     DAF attached to HANDLE. The size of the binary DAF will remain
      !>     unchanged. The space that was used by the comment records
      !>     is reclaimed.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Delete the entire comment area of a DAF file. Note that this
      !>        action should only be performed if fresh new comments are to
      !>        be placed within the DAF file.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           earthstns_itrf93_201023.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFDC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               RTRIM
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         KERNEL
      !>      PARAMETER           ( KERNEL =
      !>     .                         'earthstns_itrf93_201023.bsp' )
      !>
      !>      INTEGER               BUFFSZ
      !>      PARAMETER           ( BUFFSZ = 10 )
      !>
      !>      INTEGER               LINLEN
      !>      PARAMETER           ( LINLEN = 1000 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(LINLEN)    BUFFER ( BUFFSZ )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               N
      !>
      !>      LOGICAL               DONE
      !>
      !>C
      !>C     Open a DAF for write. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      CALL DAFOPW ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Print the first 10 lines of comments from the DAF file.
      !>C
      !>      WRITE(*,'(A)') 'Comment area of input DAF file '
      !>     .            // '(max. 10 lines): '
      !>      WRITE(*,'(A)') '---------------------------------------'
      !>     .            // '-----------------------'
      !>
      !>      CALL DAFEC  ( HANDLE, BUFFSZ, N, BUFFER, DONE )
      !>
      !>      DO I = 1, N
      !>
      !>         WRITE (*,*) BUFFER(I)(:RTRIM(BUFFER(I)))
      !>
      !>      END DO
      !>
      !>      WRITE(*,'(A)') '---------------------------------------'
      !>     .            // '-----------------------'
      !>      WRITE(*,*) ' '
      !>      WRITE(*,*) 'Deleting entire comment area...'
      !>
      !>C
      !>C     Delete all the comments from the DAF file.
      !>C
      !>      CALL DAFDC ( HANDLE )
      !>
      !>C
      !>C     Close the DAF file and re-open it for read
      !>C     access to work around the DAFEC restriction
      !>C     on comments not to be modified while they are
      !>C     being extracted.
      !>C
      !>      CALL DAFCLS( HANDLE  )
      !>
      !>      CALL DAFOPR( KERNEL, HANDLE  )
      !>
      !>C
      !>C     Check if the comments have indeed been deleted.
      !>C
      !>      CALL DAFEC  ( HANDLE, BUFFSZ, N, BUFFER, DONE )
      !>
      !>      IF ( DONE .AND. N .EQ. 0 ) THEN
      !>
      !>         WRITE(*,*) ' '
      !>         WRITE(*,*) '   Successful operation.'
      !>
      !>      ELSE
      !>
      !>         WRITE(*,*) ' '
      !>         WRITE(*,*) '   Operation failed.'
      !>
      !>      END IF
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Comment area of input DAF file (max. 10 lines):
      !>        --------------------------------------------------------------
      !>
      !>            SPK for DSN Station Locations
      !>            ========================================================***
      !>
      !>            Original file name:                   earthstns_itrf93_2***
      !>            Creation date:                        2020 October 28 12:30
      !>            Created by:                           Nat Bachman  (NAIF***
      !>
      !>            Introduction
      !>        --------------------------------------------------------------
      !>
      !>         Deleting entire comment area...
      !>
      !>            Successful operation.
      !>
      !>        Warning: incomplete output. 3 lines extended past the right
      !>        margin of the header and have been truncated. These lines are
      !>        marked by "***" at the end of each line.
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
      !>-    SPICELIB Version 1.1.0, 25-NOV-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.0.0, 23-SEP-1994 (KRG)
      module subroutine dafdc ( ctx, handle )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! The handle of a binary DAF opened for writing.

      character(ifnlen) :: ifname

      integer(ip) :: bward
      integer(ip) :: free
      integer(ip) :: fward
      integer(ip) :: ncomr
      integer(ip) :: nd
      integer(ip) :: ni

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     Verify that the DAF attached to HANDLE was opened with write
!     access.
!
      call dafsih ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return
!
!     Read the file record to obtain the current number of comment
!     records in the DAF attached to HANDLE. We will also get back some
!     extra stuff that we do not use.
!
      call dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )

      ncomr = fward - 2

      if ( ctx%failed() ) return
!
!     Now we will attempt to remove the comment records, if there are
!     any, otherwise we do nothing.
!
      if ( ncomr > 0 ) then
!
!        We have some comment records, so remove them.
!
         call dafrrr ( ctx, handle, ncomr )

         if ( ctx%failed() ) return

      end if
!
!     We're done now, so goodbye.
!
      return
      end subroutine dafdc

      !> Extract comments from the comment area of a binary DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the file handle of a binary DAF which has been opened
      !>              with read access.
      !>
      !>     BUFSIZ   is the maximum number of comments that may be placed into
      !>              BUFFER. This would typically be the declared array size
      !>              for the Fortran character string array passed into this
      !>              routine.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of comment lines extracted from the comment
      !>              area of the binary DAF attached to HANDLE. This number
      !>              will be <= BUFSIZ on output. If N = BUFSIZ and DONE <>
      !>              .TRUE., then there are more comments left to to extract.
      !>              If N = 0, then DONE = .TRUE., i.e., there were no
      !>              comments in the comment area or we have extracted all
      !>              of the comments. If there are comments in the comment
      !>              area, or comments remaining after the extraction process
      !>              has begun, N > 0, always.
      !>
      !>     BUFFER   is an array of at most BUFSIZ comments which have been
      !>              extracted from the comment area of the binary DAF
      !>              attached to HANDLE.
      !>
      !>     DONE     is a logical flag indicating whether or not all of the
      !>              comment lines from the comment area of the DAF have
      !>              been read. This variable has the value .TRUE. after the
      !>              last comment line has been read. It will have the value
      !>              .FALSE. otherwise.
      !>
      !>              If there are no comments in the comment area, this
      !>              variable will have the value .TRUE., and N = 0.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the size of the output line buffer is is not positive,
      !>         the error SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>     2)  If a comment line in a DAF is longer than the length
      !>         of a character string array element of BUFFER, the error
      !>         SPICE(COMMENTTOOLONG) is signaled.
      !>
      !>     3)  If the end of the comments cannot be found, i.e., the end of
      !>         comments marker is missing on the last comment record, the
      !>         error SPICE(BADCOMMENTAREA) is signaled.
      !>
      !>     4)  If the number of comment characters scanned exceeds the
      !>         number of comment characters computed, the error
      !>         SPICE(BADCOMMENTAREA) is signaled.
      !>
      !>     5)  If the binary DAF attached to HANDLE is not open for reading,
      !>         an error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See argument HANDLE in $Detailed_Input.
      !>
      !>## Particulars
      !>
      !>     A binary DAF contains an area which is reserved for storing
      !>     annotations or descriptive textual information describing the data
      !>     contained in a file. This area is referred to as the ``comment
      !>     area'' of the file. The comment area of a DAF is a line
      !>     oriented medium for storing textual information. The comment
      !>     area preserves any leading or embedded white space in the line(s)
      !>     of text which are stored, so that the appearance of the of
      !>     information will be unchanged when it is retrieved (extracted) at
      !>     some other time. Trailing blanks, however, are NOT preserved,
      !>     due to the way that character strings are represented in
      !>     standard Fortran 77.
      !>
      !>     This routine will read the comments from the comment area of
      !>     a binary DAF, placing them into a line buffer. If the line
      !>     buffer is not large enough to hold the entire comment area,
      !>     the portion read will be returned to the caller, and the DONE
      !>     flag will be set to .FALSE. This allows the comment area to be
      !>     read in ``chunks,'' a buffer at a time. After all of the comment
      !>     lines have been read, the DONE flag will be set to .TRUE.
      !>
      !>     This routine can be used to ``simultaneously'' extract comments
      !>     from the comment areas of multiple binary DAFs. See Example
      !>     2 in the $Examples section.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for these examples may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following example will extract a maximum of 30 lines
      !>        from the comment area of a binary DAF, displaying the
      !>        comments on the terminal screen.
      !>
      !>        Although it would be possible to read the 30 lines in one
      !>        go, for this example, use only a buffer size of 10,
      !>        demonstrating the use of the DONE logical flag.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           earthstns_itrf93_201023.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFEC_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               RTRIM
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         KERNEL
      !>      PARAMETER           ( KERNEL =
      !>     .                         'earthstns_itrf93_201023.bsp' )
      !>
      !>      INTEGER               BUFSIZ
      !>      PARAMETER           ( BUFSIZ = 10 )
      !>
      !>      INTEGER               LINLEN
      !>      PARAMETER           ( LINLEN = 1000 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(LINLEN)    BUFFER ( BUFSIZ )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               N
      !>
      !>      LOGICAL               DONE
      !>
      !>C
      !>C     Open a DAF for read. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      CALL DAFOPR ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Read a maximum of 30 lines of comments.
      !>C
      !>      WRITE(*,'(A)') 'Comment area of input DAF file '
      !>     .            // '(max. 30 lines): '
      !>      WRITE(*,'(A)') '---------------------------------------'
      !>     .            // '-----------------------'
      !>      DO I = 0, 2
      !>
      !>         CALL DAFEC  ( HANDLE, BUFSIZ, N, BUFFER, DONE )
      !>
      !>C
      !>C        Write the N lines to the terminal screen.
      !>C
      !>         DO J = 1, N
      !>
      !>            WRITE (*,*) BUFFER(J)(:RTRIM(BUFFER(J)))
      !>
      !>         END DO
      !>
      !>      END DO
      !>
      !>      WRITE(*,'(A)') '---------------------------------------'
      !>     .            // '-----------------------'
      !>
      !>C
      !>C     Have all the comments been read?
      !>C
      !>      IF ( .NOT. DONE ) THEN
      !>
      !>         WRITE(*,*) ' '
      !>         WRITE(*,*) 'Warning: Not all comments have been read!'
      !>
      !>      END IF
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Comment area of input DAF file (max. 30 lines):
      !>        --------------------------------------------------------------
      !>
      !>            SPK for DSN Station Locations
      !>            ========================================================***
      !>
      !>            Original file name:                   earthstns_itrf93_2***
      !>            Creation date:                        2020 October 28 12:30
      !>            Created by:                           Nat Bachman  (NAIF***
      !>
      !>            Introduction
      !>            ========================================================***
      !>
      !>            This file provides geocentric states---locations and vel***
      !>            set of DSN stations cited in the list below under "Posit***
      !>            position vectors point from the earth's barycenter to th***
      !>            velocities are estimates of the derivatives with respect***
      !>            vectors; in this file, velocities are constant. Station ***
      !>            magnitudes on the order of a few cm/year.
      !>
      !>            The states in this file are given relative to the terres***
      !>            frame ITRF93.
      !>
      !>            This SPK file has a companion file
      !>
      !>               earthstns_fx_201023.bsp
      !>
      !>            which differs from this one only in that it uses the ref***
      !>            frame alias 'EARTH_FIXED'. See the comment area of that ***
      !>            and the Frames Required Reading for details.
      !>
      !>        --------------------------------------------------------------
      !>
      !>         Warning: Not all comments have been read!
      !>
      !>        Warning: incomplete output. 12 lines extended past the right
      !>        margin of the header and have been truncated. These lines are
      !>        marked by "***" at the end of each line.
      !>
      !>     2) The following example demonstrates the use of this routine to
      !>        simultaneously read the comment areas of multiple DAFs. For
      !>        each file, the comments will be displayed on the screen as
      !>        they are extracted.
      !>
      !>        Use the SPK kernel below as the first DAF file for the
      !>        program.
      !>
      !>           earthstns_itrf93_201023.bsp
      !>
      !>        Use the CK kernel below as the second DAF file for the
      !>        program.
      !>
      !>           vo2_swu_ck2.bc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFEC_EX2
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               RTRIM
      !>
      !>      LOGICAL               ALLTRU
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      INTEGER               BUFSIZ
      !>      PARAMETER           ( BUFSIZ = 10   )
      !>
      !>      INTEGER               FNAMLN
      !>      PARAMETER           ( FNAMLN = 255  )
      !>
      !>      INTEGER               LINLEN
      !>      PARAMETER           ( LINLEN = 1000 )
      !>
      !>      INTEGER               NUMFIL
      !>      PARAMETER           ( NUMFIL = 2    )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(FNAMLN)    DAFNAM ( NUMFIL )
      !>      CHARACTER*(LINLEN)    BUFFER ( BUFSIZ )
      !>
      !>      INTEGER               HANDLE ( NUMFIL )
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               N
      !>
      !>      LOGICAL               DONE   ( NUMFIL )
      !>
      !>C
      !>C     Set the DAF file names.
      !>C
      !>      DATA                  DAFNAM /
      !>     .                          'earthstns_itrf93_201023.bsp',
      !>     .                          'vo2_swu_ck2.bc'          /
      !>
      !>C
      !>C     Set the initial values for DONE and HANDLE.
      !>C
      !>      DO I = 1, NUMFIL
      !>
      !>         DONE(I)   = .FALSE.
      !>         HANDLE(I) = 0
      !>
      !>      END DO
      !>
      !>C
      !>C     Open the DAFs.
      !>C
      !>      DO I = 1, NUMFIL
      !>
      !>         CALL DAFOPR ( DAFNAM(I), HANDLE(I) )
      !>
      !>      END DO
      !>
      !>C
      !>C     While there are still some comments left to read in at
      !>C     least one of the files, read them and display them.
      !>C
      !>      DO WHILE ( .NOT. ALLTRU( DONE, NUMFIL ) )
      !>
      !>         DO I = 1, NUMFIL
      !>
      !>            IF ( .NOT. DONE(I) ) THEN
      !>
      !>               WRITE (*,*)
      !>               WRITE (*,*) 'File: ',
      !>     .                     DAFNAM(I)(:RTRIM(DAFNAM(I)))
      !>               WRITE (*,*) '---------------------------------'
      !>     .                 //  '-----------------------------'
      !>               N = 0
      !>
      !>               CALL DAFEC ( HANDLE(I), BUFSIZ, N,
      !>     .                      BUFFER,    DONE(I)   )
      !>
      !>               DO J = 1, N
      !>
      !>                  WRITE (*,*) BUFFER(J)(:RTRIM(BUFFER(J)))
      !>
      !>               END DO
      !>
      !>            END IF
      !>
      !>         END DO
      !>
      !>      END DO
      !>
      !>C
      !>C     Safely close the DAF files.
      !>C
      !>      DO I = 1, NUMFIL
      !>
      !>         CALL DAFCLS ( HANDLE(I) )
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         File: earthstns_itrf93_201023.bsp
      !>         --------------------------------------------------------------
      !>
      !>            SPK for DSN Station Locations
      !>            ========================================================***
      !>
      !>            Original file name:                   earthstns_itrf93_2***
      !>            Creation date:                        2020 October 28 12:30
      !>            Created by:                           Nat Bachman  (NAIF***
      !>
      !>            Introduction
      !>
      !>         File: vo2_swu_ck2.bc
      !>         --------------------------------------------------------------
      !>         \beginlabel
      !>         PDS_VERSION_ID               = PDS3
      !>         RECORD_TYPE                  = FIXED_LENGTH
      !>         RECORD_BYTES                 = 1024
      !>         ^SPICE_KERNEL                = "vo2_swu_ck2.bc"
      !>         MISSION_NAME                 = VIKING
      !>         SPACECRAFT_NAME              = "VIKING ORBITER 2"
      !>         DATA_SET_ID                  = "VO1/VO2-M-SPICE-6-V1.0"
      !>         KERNEL_TYPE_ID               = CK
      !>         PRODUCT_ID                   = "vo2_swu_ck2.bc"
      !>
      !>         File: earthstns_itrf93_201023.bsp
      !>         --------------------------------------------------------------
      !>            ========================================================***
      !>
      !>            This file provides geocentric states---locations and vel***
      !>            set of DSN stations cited in the list below under "Posit***
      !>            position vectors point from the earth's barycenter to th***
      !>            velocities are estimates of the derivatives with respect***
      !>            vectors; in this file, velocities are constant. Station ***
      !>            magnitudes on the order of a few cm/year.
      !>
      !>            The states in this file are given relative to the terres***
      !>
      !>         File: vo2_swu_ck2.bc
      !>         --------------------------------------------------------------
      !>         PRODUCT_CREATION_TIME        = 2008-12-03T14:03:35
      !>         PRODUCER_ID                  = "NAIF/JPL"
      !>         MISSION_PHASE_NAME           = {
      !>                                        PRIMARY_MISSION,
      !>                                        EXTENDED_MISSION,
      !>                                        CONTINUATION_MISSION
      !>                                        }
      !>         PRODUCT_VERSION_TYPE         = ACTUAL
      !>         PLATFORM_OR_MOUNTING_NAME    = "SPACECRAFT BUS"
      !>         START_TIME                   = 1977-01-09T18:33:12.707
      !>
      !>         File: earthstns_itrf93_201023.bsp
      !>         --------------------------------------------------------------
      !>            frame ITRF93.
      !>
      !>            This SPK file has a companion file
      !>
      !>               earthstns_fx_201023.bsp
      !>
      !>            which differs from this one only in that it uses the ref***
      !>            frame alias 'EARTH_FIXED'. See the comment area of that ***
      !>            and the Frames Required Reading for details.
      !>
      !>         File: vo2_swu_ck2.bc
      !>         --------------------------------------------------------------
      !>         STOP_TIME                    = 1977-11-27T05:55:16.772
      !>         SPACECRAFT_CLOCK_START_COUNT = "1/0032380393.707"
      !>         SPACECRAFT_CLOCK_STOP_COUNT  = "1/0060155717.772"
      !>         TARGET_NAME                  = MARS
      !>         INSTRUMENT_NAME              = "SCAN PLATFORM"
      !>         NAIF_INSTRUMENT_ID           = -30000
      !>         SOURCE_PRODUCT_ID            = "N/A"
      !>         NOTE                         = "See comments in the file fo***
      !>         OBJECT                       = SPICE_KERNEL
      !>           INTERCHANGE_FORMAT         = BINARY
      !>
      !>         File: earthstns_itrf93_201023.bsp
      !>         --------------------------------------------------------------
      !>
      !>            Revision description
      !>            --------------------
      !>
      !>            This kernel contains data from a single, current source:***
      !>
      !>            This kernel supersedes the kernels
      !>
      !>               earthstns_itrf93_050714.bsp
      !>               dss_53_prelim_itrf93_201018.bsp (data in this kernel ***
      !>
      !>         File: vo2_swu_ck2.bc
      !>         --------------------------------------------------------------
      !>           KERNEL_TYPE                = POINTING
      !>           DESCRIPTION                = "VO2 instrument platform ori***
      !>         reconstructed during cartographic image registration by S. ***
      !>         Type 2 CK file supersedes the less usable Type 1 CK vo2_swu***
      !>         END_OBJECT                   = SPICE_KERNEL
      !>         \endlabel
      !>
      !>        [...]
      !>
      !>        Warning: incomplete output. Only 100 out of 1049 lines have
      !>        been provided. 18 lines extended past the right margin of the
      !>        header and have been truncated. These lines are marked by "***"
      !>        at the end of each line.
      !>
      !>## Restrictions
      !>
      !>     1)  The comment area may consist only of printing ASCII
      !>         characters, decimal values 32 - 126.
      !>
      !>     2)  There is NO maximum length imposed on the significant portion
      !>         of a text line that may be placed into the comment area of a
      !>         DAF. The maximum length of a line stored in the comment area
      !>         should be kept reasonable, so that they may be easily
      !>         extracted. A good value for this would be 1000 characters, as
      !>         this can easily accommodate ``screen width'' lines as well as
      !>         long lines which may contain some other form of information.
      !>
      !>     3)  This routine is only used to read records on environments
      !>         whose characters are a single byte in size. Updates
      !>         to this routine and routines in its call tree may be
      !>         required to properly handle other cases.
      !>
      !>     4)  This routine is intended to be used on DAF files whose comment
      !>         area does not change while this routine is called to extract
      !>         comments, between the start and end of the extraction process.
      !>         If the comment area does change (gets updated, reduced,
      !>         extended, or deleted) between calls to this routine on the
      !>         same DAF file, the routine's outputs are undefined and
      !>         subsequent calls to it are likely to trigger an exception.
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
      !>     B.V. Semenov       (JPL)
      !>     F.S. Turner        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 25-NOV-2021 (JDR) (BVS)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>        Added complete code examples from existing code fragments.
      !>
      !>-    SPICELIB Version 1.1.0, 12-APR-2012 (BVS)
      !>
      !>        Increased FTSIZE (from 1000 to 5000).
      !>
      !>-    SPICELIB Version 1.0.0, 08-NOV-2006 (NJB) (KRG) (FST)
      !>
      !>        Based on Support Version 2.0.0, 16-NOV-2001 (FST)
      module subroutine dafec ( ctx, handle, bufsiz, n, buffer, done )
      use pepper_containers, only: grow
      use pepper_kinds, only: ip
      use pepper_strings, only: cpos, ncpos
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of binary DAF opened with read access.
      integer(ip), intent(in) :: bufsiz  !! Maximum size, in lines, of BUFFER.
      integer(ip), intent(out) :: n  !! Number of extracted comment lines.
      character(*), intent(out) :: buffer(*)  !! Buffer where extracted comment lines are placed.
      logical, intent(out) :: done  !! Indicates whether all comments have been extracted.
      character(1) :: ch
      character(ifnlen) :: ifname
      integer(ip) :: curpos
      integer(ip) :: daflun
      integer(ip) :: eocpos
      integer(ip) :: i
      integer(ip) :: index
      integer(ip) :: j
      integer(ip) :: k
      integer(ip) :: linlen
      integer(ip) :: nchars
      integer(ip) :: ncomc
      integer(ip) :: ncomr
      integer(ip) :: nelpos
      integer(ip) :: numcom
      integer(ip) :: recno
      integer(ip) :: iostat
      integer(ip) :: bward
      integer(ip) :: fward
      integer(ip) :: free
      integer(ip) :: nd
      integer(ip) :: ni
      integer(ip) :: raw_slot
      logical :: empty
      logical :: eol
      logical :: found
      if ( .not. allocated ( ctx%dafec_state ) ) then
         allocate ( ctx%dafec_state )
         call init_dafec_state ( ctx%dafec_state )
      end if
!
!     PEPPER: make room in the table of files for one more (before the
!     ASSOCIATE block: the arrays may be reallocated).
!
      call grow ( ctx%dafec_state%filchr, ctx%dafec_state%nfiles + 1 )
      call grow ( ctx%dafec_state%filcnt, ctx%dafec_state%nfiles + 1 )
      call grow ( ctx%dafec_state%filhan, ctx%dafec_state%nfiles + 1 )
      call grow ( ctx%dafec_state%lstpos, ctx%dafec_state%nfiles + 1 )
      call grow ( ctx%dafec_state%lstrec, ctx%dafec_state%nfiles + 1 )

      associate ( crecrd => ctx%dafec_state%crecrd, &
                  eocmrk => ctx%dafec_state%eocmrk, &
                  eolmrk => ctx%dafec_state%eolmrk, &
                  filchr => ctx%dafec_state%filchr, &
                  filcnt => ctx%dafec_state%filcnt, &
                  filhan => ctx%dafec_state%filhan, &
                  lsthan => ctx%dafec_state%lsthan, &
                  lstpos => ctx%dafec_state%lstpos, &
                  lstrec => ctx%dafec_state%lstrec, &
                  nfiles => ctx%dafec_state%nfiles, &
                  first => ctx%dafec_state%first )
      if ( ctx%failed() ) return
!
!     If this is the first time that this routine has been called,
!     we need to initialize the character value of the end-of-line
!     marker, and the file table variables.
!
      if ( first ) then

         first  = .false.
         nfiles = 0
         lsthan = 0
         eocmrk = char ( inteoc )
         eolmrk = char ( inteol )

      end if
!
!     Verify that the DAF attached to HANDLE is opened for reading
!     by calling the routine to signal an invalid access mode on a
!     handle.
!
      call dafsih ( ctx, handle, 'READ' )

      if ( ctx%failed() ) return
!
!     Check for a nonpositive BUFFER size.
!
      if ( bufsiz <= 0 ) then

         call ctx%setmsg ( 'The output buffer size was not positive: #.' )
         call ctx%errint ( '#', bufsiz                                   )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)'                      )
         return

      end if
!
!     Convert the DAF handle to its corresponding Fortran logical
!     unit number for reading the comment records (unless in memory).
!
!
!     PEPPER: a DAF built in memory has no comments.
!
      if ( daf_synthetic_slot ( ctx, handle ) > 0 ) then
         n    = 0
         done = .true.
         return
      end if

      raw_slot = daf_raw_slot ( ctx, handle )
      if ( raw_slot == 0 ) then
         call zzddhhlu ( ctx, handle, 'DAF', .false., daflun )
         if ( ctx%failed() ) return
      end if
!
!     Get the length of a single character string in the buffer.
!
      linlen = len ( buffer(1) )
!
!     If we have extracted comments from at least one file and we
!     didn't finish, check to see if HANDLE is in the file table.
!
      if ( nfiles > 0 ) then

         index = findloc(filhan(1:nfiles), handle, dim=1)

      else

         index = 0

      end if
!
!     Check to see if we found HANDLE in the file handle table. If
!     we did, INDEX will be > 0.
!
      if ( index > 0 ) then
!
!        Set the record number and the starting position accordingly,
!        i.e., where we left off when we last read from that file.
!
         recno  = lstrec(index)
         curpos = lstpos(index)
         nchars = filchr(index)
         ncomc  = filcnt(index)

      else
!
!        We have not yet read any comments from this file, so start at
!        the start. To get to the first comment record, we need to skip
!        the file record. We also need to count the number of comment
!        characters.
!
!        Read the file record from the DAF attached to HANDLE. We will
!        get back some stuff that we do not use.
!
         call dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )

         if ( ctx%failed() ) return
!
!        Compute the number of comment records and the number of
!        comment characters. In order to perform these calculations,
!        we assume that we have a valid comment area in the DAF
!        attached to HANDLE.
!
         ncomr = fward - 2

         if ( ncomr > 0 ) then
!
!           The starting record number is the number of comment records
!           + 1 where the 1 skips the file record.
!
            empty  = .true.
            found  = .false.

            do while (    ( ncomr > 0 ) .and. ( .not. found ) &
                      .and. empty                                )

               recno = ncomr + 1

               if ( raw_slot > 0 ) then
                  if ( recno >= 1 .and. recno <= size(ctx%spice%zzddhman%files(raw_slot)%raw_records) ) then
                     crecrd = ctx%spice%zzddhman%files(raw_slot)%raw_records(recno)(1:len(crecrd))
                     iostat = 0
                  else
                     iostat = -1
                  end if
               else
                  read ( daflun, rec=recno, iostat=iostat ) crecrd
               end if

               if ( iostat /= 0 ) then

                  call ctx%setmsg ( 'Error reading comment area of' // &
                                ' binary file named ''#''.'     // &
                                ' IOSTAT = #.'                   )
                  if ( raw_slot > 0 ) then
                     call ctx%errhan ( '#', handle                      )
                  else
                     call ctx%errfnm ( '#', daflun                      )
                  end if
                  call ctx%errint ( '#', iostat                      )
                  call ctx%sigerr ( 'SPICE(FILEREADFAILED)'          )
                  return

               end if
!
!              Scan the comment record looking for the end of comments
!              marker.
!
               eocpos = cpos ( crecrd(1:mxcrec), eocmrk, 1 )

               if ( eocpos > 0 ) then

                  found = .true.

               else

                  nelpos = ncpos ( crecrd(1:mxcrec), eolmrk, 1 )

                  if ( nelpos /= 0 ) then

                     empty = .false.

                  else

                     ncomr  = ncomr  - 1

                  end if

               end if

            end do
!
!           If we do not find the end of comments marker and the
!           comment area is not empty, then it is an error.
!
            if ( ( .not. found ) .and. ( .not. empty ) ) then

               call ctx%setmsg ( 'The comment area in the DAF file'   // &
                             ' ''#'' may be damaged. The end of'  // &
                             ' the comments could not be found.'   )
               if ( raw_slot > 0 ) then
                  call ctx%errhan ( '#', handle                           )
               else
                  call ctx%errfnm ( '#', daflun                           )
               end if
               call ctx%sigerr ( 'SPICE(BADCOMMENTAREA)'               )
               return

            else if ( found ) then

               ncomc = mxcrec * ( ncomr - 1 ) + eocpos - 1

            else if ( empty ) then

               ncomc = 0

            end if

         else

            ncomc = 0

         end if
!
!        If the number of comment characters, NCOMC, is equal to zero,
!        then we have no comments to read, so set the number of comments
!        to zero, set DONE to .TRUE., check out,  and return.
!
         if ( ncomc == 0 ) then

            n    = 0
            done = .true.
            return

         end if
!
!        Otherwise, set the initial position  in the comment area.
!
         recno  = 2
         curpos = 1
         nchars = 0

      end if
!
!     Begin reading the comment area into the buffer.
!
      if ( handle /= lsthan ) then
!
!        If the current DAF handle is not the same as the handle on
!        the last call, then we need to read in the appropriate record
!        from the DAF comment area. Otherwise the record was saved and
!        so we don't need to read it in.
!
         if ( raw_slot > 0 ) then
            if ( recno >= 1 .and. recno <= size(ctx%spice%zzddhman%files(raw_slot)%raw_records) ) then
               crecrd = ctx%spice%zzddhman%files(raw_slot)%raw_records(recno)(1:len(crecrd))
               iostat = 0
            else
               iostat = -1
            end if
         else
            read ( daflun, rec=recno, iostat=iostat ) crecrd
         end if

         if ( iostat /= 0 ) then

            call ctx%setmsg ( 'Error reading comment area of binary' // &
                          ' file named FILE.  IOSTAT = *.'        )
            call ctx%errint ( '*', iostat                             )
            if ( raw_slot > 0 ) then
               call ctx%errhan ( 'FILE', handle                          )
            else
               call ctx%errfnm ( 'FILE', daflun                          )
            end if
            call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                 )
            return

         end if

      end if
!
!     Initialize the BUFFER line counter, I, and the line position
!     counter, J.
!
      i = 1
      j = 1
!
!     Start filling up the BUFFER.
!
      numcom = 0
      done   = .false.

      do while ( ( i <= bufsiz ) .and. ( .not. done ) )

         eol = .false.
         do while ( .not. eol )

            nchars = nchars + 1
            ch     = crecrd(curpos:curpos)

            if ( ichar( ch ) == inteol ) then

               eol           = .true.
               if ( j <= linlen ) then

                  buffer(i)(j:) = ' '

               end if

            else

               if ( j <= linlen ) then

                  buffer(i)(j:j) = ch
                  j              = j + 1

               else

                  call ctx%setmsg ( 'The output buffer line length (#)'  // &
                                ' was not long enough to contain'    // &
                                ' comment line #.'                    )
                  call ctx%errint ( '#', linlen                           )
                  call ctx%errint ( '#', i                                )
                  call ctx%sigerr ( 'SPICE(COMMENTTOOLONG)'               )
                  return

               end if

            end if
!
!           If we have reached the end of the current comment record,
!           read in the next one and reset the current position.
!           Otherwise, just increment the current position.
!
            if ( curpos == mxcrec ) then

               recno = recno + 1
               if ( raw_slot > 0 ) then
                  if ( recno >= 1 .and. recno <= size(ctx%spice%zzddhman%files(raw_slot)%raw_records) ) then
                     crecrd = ctx%spice%zzddhman%files(raw_slot)%raw_records(recno)(1:len(crecrd))
                     iostat = 0
                  else
                     iostat = -1
                  end if
               else
                  read ( daflun, rec=recno, iostat=iostat ) crecrd
               end if

               if ( iostat /= 0 ) then

                  call ctx%setmsg ( 'Error reading comment area of'  // &
                                ' binary file named #.'          // &
                                '  IOSTAT = #.'                   )
                  if ( raw_slot > 0 ) then
                     call ctx%errhan ( '#', handle                       )
                  else
                     call ctx%errfnm ( '#', daflun                       )
                  end if
                  call ctx%errint ( '#', iostat                       )
                  call ctx%sigerr ( 'SPICE(FILEREADFAILED)'           )
                  return

               end if

               curpos = 1

            else

               curpos = curpos + 1

            end if
!
!           Check to make sure that it is safe to continue, i.e.,
!           that the number of comment characters we have processed
!           has not exceeded the number of comment characters in the
!           comment area of the DAF file. This should never happen.
!
            if ( nchars > ncomc ) then

               call ctx%setmsg ( 'Count of comment characters (#)'     // &
                             ' exceeds the number of comment'      // &
                             ' characters (#) in the DAF file #.'   )
               call ctx%errint ( '#',nchars                             )
               call ctx%errint ( '#',ncomc                              )
               call ctx%errfnm ( '#',daflun                             )
               call ctx%sigerr ( 'SPICE(BADCOMMENTAREA)'                )
               return

            end if

         end do
!
!        We have just completed a comment line, so we save the comment
!        number, increment the buffer line counter, I, and reset the
!        buffer line position counter, J.
!
         numcom = i
         i      = i + 1
         j      = 1
!
!        Check for the end of the comments.
!
         if ( nchars == ncomc ) then
!
!           If we have reached the end of the comments, signaled
!           by having processed all of the comment characters, NCOMC,
!           then we are done. So, set DONE to .TRUE. and remove the
!           entry for this file from the file table.
!
            done = .true.
            lsthan = 0
!
!           0 <= INDEX <= NFILES, and we only want to remove things
!           from the file table if:
!
!              The file we are currently reading from is in the
!              file table, INDEX > 0, which implies NFILES > 0.
!
!           So, if INDEX > 0, we know that there are files in the file
!           table, and that we are currently reading from one of them.
!
            if ( index > 0 ) then

               do k = index, nfiles - 1

                  filchr(k) = filchr(k+1)
                  filcnt(k) = filcnt(k+1)
                  filhan(k) = filhan(k+1)
                  lstrec(k) = lstrec(k+1)
                  lstpos(k) = lstpos(k+1)

               end do

               nfiles = nfiles - 1

            end if

         end if

      end do
!
!     Set the number of comment lines in the buffer
!
      n = numcom
!
!     At this point, we have either filled the buffer or we have
!     finished reading in the comment area. Find out what has
!     happened and act accordingly.
!
      if ( .not. done ) then
!
!        If we are not done, then we have filled the buffer, so save
!        everything that needs to be saved in the file table before
!        exiting.
!
         if ( index == 0 ) then
!
!           This was the first time that the comment area of this file
!           has been read, so add it to the file table and save all of
!           its information if there is room in the file table.
!
            ! PEPPER: the table of files grows (SPICELIB signaled an error
            ! here when it had FTSIZE files).

            nfiles         = nfiles + 1
            filchr(nfiles) = nchars
            filcnt(nfiles) = ncomc
            filhan(nfiles) = handle
            lstrec(nfiles) = recno
            lstpos(nfiles) = curpos
            lsthan         = handle

         else
!
!           The comment area of this file is already in the file table,
!           so just update its information.
!
            filchr(index) = nchars
            lstrec(index) = recno
            lstpos(index) = curpos
            lsthan        = handle

         end if

      end if

      return
      end associate
      end subroutine dafec

      !> All the comment lines of a DAF, each of any length (PEPPER; [[dafec]]
      !> without a buffer). It reads the comment area directly: it does not
      !> change the position of [[dafec]] in the file.
      !>
      !> The errors are those of [[dafec]], without SPICE(COMMENTTOOLONG).
      module subroutine daf_comments ( ctx, handle, lines )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! the handle of a DAF open for reading
      type(string), allocatable, intent(out) :: lines(:)  !! the comment lines (size 0 after an error)
      integer(ip), parameter :: inteoc = 4  ! end of the comments
      integer(ip), parameter :: mxcrec = 1000  ! comment characters in a record
      character(60) :: ifname
      character(mxcrec) :: crecrd
      character(:), allocatable :: text
      integer(ip) :: bward
      integer(ip) :: daflun
      integer(ip) :: eoc
      integer(ip) :: free
      integer(ip) :: fward
      integer(ip) :: iostat
      integer(ip) :: k
      integer(ip) :: ncomr
      integer(ip) :: nd
      integer(ip) :: ni
      integer(ip) :: raw_slot
      allocate ( lines(0) )
      if ( ctx%failed() ) return
      call dafsih ( ctx, handle, 'READ' )
      if ( ctx%failed() ) return
      ! PEPPER: a DAF built in memory has no comments
      if ( daf_synthetic_slot ( ctx, handle ) > 0 ) return
      raw_slot = daf_raw_slot ( ctx, handle )
      if ( raw_slot == 0 ) then
         call zzddhhlu ( ctx, handle, 'DAF', .false., daflun )
      end if
      call dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )
      if ( ctx%failed() ) return
!
!     The comment area: records 2 to FWARD-1, its end marked by EOT.
!
      ncomr = fward - 2
      if ( ncomr < 1 ) return
      allocate ( character(mxcrec*ncomr) :: text )
      do k = 1, ncomr
         if ( raw_slot > 0 ) then
            if ( k+1 <= size(ctx%spice%zzddhman%files(raw_slot)%raw_records) ) then
               crecrd = ctx%spice%zzddhman%files(raw_slot)%raw_records(k+1)(1:mxcrec)
               iostat = 0
            else
               iostat = -1
            end if
         else
            read ( daflun, rec=k+1, iostat=iostat ) crecrd
         end if
         if ( iostat /= 0 ) then
            call ctx%setmsg ( 'Error reading comment area of' // &
                          ' binary file named ''#''.'     // &
                          ' IOSTAT = #.'                   )
            if ( raw_slot > 0 ) then
               call ctx%errhan ( '#', handle                      )
            else
               call ctx%errfnm ( '#', daflun                      )
            end if
            call ctx%errint ( '#', iostat                      )
            call ctx%sigerr ( 'SPICE(FILEREADFAILED)'          )
            return
         end if
         text((k-1)*mxcrec+1:k*mxcrec) = crecrd
      end do
      eoc = index ( text, char(inteoc) )
      if ( eoc == 0 ) then
         if ( verify ( text, char(0) ) /= 0 ) then
            call ctx%setmsg ( 'The comment area in the DAF file'   // &
                          ' ''#'' may be damaged. The end of'  // &
                          ' the comments could not be found.'   )
            if ( raw_slot > 0 ) then
               call ctx%errhan ( '#', handle                           )
            else
               call ctx%errfnm ( '#', daflun                           )
            end if
            call ctx%sigerr ( 'SPICE(BADCOMMENTAREA)'               )
         end if
         return
      end if
      call split_comment_lines ( text(1:eoc-1), lines )
      end subroutine daf_comments

      !> Split a comment area's text into lines at its end-of-line marks
      !> (NUL) (PEPPER; used by [[daf_comments]] and [[das_comments]]). Text
      !> after the last mark is a line too.
      pure module subroutine split_comment_lines ( text, lines )
      use pepper_kinds, only: ip
      implicit none
      character(*), intent(in) :: text  !! the comment text
      type(string), allocatable, intent(out) :: lines(:)  !! the lines
      integer(ip) :: b
      integer(ip) :: e
      integer(ip) :: k
      integer(ip) :: n
      n = 0
      do k = 1, len ( text )
         if ( text(k:k) == char(0) ) n = n + 1
      end do
      if ( len ( text ) > 0 ) then
         if ( text(len(text):len(text)) /= char(0) ) n = n + 1
      end if
      allocate ( lines(n) )
      b = 1
      do k = 1, n
         e = index ( text(b:), char(0) )
         if ( e == 0 ) then
            lines(k)%s = text(b:)
         else
            lines(k)%s = text(b:b+e-2)
            b = b + e
         end if
      end do
      end subroutine split_comment_lines

      !> Begin a new array in a DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF opened for write access
      !>              by a previous call to DAFOPW or DAFOPN.
      !>
      !>     SUM      is the summary of a new array to be added to the
      !>              specified file. The addresses (the final two integer
      !>              components) need not be filled in.
      !>
      !>     NAME     is the name of the new array.
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
      !>     1)  If the input handle is not that of a DAF that is open for
      !>         writing, an error is signaled by a routine in the call tree of
      !>         this routine. These files are implicitly of the native binary
      !>         file format.
      !>
      !>     2)  If the input array name is too long to fit in the number
      !>         of characters allowed by the summary format of the DAF
      !>         designated by HANDLE, the excess characters are truncated.
      !>         No error is signaled.
      !>
      !>     3)  If there is not enough room in the state table to add
      !>         the DAF associated with HANDLE, the error SPICE(STFULL)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE, above.
      !>
      !>## Particulars
      !>
      !>     Only one array can be added to a DAF at any one time, so
      !>     calling DAFBNA cancels any addition to the file specified
      !>     by HANDLE that may be in progress. No warning is issued.
      !>
      !>## Examples
      !>
      !>     See $Examples in DAFANA.
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
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
      !>
      !>        Updated DAFBNA to support changes made to the DAF
      !>        system that utilize the new handle manager. See
      !>        the $Revisions section of DAFANA for a detailed
      !>        discussion of the changes.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Modified to support simultaneous writes to multiple DAFs.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Modified to support simultaneous writes to multiple DAFs.
      !>        DAFBNA now adds information about DAFs to the state table,
      !>        deletes information about closed DAFs from the state table,
      !>        and initializes the state pool.
      module subroutine dafbna ( ctx, handle, sum, name )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF.
      real(wp), intent(in) :: sum      ( * )  !! Summary of new array.
      character(*), intent(in) :: name  !! Name of new array.
      integer(ip) :: sumsiz
      integer(ip) :: p
      integer(ip) :: i
      integer(ip) :: prev
      integer(ip) :: bward
      integer(ip) :: nd
      integer(ip) :: free
      character(ifnlen) :: ifname
      logical :: found
      integer(ip) :: ni
      integer(ip) :: fward
      integer(ip) :: nextp
      if ( .not. allocated ( ctx%dafana ) ) then
         allocate ( ctx%dafana )
         call init_dafana_state ( ctx%dafana )
      end if
      call dafana_room ( ctx )
      associate ( stfh => ctx%dafana%stfh, &
                  stifnm => ctx%dafana%stifnm, &
                  staddg => ctx%dafana%staddg, &
                  stfrst => ctx%dafana%stfrst, &
                  stlast => ctx%dafana%stlast, &
                  stfree => ctx%dafana%stfree, &
                  stbegn => ctx%dafana%stbegn, &
                  stlsum => ctx%dafana%stlsum, &
                  stname => ctx%dafana%stname, &
                  stpool => ctx%dafana%stpool, &
                  sthead => ctx%dafana%sthead, &
                  stfptr => ctx%dafana%stfptr )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check out the file handle before going any further.
!
      call dafsih ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Initialize the state table pool, if this hasn't been done yet.
!     Also initialize the cell used to obtain the set of handles of
!     open DAFs.
!

!
!     We know that the beginning of the array will be the first
!     free address in the file. We also need the summary format.
!     Get both items from the file record.
!
!     We won't use the information we're obtaining now until
!     after we've placed the state information for the current
!     DAF at the head of the active list, but we want to make sure
!     that we can actually read the file record first.  So, we
!     do the read now and avoid modifying the active list if the
!     read fails.
!
      call dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )

!
!     If we couldn't read the file record, bail out now.
!
      if ( ctx%failed() ) return

!
!     See whether we already have an entry for this DAF in the
!     state table.  Find the previous node if possible.
!
      p      =   sthead
      prev   =   nil
      found  =  .false.

      do while (  ( p /= nil )  .and.  ( .not. found )  )

         if ( stfh(p) == handle ) then
            found  =  .true.
         else
            prev   =  p
            p      =  stpool( p )
         end if

      end do

!
!     At this point, either FOUND is false, or P points to a
!     state table entry describing the DAF indicated by HANDLE.
!     In the latter case, PREV is the predecessor of P.
!
      if ( found ) then
!
!        We already have a dossier on this DAF.  We already have
!        the information on the summary format, but we must re-set
!        the rest of our state information.
!
!        Rather than doing the update here, we do it outside of this
!        IF block.  That way, the update gets done in just one place.
!        This just makes life easier:  if the collection of state
!        variables is changed, there are fewer places to forget to
!        make the required code changes.
!
!        Move the node for this DAF to the head of the active list,
!        if it is not already there:
!
!           - Make the predecessor of P point to the successor of P.
!
!           - Make P point to the head of the active list.
!
!           - Make P the active list head node.
!
         if ( p /= sthead ) then
!
!           P is in the active list, but is not at the head.  So,
!           the predecessor of P is not NIL.
!
            stpool(prev)  =  stpool(p)
            stpool(p)     =  sthead
            sthead        =  p

         end if

      else
!
!        We don't yet have any information on this DAF.  Make a new
!        state table entry for the DAF.  We may need to make room for
!        the new information by freeing space allocated to DAFs that
!        are no longer open.
!
         if ( stfptr == nil ) then
!
!           Oops, we're out of space.  Time for garbage collection.
!           Test each file handle to see whether it designates a DAF
!           that is still open.  DAFHOF will tell us which handles
!           point to open DAFs.
!

            p     =  sthead
            prev  =  nil
!
!           For every DAF file represented in the state table, we'll
!           delete the corresponding state information if the DAF is
!           now closed.  We traverse the active list, examining each
!           file handle as we go.
!

            do while ( p /= nil )

               if (  ctx%spice%dafah%find ( stfh(p) ) > 0  )  then
!
!                 The file is open. Have a look at the next node.
!
                  prev = p
                  p    = stpool( p )

               else
!
!                 This file handle is not on the list, so free the
!                 node pointing to the information about the DAF it
!                 designated:
!
!                    - Save the successor of P.
!
!                    - Link the predecessor of node P to the successor
!                      of P, if the predecessor is not NIL.
!
!                    - If it happens that P is the head node of the
!                      active list, set the head equal to the
!                      successor of P.
!
!                    - Link P into the free list.
!
!                    - Set P equal to its saved successor.
!
!                    - (PREV remains unchanged.)
!
                  nextp  =  stpool( p )

                  if ( p == sthead ) then
!
!                    Re-assign STHEAD so that we don't lose the head
!                    of the active list.  P has no predecessor in this
!                    case, so there's no need to set the forward pointer
!                    of node PREV.
!
                     sthead = nextp

                  else
!
!                    Since P is not the head node of the active list,
!                    PREV is not NIL, so we'll need to set the forward
!                    pointer of node PREV.
!
                     stpool( prev ) = nextp

                  end if

                  stpool( p )  =  stfptr
                  stfptr       =  p
                  p            =  nextp

               end if

            end do

!
!           At this point, we've freed all nodes from the active
!           list that were used to index information about DAFs that
!           are no longer open.  Now see if we still need to make
!           room.  If so, locate the first dossier with STADDG(P)
!           set to FALSE.  We know then that this file is not
!           currently involved in an array addition.
!
            if ( stfptr == nil ) then

               found = .false.
               p     = sthead
               prev  = nil

               do while ( ( p /= nil ) .and. ( .not. found ) )

!
!                 If STADDG(P) is TRUE, then we must continue
!                 searching.
!
                  if ( staddg(p) ) then

                     prev = p
                     p    = stpool(p)

                  else

                     found = .true.
!
!                    No array is presently being added to the DAF
!                    associated with this dossier, so free the
!                    node pointing to the information about the DAF it
!                    designated:
!
!                    - Save the successor of P.
!
!                    - Link the predecessor of node P to the successor
!                      of P, if the predecessor is not NIL.
!
!                    - If it happens that P is the head node of the
!                      active list, set the head equal to the
!                      successor of P.
!
!                    - Link P into the free list.
!
!                    - Set P equal to its saved successor.
!
!                    - (PREV remains unchanged.)
!
                     nextp  =  stpool( p )

                     if ( p == sthead ) then
!
!                       Re-assign STHEAD so that we don't lose the head
!                       of the active list.  P has no predecessor in
!                       this case, so there's no need to set the
!                       forward pointer of node PREV.
!
                        sthead = nextp

                     else
!
!                       Since P is not the head node of the active list,
!                       PREV is not NIL, so we'll need to set the
!                       forward pointer of node PREV.
!
                        stpool( prev ) = nextp

                     end if

                     stpool( p )  =  stfptr
                     stfptr       =  p
                     p            =  nextp

                     end if

               end do

            end if

!
!           Now, check to see if there is now room to add the dossier
!           for the new DAF to the state table.  If not signal an error.
!
            ! PEPPER: DAFANA_ROOM has made room (the table grows; SPICELIB
            ! signaled SPICE(STFULL) here with 20 DAFs).

         end if

!
!        If we reach here, then we have room in the state table for
!        the new DAF.  The first free node is indicated by SFTPTR.
!        Allocate this node and use it to index the state information
!        for the new DAF.
!
         p =  stfptr

!
!        Update the free list pointer, link P to the previous head
!        of the active list, and make P the head of the active list.
!
         stfptr       =  stpool( p )
         stpool( p )  =  sthead
         sthead       =  p

      end if

!
!     At this point, P is the head node of the active list, and P is
!     the index in the state table of the information for the current
!     DAF.
!

!
!     Set the state information for the current array.
!
      stfh  ( p )  =  handle
      stifnm( p )  =  ifname
      staddg( p )  = .true.
      stfrst( p )  =  fward
      stlast( p )  =  bward
      stbegn( p )  =  free
      stfree( p )  =  free

!
!     Find out how big the array summary is supposed to be.
!
      call dafhsf ( ctx, stfh(p), nd, ni )

      sumsiz  =  nd + (ni+1)/2

!
!     Set the local copies of the array's summary and name.
!
      stlsum(1:sumsiz,p) = sum(1:sumsiz)

      stname( p )  =  name

      return
      end associate
      end subroutine dafbna

      !> Add one or more double precision words of data to the newest
      !> array in the current DAF.
      !>
      !>## Detailed Input
      !>
      !>     DATA     is an arbitrary number of double precision words to
      !>              be added to the data in the array being created.
      !>
      !>     N        is the number of double precision words in DATA.
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
      !>     1)  If there are no DAFs to which data is currently being added,
      !>         the error SPICE(DAFNOWRITE) is signaled.
      !>
      !>     2)  If a new array has not been started in the current DAF (by a
      !>         call to DAFBNA), the error SPICE(DAFNEWCONFLICT) is signaled.
      !>
      !>     3)  If N is less than one, no data are added to the file.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     DAFADA adds data to the last array begun by DAFBNA or selected
      !>     by DAFCAD.
      !>
      !>     Data can be added to a DAF in chunks of any size, so long
      !>     as the chunks are added in the proper order.
      !>
      !>## Examples
      !>
      !>     See $Examples in DAFANA.
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
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
      !>
      !>        Updated entry points to support changes made to the DAF
      !>        system that utilize the new handle manager. See
      !>        the $Revisions section of DAFANA for a detailed
      !>        discussion of the changes.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to work with new DAF routines that allow writing
      !>        to multiple DAFs simultaneously. Functionality for
      !>        applications that write to one DAF at a time is unchanged.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to work with new DAF routines that allow writing
      !>        to multiple DAFs simultaneously. Functionality for
      !>        applications that write to one DAF at a time is unchanged.
      module subroutine dafada ( ctx, data, n )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: data     ( * )  !! Elements of the new array.
      integer(ip), intent(in) :: n  !! Number of elements in DATA.
      integer(ip) :: p
      if ( .not. allocated ( ctx%dafana ) ) then
         allocate ( ctx%dafana )
         call init_dafana_state ( ctx%dafana )
      end if
      associate ( stfh => ctx%dafana%stfh, &
                  staddg => ctx%dafana%staddg, &
                  stfree => ctx%dafana%stfree, &
                  sthead => ctx%dafana%sthead )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     This routine operates on the DAF at the head of the active list.
!
      p = sthead

!
!     We must make sure that the requested addition can be performed.
!     We don't validate the file handle here because this is one place
!     where we are concerned about speed.  The low-level writer routine
!     DAFWDR will handle the check.
!
      if ( p == nil ) then

         call ctx%setmsg ( 'No DAF is currently being written.' )
         call ctx%sigerr ( 'SPICE(DAFNOWRITE)'                  )
         return

!
!     An array cannot be extended unless begun first.
!
      else if ( .not. staddg(p) ) then
!
!        Validate the current handle, then get the name of the DAF.
!
         call dafsih ( ctx, stfh(p), 'WRITE' )

         if ( ctx%failed() ) return

         call ctx%setmsg ( 'An attempt was made to add data to an array ' // &
                       'that has not yet been begun, in file #.'       )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(DAFNEWCONFLICT)'                         )
         return

!
!     Start adding data at the first free address, then update that
!     address to get ready for the next addition.
!
      else if ( n >= 1 ) then

         call dafwda ( ctx, stfh(p), stfree(p), stfree(p)+n-1, data )
         stfree(p) = stfree(p) + n

      end if

      return
      end associate
      end subroutine dafada

      !> End the addition of data to the newest array in the current DAF.
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
      !>     1)  If there are no DAFs to which data is currently being added,
      !>         the error SPICE(DAFNOWRITE) is signaled, or the error will
      !>         be detected by routines called by this routine.
      !>
      !>     2)  If a new array has not been started in the current DAF (by a
      !>         call to DAFBNA), the error SPICE(DAFNEWCONFLICT) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     DAFENA makes the current array a permanent addition to the
      !>     current DAF.
      !>
      !>     The pointers within the file are not changed until an array
      !>     is ended successfully. If an error occurs or if the current
      !>     DAF is closed before DAFENA is called, the last array will
      !>     not be visible to the DAF reader routines.
      !>
      !>## Examples
      !>
      !>     See $Examples in DAFANA.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary entries from $Revisions section.
      !>
      !>-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
      !>
      !>        Updated entry points to support changes made to the DAF
      !>        system that utilize the new handle manager. See
      !>        the $Revisions section of DAFANA for a detailed
      !>        discussion of the changes.
      !>
      !>-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
      !>
      !>        Updated to remove potential compiler warnings from the
      !>        truncation of double precision numbers to integers.
      !>
      !>        Also changed was a numeric constant from 1.D0 to the
      !>        equivalent, but more aesthetically pleasing 1.0D0.
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to work with new DAF routines that allow writing
      !>        to multiple DAFs simultaneously. Functionality for
      !>        applications that write to one DAF at a time is unchanged.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
      !>
      !>        Updated to remove potential compiler warnings from the
      !>        truncation of double precision numbers to integers. Two
      !>        assignments to NARRAY were updated, being changed from:
      !>
      !>           NARRAY = SUMREC(ARYCNT)
      !>
      !>        to
      !>
      !>           NARRAY = IDINT ( SUMREC(ARYCNT) )
      !>
      !>        Also changed was a numeric constant from 1.D0 to the
      !>        equivalent, but more aesthetically pleasing 1.0D0.
      module subroutine dafena ( ctx )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), parameter :: arycnt = 3
      integer(ip), parameter :: bwdptr = 2
      integer(ip), parameter :: fwdptr = 1
      integer(ip) :: cloc
      integer(ip) :: sumsiz
      integer(ip) :: p
      integer(ip) :: namsiz
      integer(ip) :: dloc
      integer(ip) :: narray
      character(crlen) :: namrec
      real(wp) :: sumrec ( dprsiz )
      integer(ip) :: next
      integer(ip) :: nd
      logical :: found
      integer(ip) :: ic     ( maxnic )
      integer(ip) :: ni
      integer(ip) :: word
      real(wp) :: dc     ( maxndc )
      if ( .not. allocated ( ctx%dafana ) ) then
         allocate ( ctx%dafana )
         call init_dafana_state ( ctx%dafana )
      end if
      associate ( stfh => ctx%dafana%stfh, &
                  stifnm => ctx%dafana%stifnm, &
                  staddg => ctx%dafana%staddg, &
                  stfrst => ctx%dafana%stfrst, &
                  stlast => ctx%dafana%stlast, &
                  stfree => ctx%dafana%stfree, &
                  stbegn => ctx%dafana%stbegn, &
                  stlsum => ctx%dafana%stlsum, &
                  stname => ctx%dafana%stname, &
                  sthead => ctx%dafana%sthead )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     This routine operates on the DAF at the head of the active list.
!
      p = sthead

      if ( p == nil ) then

         call ctx%setmsg ( 'No DAF is currently being written.' )
         call ctx%sigerr ( 'SPICE(DAFNOWRITE)'                  )
         return

!
!     A new array cannot be ended unless begun first.
!
      else if ( .not. staddg(p) ) then
!
!        Validate the current handle, then get the name of the DAF.
!
         call dafsih ( ctx, stfh(p), 'WRITE' )

         if ( ctx%failed() ) return

         call ctx%setmsg ( 'An attempt was made to end an array that has '// &
                       'not yet been begun, in file #.'                )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(DAFNEWCONFLICT)'                         )
         return

      end if

!
!     No more data. The array ends just before the next free
!     address. The summary should be complete except for the
!     initial and final addresses of the data, of which we
!     have been keeping track.
!
      call dafhsf ( ctx, stfh(p), nd, ni )

      call dafus  ( ctx, stlsum(1,p), nd, ni, dc, ic )

      ic( ni-1 )  =  stbegn(p)
      ic( ni   )  =  stfree(p) - 1

      call dafps ( ctx, nd, ni, dc, ic, stlsum(1,p) )

!
!     The summary should be stored in the final summary record (the
!     one at the end of the file). Get that entire record, and the
!     corresponding name record.
!
      call dafrdr ( ctx, stfh(p), stlast(p), 1, dprsiz, sumrec, found )
      call dafrcr ( ctx, stfh(p), stlast(p)+1,          namrec        )
      narray = idint ( sumrec(arycnt) )

!
!     The number of arrays determines where the summary and name
!     are stored within the summary record. Adding this array increases
!     the number of arrays by one.
!
      sumsiz  =  nd      +         ( ni + 1 ) / 2
      dloc    =  arycnt  +  1  +   narray * sumsiz

      sumrec(dloc:dloc+sumsiz-1) = stlsum(1:sumsiz,p)

      namsiz  =        8      * sumsiz
      cloc    =  1  +  narray * namsiz

      namrec(cloc : cloc+namsiz-1) = stname(p)

      sumrec(arycnt) =  sumrec(arycnt) + 1.0_wp
      narray         =  idint ( sumrec(arycnt) )

!
!     Usually, adding an array does not fill the final summary
!     record, and it can simply be replaced.
!
      if ( narray < maxsum / sumsiz ) then
         call dafwdr ( ctx, stfh(p), stlast(p),   sumrec )
         call dafwcr ( ctx, stfh(p), stlast(p)+1, namrec )

!
!     When the record becomes full, a new one must be written.
!     However, this fact should be transparent to the user.
!
      else

!
!        The new summary record will be stored in the next free record
!        in the file. This summary record should point to it.
!
!        To find out which record the next free address is in, we use
!        DAFARW (`address to record and word').
!
         call dafarw ( ctx, stfree(p)-1, next, word )
         next           = next + 1
         sumrec(fwdptr) = real( next, wp )

         call dafwdr ( ctx, stfh(p), stlast(p),   sumrec )
         call dafwcr ( ctx, stfh(p), stlast(p)+1, namrec )

!
!        The new summary record should point backwards to the one just
!        written, and should point forwards to nothing. Of course,
!        it contains no summaries, and no names.
!
         sumrec = 0.0_wp
         sumrec(fwdptr)  =  0.0_wp
         sumrec(bwdptr)  =  real( stlast(p), wp )
         sumrec(arycnt)  =  0.0_wp
         namrec          = ' '

         call dafwdr ( ctx, stfh(p), next,   sumrec )
         call dafwcr ( ctx, stfh(p), next+1, namrec )

!
!        If a new summary record  was added, the first free address
!        lies just beyond the end of the matching character record.
!
!        We use DAFRWA (`record and word to address') to calculate
!        the next free address.
!
         stlast(p) = next
         call dafrwa ( ctx, stlast(p)+2, 1, stfree(p) )

      end if

!
!     The new value STFREE(P) must be rewritten in the file record each
!     time a new array is added. If a new record was added, the new
!     value of STLAST(P) will be rewritten as well.
!
      call dafwfr ( ctx, stfh(p), &
                    nd, &
                    ni, &
                    stifnm(p), &
                    stfrst(p), &
                    stlast(p), &
                    stfree(p) )

!
!     Ready for another array.
!
      staddg(p) = .false.

      return
      end associate
      end subroutine dafena

      !> Select a DAF that already has a new array in progress as the
      !> one to continue adding data to.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF that is open for write
      !>              access and in which a new array has been
      !>              started by a call to DAFBNA.
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
      !>     1)  If the input handle is not that of a DAF that is open for
      !>         writing, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     2)  If no array is currently being added to in the file indicated
      !>         by HANDLE, the error SPICE(NOARRAYSTARTED) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     DAFCAD supports simultaneous addition of data to arrays in
      !>     multiple DAFs. In applications that use this capability,
      !>     DAFCAD should be called prior to each call to DAFADA or DAFENA
      !>     to specify which DAF is to be acted upon.
      !>
      !>     Here is a code fragment that adds a new array to each of N
      !>     existing DAFs, simultaneously. The data to be added to each
      !>     is broken up into M chunks; one chunk is written to each DAF
      !>     at a time. The data is contained in the array CHUNK, dimensioned
      !>
      !>         DOUBLE PRECISION      CHUNK ( MAXDAT, M, N )
      !>
      !>     The actual amount of data in the Jth chunk for the Ith file is
      !>     given by
      !>
      !>         AMOUNT (J,I)
      !>
      !>         DO I = 1, N
      !>            CALL DAFOPW ( HANDLE(I) )
      !>            CALL DAFBNA ( HANDLE(I) )
      !>         END DO
      !>
      !>         DO J = 1, M
      !>
      !>            DO I = 1, N
      !>               CALL DAFCAD  ( HANDLE(I)                  )
      !>               CALL DAFADA  ( CHUNK(1,J,I),  AMOUNT(J,I) )
      !>            END DO
      !>
      !>         END DO
      !>
      !>         DO I = 1, N
      !>            CALL DAFCAD  ( HANDLE(I) )
      !>            CALL DAFENA
      !>         END DO
      !>
      !>     Note that if we write all of the data for each array to just one
      !>     DAF at a time, we don't need to use DAFCAD:
      !>
      !>        DO I = 1, N
      !>
      !>           CALL DAFOPW ( HANDLE(I) )
      !>           CALL DAFBNA ( HANDLE(I) )
      !>
      !>           DO J = 1, M
      !>              CALL DAFADA ( CHUNK(1,J,I),  AMOUNT(J,I) )
      !>           END DO
      !>
      !>           CALL DAFENA
      !>
      !>        END DO
      !>
      !>## Examples
      !>
      !>     See $Examples in DAFANA.
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
      !>-    SPICELIB Version 3.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Edited entry #2
      !>        in $Exceptions section: No write in progress is detected by
      !>        this routine.
      !>
      !>-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
      !>
      !>        Updated entry points to support changes made to the DAF
      !>        system that utilize the new handle manager. See
      !>        the $Revisions section of DAFANA for a detailed
      !>        discussion of the changes.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 04-SEP-1991 (NJB) (WLT)
      module subroutine dafcad ( ctx, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF to continue adding data to.
      integer(ip) :: p
      integer(ip) :: prev
      logical :: found
      if ( .not. allocated ( ctx%dafana ) ) then
         allocate ( ctx%dafana )
         call init_dafana_state ( ctx%dafana )
      end if
      associate ( stfh => ctx%dafana%stfh, &
                  staddg => ctx%dafana%staddg, &
                  stpool => ctx%dafana%stpool, &
                  sthead => ctx%dafana%sthead )
      if ( ctx%failed() ) return

!
!     Check out the file handle before going any further.
!
      call dafsih ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     See whether we already have an entry for this DAF in the
!     state table.  Find the previous node if possible.
!
      p      =   sthead
      prev   =   nil
      found  =  .false.

      do while (  ( p /= nil )  .and.  ( .not. found )  )

         if ( stfh(p) == handle ) then
            found  =  .true.
         else
            prev   =  p
            p      =  stpool( p )
         end if

      end do

!
!     Either FOUND is false, or P is the index in the state table of
!     the DAF specified by HANDLE, and PREV is the predecessor of P.
!

!
!     You can't continue writing to a DAF that you're not
!     already writing to.
!
      if ( .not. found ) then

         call ctx%setmsg ( 'No write in progress to #. (Handle was #.) ' )
         call ctx%errhan ( '#', handle )
         call ctx%errint ( '#',  handle                                  )
         call ctx%sigerr ( 'SPICE(NOARRAYSTARTED)'                       )
         return

      else if ( .not. staddg(p) ) then

         call ctx%setmsg ( 'No write in progress to #. (Handle was #.) ' )
         call ctx%errhan ( '#', handle )
         call ctx%errint ( '#',  handle                                  )
         call ctx%sigerr ( 'SPICE(NOARRAYSTARTED)'                       )
         return

      end if

!
!     Move the node for this DAF to the head of the active list,
!     if it is not already there:
!
!        - Make the predecessor of P point to the successor of P.
!
!        - Make P point to the head of the active list.
!
!        - Make P the active list head node.
!

      if ( p /= sthead ) then
!
!        P is in the active list, but is not at the head.  So,
!        the predecessor of P is not NIL.
!
         stpool(prev)  =  stpool(p)
         stpool(p)     =  sthead
         sthead        =  p

      end if

      return
      end associate
      end subroutine dafcad

      !> Begin a forward search for arrays in a DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF on which a forward
      !>              search is to be conducted.
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
      !>     1)  If the input handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If the first summary record of the DAF file cannot be read,
      !>         the error SPICE(RECORDNOTFOUND) is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See DAFFA.
      !>
      !>## Examples
      !>
      !>     1) See DAFFA.
      !>
      !>     2) Create a simple program to output the double precision and
      !>        integer values stored in an SPK's segment's descriptors. This
      !>        program opens a DAF for read, performs a forwards search for
      !>        the DAF arrays, prints the segment description for each array
      !>        found, then closes the DAF.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           de421.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFBFS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Define the summary parameters appropriate
      !>C     for an SPK file.
      !>C
      !>      INTEGER               MAXSUM
      !>      PARAMETER           ( MAXSUM = 125 )
      !>
      !>      INTEGER               ND
      !>      PARAMETER           ( ND = 2 )
      !>
      !>      INTEGER               NI
      !>      PARAMETER           ( NI = 6 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(32)        KERNEL
      !>
      !>      DOUBLE PRECISION      DC ( ND     )
      !>      DOUBLE PRECISION      SUM( MAXSUM )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               IC( NI )
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Open a DAF for read. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      KERNEL = 'de421.bsp'
      !>      CALL DAFOPR ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Begin a forward search on the file.
      !>C
      !>      CALL DAFBFS ( HANDLE )
      !>
      !>C
      !>C     Search until a DAF array is found.
      !>C
      !>      CALL DAFFNA ( FOUND )
      !>
      !>C
      !>C     Loop while the search finds subsequent DAF arrays.
      !>C
      !>      DO WHILE ( FOUND )
      !>
      !>         CALL DAFGS ( SUM )
      !>         CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>         WRITE(*,*)                'Doubles:', DC(1:ND)
      !>         WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
      !>
      !>C
      !>C        Check for another segment.
      !>C
      !>         CALL DAFFNA ( FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        1        0        1        2      641   310404
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        2        0        1        2   310405   423048
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        3        0        1        2   423049   567372
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        4        0        1        2   567373   628976
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        5        0        1        2   628977   674740
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        6        0        1        2   674741   715224
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        7        0        1        2   715225   750428
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        8        0        1        2   750429   785632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        9        0        1        2   785633   820836
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:       10        0        1        2   820837   944040
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      301        3        1        2   944041  1521324
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      399        3        1        2  1521325  2098608
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      199        1        1        2  2098609  2098620
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      299        2        1        2  2098621  2098632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      499        4        1        2  2098633  2098644
      !>
      !>        Note, the final entries in the integer array contains the
      !>        segment start/end indexes. The output indicates the search
      !>        proceeded from the start of the file (low value index) towards
      !>        the end (high value index).
      !>
      !>## Restrictions
      !>
      !>     1)  Calls that do or may change DAF addresses of DAF summaries,
      !>         names, or data of a given DAF file should not be made during
      !>         a search of that file initiated by either DAFBFS or DAFBBS.
      !>         No such changes should be made between the start of a search
      !>         and calls to any entry point that reads or writes to the
      !>         summary of the "current array" found by that search, or
      !>         that returns a "found" flag indicating whether the current
      !>         array exists.
      !>
      !>         Changing the size of the comment area while a search is in
      !>         progress can invalidate record numbers stored in local data
      !>         structures of this routine. This can cause corrupted array
      !>         summaries and names to be returned upon read access and file
      !>         corruption to occur upon write access.
      !>
      !>         Adding arrays (aka "segments") while either a forward or
      !>         backward search is in progress can cause the search to miss
      !>         the new segments.
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
      !>-    SPICELIB Version 2.1.1, 26-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added undeclared variables to code example.
      !>
      !>        Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 2.1.0, 10-OCT-2012 (EDW)
      !>
      !>        Added a functional code example to the $Examples section.
      !>
      !>        Added check on value of "found" boolean returned from
      !>        DAFGSR calls. Failure to check this value can cause an
      !>        infinite loop during segment searches on damaged SPKs.
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to support simultaneous searches of multiple DAFs.
      !>
      !>        This routine now operates on the current DAF---the one at
      !>        the head of the active list. All saved state variables
      !>        used by this routine are now part of the state table, or
      !>        its associated set of pointers.
      !>
      !>        Also, the $Exceptions section was filled out.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafbfs ( ctx, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of file to be searched.
      integer(ip) :: i
      integer(ip) :: prev
      integer(ip) :: nextp
      integer(ip) :: p
      integer(ip) :: bward
      integer(ip) :: nd
      integer(ip) :: free
      character(ifnlen) :: ifname
      integer(ip) :: ni
      integer(ip) :: fward
      logical :: fnd
      if ( .not. allocated ( ctx%daffa ) ) then
         allocate ( ctx%daffa )
         call init_daffa_state ( ctx%daffa )
      end if
      call daffa_room ( ctx )
      associate ( stfh => ctx%daffa%stfh, &
                  stprev => ctx%daffa%stprev, &
                  stthis => ctx%daffa%stthis, &
                  stnext => ctx%daffa%stnext, &
                  stnseg => ctx%daffa%stnseg, &
                  stcurr => ctx%daffa%stcurr, &
                  sthvnr => ctx%daffa%sthvnr, &
                  stsr => ctx%daffa%stsr, &
                  stpool => ctx%daffa%stpool, &
                  sthead => ctx%daffa%sthead, &
                  stfptr => ctx%daffa%stfptr )
      if ( ctx%failed() ) return

!
!     Check out the file handle before going any further.
!
      call dafsih ( ctx, handle, 'READ' )

      if ( ctx%failed() ) return

!
!     Initialize the state table pool, if this hasn't been done yet.
!     Also initialize the cell used to obtain the set of handles of
!     open DAFs.
!

!
!     See whether we already have an entry for this DAF in the
!     state table. Find the previous node if possible.
!
      p      =   sthead
      prev   =   nil
      fnd    =  .false.

      do while (  ( p /= nil )  .and.  ( .not. fnd )  )

         if ( stfh(p) == handle ) then
            fnd   =  .true.
         else
            prev  =  p
            p     =  stpool( p )
         end if

      end do

!
!     At this point, either FND is false, or P points to a
!     state table entry describing the DAF indicated by HANDLE.
!     In the latter case, PREV is the predecessor of P.
!
      if ( fnd ) then
!
!        We already have a dossier on this DAF. We already have
!        the information on the summary format, but we must re-set
!        our summary record pointers and our name record availability
!        flag.
!
!        Rather than doing the update here, we do it outside of this
!        IF block. That way, the update gets done in just one place.
!        This just makes life easier:  if the collection of state
!        variables is changed, there are fewer places to forget to
!        make the required code changes.
!
!        Move the node for this DAF to the head of the active list,
!        if it is not already there:
!
!           - Make the predecessor of P point to the successor of P.
!
!           - Make P point to the head of the active list.
!
!           - Make P the active list head node.
!
         if ( p /= sthead ) then
!
!           P is in the active list, but is not at the head. So,
!           the predecessor of P is not NIL.
!
            stpool(prev)  =  stpool(p)
            stpool(p)     =  sthead
            sthead        =  p

         end if

      else
!
!        We don't yet have any information on this DAF. Make a new
!        state table entry for the DAF. We may need to make room for
!        the new information by freeing space allocated to DAFs that
!        are no longer open.
!
         ! PEPPER: DAFFA_ROOM has made room (before the ASSOCIATE block).

!
!        If there still is no room, there is a bug in DAFAH, since DAFAH
!        should not allow more than TBSIZE DAFs to be open. So, we
!        assume that we've found some room. The first free node is
!        indicated by STFPTR. We'll allocate this node and use it to
!        index the state information for the new DAF.
!
         p  =  stfptr

!
!        Update the free list pointer, link P to the previous head
!        of the active list, and make P the head of the active list.
!
         stfptr       =  stpool( p )
         stpool( p )  =  sthead
         sthead       =  p

      end if

!
!     At this point, P is the head node of the active list, and P is
!     the index in the state table of the information for the current
!     DAF.
!

!
!     Read the file record and first summary record. Do not read the
!     corresponding name record until necessary. In most searches,
!     names are of no interest.
!
      call dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )
      call dafgsr ( ctx, handle, fward, 1, 128, stsr(1,p), fnd )

      if ( .not. fnd ) then

         call ctx%setmsg ( 'Attempt to read descriptor record # '    // &
                       'of DAF ''#'' failed; record was '        // &
                       'not found. This condition may indicate ' // &
                       'a corrupted DAF.' )
         call ctx%errint ( '#', stnext(p)                            )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(RECORDNOTFOUND)'                   )
         return

      end if

!
!     Set up the state information for this file. Note that we
!     don't have a name record yet, and we have no current array
!     yet.
!
      stfh  ( p )  =  handle
      stthis( p )  =  fward
      stnext( p )  =  int ( stsr(1,p) )
      stprev( p )  =  int ( stsr(2,p) )
      stnseg( p )  =  int ( stsr(3,p) )
      sthvnr( p )  = .false.

!
!     The arrays are returned in forward order within each summary
!     record.
!
      stcurr( p )  =  0

      return
      end associate
      end subroutine dafbfs

      !> Find the next (forward) array in the current DAF.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     FOUND    is .TRUE. if an array was found, and is .FALSE. if,
      !>              when this routine is called, the current array is
      !>              the tail of the array list. (Recall that the
      !>              arrays in a DAF may be viewed as a doubly linked
      !>              list, with the tail being the last array in the file.)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If this routine is called before a search is begun, the
      !>         error SPICE(DAFNOSEARCH) is signaled.
      !>
      !>     2)  If the DAF to be searched has actually been closed, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     3)  If the end of the array list has already been reached when
      !>         this routine is called, this routine has no effect.
      !>
      !>     4)  If the summary record of the next array (aka "segment") in
      !>         the DAF file cannot be read, the error SPICE(RECORDNOTFOUND)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See DAFFA.
      !>
      !>## Examples
      !>
      !>     1) See DAFFA.
      !>
      !>     2) Use a simple routine to output the double precision and
      !>        integer values stored in an SPK's segment's descriptors. This
      !>        function opens a DAF for read, performs a forwards search for
      !>        the DAF arrays, prints the segment descriptor for each array
      !>        found, then closes the DAF.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           de421.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFFNA_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Define the summary parameters appropriate
      !>C     for an SPK file.
      !>C
      !>      INTEGER               MAXSUM
      !>      PARAMETER           ( MAXSUM = 125 )
      !>
      !>      INTEGER               ND
      !>      PARAMETER           ( ND = 2 )
      !>
      !>      INTEGER               NI
      !>      PARAMETER           ( NI = 6 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(32)        KERNEL
      !>
      !>      DOUBLE PRECISION      DC ( ND     )
      !>      DOUBLE PRECISION      SUM( MAXSUM )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               IC( NI )
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Open a DAF for read. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      KERNEL = 'de421.bsp'
      !>      CALL DAFOPR ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Begin a forward search on the file.
      !>C
      !>      CALL DAFBFS ( HANDLE )
      !>
      !>C
      !>C     Search until a DAF array is found.
      !>C
      !>      CALL DAFFNA ( FOUND )
      !>
      !>C
      !>C     Loop while the search finds subsequent DAF arrays.
      !>C
      !>      DO WHILE ( FOUND )
      !>
      !>         CALL DAFGS ( SUM )
      !>         CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>         WRITE(*,*)                'Doubles:', DC(1:ND)
      !>         WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
      !>
      !>C
      !>C        Check for another segment.
      !>C
      !>         CALL DAFFNA ( FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        1        0        1        2      641   310404
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        2        0        1        2   310405   423048
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        3        0        1        2   423049   567372
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        4        0        1        2   567373   628976
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        5        0        1        2   628977   674740
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        6        0        1        2   674741   715224
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        7        0        1        2   715225   750428
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        8        0        1        2   750429   785632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        9        0        1        2   785633   820836
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:       10        0        1        2   820837   944040
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      301        3        1        2   944041  1521324
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      399        3        1        2  1521325  2098608
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      199        1        1        2  2098609  2098620
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      299        2        1        2  2098621  2098632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      499        4        1        2  2098633  2098644
      !>
      !>        Note, the final entries in the integer array contains the
      !>        segment start/end indexes. The output indicates the search
      !>        proceeded from the start of the file (low value index) towards
      !>        the end (high value index).
      !>
      !>## Restrictions
      !>
      !>     1)  Calls that do or may change DAF addresses of DAF summaries,
      !>         names, or data of a given DAF file should not be made during
      !>         a search of that file initiated by either DAFBFS or DAFBBS.
      !>         No such changes should be made between the start of a search
      !>         and calls to any entry point that reads or writes to the
      !>         summary of the "current array" found by that search, or
      !>         that returns a "found" flag indicating whether the current
      !>         array exists.
      !>
      !>         Changing the size of the comment area while a search is in
      !>         progress can invalidate record numbers stored in local data
      !>         structures of this routine. This can cause corrupted array
      !>         summaries and names to be returned upon read access and file
      !>         corruption to occur upon write access.
      !>
      !>         Adding arrays (aka "segments") while either a forward or
      !>         backward search is in progress can cause the search to miss
      !>         the new segments.
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
      !>-    SPICELIB Version 2.1.1, 26-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added undeclared variables to code example.
      !>
      !>        Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 2.1.0, 10-OCT-2012 (EDW)
      !>
      !>        Added a functional code example to the $Examples section.
      !>
      !>        Added check on value of "found" boolean returned from
      !>        DAFGSR calls. Failure to check this value can cause an
      !>        infinite loop during segment searches on damaged SPKs.
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to support simultaneous searches of multiple DAFs.
      !>
      !>        This routine now operates on the current DAF---the one at
      !>        the head of the active list. All saved state variables
      !>        used by this routine are now part of the state table, or
      !>        its associated set of pointers.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine daffna ( ctx, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(out) :: found  !! .TRUE. if an array was found.
      integer(ip) :: p
      logical :: fnd
      if ( .not. allocated ( ctx%daffa ) ) then
         allocate ( ctx%daffa )
         call init_daffa_state ( ctx%daffa )
      end if
      associate ( stfh => ctx%daffa%stfh, &
                  stprev => ctx%daffa%stprev, &
                  stthis => ctx%daffa%stthis, &
                  stnext => ctx%daffa%stnext, &
                  stnseg => ctx%daffa%stnseg, &
                  stcurr => ctx%daffa%stcurr, &
                  sthvnr => ctx%daffa%sthvnr, &
                  stsr => ctx%daffa%stsr, &
                  sthead => ctx%daffa%sthead )
      if ( ctx%failed() ) return

!
!     FOUND will be false until we make it past the error checks.
!
      found = .false.

!
!     Operate on the last DAF in which a search has been started.
!
      p  =  sthead

!
!     Make sure that a search has been started in this DAF.
!
      if ( p == nil ) then

         call ctx%setmsg ( 'No DAF is currently being searched.' )
         call ctx%sigerr ( 'SPICE(DAFNOSEARCH)'                  )
         return

!
!     Make sure that the `current' DAF is still open.
!
      else

         call dafsih ( ctx, stfh(p), 'READ' )

         if ( ctx%failed() ) return

      end if

!
!     Now that we know a search is going on, assume that we will find
!     an array until proven otherwise.
!
      found = .true.

!
!     Either there are more summaries left in this record, or
!     there aren't. If there are, just incrementing the pointer
!     is sufficient. If there aren't, we have to find the next
!     record and point to the first array there. (If that
!     record is empty, or doesn't exist, then there are simply
!     no more arrays to be found.)
!

      stcurr(p) = stcurr(p) + 1

      if ( stcurr(p) > stnseg(p) ) then

         if ( stnext(p) == 0 ) then
!
!           There are no more arrays in the list.
!
            found     =  .false.
!
!           Make sure that the array pointer stays pointing to
!           the position following the end of the list. Otherwise,
!           a call to DAFFPA might fail to find the last array in
!           the list.
!
            stcurr(p) =   stnseg(p) + 1
!
!           The careful reader may note that we're not updating any
!           of the pointers
!
!              STTHIS
!              STNEXT
!              STPREV
!
!           These will not be accessed if there is no current array.
!           If the array pointer is backed up again by a call to
!           DAFFPA, the values we have right now will be correct.
!
         else

            call dafgsr ( ctx, stfh(p), stnext(p), 1, 128, stsr(1,p), fnd )

            if ( .not. fnd ) then

               call ctx%setmsg ( 'Attempt to read descriptor record # '   // &
                             'of DAF ''#'' failed; record was '       // &
                             'not found. This condition may indicate '// &
                             'a corrupted DAF.' )
               call ctx%errint ( '#', stnext(p)                            )
               call ctx%errhan ( '#', stfh(p) )
               call ctx%sigerr ( 'SPICE(RECORDNOTFOUND)'                   )
               return

            end if

!
!           The name (character) record we've saved no longer applies
!           to the current summary record. However, we've just updated
!           the summary record, so the summary record remains valid.
!
            sthvnr( p )  = .false.

            stthis( p )  =  stnext(p)
            stnext( p )  =  int ( stsr(1,p) )
            stprev( p )  =  int ( stsr(2,p) )
            stnseg( p )  =  int ( stsr(3,p) )
            stcurr( p )  =  1

            found        = ( stnseg(p) > 0 )

         end if

      end if

      return
      end associate
      end subroutine daffna

      !> Begin a backward search for arrays in a DAF.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF on which a backward
      !>              search is to be conducted.
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
      !>     1)  If the input handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If the last summary record of the DAF file cannot be read,
      !>         the error SPICE(RECORDNOTFOUND) is signaled.
      !>
      !>## Files
      !>
      !>     See argument HANDLE.
      !>
      !>## Particulars
      !>
      !>     See DAFFA.
      !>
      !>## Examples
      !>
      !>     1) See DAFFA.
      !>
      !>     2) Create a simple routine to output the double precision and
      !>        integer values stored in an SPK's segment's descriptors. This
      !>        program opens a DAF for read, performs a backward search for
      !>        the DAF arrays, prints the segment descriptor for each array
      !>        found, then closes the DAF.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           de421.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFBBS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Define the summary parameters appropriate
      !>C     for an SPK file.
      !>C
      !>      INTEGER               MAXSUM
      !>      PARAMETER           ( MAXSUM = 125 )
      !>
      !>      INTEGER               ND
      !>      PARAMETER           ( ND = 2 )
      !>
      !>      INTEGER               NI
      !>      PARAMETER           ( NI = 6 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(32)        KERNEL
      !>
      !>      DOUBLE PRECISION      DC ( ND     )
      !>      DOUBLE PRECISION      SUM( MAXSUM )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               IC( NI )
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Open a DAF for read. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      KERNEL = 'de421.bsp'
      !>      CALL DAFOPR ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Begin a backward search on the file.
      !>C
      !>      CALL DAFBBS ( HANDLE )
      !>
      !>C
      !>C     Search until a DAF array is found.
      !>C
      !>      CALL DAFFPA ( FOUND )
      !>
      !>C
      !>C     Loop while the search finds subsequent DAF arrays.
      !>C
      !>      DO WHILE ( FOUND )
      !>
      !>         CALL DAFGS ( SUM )
      !>         CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>         WRITE(*,*)                'Doubles:', DC(1:ND)
      !>         WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
      !>
      !>C
      !>C        Check for another segment.
      !>C
      !>         CALL DAFFPA ( FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      499        4        1        2  2098633  2098644
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      299        2        1        2  2098621  2098632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      199        1        1        2  2098609  2098620
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      399        3        1        2  1521325  2098608
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      301        3        1        2   944041  1521324
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:       10        0        1        2   820837   944040
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        9        0        1        2   785633   820836
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        8        0        1        2   750429   785632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        7        0        1        2   715225   750428
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        6        0        1        2   674741   715224
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        5        0        1        2   628977   674740
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        4        0        1        2   567373   628976
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        3        0        1        2   423049   567372
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        2        0        1        2   310405   423048
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        1        0        1        2      641   310404
      !>
      !>        Note, the final entries in the integer arrays record the
      !>        segment start/end indexes. The output indicates the search
      !>        proceeded from the end of the file (high value index) towards
      !>        the beginning (low value index).
      !>
      !>## Restrictions
      !>
      !>     1)  Calls that do or may change DAF addresses of DAF summaries,
      !>         names, or data of a given DAF file should not be made during
      !>         a search of that file initiated by either DAFBFS or DAFBBS.
      !>         No such changes should be made between the start of a search
      !>         and calls to any entry point that reads or writes to the
      !>         summary of the "current array" found by that search, or
      !>         that returns a "found" flag indicating whether the current
      !>         array exists.
      !>
      !>         Changing the size of the comment area while a search is in
      !>         progress can invalidate record numbers stored in local data
      !>         structures of this routine. This can cause corrupted array
      !>         summaries and names to be returned upon read access and file
      !>         corruption to occur upon write access.
      !>
      !>         Adding arrays (aka "segments") while either a forward or
      !>         backward search is in progress can cause the search to miss
      !>         the new segments.
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
      !>-    SPICELIB Version 2.1.1, 26-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added undeclared variables to code example.
      !>
      !>        Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 2.1.0, 10-OCT-2012 (EDW)
      !>
      !>        Added a functional code example to the $Examples section.
      !>
      !>        Added check on value of "found" boolean returned from
      !>        DAFGSR calls. Failure to check this value can cause an
      !>        infinite loop during segment searches on damaged SPKs.
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to support simultaneous searches of multiple DAFs.
      !>
      !>        This routine now makes the DAF designated by HANDLE the
      !>        current DAF---the one at the head of the active list. All
      !>        saved state variables used by this routine are now part of the
      !>        state table, or its associated set of pointers.
      !>
      !>        Also, the $Exceptions section was filled out.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafbbs ( ctx, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF to be searched.
      integer(ip) :: i
      integer(ip) :: prev
      integer(ip) :: nextp
      integer(ip) :: p
      integer(ip) :: bward
      integer(ip) :: nd
      integer(ip) :: free
      character(ifnlen) :: ifname
      integer(ip) :: ni
      integer(ip) :: fward
      logical :: fnd
      if ( .not. allocated ( ctx%daffa ) ) then
         allocate ( ctx%daffa )
         call init_daffa_state ( ctx%daffa )
      end if
      call daffa_room ( ctx )
      associate ( stfh => ctx%daffa%stfh, &
                  stprev => ctx%daffa%stprev, &
                  stthis => ctx%daffa%stthis, &
                  stnext => ctx%daffa%stnext, &
                  stnseg => ctx%daffa%stnseg, &
                  stcurr => ctx%daffa%stcurr, &
                  sthvnr => ctx%daffa%sthvnr, &
                  stsr => ctx%daffa%stsr, &
                  stpool => ctx%daffa%stpool, &
                  sthead => ctx%daffa%sthead, &
                  stfptr => ctx%daffa%stfptr )
      if ( ctx%failed() ) return

!
!     Check out the file handle before going any further.
!
      call dafsih ( ctx, handle, 'READ' )

      if ( ctx%failed() ) return

!
!     Initialize the state table pool, if this hasn't been done yet.
!     Also initialize the cell used to obtain the set of handles of
!     open DAFs.
!

!
!     See whether we already have an entry for this DAF in the
!     state table. Find the previous node if possible.
!
      p     =   sthead
      prev  =   nil
      fnd   =  .false.

      do while (  ( p /= nil )  .and.  ( .not. fnd )  )

         if ( stfh(p) == handle ) then
            fnd   =  .true.
         else
            prev  =  p
            p     =  stpool( p )
         end if

      end do

!
!     At this point, either FND is false, or P points to a
!     state table entry describing the DAF indicated by HANDLE.
!     In the latter case, PREV is the predecessor of P.
!
      if ( fnd ) then
!
!        We already have a dossier on this DAF. We already have
!        the information on the summary format, but we must re-set
!        our summary record pointers and our name record availability
!        flag.
!
!        Rather than doing the update here, we do it outside of this
!        IF block. That way, the update gets done in just one place.
!        This just makes life easier:  if the collection of state
!        variables is changed, there are fewer places to forget to
!        make the required code changes.
!
!        Move the node for this DAF to the head of the active list,
!        if it is not already there:
!
!           - Make the predecessor of P point to the successor of P.
!
!           - Make P point to the head of the active list.
!
!           - Make P the active list head node.
!
         if ( p /= sthead ) then
!
!           P is in the active list, but is not at the head. So,
!           the predecessor of P is not NIL.
!
            stpool(prev)  =  stpool(p)
            stpool(p)     =  sthead
            sthead        =  p

         end if

      else
!
!        We don't yet have any information on this DAF. Make a new
!        state table entry for the DAF. We may need to make room for
!        the new information by freeing space allocated to DAFs that
!        are no longer open.
!
         ! PEPPER: DAFFA_ROOM has made room (before the ASSOCIATE block).

!
!        If there still is no room, there is a bug in DAFAH, since DAFAH
!        should not allow more than TBSIZE DAFs to be open. So, we
!        assume that we've found some room. The first free node is
!        indicated by STFPTR. We'll allocate this node and use it to
!        index the state information for the new DAF.
!
         p  =  stfptr
!
!        Update the free list pointer, link P to the previous head
!        of the active list, and make P the head of the active list.
!
         stfptr       =  stpool( p )
         stpool( p )  =  sthead
         sthead       =  p

      end if

!
!     At this point, P is the head node of the active list, and P is
!     the index in the state table of the information for the current
!     DAF.
!

!
!     Read the file record and last summary record. Do not read the
!     corresponding name record until necessary. In most searches,
!     names are of no interest.
!
      call dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )
      call dafgsr ( ctx, handle, bward, 1, 128, stsr(1,p), fnd )

      if ( .not. fnd ) then

         call ctx%setmsg ( 'Attempt to read descriptor record # '    // &
                       'of DAF ''#'' failed; record was '        // &
                       'not found. This condition may indicate ' // &
                       'a corrupted DAF.' )
         call ctx%errint ( '#', stnext(p)                            )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(RECORDNOTFOUND)'                   )
         return

      end if

      stfh  ( p )  =  handle
      stthis( p )  =  bward
      stnext( p )  =  int ( stsr(1,p) )
      stprev( p )  =  int ( stsr(2,p) )
      stnseg( p )  =  int ( stsr(3,p) )
      sthvnr( p )  = .false.

!
!     The arrays are returned in backward order from each summary
!     record.
!
      stcurr( p )  =  stnseg( p ) + 1

      return
      end associate
      end subroutine dafbbs

      !> Find the previous (backward) array in the current DAF.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     FOUND    is .TRUE. if an array was found, and is .FALSE. if,
      !>              when this routine is called, the current array is
      !>              the head of the array list. (Recall that the
      !>              arrays in a DAF may be viewed as a doubly linked
      !>              list, with the head being the first array in the
      !>              file.)
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If this routine is called before a search is begun, the
      !>         error SPICE(DAFNOSEARCH) is signaled.
      !>
      !>     2)  If the DAF to be searched has actually been closed, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     3)  If the beginning of the array list has already been reached
      !>         when this routine is called, this routine will not change the
      !>         current array. FOUND will be false on output.
      !>
      !>     4)  If the summary record of the previous array (aka "segment")
      !>         in the DAF file cannot be read, the error
      !>         SPICE(RECORDNOTFOUND) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See DAFFA.
      !>
      !>## Examples
      !>
      !>     1) See DAFFA.
      !>
      !>     2) Use a simple routine to output the double precision and
      !>        integer values stored in an SPK's segment's descriptors. This
      !>        function opens a DAF for read, performs a backward search for
      !>        the DAF arrays, prints the segment descriptor for each array
      !>        found, then closes the DAF.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           de421.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFFPA_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Define the summary parameters appropriate
      !>C     for an SPK file.
      !>C
      !>      INTEGER               MAXSUM
      !>      PARAMETER           ( MAXSUM = 125 )
      !>
      !>      INTEGER               ND
      !>      PARAMETER           ( ND = 2 )
      !>
      !>      INTEGER               NI
      !>      PARAMETER           ( NI = 6 )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(32)        KERNEL
      !>
      !>      DOUBLE PRECISION      DC ( ND     )
      !>      DOUBLE PRECISION      SUM( MAXSUM )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               IC( NI )
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Open a DAF for read. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      KERNEL = 'de421.bsp'
      !>      CALL DAFOPR ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Begin a backward search on the file.
      !>C
      !>      CALL DAFBBS ( HANDLE )
      !>
      !>C
      !>C     Search until a DAF array is found.
      !>C
      !>      CALL DAFFPA ( FOUND )
      !>
      !>C
      !>C     Loop while the search finds subsequent DAF arrays.
      !>C
      !>      DO WHILE ( FOUND )
      !>
      !>         CALL DAFGS ( SUM )
      !>         CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>         WRITE(*,*)                'Doubles:', DC(1:ND)
      !>         WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
      !>
      !>C
      !>C        Check for another segment.
      !>C
      !>         CALL DAFFPA ( FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      499        4        1        2  2098633  2098644
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      299        2        1        2  2098621  2098632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      199        1        1        2  2098609  2098620
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      399        3        1        2  1521325  2098608
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      301        3        1        2   944041  1521324
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:       10        0        1        2   820837   944040
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        9        0        1        2   785633   820836
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        8        0        1        2   750429   785632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        7        0        1        2   715225   750428
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        6        0        1        2   674741   715224
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        5        0        1        2   628977   674740
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        4        0        1        2   567373   628976
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        3        0        1        2   423049   567372
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        2        0        1        2   310405   423048
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        1        0        1        2      641   310404
      !>
      !>        Note, the final entries in the integer arrays record the
      !>        segment start/end indexes. The output indicates the search
      !>        proceeded from the end of the file (high value index) towards
      !>        the beginning (low value index).
      !>
      !>## Restrictions
      !>
      !>     1)  Calls that do or may change DAF addresses of DAF summaries,
      !>         names, or data of a given DAF file should not be made during
      !>         a search of that file initiated by either DAFBFS or DAFBBS.
      !>         No such changes should be made between the start of a search
      !>         and calls to any entry point that reads or writes to the
      !>         summary of the "current array" found by that search, or
      !>         that returns a "found" flag indicating whether the current
      !>         array exists.
      !>
      !>         Changing the size of the comment area while a search is in
      !>         progress can invalidate record numbers stored in local data
      !>         structures of this routine. This can cause corrupted array
      !>         summaries and names to be returned upon read access and file
      !>         corruption to occur upon write access.
      !>
      !>         Adding arrays (aka "segments") while either a forward or
      !>         backward search is in progress can cause the search to miss
      !>         the new segments.
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
      !>-    SPICELIB Version 2.1.1, 26-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added undeclared variables to code example.
      !>
      !>        Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 2.1.0, 10-OCT-2012 (EDW)
      !>
      !>        Added a functional code example to the $Examples section.
      !>
      !>        Added check on value of "found" boolean returned from
      !>        DAFGSR calls. Failure to check this value can cause an
      !>        infinite loop during segment searches on damaged SPKs.
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to support simultaneous searches of multiple DAFs.
      !>
      !>        This routine now operates on the current DAF---the one at
      !>        the head of the active list. All saved state variables
      !>        used by this routine are now part of the state table, or
      !>        its associated set of pointers.
      !>
      !>        Also, a bug fix was made to the array pointer adjustment
      !>        algorithm: the pointer is no longer decremented if it
      !>        is already less than 1 and the array summary pointer
      !>        is already pointing to the first array summary. In
      !>        addition, a test made to detect this condition was fixed:
      !>        the test
      !>
      !>           CURR .EQ. 0
      !>
      !>        was replaced by
      !>
      !>           STCURR(P) .LE. 0
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine daffpa ( ctx, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical, intent(out) :: found  !! .TRUE. if an array was found.
      integer(ip) :: p
      logical :: fnd
      if ( .not. allocated ( ctx%daffa ) ) then
         allocate ( ctx%daffa )
         call init_daffa_state ( ctx%daffa )
      end if
      associate ( stfh => ctx%daffa%stfh, &
                  stprev => ctx%daffa%stprev, &
                  stthis => ctx%daffa%stthis, &
                  stnext => ctx%daffa%stnext, &
                  stnseg => ctx%daffa%stnseg, &
                  stcurr => ctx%daffa%stcurr, &
                  sthvnr => ctx%daffa%sthvnr, &
                  stsr => ctx%daffa%stsr, &
                  sthead => ctx%daffa%sthead )
      if ( ctx%failed() ) return

!
!     Operate on the last DAF in which a search has been started.
!
      p  =  sthead

!
!     FOUND will be false until we make it past the error checks.
!
      found = .false.

!
!     Make sure that a search has been started in this DAF.
!
      if ( p == nil ) then

         call ctx%setmsg ( 'No DAF is currently being searched.' )
         call ctx%sigerr ( 'SPICE(DAFNOSEARCH)'                  )
         return

!
!     Make sure that the `current' DAF is still open.
!
      else

         call dafsih ( ctx, stfh(p), 'READ' )

         if ( ctx%failed() ) return

      end if

!
!     Now that we know a search is going on, assume that we will find
!     an array until proven otherwise.
!
      found = .true.

!
!     Either there are more summaries left in this record, or
!     there aren't. If there are, just decrementing the pointer
!     is sufficient. If there aren't, we have to find the previous
!     record and point to the last array there. (If that
!     record is empty, or doesn't exist, then there are simply
!     no more arrays to be found.)
!
      stcurr( p )  =  stcurr( p ) - 1

      if ( stcurr(p) <= 0 ) then

         if ( stprev(p) == 0 ) then
!
!           There is no predecessor of the current array in the list.
!
            found = .false.
!
!           Make sure that the array pointer stays pointing to
!           the position preceding the front of the list. Otherwise,
!           a call to DAFFNA might fail to find the first array in
!           the list.
!
            stcurr(p) = 0
!
!           The careful reader may note that we're not updating any
!           of the pointers
!
!              STTHIS
!              STNEXT
!              STPREV
!
!           These will not be accessed if there is no current array.
!           If the array pointer is moved forward again by a call to
!           DAFFNA, the values we have right now will be correct.
!
         else

            call dafgsr ( ctx, stfh(p), stprev(p), 1, 128, stsr(1,p), fnd )

            if ( .not. fnd ) then

               call ctx%setmsg ( 'Attempt to read descriptor record # '   // &
                             'of DAF ''#'' failed; record was '       // &
                             'not found. This condition may indicate '// &
                             'a corrupted DAF.' )
               call ctx%errint ( '#', stnext(p)                            )
               call ctx%errhan ( '#', stfh(p) )
               call ctx%sigerr ( 'SPICE(RECORDNOTFOUND)'                   )
               return

            end if

!
!           The name (character) record we've saved no longer applies
!           to the current summary record. However, we've just updated
!           the summary record, so the summary record remains valid.
!
            sthvnr( p )  = .false.

            stthis( p )  =  stprev( p )
            stnext( p )  =  int ( stsr(1,p) )
            stprev( p )  =  int ( stsr(2,p) )
            stnseg( p )  =  int ( stsr(3,p) )
            stcurr( p )  =  stnseg( p )

            found        =  ( stnseg(p) > 0 )

         end if

      end if

      return
      end associate
      end subroutine daffpa

      !> Return (get) the summary for the current array in the current
      !> DAF.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     SUM      is the summary for the current array (the array
      !>              found by the latest call to DAFFNA or DAFFPA).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If this routine is called when no search is in progress in the
      !>         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
      !>
      !>     2)  If the DAF for which the "current" array's summary is to be
      !>         returned has actually been closed, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If no array is current in the current DAF, the error
      !>         SPICE(NOCURRENTARRAY) is signaled. There is no current array
      !>         when a search is started by DAFBFS or DAFBBS, but no calls to
      !>         DAFFNA or DAFFPA have been made yet, or whenever DAFFNA or
      !>         DAFFPA return the value .FALSE. in the FOUND argument.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See DAFFA.
      !>
      !>## Examples
      !>
      !>     1) See $Examples in DAFFA.
      !>
      !>     2) Use a simple routine to output the double precision and
      !>        integer values stored in an SPK's segment's descriptors. This
      !>        function opens a DAF for read, performs a forwards search for
      !>        the DAF arrays, prints the segment descriptor for each array
      !>        found, then closes the DAF.
      !>
      !>        Use the SPK kernel below as input DAF file for the program.
      !>
      !>           de421.bsp
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM DAFGS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Define the summary parameters appropriate
      !>C     for an SPK file.
      !>C
      !>      INTEGER               MAXSUM
      !>      PARAMETER           ( MAXSUM = 125 )
      !>
      !>      INTEGER               ND
      !>      PARAMETER           ( ND = 2 )
      !>
      !>      INTEGER               NI
      !>      PARAMETER           ( NI = 6 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(32)        KERNEL
      !>
      !>      DOUBLE PRECISION      DC ( ND     )
      !>      DOUBLE PRECISION      SUM( MAXSUM )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               IC( NI )
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Open a DAF for read. Return a HANDLE referring to the
      !>C     file.
      !>C
      !>      KERNEL = 'de421.bsp'
      !>      CALL DAFOPR ( KERNEL, HANDLE )
      !>
      !>C
      !>C     Begin a forward search on the file.
      !>C
      !>      CALL DAFBFS ( HANDLE )
      !>
      !>C
      !>C     Search until a DAF array is found.
      !>C
      !>      CALL DAFFNA ( FOUND )
      !>
      !>C
      !>C     Loop while the search finds subsequent DAF arrays.
      !>C
      !>      DO WHILE ( FOUND )
      !>
      !>         CALL DAFGS ( SUM )
      !>         CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>         WRITE(*,*)                'Doubles:', DC(1:ND)
      !>         WRITE(*, FMT='(A,6I9)' ) 'Integers:', IC(1:NI)
      !>
      !>C
      !>C        Check for another segment.
      !>C
      !>         CALL DAFFNA ( FOUND )
      !>
      !>      END DO
      !>
      !>C
      !>C     Safely close the DAF.
      !>C
      !>      CALL DAFCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        1        0        1        2      641   310404
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        2        0        1        2   310405   423048
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        3        0        1        2   423049   567372
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        4        0        1        2   567373   628976
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        5        0        1        2   628977   674740
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        6        0        1        2   674741   715224
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        7        0        1        2   715225   750428
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        8        0        1        2   750429   785632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:        9        0        1        2   785633   820836
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:       10        0        1        2   820837   944040
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      301        3        1        2   944041  1521324
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      399        3        1        2  1521325  2098608
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      199        1        1        2  2098609  2098620
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      299        2        1        2  2098621  2098632
      !>         Doubles:  -3169195200.0000000        1696852800.0000000
      !>        Integers:      499        4        1        2  2098633  2098644
      !>
      !>        Note, the final entries in the integer array contains the
      !>        segment start/end indexes. The output indicates the search
      !>        proceeded from the start of the file (low value index) towards
      !>        the end (high value index).
      !>
      !>## Restrictions
      !>
      !>     1)  Calls that do or may change DAF addresses of DAF summaries,
      !>         names, or data of a given DAF file should not be made during
      !>         a search of that file initiated by either DAFBFS or DAFBBS.
      !>         No such changes should be made between the start of a search
      !>         and calls to any entry point that reads or writes to the
      !>         summary of the "current array" found by that search, or
      !>         that returns a "found" flag indicating whether the current
      !>         array exists.
      !>
      !>         Changing the size of the comment area while a search is in
      !>         progress can invalidate record numbers stored in local data
      !>         structures of this routine. This can cause corrupted array
      !>         summaries and names to be returned upon read access and file
      !>         corruption to occur upon write access.
      !>
      !>         Adding arrays (aka "segments") while either a forward or
      !>         backward search is in progress can cause the search to miss
      !>         the new segments.
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
      !>-    SPICELIB Version 2.0.3, 26-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added undeclared variables to code example.
      !>
      !>        Fixed typo in $Exceptions entry #3: DAFFPA is used to find the
      !>        previous array, not the non existing API DAFBNA.
      !>
      !>        Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 2.0.2, 10-OCT-2012 (EDW)
      !>
      !>        Added a functional code example to the $Examples section.
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to support simultaneous searches of multiple DAFs.
      !>        Bug fix made to handle case of having no current array.
      !>
      !>        This routine now operates on the current DAF---the one at
      !>        the head of the active list. All saved state variables
      !>        used by this routine are now part of the state table, or
      !>        its associated set of pointers.
      !>
      !>        In addition, this routine now checks whether an array
      !>        is current before trying to read its summary. The routine
      !>        previously crashed under these conditions.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafgs ( ctx, sum )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(out) :: sum    ( * )  !! Summary for current array.
      integer(ip) :: offset
      integer(ip) :: sumsiz
      integer(ip) :: p
      integer(ip) :: nd
      integer(ip) :: ni
      if ( .not. allocated ( ctx%daffa ) ) then
         allocate ( ctx%daffa )
         call init_daffa_state ( ctx%daffa )
      end if
      associate ( stfh => ctx%daffa%stfh, &
                  stnseg => ctx%daffa%stnseg, &
                  stcurr => ctx%daffa%stcurr, &
                  stsr => ctx%daffa%stsr, &
                  sthead => ctx%daffa%sthead )
      if ( ctx%failed() ) return

!
!     Operate on the last DAF in which a search has been started.
!
      p  =  sthead

!
!     Make sure that a search has been started in this DAF.
!
      if ( p == nil ) then

         call ctx%setmsg ( 'No DAF is currently being searched.' )
         call ctx%sigerr ( 'SPICE(DAFNOSEARCH)'                  )
         return

!
!     Make sure that the `current' DAF is still open.
!
      else

         call dafsih ( ctx, stfh(p), 'READ' )

         if ( ctx%failed() ) return

      end if

!
!     Check the current pointer position to make sure that it's in
!     bounds. If there is no current array, then we cannot return
!     a summary. This situation occurs if DAFFNA was called when the
!     current array was the last, or if DAFFPA was called when the
!     current array was the first.
!
      if ( stcurr(p) == 0 ) then

         call ctx%setmsg ( 'No array is current; the `next'' array is '   // &
                       'the first array of DAF #'                      )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(NOCURRENTARRAY)'                         )
         return

      else if ( stcurr(p) > stnseg(p) ) then

         call ctx%setmsg ( 'No array is current; the `previous'' array is'// &
                       ' the last array of DAF #'                      )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(NOCURRENTARRAY)'                         )
         return

      end if

!
!     The location of the summary depends on the current pointer
!     position.
!
      call dafhsf ( ctx, stfh(p), nd, ni )

      sumsiz  =  nd   +   ( ni + 1 ) / 2

      offset  =  3    +   ( stcurr(p) - 1 )  *  sumsiz

      sum(1:sumsiz) = stsr(offset+1:offset+1+sumsiz-1,p)

      return
      end associate
      end subroutine dafgs

      !> Return (get) the name for the current array in the current DAF.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     NAME     is the name for the current array (the array found by the
      !>              latest call to DAFFNA or DAFFPA).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If this routine is called when no search is in progress in the
      !>         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
      !>
      !>     2)  If the DAF for which the "current" array's name is to be
      !>         returned has actually been closed, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     3)  If no array is current in the current DAF, the error
      !>         SPICE(NOCURRENTARRAY) is signaled. There is no current array
      !>         when a search is started by DAFBFS or DAFBBS, but no calls to
      !>         DAFFNA or DAFFPA have been made yet, or whenever DAFFNA or
      !>         DAFFPA return the value .FALSE. in the FOUND argument.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See DAFFA.
      !>
      !>## Examples
      !>
      !>     See $Examples in DAFFA.
      !>
      !>## Restrictions
      !>
      !>     1)  Calls that do or may change DAF addresses of DAF summaries,
      !>         names, or data of a given DAF file should not be made during
      !>         a search of that file initiated by either DAFBFS or DAFBBS.
      !>         No such changes should be made between the start of a search
      !>         and calls to any entry point that reads or writes to the
      !>         summary of the "current array" found by that search, or
      !>         that returns a "found" flag indicating whether the current
      !>         array exists.
      !>
      !>         Changing the size of the comment area while a search is in
      !>         progress can invalidate record numbers stored in local data
      !>         structures of this routine. This can cause corrupted array
      !>         summaries and names to be returned upon read access and file
      !>         corruption to occur upon write access.
      !>
      !>         Adding arrays (aka "segments") while either a forward or
      !>         backward search is in progress can cause the search to miss
      !>         the new segments.
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
      !>-    SPICELIB Version 2.0.3, 26-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Fixed typo in $Exceptions entry #3: DAFFPA is used to find the
      !>        previous array, not the non existing API DAFBNA.
      !>
      !>        Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to support simultaneous searches of multiple DAFs.
      !>        Bug fix made to handle case of having no current array.
      !>
      !>        This routine now operates on the current DAF---the one at
      !>        the head of the active list. All saved state variables
      !>        used by this routine are now part of the state table, or
      !>        its associated set of pointers.
      !>
      !>        In addition, this routine now checks whether an array
      !>        is current before trying to read its summary. The routine
      !>        previously crashed under these conditions.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafgn ( ctx, name )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: name  !! Name of current array.
      integer(ip) :: offset
      integer(ip) :: sumsiz
      integer(ip) :: p
      integer(ip) :: namsiz
      integer(ip) :: nd
      integer(ip) :: ni
      if ( .not. allocated ( ctx%daffa ) ) then
         allocate ( ctx%daffa )
         call init_daffa_state ( ctx%daffa )
      end if
      associate ( stfh => ctx%daffa%stfh, &
                  stthis => ctx%daffa%stthis, &
                  stnseg => ctx%daffa%stnseg, &
                  stcurr => ctx%daffa%stcurr, &
                  stnr => ctx%daffa%stnr, &
                  sthvnr => ctx%daffa%sthvnr, &
                  sthead => ctx%daffa%sthead )
      if ( ctx%failed() ) return

!
!     Operate on the last DAF in which a search has been started.
!
      p  =  sthead

!
!     Make sure that a search has been started in this DAF.
!
      if ( p == nil ) then

         call ctx%setmsg ( 'No DAF is currently being searched.' )
         call ctx%sigerr ( 'SPICE(DAFNOSEARCH)'                  )
         return

!
!     Make sure that the `current' DAF is still open.
!
      else

         call dafsih ( ctx, stfh(p), 'READ' )

         if ( ctx%failed() ) return

      end if

!
!     Check the current pointer position to make sure that it's in
!     bounds. If there is no current array, then we cannot get the
!     array's summary's name. This situation occurs if DAFFNA was
!     called when the current array was the last, or if DAFFPA was
!     called when the current array was the first.
!
      if ( stcurr(p) == 0 ) then

         call ctx%setmsg ( 'No array is current; the `next'' array is '   // &
                       'the first array of DAF #'                      )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(NOCURRENTARRAY)'                         )
         return

      else if ( stcurr(p) > stnseg(p) ) then

         call ctx%setmsg ( 'No array is current; the `previous'' array is'// &
                       ' the last array of DAF #'                      )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(NOCURRENTARRAY)'                         )
         return

      end if

!
!     Read the name record for this summary record, if we don't have it
!     already.
!
      if ( .not. sthvnr(p) ) then

         call dafrcr ( ctx, stfh(p), stthis(p)+1, stnr(p) )

         sthvnr( p )  =  .true.

      end if

!
!     The location of the name depends on the current pointer
!     position.
!
      call dafhsf ( ctx, stfh(p), nd, ni )

      sumsiz  =  nd   +   ( ni + 1 ) / 2

      namsiz  =  sumsiz * 8

      offset  =  ( stcurr(p) - 1 )  *  namsiz

      name    =  stnr(p) (  offset + 1  :  offset + namsiz  )

      return
      end associate
      end subroutine dafgn

      !> Return (get) the handle of the DAF currently being searched.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the handle for the current DAF (the handle connected
      !>              to the DAF that is currently being actively searched).
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If this routine is called when no search is in progress in the
      !>         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
      !>
      !>     2)  If the DAF whose handle is to be returned has actually been
      !>         closed, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>## Files
      !>
      !>     This routine returns the handle of a DAF that is currently
      !>     being searched.
      !>
      !>## Particulars
      !>
      !>     Under rare circumstances, it may be necessary to identify the
      !>     particular DAF that is being searched (such as when the search is
      !>     begun by one module and continued by another).
      !>
      !>## Examples
      !>
      !>     Consider a program like the following, which examines the
      !>     individual arrays in a DAF and examines the contents of those
      !>     meeting certain criteria.
      !>
      !>        CALL DAFOPW ( FNAME, HANDLE )
      !>        CALL DAFBFS ( HANDLE )
      !>        CALL DAFFNA ( FOUND  )
      !>
      !>        DO WHILE ( FOUND )
      !>
      !>           CALL CHECK_DAF ( STATUS )
      !>
      !>           IF ( STATUS .EQ. 'EXAMINE' ) THEN
      !>              CALL EXAMINE_DAF
      !>           END IF
      !>
      !>           CALL DAFFNA ( FOUND )
      !>
      !>        END DO
      !>
      !>     The subroutine CHECK_DAF, which assumes that a search is in
      !>     progress, gets the summary and name for the current array, and
      !>     uses them to decide whether the data in the array merit further
      !>     consideration.
      !>
      !>        SUBROUTINE CHECK_DAF ( STATUS )
      !>
      !>        CALL DAFGS ( SUM )
      !>        CALL DAFGN ( NAME )
      !>        CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>         .
      !>         .
      !>
      !>     The subroutine EXAMINE_DAF needs to examine the data in
      !>     the array itself. In order to do do, it needs to have access
      !>     not only to the summary, but to the handle of the file
      !>     containing the array. This is provided by DAFGH.
      !>
      !>        SUBROUTINE EXAMINE_DAF
      !>
      !>        CALL DAFGS ( SUM  )
      !>        CALL DAFGH ( HANDLE )
      !>        CALL DAFUS ( SUM, ND, NI, DC, IC )
      !>
      !>        CALL DAFGDA ( HANDLE, BEGIN, END, DATA )
      !>         .
      !>         .
      !>
      !>## Restrictions
      !>
      !>     1)  Calls that do or may change DAF addresses of DAF summaries,
      !>         names, or data of a given DAF file should not be made during
      !>         a search of that file initiated by either DAFBFS or DAFBBS.
      !>         No such changes should be made between the start of a search
      !>         and calls to any entry point that reads or writes to the
      !>         summary of the "current array" found by that search, or
      !>         that returns a "found" flag indicating whether the current
      !>         array exists.
      !>
      !>         Changing the size of the comment area while a search is in
      !>         progress can invalidate record numbers stored in local data
      !>         structures of this routine. This can cause corrupted array
      !>         summaries and names to be returned upon read access and file
      !>         corruption to occur upon write access.
      !>
      !>         Adding arrays (aka "segments") while either a forward or
      !>         backward search is in progress can cause the search to miss
      !>         the new segments.
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
      !>-    SPICELIB Version 2.0.4, 26-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard. Extended $Files
      !>        section.
      !>
      !>        Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 2.0.3, 14-MAR-2017 (NJB)
      !>
      !>        Updated header example: changed DAFRDA call to DAFGDA call.
      !>
      !>-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to support simultaneous searches of multiple DAFs.
      !>
      !>        This routine now operates on the current DAF---the one at
      !>        the head of the active list. All saved state variables
      !>        used by this routine are now part of the state table, or
      !>        its associated set of pointers.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafgh ( ctx, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: handle  !! Handle for current DAF.
      integer(ip) :: p
      if ( .not. allocated ( ctx%daffa ) ) then
         allocate ( ctx%daffa )
         call init_daffa_state ( ctx%daffa )
      end if
      associate ( stfh => ctx%daffa%stfh, &
                  sthead => ctx%daffa%sthead )
      if ( ctx%failed() ) return

!
!     Operate on the last DAF in which a search has been started.
!
      p  =  sthead

!
!     Make sure that a search has been started in this DAF.
!
      if ( p == nil ) then

         call ctx%setmsg ( 'No DAF is currently being searched.' )
         call ctx%sigerr ( 'SPICE(DAFNOSEARCH)'                  )
         return

!
!     Make sure that the `current' DAF is still open.
!
      else

         call dafsih ( ctx, stfh(p), 'READ' )

         if ( ctx%failed() ) return

      end if

      handle = stfh( p )

      return
      end associate
      end subroutine dafgh

      !> Change the summary for the current array in the current DAF.
      !>
      !>## Detailed Input
      !>
      !>     SUM      is the new summary for the current array. This
      !>              replaces the existing summary. However, the addresses
      !>              (the final two integer components) of the original
      !>              summary are not changed.
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
      !>     1)  If this routine is called when no search is in progress in the
      !>         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
      !>
      !>     2)  If the DAF containing the "current" array has actually been
      !>         closed, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     3)  If the DAF containing the "current" array is not open for
      !>         writing, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     4)  If no array is current in the current DAF, the error
      !>         SPICE(NOCURRENTARRAY) is signaled. There is no current array
      !>         when a search is started by DAFBFS or DAFBBS, but no calls to
      !>         DAFFNA or DAFFPA have been made yet, or whenever DAFFNA or
      !>         DAFFPA return the value .FALSE. in the FOUND argument.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See DAFFA.
      !>
      !>## Examples
      !>
      !>     See $Examples in DAFFA.
      !>
      !>## Restrictions
      !>
      !>     1)  Calls that do or may change DAF addresses of DAF summaries,
      !>         names, or data of a given DAF file should not be made during
      !>         a search of that file initiated by either DAFBFS or DAFBBS.
      !>         No such changes should be made between the start of a search
      !>         and calls to any entry point that reads or writes to the
      !>         summary of the "current array" found by that search, or
      !>         that returns a "found" flag indicating whether the current
      !>         array exists.
      !>
      !>         Changing the size of the comment area while a search is in
      !>         progress can invalidate record numbers stored in local data
      !>         structures of this routine. This can cause corrupted array
      !>         summaries and names to be returned upon read access and file
      !>         corruption to occur upon write access.
      !>
      !>         Adding arrays (aka "segments") while either a forward or
      !>         backward search is in progress can cause the search to miss
      !>         the new segments.
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
      !>-    SPICELIB Version 2.0.3, 27-OCT-2021 (JDR) (NJB)
      !>
      !>        Fixed typo in the $Declarations section.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Fixed typo in $Exceptions entry #4: DAFFPA is used to find the
      !>        previous array, not the non existing API DAFBNA.
      !>
      !>        Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to support simultaneous searches of multiple DAFs.
      !>        Bug fix made to handle case of having no current array.
      !>
      !>        This routine now operates on the current DAF---the one at
      !>        the head of the active list. All saved state variables
      !>        used by this routine are now part of the state table, or
      !>        its associated set of pointers.
      !>
      !>        In addition, this routine now checks whether an array
      !>        is current before trying to read its summary. The routine
      !>        previously crashed under these conditions.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafrs ( ctx, sum )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: sum    ( * )  !! New summary for current array.
      integer(ip) :: offset
      integer(ip) :: sumsiz
      integer(ip) :: p
      integer(ip) :: exic   ( maxnic )
      real(wp) :: newdc  ( maxndc )
      integer(ip) :: nd
      real(wp) :: exdc   ( maxndc )
      integer(ip) :: ni
      real(wp) :: exsum  ( maxndc )
      integer(ip) :: newic  ( maxnic )
      if ( .not. allocated ( ctx%daffa ) ) then
         allocate ( ctx%daffa )
         call init_daffa_state ( ctx%daffa )
      end if
      associate ( stfh => ctx%daffa%stfh, &
                  stthis => ctx%daffa%stthis, &
                  stnseg => ctx%daffa%stnseg, &
                  stcurr => ctx%daffa%stcurr, &
                  stsr => ctx%daffa%stsr, &
                  sthead => ctx%daffa%sthead )
      if ( ctx%failed() ) return

!
!     Operate on the last DAF in which a search has been started.
!
      p  =  sthead

!
!     Make sure that a search has been started in this DAF.
!
      if ( p == nil ) then

         call ctx%setmsg ( 'No DAF is currently being searched.' )
         call ctx%sigerr ( 'SPICE(DAFNOSEARCH)'                  )
         return

!
!     Make sure that the `current' DAF is still open, and that it
!     is open for writing.
!
      else

         call dafsih ( ctx, stfh(p), 'WRITE' )

         if ( ctx%failed() ) return

      end if

!
!     Check the current pointer position to make sure that it's in
!     bounds. If there is no current array, then we cannot replace the
!     array's  summary. This situation occurs if DAFFNA was called
!     when the current array was the last, or if DAFFPA was called when
!     the current array was the first.
!
      if ( stcurr(p) == 0 ) then

         call ctx%setmsg ( 'No array is current; the `next'' array is '   // &
                       'the first array of DAF #'                      )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(NOCURRENTARRAY)'                         )
         return

      else if ( stcurr(p) > stnseg(p) ) then

         call ctx%setmsg ( 'No array is current; the `previous'' array is'// &
                       ' the last array of DAF #'                      )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(NOCURRENTARRAY)'                         )
         return

      end if

!
!     The location of the summary depends on the current pointer
!     position.
!
      call dafhsf ( ctx, stfh(p), nd, ni )

      sumsiz  =  nd   +   ( ni + 1 ) / 2

      offset  =  3    +   ( stcurr(p) - 1 ) * sumsiz

!
!     Get the existing summary, and unpack it. Replace everything
!     but the addresses (the final two integer components), and
!     repack. Then replace the existing summary within the record.
!
      exsum(1:sumsiz) = stsr(offset+1:offset+1+sumsiz-1,p)

      call dafus ( ctx, exsum, nd, ni, exdc,  exic  )
      call dafus ( ctx, sum,   nd, ni, newdc, newic )

      exdc(1:nd) = newdc(1:nd)
      exic(1:ni-2) = newic(1:ni-2)
      call dafps ( ctx, nd, ni, exdc, exic, exsum )

      stsr(offset+1:offset+1+sumsiz-1,p) = exsum(1:sumsiz)

!
!     Rewrite the modified summary record.
!
      call dafwdr ( ctx, stfh(p), stthis(p), stsr(1,p) )

      return
      end associate
      end subroutine dafrs

      !> Replace the name for the current array in the current DAF.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the new name for the current array.
      !>              This replaces the existing name.
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
      !>     1)  If this routine is called when no search is in progress in the
      !>         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
      !>
      !>     2)  If the DAF containing the "current" array has actually been
      !>         closed, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     3)  If the DAF containing the "current" array is not open for
      !>         writing, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     4)  If no array is current in the current DAF, the error
      !>         SPICE(NOCURRENTARRAY) is signaled. There is no current
      !>         array when a search is started by DAFBFS or DAFBBS, but no
      !>         calls to DAFFNA or DAFBNA have been made yet, or whenever
      !>         DAFFNA or DAFFPA return the value .FALSE. in the FOUND
      !>         argument.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See DAFFA.
      !>
      !>## Examples
      !>
      !>     See $Examples in DAFFA.
      !>
      !>## Restrictions
      !>
      !>     1)  Calls that do or may change DAF addresses of DAF summaries,
      !>         names, or data of a given DAF file should not be made during
      !>         a search of that file initiated by either DAFBFS or DAFBBS.
      !>         No such changes should be made between the start of a search
      !>         and calls to any entry point that reads or writes to the
      !>         summary of the "current array" found by that search, or
      !>         that returns a "found" flag indicating whether the current
      !>         array exists.
      !>
      !>         Changing the size of the comment area while a search is in
      !>         progress can invalidate record numbers stored in local data
      !>         structures of this routine. This can cause corrupted array
      !>         summaries and names to be returned upon read access and file
      !>         corruption to occur upon write access.
      !>
      !>         Adding arrays (aka "segments") while either a forward or
      !>         backward search is in progress can cause the search to miss
      !>         the new segments.
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
      !>-    SPICELIB Version 2.0.3, 26-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to support simultaneous searches of multiple DAFs.
      !>
      !>        This routine now operates on the current DAF---the one at
      !>        the head of the active list. All saved state variables
      !>        used by this routine are now part of the state table, or
      !>        its associated set of pointers.
      !>
      !>        In addition, this routine now checks whether an array
      !>        is current before trying to read its summary. The routine
      !>        previously crashed under these conditions.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafrn ( ctx, name )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! New name for current array.
      integer(ip) :: offset
      integer(ip) :: sumsiz
      integer(ip) :: p
      integer(ip) :: namsiz
      integer(ip) :: nd
      integer(ip) :: ni
      if ( .not. allocated ( ctx%daffa ) ) then
         allocate ( ctx%daffa )
         call init_daffa_state ( ctx%daffa )
      end if
      associate ( stfh => ctx%daffa%stfh, &
                  stthis => ctx%daffa%stthis, &
                  stnseg => ctx%daffa%stnseg, &
                  stcurr => ctx%daffa%stcurr, &
                  stnr => ctx%daffa%stnr, &
                  sthvnr => ctx%daffa%sthvnr, &
                  sthead => ctx%daffa%sthead )
      if ( ctx%failed() ) return

!
!     Operate on the last DAF in which a search has been started.
!
      p  =  sthead

!
!     Make sure that a search has been started in this DAF.
!
      if ( p == nil ) then

         call ctx%setmsg ( 'No DAF is currently being searched.' )
         call ctx%sigerr ( 'SPICE(DAFNOSEARCH)'                  )
         return

!
!     Make sure that the `current' DAF is still open, and that it
!     is open for writing.
!
      else

         call dafsih ( ctx, stfh(p), 'WRITE' )

         if ( ctx%failed() ) return

      end if

!
!     Check the current pointer position to make sure that it's in
!     bounds. If there is no current array, then we cannot replace
!     the array's summary's name. This situation occurs if DAFFNA was
!     called when the current array was the last, or if DAFFPA was
!     called when the current array was the first.
!
      if ( stcurr(p) == 0 ) then

         call ctx%setmsg ( 'No array is current; the `next'' array is '   // &
                       'the first array of DAF #'                      )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(NOCURRENTARRAY)'                         )
         return

      else if ( stcurr(p) > stnseg(p) ) then

         call ctx%setmsg ( 'No array is current; the `previous'' array is'// &
                       ' the last array of DAF #'                      )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(NOCURRENTARRAY)'                         )
         return

      end if

!
!     Read the name record for this summary record, if we don't have it
!     already.
!
      if ( .not. sthvnr(p) ) then

         call dafrcr ( ctx, stfh(p), stthis(p)+1, stnr(p) )

         sthvnr( p )  =  .true.

      end if

!
!     The location of the name depends on the current pointer
!     position.
!
      call dafhsf ( ctx, stfh(p), nd, ni )

      sumsiz  =  nd   +   ( ni + 1 ) / 2

      namsiz  =  sumsiz * 8

      offset  =  ( stcurr(p) - 1 )  *  namsiz

      stnr(p) (  offset + 1  :  offset + namsiz  )   =   name

!
!     Rewrite the character record.
!
      call dafwcr ( ctx, stfh(p), stthis(p)+1, stnr(p) )

      return
      end associate
      end subroutine dafrn

      !> Write a new summary for the current array in the current DAF.
      !>
      !>## Detailed Input
      !>
      !>     SUM      is the new summary for the current array. This
      !>              replaces the existing summary, including the
      !>              addresses (the final two integer components) of
      !>              the original summary.
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
      !>     1)  If this routine is called when no search is in progress in the
      !>         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
      !>
      !>     2)  If the DAF containing the "current" array has actually been
      !>         closed, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     3)  If the DAF containing the "current" array is not open for
      !>         writing, an error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     4)  If no array is current in the current DAF, the error
      !>         SPICE(NOCURRENTARRAY) is signaled. There is no current
      !>         array when a search is started by DAFBFS or DAFBBS, but no
      !>         calls to DAFFNA or DAFBNA have been made yet, or whenever
      !>         DAFFNA or DAFFPA return the value .FALSE. in the FOUND
      !>         argument.
      !>
      !>## Files
      !>
      !>     DAFWS updates the DAF currently being searched. The handle
      !>     of this DAF can be retrieved using the routine DAFGH.
      !>
      !>## Particulars
      !>
      !>     Unless you are reordering the arrays in the file being searched,
      !>     you should be using DAFRS instead of this routine.
      !>
      !>     See also DAFFA, DAFRS.
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     1)  Calls that do or may change DAF addresses of DAF summaries,
      !>         names, or data of a given DAF file should not be made during
      !>         a search of that file initiated by either DAFBFS or DAFBBS.
      !>         No such changes should be made between the start of a search
      !>         and calls to any entry point that reads or writes to the
      !>         summary of the "current array" found by that search, or
      !>         that returns a "found" flag indicating whether the current
      !>         array exists.
      !>
      !>         Changing the size of the comment area while a search is in
      !>         progress can invalidate record numbers stored in local data
      !>         structures of this routine. This can cause corrupted array
      !>         summaries and names to be returned upon read access and file
      !>         corruption to occur upon write access.
      !>
      !>         Adding arrays (aka "segments") while either a forward or
      !>         backward search is in progress can cause the search to miss
      !>         the new segments.
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
      !>-    SPICELIB Version 2.0.3, 27-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 2.0.2, 18-AUG-2011 (EDW)
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>        Removed the obsolete Reference citation to "NAIF
      !>        Document 167.0."
      !>
      !>-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT)
      !>
      !>        Updated to support simultaneous searches of multiple DAFs.
      !>        Bug fix made to handle case of having no current array.
      !>
      !>        This routine now operates on the current DAF---the one at
      !>        the head of the active list. All saved state variables
      !>        used by this routine are now part of the state table, or
      !>        its associated set of pointers.
      !>
      !>        In addition, this routine now checks whether an array
      !>        is current before trying to read its summary. The routine
      !>        previously crashed under these conditions.
      !>
      !>-    SPICELIB Version 1.0.0, 28-MAR-1991 (IMU)
      module subroutine dafws ( ctx, sum )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: sum    ( * )  !! New summary for current array in the current DAF.
      integer(ip) :: offset
      integer(ip) :: sumsiz
      integer(ip) :: p
      integer(ip) :: nd
      integer(ip) :: ni
      if ( .not. allocated ( ctx%daffa ) ) then
         allocate ( ctx%daffa )
         call init_daffa_state ( ctx%daffa )
      end if
      associate ( stfh => ctx%daffa%stfh, &
                  stthis => ctx%daffa%stthis, &
                  stnseg => ctx%daffa%stnseg, &
                  stcurr => ctx%daffa%stcurr, &
                  stsr => ctx%daffa%stsr, &
                  sthead => ctx%daffa%sthead )
      if ( ctx%failed() ) return

!
!     Operate on the last DAF in which a search has been started.
!
      p  =  sthead

!
!     Make sure that a search has been started in this DAF.
!
      if ( p == nil ) then

         call ctx%setmsg ( 'No DAF is currently being searched.' )
         call ctx%sigerr ( 'SPICE(DAFNOSEARCH)'                  )
         return

!
!     Make sure that the `current' DAF is still open, and that it is
!     open for writing.
!
      else

         call dafsih ( ctx, stfh(p), 'READ' )

         if ( ctx%failed() ) return

      end if

!
!     Check the current pointer position to make sure that it's in
!     bounds. If there is no current array, then we cannot write a
!     new array summary. This situation occurs if DAFFNA was called
!     when the current array was the last, or if DAFFPA was called
!     when the current array was the first.
!
      if ( stcurr(p) == 0 ) then

         call ctx%setmsg ( 'No array is current; the `next'' array is '   // &
                       'the first array of DAF #'                      )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(NOCURRENTARRAY)'                         )
         return

      else if ( stcurr(p) > stnseg(p) ) then

         call ctx%setmsg ( 'No array is current; the `previous'' array is'// &
                       ' the last array of DAF #'                      )
         call ctx%errhan ( '#', stfh(p) )
         call ctx%sigerr ( 'SPICE(NOCURRENTARRAY)'                         )
         return

      end if

!
!     The location of the summary depends on the current pointer
!     position.
!
      call dafhsf ( ctx, stfh(p), nd, ni )

      sumsiz  =  nd   +   ( ni + 1 ) / 2

      offset  =  3    +   ( stcurr(p) - 1 ) * sumsiz

      stsr(offset+1:offset+1+sumsiz-1,p) = sum(1:sumsiz)

!
!     Rewrite the modified summary record.
!
      call dafwdr ( ctx, stfh(p), stthis(p), stsr(1,p) )

      return
      end associate
      end subroutine dafws

      !> Select a DAF that already has a search in progress as the
      !> one to continue searching.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of a DAF in which either a forward
      !>              or backward search has already been started by
      !>              DAFBFS or DAFBBS. The DAF may be open for read
      !>              or write access.
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
      !>     1)  If the input handle is invalid, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     2)  If this routine is called when no search is in progress in the
      !>         the current DAF, the error SPICE(DAFNOSEARCH) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     DAFCS supports simultaneous searching of multiple DAFs. In
      !>     applications that use this capability, DAFCS should be called
      !>     prior to each call to DAFFNA, DAFFPA, DAFGN, DAFGS, DAFRS, or
      !>     DAFWS, to specify which DAF is to be acted upon.
      !>
      !>## Examples
      !>
      !>     See $Examples in DAFFA.
      !>
      !>## Restrictions
      !>
      !>     1)  Calls that do or may change DAF addresses of DAF summaries,
      !>         names, or data of a given DAF file should not be made during
      !>         a search of that file initiated by either DAFBFS or DAFBBS.
      !>         No such changes should be made between the start of a search
      !>         and calls to any entry point that reads or writes to the
      !>         summary of the "current array" found by that search, or
      !>         that returns a "found" flag indicating whether the current
      !>         array exists.
      !>
      !>         Changing the size of the comment area while a search is in
      !>         progress can invalidate record numbers stored in local data
      !>         structures of this routine. This can cause corrupted array
      !>         summaries and names to be returned upon read access and file
      !>         corruption to occur upon write access.
      !>
      !>         Adding arrays (aka "segments") while either a forward or
      !>         backward search is in progress can cause the search to miss
      !>         the new segments.
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
      !>-    SPICELIB Version 1.0.3, 26-OCT-2021 (JDR) (NJB)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Updated $Restrictions section.
      !>
      !>-    SPICELIB Version 1.0.2, 10-FEB-2014 (BVS)
      !>
      !>        Added full declaration of HANDLE to the $Declarations section
      !>        of the header.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 04-SEP-1991 (NJB) (WLT)
      module subroutine dafcs ( ctx, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of DAF to continue searching.
      integer(ip) :: prev
      integer(ip) :: p
      logical :: fnd
      if ( .not. allocated ( ctx%daffa ) ) then
         allocate ( ctx%daffa )
         call init_daffa_state ( ctx%daffa )
      end if
      associate ( stfh => ctx%daffa%stfh, &
                  stpool => ctx%daffa%stpool, &
                  sthead => ctx%daffa%sthead )
      if ( ctx%failed() ) return

!
!     Validate the DAF's handle before going any further. DAFSIH will
!     signal an error if HANDLE doesn't designate an open DAF.
!
      call dafsih ( ctx, handle, 'READ' )

      if ( ctx%failed() ) return

!
!     See whether we already have an entry for this DAF in the
!     state table. Find the previous node if possible.
!
      p     =   sthead
      prev  =   nil
      fnd   =  .false.

      do while (  ( p /= nil )  .and.  ( .not. fnd )  )

         if ( stfh(p) == handle ) then
            fnd   =  .true.
         else
            prev  =  p
            p     =  stpool( p )
         end if

      end do

!
!     Either FND is false, or P is the index in the state table of
!     the DAF specified by HANDLE, and PREV is the predecessor of P.
!

!
!     You can't continue searching a DAF that you're not already
!     searching.
!
      if ( .not. fnd ) then

         call ctx%setmsg ( 'No DAF is currently being searched.' )
         call ctx%sigerr ( 'SPICE(DAFNOSEARCH)'                  )
         return

      end if

!
!     Move the node for this DAF to the head of the active list,
!     if it is not already there:
!
!        - Make the predecessor of P point to the successor of P.
!
!        - Make P point to the head of the active list.
!
!        - Make P the active list head node.
!

      if ( p /= sthead ) then
!
!        P is in the active list, but is not at the head. So,
!        the predecessor of P is not NIL.
!
         stpool(prev)  =  stpool(p)
         stpool(p)     =  sthead
         sthead        =  p

      end if

      return
      end associate
      end subroutine dafcs

      !> Convert the contents of a binary DAF file to an equivalent DAF
      !> transfer file.
      !>
      !>## Detailed Input
      !>
      !>     BINFIL   is the name of a binary DAF file which is to be converted
      !>              to an equivalent DAF transfer file.
      !>
      !>     XFRLUN   is the Fortran logical unit number of a previously opened
      !>              file. The DAF transfer file will be written to the
      !>              file attached to this logical unit beginning at the
      !>              current position in the file.
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
      !>     1)  If the binary DAF file specified by the filename BINFIL cannot
      !>         be opened for read access, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     2)  If for some reason the DAF transfer file cannot be written
      !>         to, the error SPICE(FILEWRITEFAILED) is signaled.
      !>
      !>     3)  If, for any reason, the DAF file cannot be read, an error is
      !>         signaled by a routine in the call tree of this routine.
      !>
      !>     4)  If the ID word cannot be read from the binary file, the error
      !>         SPICE(FILEREADFAILED) is signaled.
      !>
      !>     5)  The binary DAF file opened by this routine, BINFIL, is only
      !>         GUARANTEED to be closed upon successful completion of the
      !>         conversion process. In the event of an error, the caller of
      !>         this routine is required to close the binary DAF file BINFIL.
      !>
      !>## Files
      !>
      !>     See arguments BINFIL, XFRLUN.
      !>
      !>## Particulars
      !>
      !>     Any binary DAF file may be transferred between heterogeneous
      !>     Fortran environments by converting it to an equivalent file
      !>     containing only ASCII characters. Such a file can be transferred
      !>     almost universally, using any number of established protocols.
      !>     Once transferred, the ASCII file can be converted to a binary
      !>     file, using the representations native to the new host
      !>     environment.
      !>
      !>     This routine provides a mechanism for converting a binary DAF
      !>     file into an equivalent encoded ASCII file called a DAF transfer
      !>     file. It is one of a pair of routines for performing conversions
      !>     between the binary format of a DAF file and the DAF transfer file.
      !>     The inverse of this routine is the routine DAFTB.
      !>
      !>     The contents of the reserved records in a binary DAF file are
      !>     ignored by this routine. They are not written to the DAF transfer
      !>     file. The reserved records must be dealt with separately from the
      !>     data in a DAF file.
      !>
      !>     Upon successful completion, the DAF transfer file attached to
      !>     Fortran logical unit XFRLUN will contain the same data as the
      !>     binary DAF file BINFIL. The binary DAF file BINFIL will be closed
      !>     when this routine exits. The DAF transfer file will remain open,
      !>     as it was on entry, and it will be positioned to write on the
      !>     first line following the encoded DAF data.
      !>
      !>## Examples
      !>
      !>     Let
      !>
      !>        BINFIL   be the name of a binary DAF file which is to be
      !>                 converted to an equivalent DAF transfer file.
      !>
      !>        XFRLUN   be the Fortran logical unit to which the DAF transfer
      !>                 file is to be written.
      !>
      !>     The following subroutine call would read the binary DAF
      !>     file with the name BINFIL, convert its data into an encoded
      !>     format, and write that data to the DAF transfer file attached
      !>     to the Fortran logical unit XFRLUN, beginning at the current
      !>     position in the file.
      !>
      !>        CALL DAFBT( BINFIL, XFRLUN )
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
      !>     F.S. Turner        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 4.0.0, 16-NOV-2001 (FST)
      !>
      !>        Updated the routine to utilize the new handle manager
      !>        interfaces.
      !>
      !>-    SPICELIB Version 3.0.0, 25-JAN-1995 (KRG)
      !>
      !>        Updated the header and in line comments to reflect the change
      !>        from calling files text files to calling them transfer files.
      !>
      !>        Changed the variable name TXTLUN to XFRLUN to make it
      !>        compatible with the change in terminology.
      !>
      !>-    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG)
      !>
      !>        No changes to this routine were necessary to incorporate the
      !>        new file ID word format. This routine already read and copied
      !>        the ID word to the text file being created.
      !>
      !>        Also, all list directed writes in this routine were replaced by
      !>        formatted writes with FMT = '(A)'. This routine only writes
      !>        character data.
      !>
      !>        Added a test of FAILED() after the call to DAFHLU for
      !>        completeness.
      !>
      !>-    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG)
      !>
      !>        Modified the description of the DAF encoded text file format
      !>        appearing before the program code.
      !>
      !>-    SPICELIB Version 1.0.0, 29-OCT-1992 (KRG)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.0.0, 16-NOV-2001 (FST)
      !>
      !>        This routine still uses a naked READ to retrieve the
      !>        file IDWORD from the first 8 characters stored in the
      !>        file record. It may be that future environments
      !>        will have characters whose storage exceeds 1 byte,
      !>        in which case this routine will require modification.
      !>        One possibility is to call the private file record
      !>        reader ZZDAFGFR, which must address the translation
      !>        for all supported non-native binary file formats on this
      !>        platform.
      !>
      !>        The existing call to DAFHLU was replaced with ZZDDHHLU.
      !>        The call to DAFRDA was replaced with a call to the new,
      !>        translation-aware routine DAFGDA.
      !>
      !>-    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG)
      !>
      !>        No changes to this routine were necessary to incorporate the
      !>        new file ID word format. This routine already read and copied
      !>        the ID word to the text file being created.
      !>
      !>        Also, all list directed writes in this routine were replaced by
      !>        formatted writes with FMT = '(A)'. This routine only writes
      !>        character data.
      !>
      !>        Added a test of FAILED() after the call to DAFHLU for
      !>        completeness.
      !>
      !>-    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG)
      !>
      !>        Modified the description of the DAF encoded text file format
      !>        appearing before the program code. Changed the line:
      !>
      !>           C        < DAF ND value > < DAF NI value >
      !>
      !>        to the lines:
      !>
      !>           C        < DAF ND value >
      !>           C        < DAF NI value >
      !>
      !>        This change was necessary because the output format for the
      !>        low level routines which encode and write the data were
      !>        modified to fix a problem. See the routines WRENCD and WRENCI
      !>        for details of the modification.
      module subroutine dafbt ( ctx, binfil, xfrlun )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: rtrim, repmi_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: binfil  !! The name of a binary DAF file to be converted.
      integer(ip), intent(in) :: xfrlun  !! Logical unit of a previously opened file.

      character(*), parameter :: quote = ''''




      character(*), parameter :: ftypid = 'DAFETF'

      character(*), parameter :: infoln = 'NAIF DAF ENCODED TRANSFER FILE'





      integer(ip), parameter :: linlen = 80
      character(idwlen) :: idword
      character(ifnlen) :: ifname
      character(linlen) :: line
      character(crecl) :: name

      real(wp) :: buffer( bufsiz )
      real(wp) :: dsumry( 125    )
      real(wp) :: summry( 125    )

      integer(ip) :: binhdl
      integer(ip) :: binlun
      integer(ip) :: bward
      integer(ip) :: dtabeg
      integer(ip) :: dtacnt
      integer(ip) :: free
      integer(ip) :: fward
      integer(ip) :: iostat
      integer(ip) :: isumry( 250    )
      integer(ip) :: nd
      integer(ip) :: ni
      integer(ip) :: numarr
      integer(ip) :: numdta
      integer(ip) :: numlft
      integer(ip) :: snmlen

      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     A brief description of the DAF transfer file format and its
!     intended use follows. This description is intended to provide a
!     simple ``picture'' of the DAF transfer file format to aid in the
!     understanding of this routine. This description is NOT intended to
!     be a detailed specification of the file format.
!
!     A DAF transfer file contains all of the data from a binary
!     DAF file, except for the reserved record area, in an encoded
!     ASCII format. The file also contains some bookkeeping information
!     for maintaining the integrity of the data. The DAF transfer file
!     format allows the full precision of both integer and floating
!     point numeric data to be maintained in a portable fashion. The DAF
!     transfer file format is intended to provide a reliable and
!     accurate means for porting data among multiple computer systems
!     and for the archival storage of data.
!
!     A DAF transfer file is not intended to be used directly to
!     provide data to a program, the equivalent binary DAF file is
!     to be used for this purpose. In no way should any program, other
!     than a DAF binary <-> transfer conversion program, rely on the DAF
!     encoded transfer file format.
!
!     To correctly understand the DAF transfer file description
!     the reader should be familiar with the DAF file architecture.
!     Items enclosed in angle brackets, '<' and '>', are used to
!     represent the data which is to be placed at that position in
!     the file. The bookkeeping information is represented exactly
!     as it would appear in a DAF transfer file.
!
!     Let
!
!       BOF    denote the beginning of the file
!       EOF    denote the end of the file
!
!    and
!
!       n      denote the total number of arrays in a DAF file
!       NA(i)  denote the number of double precision numbers in array i
!       m(i)   denote the number of blocks of encoded data for array i
!       N(i,j) denote the number of encoded double precision numbers
!              in block j of array i
!
!     and
!
!              m(i)
!             -----
!             \
!              >   N(i,k) = NA(i),   i = 1, ..., n.
!             /
!             -----
!              k=1
!
!     A DAF encoded transfer file has the following format:
!
!        <BOF>
!        < Information line >
!        < DAF file ID word >
!        < DAF ND value >
!        < DAF NI value >
!        < DAF internal file name >
!        BEGIN_ARRAY 1 NA(1)
!        < Name for array 1 >
!        < ND double precision summary values >
!        < NI-2 integer summary values >
!        N(1,1)
!        < N(1,1) Encoded double precision numbers >
!        N(1,2)
!        < N(1,2) Encoded double precision numbers >
!                          .
!                          .
!                          .
!        N(1,m(1))
!        < N(1,m(1)) Encoded double precision numbers >
!        END_ARRAY 1 NA(1)
!        BEGIN_ARRAY 2 NA(2)
!        < Name for array 2 >
!        < ND double precision summary values >
!        < NI-2 integer summary values >
!        N(2,1)
!        < N(2,1) Encoded double precision numbers >
!        N(2,2)
!        < N(2,2) Encoded double precision numbers >
!                          .
!                          .
!                          .
!        N(2,m(2))
!        < N(2,m(2)) Encoded double precision numbers >
!        END_ARRAY 2 NA(2)
!                          .
!                          .
!                          .
!        BEGIN_ARRAY n NA(n)
!        < Name for array n >
!        < ND double precision summary values >
!        < NI-2 integer summary values >
!        N(n,1)
!        < N(n,1) Encoded double precision numbers >
!        N(n,2)
!        < N(n,2) Encoded double precision numbers >
!                          .
!                          .
!                          .
!        N(n,m(n))
!        < N(n,m(n)) Encoded double precision numbers >
!        END_ARRAY n NA(n)
!        TOTAL_ARRAYS  n
!        <EOF>
!
!     This routine will check the SPICELIB function FAILED() after
!     each call, or consecutive sequence of calls, to data encoding
!     routines, and if an error was signaled it will simply check out
!     and return to the caller.
!
!     This routine will check the SPICELIB function FAILED() after
!     each DAF file access call, and if an error was signaled it will
!     simply return to the caller.
!
!     We begin by opening the binary DAF file specified by BINFIL for
!     read access, obtaining a DAF file handle.
!
      call dafopr( ctx, binfil, binhdl )
!
!     If the open failed, return, as an appropriate error
!     message should have already been set.
!
      if ( ctx%failed() ) return
!
!     At this point, we know that we have a DAF file, because we were
!     able to successfully open it, so we will attempt to proceed with
!     the file conversion process.
!
!     Convert the DAF file handle to its equivalent Fortran logical
!     unit. We need to do this in order to accurately move the file
!     ID word to the DAF transfer file.
!
      call zzddhhlu ( ctx, binhdl, 'DAF', .false., binlun )

!
!     If the translation failed, return, as an appropriate
!     error message should have already been set.
!
      if ( ctx%failed() ) return
!
!     Read the ID word from the binary file. It should be the first 8
!     characters on the first record in the file.
!
      read( binlun,rec=1,iostat=iostat ) idword

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'Error reading the file ID word from the'  // &
                       ' binary DAF file ''#''. IOSTAT = #.'       )
         call ctx%errfnm ( '#', binlun                                 )
         call ctx%errint ( '#', iostat                                 )
         call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                     )
         return

      end if
!
!     Get the contents of the file record: the number of double
!     precision numbers in the summary (ND), the number of integers
!     in the summary (NI), the internal filename (IFNAME), and some
!     data pointer information (FWARD, BWARD, FREE).
!
      call dafrfr ( ctx, binhdl, nd, ni, ifname, fward, bward, free )

      if ( ctx%failed() ) return
!
!     Write the information line containing the file type information
!     for the DAF transfer file format to the current position in the
!     DAF transfer file. The file type information must be the first
!     ``word'' on the information line. The rest of the line may be used
!     for other purposes. Right now, it simply contains an expanded
!     description of the file type information ``word.''
!
      write(xfrlun,fmt='(A)',iostat=iostat) ftypid // ' ' // infoln

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'Error writing to the DAF transfer'  // &
                       ' file ''#''.IOSTAT = #.'             )
         call ctx%errfnm ( '#', xfrlun                           )
         call ctx%errint ( '#', iostat                           )
         call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'              )
         return

       end if
!
!     Write the ID word to the DAF transfer file.
!
      write (xfrlun,fmt='(A)',iostat=iostat) quote // idword // quote

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'Error writing to the DAF transfer'   // &
                       ' file ''#''. IOSTAT = #.'             )
         call ctx%errfnm ( '#', xfrlun                            )
         call ctx%errint ( '#', iostat                            )
         call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'               )
         return

      end if
!
!     Write out the ND and NI values for the DAF file architecture.
!
      isumry(1) = nd
      isumry(2) = ni

      call wrenci( ctx, xfrlun, 2, isumry )

      if ( ctx%failed() ) return
!
!     Write out the internal file name.
!
      write (xfrlun,fmt='(A)',iostat=iostat) quote // ifname // quote

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'Error writing to the DAF transfer'   // &
                       ' file ''#''. IOSTAT = #.'             )
         call ctx%errfnm ( '#', xfrlun                            )
         call ctx%errint ( '#', iostat                            )
         call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'               )
         return

      end if
!
!     Calculate the length of the segment names.
!
      snmlen = 8 * ( nd + ( ni + 1 ) / 2 )
!
!     Get ready to begin a forward search through the DAF file for the
!     data.
!
      call dafbfs ( ctx, binhdl )

      if ( ctx%failed() ) return
!
!     Initialize the number of arrays processed to zero.
!
      numarr = 0
!
!     We'll assume that we will find some data, until proven otherwise.
!
      found = .true.
!
!     Begin looking for and processing the arrays in the binary DAF
!     file.
!
      do while ( found )
!
!        Look for a DAF array.
!
         call daffna ( ctx, found  )

         if ( ctx%failed() ) return
!
!        If we found an array, then we need to process it. Start
!        by incrementing the number of arrays processed. If not,
!        we just skip to the bottom of the loop.
!
         if ( found ) then

            numarr = numarr + 1
!
!           Get and unpack the summary information for the current
!           array.
!
            call dafgs ( ctx, summry )
            call dafus ( ctx, summry, nd, ni, dsumry, isumry )
!
!           Get the name of the current array.
!
            call dafgn ( ctx, name )

            if ( ctx%failed() ) then
!
!              If an error occurred on any of the DAF system calls
!              above, return to the caller. An appropriate error
!              message will have already been set by the routine which
!              signaled the error.
!
               return

            end if
!
!           Get the beginning address for the data in the current array.
!
            dtabeg = isumry(ni-1)
!
!           Set the number of double precision numbers in the current
!           array.
!
            dtacnt = isumry(ni) - isumry(ni-1) + 1

            line = begarr // ' #' // ' #'
            call repmi_inplace ( line, '#', numarr )
            call repmi_inplace ( line, '#', dtacnt )
            write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

            if ( iostat /= 0 ) then

               call ctx%setmsg ( 'Error writing to the DAF transfer'   // &
                             ' file ''#''. IOSTAT = #.'             )
               call ctx%errfnm ( '#', xfrlun                            )
               call ctx%errint ( '#', iostat                            )
               call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'               )
               return

            end if
!
!           Write the name of the current array.
!
            write (xfrlun,fmt='(A)',iostat=iostat) &
                                    quote // name( :snmlen) // quote

            if ( iostat /= 0 ) then

               call ctx%setmsg ( 'Error writing to the DAF transfer'   // &
                             ' file ''#''. IOSTAT = #.'             )
               call ctx%errfnm ( '#', xfrlun                            )
               call ctx%errint ( '#', iostat                            )
               call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'               )
               return

            end if
!
!           Write out the double precision part of the summary.
!
            call wrencd( ctx, xfrlun, nd, dsumry )
!
!           Write out the integer part of the summary, excluding the
!           beginning and ending addresses of the data in the array,
!           ISUMRY(NI-1) and ISUMRY(NI), since these values vary with
!           the number of reserved records allocated.
!
            call wrenci( ctx, xfrlun, ni-2, isumry )

            if ( ctx%failed() ) then
!
!              If an error occurred on any of the data encoding calls
!              above, return to the caller. An appropriate error message
!              will have already been set by the routine which signaled
!              the error.
!
               return

            end if

            numlft = dtacnt

            do while ( numlft > 0 )

               if ( numlft >= bufsiz ) then

                  numdta = bufsiz

               else

                  numdta = numlft

               end if
!
!              Read in NUMDTA numbers from the current array. The
!              desired data are specified by beginning and ending
!              indices into the array, inclusive: thus the subtraction
!              of 1 in the call.
!
               call dafgda ( ctx, binhdl, dtabeg, dtabeg+numdta-1, buffer )

               if ( ctx%failed() ) then
!
!                 We want to check failed here because were in a loop.
!                 We should exit the loop, and the routine, as soon as
!                 an error is detected, so we don't continue doing
!                 things for a long time.
!
                  return

               end if
!
!              Write out the count of double precision numbers which are
!              in the buffer.
!
               line = '#'
               call repmi_inplace ( line, '#', numdta )
               write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

               if ( iostat /= 0 ) then

               call ctx%setmsg ( 'Error writing to the DAF transfer'   // &
                             ' file ''#''. IOSTAT = #.'             )
                  call ctx%errfnm ( '#', xfrlun                         )
                  call ctx%errint ( '#', iostat                         )
                  call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'            )
                  return

               end if
!
!              Encode and write out a buffer of double precision
!              numbers.
!
               call wrencd( ctx, xfrlun, numdta, buffer )

               if ( ctx%failed() ) then
!
!                 We want to check failed here because were in a loop.
!                 We should exit the loop, and the routine, as soon as
!                 an error is detected, so we don't continue doing
!                 things for a long time.
!
                  return

               end if

               numlft = numlft - numdta
               dtabeg = dtabeg + numdta

            end do

            line = endarr // ' #' // ' #'
            call repmi_inplace ( line, '#', numarr )
            call repmi_inplace ( line, '#', dtacnt )
            write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

            if ( iostat /= 0 ) then

               call ctx%setmsg ( 'Error writing to the DAF transfer'   // &
                             ' file ''#''. IOSTAT = #.'             )
               call ctx%errfnm ( '#', xfrlun                            )
               call ctx%errint ( '#', iostat                            )
               call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'               )
               return

            end if

         end if
!
!        At this point, one complete DAF array has been written to the
!        DAF transfer file.
!
      end do
!
!     Write out the number of arrays processed.
!
      line = totarr // ' #'
      call repmi_inplace ( line, '#', numarr )
      write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'Error writing to the DAF transfer'   // &
                       ' file ''#''. IOSTAT = #.'             )
         call ctx%errfnm ( '#', xfrlun                            )
         call ctx%errint ( '#', iostat                            )
         call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)'               )
         return

      end if
!
!     Close only the binary file.
!
      call dafcls ( ctx, binhdl )

      return
      end subroutine dafbt

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine [[daftb]]. NAIF supports this routine only to provide backward
      !> compatibility.
      !>
      !> Convert an ASCII (text) DAF to an equivalent binary DAF.
      !>
      !>## Detailed Input
      !>
      !>     ASCII    is the name of an existing ASCII (text) DAF.
      !>
      !>     BINARY   is the name of the binary DAF to be created.
      !>              The binary DAF contains the same data as the
      !>              ASCII DAF, but in a form more suitable for use
      !>              by application programs.
      !>
      !>     RESV     is the number of records to be reserved in the
      !>              binary DAF.
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
      !>     1)  If an error occurs while converting the input ASCII text DAF
      !>         file to binary format, the error is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>     2)  If an error occurs while writing data to the output binary DAF
      !>         file, the error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>## Files
      !>
      !>     See arguments ASCII, BINARY.
      !>
      !>## Particulars
      !>
      !>     This routine has been made obsolete by the new DAF text to binary
      !>     conversion routine DAFTB. This routine remains available for
      !>     reasons of backward compatibility. We strongly recommend that the
      !>     conversion routine DAFTB be used for any new software development.
      !>     Please see the header of the routine DAFTB for details.
      !>
      !>     This routine is used for converting older DAF text files, which
      !>     use a decimal format for numbers, into their equivalent binary
      !>     formats. Note that the routine DAFTB makes use of a text file
      !>     format that is incompatible with the text file format expected by
      !>     the routines called by this routine.
      !>
      !>     Note that you must select the number of records to be reserved
      !>     in the binary DAF. The contents of reserved records are ignored
      !>     by the normal transfer process.
      !>
      !>## Examples
      !>
      !>     DAFB2A and DAFA2B are typically used to transfer files.
      !>     If file A.DAF is a binary DAF in environment 1, it
      !>     can be transferred to environment 2 in three steps.
      !>
      !>        1) Convert it to ASCII,
      !>
      !>              CALL DAFB2A ( 'A.DAF', 'A.ASCII' )
      !>
      !>        2) Transfer the ASCII file, using FTP, Kermit, or some other
      !>           file transfer utility,
      !>
      !>              ftp> put a.ascii
      !>
      !>        3) Convert it to binary on the new machine,
      !>
      !>              CALL DAFA2B ( 'A.ASCII', 'A.DAF', RESV )
      !>
      !>     Note that DAFB2A and DAFA2B work in any standard Fortran-77
      !>     environment.
      !>
      !>## Restrictions
      !>
      !>     1)  DAFA2B cannot be executed while any other DAF is open
      !>         for writing.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        $Exceptions section to better describe the issues detected by
      !>        this routine. Moved DAF required reading from
      !>        $Literature_References to $Required_Reading section.
      !>
      !>-    SPICELIB Version 2.0.1, 26-JUL-2012 (EDW)
      !>
      !>        Edited $Abstract section to use "Deprecated" keyword
      !>        and state replacement routine.
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>-    SPICELIB Version 2.0.0, 30-SEP-1993 (KRG)
      !>
      !>        This routine was completely rewritten to make use of the
      !>        routines DAFT2B and TXTOPR, for converting a text file to
      !>        binary and opening a text file. It now simply calls the
      !>        routine DAFT2B after opening the text file.
      !>
      !>        Added a statement to the $Particulars section to the effect
      !>        that this routine has been made obsolete by the introduction of
      !>        the routine DAFTB, and that the use of the new routine is
      !>        strongly recommended for new software development.
      !>
      !>        Modified the $Abstract section to reflect the fact that this
      !>        routine is obsolete.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafa2b ( ctx, ascii, binary, resv )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: ascii  !! Name of an existing ASCII (text) DAF.
      character(*), intent(in) :: binary  !! Name of a binary DAF to be created.
      integer(ip), intent(in) :: resv  !! Number of records to reserve.

      integer(ip) :: unit

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Open the ASCII file for reading. If an error occurs, then check
!     out and return. An appropriate error message will have already
!     been set.
!
      call txtopr ( ctx, ascii, unit )

      if ( ctx%failed() ) return

!
!     Call DAFT2B to perform the conversion. If it fails, then just
!     return, as an appropriate error message should have
!     already been set. Also close the text file that we opened.
!
      call daft2b ( ctx, unit, binary, resv )

      if ( ctx%failed() ) then

         close       ( unit     )
         return

      end if

!
!     Close the file.
!
      close ( unit )

      return
      end subroutine dafa2b

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine [[dafbt]]. NAIF supports this routine only to provide backward
      !> compatibility.
      !>
      !> Convert a binary DAF to an equivalent ASCII (text) DAF.
      !>
      !>## Detailed Input
      !>
      !>     BINARY   is the name of an existing binary DAF.
      !>
      !>     ASCII    is the name of an ASCII (text) DAF to be created.
      !>              The ASCII file contains the same data as the binary
      !>              file, but in a form more suitable for transfer
      !>              between heterogeneous computing environments.
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
      !>     1)  If an error occurs while reading the input binary DAF file,
      !>         the error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     2)  If an error occurs while converting the input binary DAF file
      !>         to text format, the error is signaled by a routine in the call
      !>         tree of this routine.
      !>
      !>     3)  If an error occurs while writing data to the output ASCII text
      !>         DAF file, the error is signaled by a routine in the call tree
      !>         of this routine.
      !>
      !>## Files
      !>
      !>     See arguments BINARY, ASCII.
      !>
      !>## Particulars
      !>
      !>     This routine has been made obsolete by the new DAF binary to text
      !>     conversion routine DAFBT. This routine remains available for
      !>     reasons of backward compatibility. We strongly recommend that the
      !>     conversion routine DAFBT be used for any new software development.
      !>     Please see the header of the routine DAFBT for details.
      !>
      !>     Note that the contents of reserved records in the binary file
      !>     are not stored in the ASCII file.
      !>
      !>## Examples
      !>
      !>     DAFB2A and DAFA2B are typically used to transfer files.
      !>     If file A.DAF is a binary DAF in environment 1, it can be
      !>     transferred to environment 2 in three steps.
      !>
      !>        1) Convert it to ASCII,
      !>
      !>              CALL DAFB2A ( 'A.DAF', 'A.ASCII' )
      !>
      !>        2) Transfer the ASCII file, using FTP, Kermit, or some other
      !>           file transfer utility,
      !>
      !>              ftp> put a.ascii
      !>
      !>        3) Convert it to binary on the new machine,
      !>
      !>              CALL DAFA2B ( 'A.ASCII', 'A.DAF', RESV )
      !>
      !>     Note that DAFB2A and DAFA2B work in any standard Fortran-77
      !>     environment.
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.2.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        $Exceptions section to better describe the issues detected by
      !>        this routine. Moved DAF required reading from
      !>        $Literature_References to $Required_Reading section.
      !>
      !>-    SPICELIB Version 2.1.1, 26-JUL-2012 (EDW)
      !>
      !>        Edited $Abstract section to use "Deprecated" keyword
      !>        and state replacement routine.
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>-    SPICELIB Version 2.1.0, 18-JUN-1999 (WLT)
      !>
      !>        Fixed call to CHKOUT with wrong name.
      !>
      !>-    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG)
      !>
      !>        This routine was completely rewritten to make use of the
      !>        routines DAFB2T and TXTOPN, for converting a text file to
      !>        binary and opening a text file. It now simply calls the
      !>        routine DAFT2B after opening the text file with TXTOPN.
      !>
      !>        Added a statement to the $Particulars section to the effect
      !>        that this routine has been made obsolete by the introduction of
      !>        the routine DAFBT, and that we strongly recommend the use of
      !>        the new routine.
      !>
      !>        Modified the $Abstract section to reflect the fact that this
      !>        routine is obsolete.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafb2a ( ctx, binary, ascii )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: binary  !! Name of an existing binary DAF.
      character(*), intent(in) :: ascii  !! Name of an ASCII (text) DAF to be created.

      integer(ip) :: unit

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     Open the ASCII file for writing. If an error occurs, then check
!     out and return. An appropriate error message will have already
!     been set.
!
      call txtopn ( ctx, ascii, unit )

      if ( ctx%failed() ) return
!
!     Attempt to perform the file conversion. If it fails, close the
!     text file with STATUS = 'DELETE', return, as an
!     appropriate error message should have already been set.
!
      call dafb2t ( ctx, binary, unit )

      if ( ctx%failed() ) then

         close       ( unit, status='DELETE' )
         return

      end if
!
!     Close the text file.
!
      close ( unit )

      return
      end subroutine dafb2a

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine [[dafbt]]. NAIF supports this routine only to provide backward
      !> compatibility.
      !>
      !> Write the contents of a binary DAF to a text file opened by
      !> the calling program.
      !>
      !>## Detailed Input
      !>
      !>     BINARY   is the name of an existing binary DAF.
      !>
      !>     TEXT     is a logical unit number, to which a text file has
      !>              been connected by the calling program, and into
      !>              which the contents of BINARY are to be written
      !>              (in a form more suitable for transfer between
      !>              heterogeneous computing environments).
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
      !>     1)  If for some reason the text file cannot be written,
      !>         the error SPICE(DAFWRITEFAIL) is signaled.
      !>
      !>     2)  If for some reason the ID word cannot be read from the DAF
      !>         file, the error SPICE(DAFREADFAIL) is signaled.
      !>
      !>## Files
      !>
      !>     See arguments BINARY, TEXT.
      !>
      !>## Particulars
      !>
      !>     This routine has been made obsolete by the new DAF binary to text
      !>     conversion routine DAFBT. This routine remains available for
      !>     reasons of backward compatibility. We strongly recommend that you
      !>     use the new conversion routines for any new software development.
      !>     Please see the header of the routine DAFBT for details.
      !>
      !>     Any binary DAF may be transferred between heterogeneous
      !>     Fortran environments by converting it to an equivalent file
      !>     containing only ASCII characters. Such a file can be transferred
      !>     almost universally, using any number of established protocols
      !>     (Kermit, FTP, and so on). Once transferred, the ASCII file can
      !>     be converted to a binary file, using the representations
      !>     native to the new host environment.
      !>
      !>     There are two pairs of routines that can be used to convert
      !>     DAFs between binary and text. The first pair, DAFB2A
      !>     and DAFA2B, works with complete files. That is, DAFB2A creates
      !>     a complete ASCII file containing all of the information in
      !>     a particular binary file, and nothing else; this file can
      !>     be fed directly into DAFA2B to produce a complete binary file.
      !>     In each case, the names of the files are specified.
      !>
      !>     A related pair of routines, DAFB2T and DAFT2B, assume that
      !>     the ASCII data are to be stored in the midst of a text file.
      !>     This allows the calling program to surround the data with
      !>     standardized labels, to append several binary files into a
      !>     single text file, and so on.
      !>
      !>     Note that the contents of reserved records in the binary file
      !>     are not written by this routine (although they may be stored
      !>     in the ASCII file by the calling program).
      !>
      !>## Examples
      !>
      !>     DAFB2A and DAFA2B are typically used for simple file transfers.
      !>     If file A.DAF is a binary DAF in environment 1, it can be
      !>     transferred to environment 2 in three steps.
      !>
      !>        1) Convert it to ASCII:
      !>
      !>              CALL DAFB2A ( 'A.DAF', 'A.ASCII' )
      !>
      !>        2) Transfer the ASCII file, using FTP, Kermit, or some other
      !>           file transfer utility:
      !>
      !>              ftp> put a.ascii
      !>
      !>        3) Convert it to binary on the new machine,
      !>
      !>              CALL DAFA2B ( 'A.ASCII', 'A.DAF', RESV )
      !>
      !>     Note that DAFB2A and DAFA2B work in any standard Fortran-77
      !>     environment.
      !>
      !>     If the file needs to contain other information---a standard
      !>     label, for instance---the first and third steps must be modified
      !>     to use DAFB2T and DAFT2B. The first step becomes
      !>
      !>        (Open a text file)
      !>        (Write the label)
      !>        CALL DAFB2T ( BINARY, UNIT  )
      !>        (Close the text file)
      !>
      !>     The third step becomes
      !>
      !>        (Open the text file)
      !>        (Read the label)
      !>        CALL DAFT2B ( UNIT, BINARY, RESV )
      !>        (Close the text file)
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved DAF
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 3.0.1, 26-JUL-2012 (EDW)
      !>
      !>        Edited $Abstract section to use "Deprecated" keyword
      !>        and state replacement routine.
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
      !>
      !>        This routine still uses a naked READ to retrieve the
      !>        file IDWORD from the first 8 characters stored in the
      !>        file record. It may be that future environments
      !>        will have characters whose storage exceeds 1 byte,
      !>        in which case this routine will require modification.
      !>        One possibility is to call the private file record
      !>        reader ZZDAFGFR, which must address the translation
      !>        for all supported non-native binary file formats on this
      !>        platform.
      !>
      !>        The existing call to DAFHLU was replaced with ZZDDHHLU.
      !>        The call to DAFRDA was replaced with a call to the new,
      !>        translation-aware routine DAFGDA.
      !>
      !>-    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG)
      !>
      !>        Added the variable IDWORD to the routine for storing the ID
      !>        word from the file being converted. This replaces a hard coded
      !>        value of 'NAIF/DAF', and supports the new interpretation of the
      !>        ID word.
      !>
      !>        Removed the error SPICE(DAFNOIDWORD) as it was no longer
      !>        relevant.
      !>
      !>        There were no checks of the IOSTAT variable after attempting to
      !>        write to the text file, a single test of the IOSTAT variable
      !>        was made at the end of the routine. This was not adequate to
      !>        detect errors when writing to the text file. So after all of
      !>        these write statements, an IF ... END IF block was added to
      !>        signal an error if IOSTAT .NE. 0.
      !>
      !>           IF ( IOSTAT .NE. 0 ) THEN
      !>
      !>              CALL DAFCLS ( HANDLE                                )
      !>              CALL SETMSG ( 'The attempt to write to file ''#''' //
      !>        .                   ' failed. IOSTAT = #.'                )
      !>              CALL ERRFNM ( '#', TEXT                             )
      !>              CALL SIGERR ( SPICE(DAFWRITEFAIL)                 )
      !>              CALL CHKOUT ( 'DAFB2T'                              )
      !>              RETURN
      !>
      !>           END IF
      !>
      !>        Removed the code from the end of the routine that purported to
      !>        check for read errors:
      !>
      !>           C
      !>           C     If any write screws up, they should all screw up. Why
      !>           C     make a billion separate checks?
      !>           C
      !>                 IF ( IOSTAT .NE. 0 ) THEN
      !>                    CALL SETMSG ( 'Value of IOSTAT was: #. ' )
      !>                    CALL ERRINT ( '#', IOSTAT                )
      !>                    CALL SIGERR ( SPICE(DAFWRITEFAIL)      )
      !>                  END IF
      !>
      !>        The answer to the question is:
      !>
      !>           You have to do a billion separate checks because the IOSTAT
      !>           value is only valid for the most recently executed write.
      !>
      !>        Added the following error message to the routine:
      !>
      !>           C     2) If for some reason the ID word cannot be read from
      !>           C        the DAF file, the error SPICE(DAFREADFAIL) will be
      !>           C        signaled.
      !>
      !>        because the file ID word is now read from the binary DAF file
      !>        rather than being hard coded as 'NAIF/DAF' in this routine.
      !>
      !>        Added a statement to the $Particulars section to the effect
      !>        that this routine has been made obsolete by the introduction of
      !>        the routine DAFBT, and that we strongly recommend the use of
      !>        the new routine.
      !>
      !>        Modified the $Abstract section to reflect the fact that this
      !>        routine is obsolete.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine dafb2t ( ctx, binary, text )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: binary  !! Name of an existing binary DAF.
      integer(ip), intent(in) :: text  !! Logical unit connected to text file.

      integer(ip), parameter :: bsize = 100


      character(*), parameter :: quote = ''''

      character(idwlen) :: idword
      character(60) :: ifname
      character(1000) :: name

      real(wp) :: buffer   ( bsize )
      real(wp) :: dc       ( 125 )
      real(wp) :: sum      ( 125 )

      integer(ip) :: begin
      integer(ip) :: bward
      integer(ip) :: chunk
      integer(ip) :: csize
      integer(ip) :: daflun
      integer(ip) :: end
      integer(ip) :: free
      integer(ip) :: fward
      integer(ip) :: handle
      integer(ip) :: i
      integer(ip) :: ic       ( 250 )
      integer(ip) :: iostat
      integer(ip) :: isize
      integer(ip) :: lsize
      integer(ip) :: nd
      integer(ip) :: ni

      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     Initialize the IDWORD.
!
      idword = ' '

!
!     Open the binary file for reading and read the ID word from the
!     first record of the file.
!
      call dafopr ( ctx, binary, handle )

      if ( ctx%failed() ) return
!
!     At this point, we know that we have a DAF file, because we were
!     able to successfully open it, so we will attempt to proceed with
!     the file conversion process.
!
!     Convert the DAF file handle to its equivalent Fortran logical
!     unit. We need to do this in order to accurately move the file
!     ID word to the text file.
!
      call zzddhhlu ( ctx, handle, 'DAF', .false., daflun )

      if ( ctx%failed() ) return

      read ( daflun, rec=1, iostat=iostat ) idword

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'Could not read ID word from file ''#''.' // &
                       ' IOSTAT = #.'                             )
         call ctx%errch  ( '#', binary                                )
         call ctx%errint ( '#', iostat                                )
         call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                       )
         return

      end if
!
!     Get the contents of the file record. The ASCII file begins
!     with the ID word which is followed by the summary format,
!     which is followed by the internal file name.
!
      call dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )

      if ( ctx%failed() ) return

      write (text,*,iostat=iostat) quote // idword // quote

      if ( iostat /= 0 ) then

         call dafcls ( ctx, handle                                )
         call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call ctx%errfnm ( '#', text                             )
         call ctx%errint ( '#', iostat                           )
         call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         return

      end if

      write (text,*,iostat=iostat) nd

      if ( iostat /= 0 ) then

         call dafcls ( ctx, handle                                )
         call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call ctx%errfnm ( '#', text                             )
         call ctx%errint ( '#', iostat                           )
         call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         return

      end if

      write (text,*,iostat=iostat) ni

      if ( iostat /= 0 ) then

         call dafcls ( ctx, handle                                )
         call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call ctx%errfnm ( '#', text                             )
         call ctx%errint ( '#', iostat                           )
         call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         return

      end if

      write (text,*,iostat=iostat) quote //  ifname    // quote

      if ( iostat /= 0 ) then

         call dafcls ( ctx, handle                                )
         call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call ctx%errfnm ( '#', text                             )
         call ctx%errint ( '#', iostat                           )
         call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         return

      end if
!
!     Each array is preceded by a '1', which indicates that more
!     arrays are to come. The array itself begins with the name
!     and the summary components, and ends with the name again.
!     The elements are written in arbitrary chunks. The final
!     chunk is followed by a '0', which indicates that no chunks
!     remain.
!
!     Write the arrays in forward order.
!
      lsize = nd + (ni - 1) / 2 + 1
      isize = lsize * 8

      call dafbfs ( ctx, handle )
      call daffna ( ctx, found  )

      if ( ctx%failed() ) return

      do while ( found )

         call dafgs ( ctx, sum  )
         call dafgn ( ctx, name )
         call dafus ( ctx, sum, nd, ni, dc, ic )

         if ( ctx%failed() ) return

         write (text,*,iostat=iostat) '1'

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                )
            call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call ctx%errfnm ( '#', text                             )
            call ctx%errint ( '#', iostat                           )
            call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            return

         end if

         write (text,*,iostat=iostat) quote // name( :isize) // quote

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                )
            call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call ctx%errfnm ( '#', text                             )
            call ctx%errint ( '#', iostat                           )
            call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            return

         end if

         write (text,*,iostat=iostat) ( dc(i), i = 1, nd     )

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                )
            call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call ctx%errfnm ( '#', text                             )
            call ctx%errint ( '#', iostat                           )
            call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            return

         end if

         write (text,*,iostat=iostat) ( ic(i), i = 1, ni - 2 )

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                )
            call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call ctx%errfnm ( '#', text                             )
            call ctx%errint ( '#', iostat                           )
            call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            return

         end if

         begin = ic(ni-1)
         end   = ic(ni  )

         do while ( begin <= end )

            chunk = min ( begin + bsize - 1, end )
            csize =       chunk - begin + 1

            call dafgda ( ctx, handle, begin, chunk, buffer )

            if ( ctx%failed() ) return

            write (text,*,iostat=iostat) csize

            if ( iostat /= 0 ) then

               call dafcls ( ctx, handle                                )
               call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                             ' failed. IOSTAT = #.'                )
               call ctx%errfnm ( '#', text                             )
               call ctx%errint ( '#', iostat                           )
               call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
               return

            end if

            write (text,*,iostat=iostat) ( buffer(i), i = 1, csize )

            if ( iostat /= 0 ) then

               call dafcls ( ctx, handle                                )
               call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                             ' failed. IOSTAT = #.'                )
               call ctx%errfnm ( '#', text                             )
               call ctx%errint ( '#', iostat                           )
               call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
               return

            end if

            begin = begin + bsize

         end do

         write (text,*,iostat=iostat) '0'

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                )
            call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call ctx%errfnm ( '#', text                             )
            call ctx%errint ( '#', iostat                           )
            call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            return

         end if

         write (text,*,iostat=iostat) quote // name( :isize) // quote

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                )
            call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call ctx%errfnm ( '#', text                             )
            call ctx%errint ( '#', iostat                           )
            call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            return

         end if

         call daffna ( ctx, found )

         if ( ctx%failed() ) return

      end do

!
!     A final '0' indicates that no arrays remain. The first shall be
!     last: the internal file name brings up the rear.
!
      write (text,*,iostat=iostat) '0'

      if ( iostat /= 0 ) then

         call dafcls ( ctx, handle                                )
         call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call ctx%errfnm ( '#', text                             )
         call ctx%errint ( '#', iostat                           )
         call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         return

      end if

      write (text,*,iostat=iostat) quote // ifname // quote

      if ( iostat /= 0 ) then

         call dafcls ( ctx, handle                                )
         call ctx%setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call ctx%errfnm ( '#', text                             )
         call ctx%errint ( '#', iostat                           )
         call ctx%sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         return

      end if

!
!     Close only the binary file.
!
      call dafcls ( ctx, handle )

      return
      end subroutine dafb2t

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine [[daftb]]. NAIF supports this routine only to provide backward
      !> compatibility.
      !>
      !> Reconstruct a binary DAF from a text file opened by
      !> the calling program.
      !>
      !>## Detailed Input
      !>
      !>     TEXT     is a logical unit number, to which a text file has
      !>              been connected by the calling program, and into
      !>              which the contents of binary DAF have been
      !>              written. The file pointer should be placed just
      !>              before the file ID word.
      !>
      !>     BINARY   is the name of a binary DAF to be created.
      !>              The binary DAF contains the same data as the
      !>              text file, but in a form more suitable for use
      !>              by application programs.
      !>
      !>     RESV     is the number of records to be reserved in the
      !>              binary DAF.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     BSIZE    is the size of the buffer used to read array elements
      !>              from the text file. No single group of elements should
      !>              contains more than BSIZE elements.
      !>
      !>## Exceptions
      !>
      !>     1)  If for some reason the text file cannot be read,
      !>         the error SPICE(DAFREADFAIL) is signaled.
      !>
      !>     2)  If the architecture of the file is not DAF, as specified by
      !>         the ID word, the error SPICE(NOTADAFFILE) is signaled.
      !>
      !>     3)  If the text file does not contain matching internal file
      !>         names, the error SPICE(DAFNOIFNMATCH) is signaled.
      !>
      !>     4)  If the text file does not contain matching array names,
      !>         the error SPICE(DAFNONAMEMATCH) is signaled.
      !>
      !>     5)  If the buffer size is not sufficient, the error
      !>         SPICE(DAFOVERFLOW) is signaled.
      !>
      !>## Files
      !>
      !>     See arguments TEXT, BINARY.
      !>
      !>## Particulars
      !>
      !>     This routine has been made obsolete by the new DAF text to binary
      !>     conversion routine DAFTB. This routine remains available for
      !>     reasons of backward compatibility. We strongly recommend that you
      !>     use the new conversion routines for any new software development.
      !>     Please see the header of the routine DAFTB for details.
      !>
      !>     This routine is necessary for converting older DAF text files into
      !>     their equivalent binary formats, as DAFTB uses a different text
      !>     file format that is incompatible with the text file format
      !>     expected by this routine.
      !>
      !>     Any binary DAF may be transferred between heterogeneous
      !>     Fortran environments by converting it to an equivalent file
      !>     containing only ASCII characters. Such a file can be transferred
      !>     almost universally, using any number of established protocols
      !>     (Kermit, FTP, and so on). Once transferred, the ASCII file can
      !>     be reconverted to a binary DAF, using the representations
      !>     native to the new host environment.
      !>
      !>     There are two pairs of routines that can be used to convert
      !>     DAFs between binary and ASCII. The first pair, DAFB2A
      !>     and DAFA2B, works with complete files. That is, DAFB2A creates
      !>     a complete ASCII file containing all of the information in
      !>     a particular binary DAF, and nothing else; this file can
      !>     be fed directly into DAFA2B to produce a complete binary DAF.
      !>     In each case, the names of the files are specified.
      !>
      !>     A related pair of routines, DAFB2T and DAFT2B, assume that
      !>     the ASCII data are to be stored in the midst of a text file.
      !>     This allows the calling program to surround the data with
      !>     standardized labels, to append several binary DAFs into a
      !>     single text file, and so on.
      !>
      !>     Note that you must select the number of records to be reserved
      !>     in the binary DAF. The contents of reserved records are ignored
      !>     by the normal transfer process.
      !>
      !>## Examples
      !>
      !>     DAFB2A and DAFA2B are typically used for simple transfers.
      !>     If A.DAF is a binary DAF in environment 1, it can be transferred
      !>     to environment 2 in three steps.
      !>
      !>        1) Convert it to ASCII:
      !>
      !>              CALL DAFB2A ( 'A.DAF', 'A.ASCII' )
      !>
      !>        2) Transfer the ASCII file, using FTP, Kermit, or some other
      !>           file transfer utility:
      !>
      !>              ftp> put a.ascii
      !>
      !>        3) Convert it to binary on the new machine,
      !>
      !>              CALL DAFA2B ( 'A.ASCII', 'A.DAF', RESV )
      !>
      !>     Note that DAFB2A and DAFA2B work in any standard Fortran-77
      !>     environment.
      !>
      !>     If the file needs to contain other information---a standard
      !>     label, for instance---the first and third steps must be modified
      !>     to use DAFB2T and DAFT2B. The first step becomes
      !>
      !>        (Open a text file)
      !>        (Write the label)
      !>        CALL DAFB2T ( BINARY, UNIT  )
      !>        (Close the text file)
      !>
      !>     The third step becomes
      !>
      !>        (Open the text file)
      !>        (Read the label)
      !>        CALL DAFT2B ( UNIT, BINARY, RESV )
      !>        (Close the text file)
      !>
      !>## Restrictions
      !>
      !>     1)  DAFT2B cannot be executed while any other DAF is open
      !>         for writing.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     J. Diaz del Rio    (ODC Space)
      !>     K.R. Gehringer     (JPL)
      !>     J.E. McLean        (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.1.0, 26-OCT-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved DAF
      !>        required reading from $Literature_References to
      !>        $Required_Reading section.
      !>
      !>-    SPICELIB Version 3.0.1, 26-JUL-2012 (EDW)
      !>
      !>        Edited $Abstract section to use "Deprecated" keyword
      !>        and state replacement routine.
      !>
      !>        Eliminated unneeded $Revisions section.
      !>
      !>-    SPICELIB Version 3.0.0, 04-OCT-1993 (KRG)
      !>
      !>        Removed the error SPICE(DAFNOIDWORD) as it was no longer
      !>        relevant.
      !>
      !>        Added the error SPICE(NOTADAFFILE) if this routine is called
      !>        with a file that does not contain an ID word identifying the
      !>        file as a DAF file.
      !>
      !>        There were no checks of the IOSTAT variable after attempting to
      !>        read from the text file, a single test of the IOSTAT variable
      !>        was made at the end of the routine. This was not adequate to
      !>        detect errors when writing to the text file. So after all of
      !>        these read statements, an IF ... END IF block was added to
      !>        signal an error if IOSTAT .NE. 0.
      !>
      !>            IF ( IOSTAT .NE. 0 ) THEN
      !>
      !>               CALL SETMSG ( 'The attempt to read from file ''#''' //
      !>         .                   ' failed. IOSTAT = #.'                 )
      !>               CALL ERRFNM ( '#', UNIT                              )
      !>               CALL SIGERR ( SPICE(DAFREADFAIL)                   )
      !>               CALL CHKOUT ( 'DAFT2B'                               )
      !>               RETURN
      !>
      !>            END IF
      !>
      !>        Removed the code from the end of the routine that purported to
      !>        check for read errors:
      !>
      !>            C
      !>            C     If any read screws up, they should all screw up. Why
      !>            C     make a billion separate checks?
      !>            C
      !>                  IF ( IOSTAT .NE. 0 ) THEN
      !>                     CALL SETMSG ( 'Value of IOSTAT was: #. ' )
      !>                     CALL ERRINT ( '#', IOSTAT                )
      !>                     CALL SIGERR ( SPICE(DAFREADFAIL)       )
      !>                   END IF
      !>
      !>        The answer to the question is:
      !>
      !>            You have to do a billion separate checks because the IOSTAT
      !>            value is only valid for the most recently executed read.
      !>
      !>        Added a statement to the $Particulars section to the effect
      !>        that this routine has been made obsolete by the introduction of
      !>        the routine DAFTB, and that we strongly recommend the use of
      !>        the new routine. This routine must, however, be used when
      !>        converting older text files to binary, as the old and new
      !>        formats are not compatible.
      !>
      !>        Modified the $Abstract section to reflect the fact that this
      !>        routine is obsolete and maintained for purposes of backward
      !>        compatibility only.
      !>
      !>-    SPICELIB Version 2.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 2.0.1, 06-AUG-1990 (HAN)
      !>
      !>        Header documentation was corrected. This routine will
      !>        convert a file containing either ID word, 'NAIF/DAF' or
      !>        'NAIF/NIP'. (Previous versions of SPICELIB software used
      !>        the ID word 'NAIF/NIP'.)
      !>
      !>-    SPICELIB Version 2.0.0, 02-AUG-1990 (JEM)
      !>
      !>        The previous version of this routine always failed and
      !>        signaled the error SPICE(DAFNOIDWORD) because of a faulty
      !>        logical expression in an error-checking IF statement.
      !>        The error SPICE(DAFNOIDWORD) should be signaled if the
      !>        next non-blank line in the text file does not begin with the
      !>        word 'NAIF/DAF' AND does not begin with the word 'NAIF/NIP'.
      !>        Previously the logic was incorrect causing the error to be
      !>        signaled every time no matter what the word was. The
      !>        correction consisted of replacing '.OR.' with '.AND.'
      !>        in the logical expression.
      !>
      !>-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
      !>
      !>        Literature references added to the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
      module subroutine daft2b ( ctx, text, binary, resv )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: decimal_value
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: text  !! Logical unit connected to text file.
      character(*), intent(in) :: binary  !! Name of a binary DAF to be created.
      integer(ip), intent(in) :: resv  !! Number of records to reserve.

      integer(ip), parameter :: bsize = 1024


      character(1000) :: name     ( 2 )
      character(idwlen) :: idword
      character(60) :: ifname   ( 2 )
      character(idwlen) :: tarch
      character(idwlen) :: ttype

      real(wp) :: buffer   ( bsize )
      real(wp) :: dc       ( 125 )
      real(wp) :: sum      ( 125 )

      integer(ip) :: chunk
      integer(ip) :: handle
      integer(ip) :: i
      integer(ip) :: ic       ( 250 )
      integer(ip) :: iostat
      integer(ip) :: isize
      integer(ip) :: lsize
      integer(ip) :: more
      integer(ip) :: nd
      integer(ip) :: ni

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      idword = ' '
      tarch  = ' '
      ttype  = ' '
!
!     We should be positioned and ready to read the file ID word from
!     the text file, so let's try it.
!
      read (text,*,iostat=iostat) idword

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                       ' failed. IOSTAT = #.'                 )
         call ctx%errfnm ( '#', text                              )
         call ctx%errint ( '#', iostat                            )
         call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
         return

      end if
!
!     Split the ID word into an architecture and type, and verify that
!     the architecture is 'DAF'. If it is not, this is the wrong
!     routine, and an error will be signaled.
!
      call idw2at ( ctx, idword, tarch, ttype )

      if ( tarch /= 'DAF' ) then

         call ctx%setmsg ( 'File architecture is not ''DAF'' for' // &
                       ' file ''#'''                           )
         call ctx%errfnm ( '#', text                               )
         call ctx%sigerr ( 'SPICE(NOTADAFFILE)'                    )
         return

      end if

      read (text,*,iostat=iostat) nd, ni, ifname(1)

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                       ' failed. IOSTAT = #.'                 )
         call ctx%errfnm ( '#', text                              )
         call ctx%errint ( '#', iostat                            )
         call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
         return

      end if

!
!     Open the new binary file.
!
      call dafopn ( ctx, binary, nd, ni, ifname(1), resv, handle )

      if ( ctx%failed() ) return
!
!     Each array is preceded by a '1', which indicates that more
!     arrays are to come. The array itself begins with the name
!     and the summary components, and ends with the name again.
!     The contents are written in arbitrary chunks. The final
!     chunk is followed by a '0', which indicates that no chunks
!     remain. The names must match, or the array should not
!     be terminated normally.
!
!     If the chunks in the file are bigger than the local buffer
!     size, we are in trouble.
!
      lsize = nd + (ni - 1) / 2 + 1
      isize = lsize * 8

      read (text,*,iostat=iostat) more

      if ( iostat /= 0 ) then

         call dafcls ( ctx, handle                                 )
         call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                       ' failed. IOSTAT = #.'                 )
         call ctx%errfnm ( '#', text                              )
         call ctx%errint ( '#', iostat                            )
         call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
         return

      end if

      do while ( more > 0 )

         read (text,*,iostat=iostat) name(1)( :isize)

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                 )
            call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call ctx%errfnm ( '#', text                              )
            call ctx%errint ( '#', iostat                            )
            call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

         call read_doubles ( nd, dc, iostat )

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                 )
            call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call ctx%errfnm ( '#', text                              )
            call ctx%errint ( '#', iostat                            )
            call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

         read (text,*,iostat=iostat) ( ic(i), i = 1, ni - 2 )

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                 )
            call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call ctx%errfnm ( '#', text                              )
            call ctx%errint ( '#', iostat                            )
            call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

         call dafps  ( ctx, nd, ni, dc, ic, sum           )
         call dafbna ( ctx, handle, sum, name(1)( :isize) )

         if ( ctx%failed() ) return

         read (text,*,iostat=iostat) chunk

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                 )
            call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call ctx%errfnm ( '#', text                              )
            call ctx%errint ( '#', iostat                            )
            call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

         do while ( chunk > 0 )

            if ( chunk > bsize ) then

               call dafcls ( ctx, handle                                 )
               call ctx%setmsg ( 'Buffer size exceeded. Increase to #.' )
               call ctx%errint ( '#', chunk                             )
               call ctx%sigerr ( 'SPICE(DAFOVERFLOW)'                   )
               return

            else

               call read_doubles ( chunk, buffer, iostat )

               if ( iostat /= 0 ) then

                  call dafcls ( ctx, handle                                 )
                  call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                                ' failed. IOSTAT = #.'                 )
                  call ctx%errfnm ( '#', text                              )
                  call ctx%errint ( '#', iostat                            )
                  call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
                  return

               end if

               call dafada ( ctx, buffer, chunk )

               if ( ctx%failed() ) return

            end if

            read (text,*,iostat=iostat) chunk

            if ( iostat /= 0 ) then

               call dafcls ( ctx, handle                                 )
               call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                             ' failed. IOSTAT = #.'                 )
               call ctx%errfnm ( '#', text                              )
               call ctx%errint ( '#', iostat                            )
               call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
               return

            end if

         end do

         read (text,*,iostat=iostat) name(2)( :isize)

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                 )
            call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call ctx%errfnm ( '#', text                              )
            call ctx%errint ( '#', iostat                            )
            call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

         if ( name(1)( :isize) /= name(2)( :isize) ) then

            call dafcls ( ctx, handle                          )
            call ctx%setmsg ( 'Array name mismatch: # and #.' )
            call ctx%errch  ( '#', name(1)( :isize)           )
            call ctx%errch  ( '#', name(2)( :isize)           )
            call ctx%sigerr ( 'SPICE(DAFNONAMEMATCH)'         )
            return

         else

            call dafena ( ctx )

            if ( ctx%failed() ) return

         end if

         read (text,*,iostat=iostat) more

         if ( iostat /= 0 ) then

            call dafcls ( ctx, handle                                 )
            call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call ctx%errfnm ( '#', text                              )
            call ctx%errint ( '#', iostat                            )
            call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

      end do

!
!     The final '0' indicates that no arrays remain. The first shall
!     be last: the internal file name brings up the rear. If it doesn't
!     match the one at the front, complain.
!
      read (text,*,iostat=iostat) ifname(2)

      if ( iostat /= 0 ) then

         call dafcls ( ctx, handle                                 )
         call ctx%setmsg ( 'The attempt to read from file ''#''' // &
                       ' failed. IOSTAT = #.'                 )
         call ctx%errfnm ( '#', text                              )
         call ctx%errint ( '#', iostat                            )
         call ctx%sigerr ( 'SPICE(DAFREADFAIL)'                   )
         return

      end if

      if ( ifname(1) /= ifname(2) ) then

         call dafcls ( ctx, handle                                 )
         call ctx%setmsg ( 'Internal file name mismatch: # and #' )
         call ctx%errch  ( '#', ifname(1)                         )
         call ctx%errch  ( '#', ifname(2)                         )
         call ctx%sigerr ( 'SPICE(DAFNOIFNMATCH)'                 )
         return

      end if

!
!     Close the DAF file we just created.
!
      call dafcls ( ctx, handle )

      return

      contains

      !> Read N doubles from the text file as a list-directed READ does
      !> (PEPPER): from the next line on, as many lines as they take, the
      !> rest of the last line ignored; values separated by blanks or
      !> commas, R*C for R copies of C, a slash ending the input. Each value
      !> is converted by the library's one conversion of decimal text,
      !> DECIMAL_VALUE (a list-directed READ of doubles converts them
      !> itself).
      subroutine read_doubles ( n, values, iostat )
         integer(ip), intent(in) :: n  !! how many values
         real(wp), intent(inout) :: values(*)  !! the values (those after a slash: unchanged)
         integer(ip), intent(out) :: iostat  !! 0, or the error of a READ or of a value
         character(4096) :: line
         real(wp) :: x
         integer(ip) :: k, p, q, star, r, ios
         integer :: dios
         iostat = 0
         k = 0
         do while ( k < n )
            read ( text, '(A)', iostat = ios ) line
            if ( ios /= 0 ) then
               iostat = ios
               return
            end if
            p = 1
            do while ( k < n )
               do while ( p <= len_trim(line) )  ! the separators
                  if ( index ( ' ,' // achar(9), line(p:p) ) == 0 ) exit
                  p = p + 1
               end do
               if ( p > len_trim(line) ) exit  ! the next line
               if ( line(p:p) == '/' ) return
               q = p
               do while ( q <= len_trim(line) )
                  if ( index ( ' ,/' // achar(9), line(q:q) ) /= 0 ) exit
                  q = q + 1
               end do
               r = 1
               star = index ( line(p:q-1), '*' )
               if ( star > 0 ) then
                  read ( line(p:p+star-2), *, iostat = ios ) r
                  if ( ios /= 0 .or. r < 1 ) then
                     iostat = max ( ios, 1 )
                     return
                  end if
                  p = p + star
               end if
               call decimal_value ( line(p:q-1), x, dios )
               if ( dios /= 0 ) then
                  iostat = dios
                  return
               end if
               do while ( r > 0 .and. k < n )
                  k = k + 1
                  values(k) = x
                  r = r - 1
               end do
               p = q
            end do
         end do
      end subroutine read_doubles
      end subroutine daft2b

      !> Convert the contents of an DAF transfer file into an equivalent
      !> binary DAF file.
      !>
      !>## Detailed Input
      !>
      !>     XFRLUN   is the Fortran logical unit number of a previously opened
      !>              DAF transfer file has been.
      !>
      !>              The file pointer should be positioned ready to read
      !>              the file ID word.
      !>
      !>     BINFIL   is the name of the binary DAF file to be created.
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
      !>     1)  If the DAF transfer file cannot be read, the error
      !>         SPICE(FILEREADFAILED) is signaled.
      !>
      !>     2)  If the architecture of the file is not DAF, as specified by
      !>         the ID word, the error SPICE(NOTADAFFILE) is signaled.
      !>
      !>     3)  If an error occurs while attempting to decode data in the DAF
      !>         transfer file, the error SPICE(BADDAFTRANSFERFILE) is
      !>         signaled.
      !>
      !>     4)  If the DAF file cannot be written, an error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>     5)  The binary DAF file opened by this routine, BINFIL, is only
      !>         GUARANTEED to be closed upon successful completion of the
      !>         transfer file to binary file conversion process. In the event
      !>         of an error, the caller of this routine is required to close
      !>         the binary DAF file BINFIL.
      !>
      !>## Files
      !>
      !>     See arguments XFRLUN, BINFIL.
      !>
      !>## Particulars
      !>
      !>     Any binary DAF file may be transferred between heterogeneous
      !>     Fortran environments by converting it to an equivalent file
      !>     containing only ASCII characters. Such a file can be transferred
      !>     almost universally, using any number of established protocols.
      !>     Once transferred, the ASCII file can be converted to a binary
      !>     file, using the representations native to the new host
      !>     environment.
      !>
      !>     This routine provides a mechanism for converting an DAF transfer
      !>     file created by DAFBT, or an equivalent procedure, into an
      !>     equivalent binary DAF file which may be used with the SPICE
      !>     system. It is one of a pair of routines for performing conversions
      !>     between the binary format of a DAF file and the DAF transfer file.
      !>     The inverse of this routine is the routine DAFBT.
      !>
      !>     This routine makes NO use of the DAF reserved record area. It
      !>     can only deal with the data portion of a DAF file in the DAF
      !>     transfer file.
      !>
      !>     Upon successful completion, the binary DAF file specified by
      !>     BINFIL will have been created. The binary DAF file that was
      !>     created will be closed when this routine exits. The DAF transfer
      !>     file will remain open, as it was on entry, and it will be
      !>     positioned to read the first line after the encoded DAF file data.
      !>
      !>## Examples
      !>
      !>     Let
      !>
      !>        XFRLUN   be the Fortran logical unit attached to a DAF
      !>                 transfer file which is to be converted into its binary
      !>                 DAF equivalent.
      !>
      !>        BINFIL   be the name of the binary DAF file which will be
      !>                 created from the DAF transfer file.
      !>
      !>     The following subroutine call would read the DAF transfer file
      !>     attached to the Fortran logical unit XFRLUN, convert its data into
      !>     binary format, and write that data to the binary DAF file which
      !>     has been created:
      !>
      !>        CALL DAFTB( XFRLUN, BINFIL )
      !>
      !>## Restrictions
      !>
      !>     1)  This routine assumes that it is positioned ready to read the
      !>         file ID word from the DAF transfer file.
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
      !>-    SPICELIB Version 3.1.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.0.1, 22-AUG-2001 (EDW)
      !>
      !>        Corrected ENDIF to END IF.
      !>
      !>-    SPICELIB Version 3.0.0, 25-JAN-1995 (KRG)
      !>
      !>        Updated the header and in line comments to reflect the change
      !>        from calling files text files to calling them transfer files.
      !>
      !>        Changed the variable name TXTLUN to XFRLUN to make it
      !>        compatible with the change in terminology.
      !>
      !>        Changed the short error message from "BADDAFTEXTFILE" to
      !>        "BADDAFTRANSFERFILE".
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1993 (KRG)
      !>
      !>        This routine was modified to incorporate the file ID word
      !>        changes which will allow run time identification of the type of
      !>        data in a SPICE binary file.
      !>
      !>        Removed the error SPICE(IDWORDNOTKNOWN) as it was no longer
      !>        relevant.
      !>
      !>        Added the error SPICE(NOTADAFFILE) if this routine is called
      !>        with a file that does not contain an ID word identifying the
      !>        file as a DAF file.
      !>
      !>-    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG)
      !>
      !>        Modified the description of the DAF encoded text file format
      !>        appearing before the program code.
      !>
      !>-    SPICELIB Version 1.0.0, 02-NOV-1992 (KRG)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 3.0.0, 25-JAN-1995 (KRG)
      !>
      !>        Updated the header and in line comments to reflect the change
      !>        from calling files text files to calling them transfer files.
      !>
      !>        Changed the variable name TXTLUN to XFRLUN to make it
      !>        compatible with the change in terminology.
      !>
      !>        Changed the short error message from "BADDAFTEXTFILE" to
      !>        "BADDAFTRANSFERFILE".
      !>
      !>-    SPICELIB Version 2.0.0, 04-SEP-1993 (KRG)
      !>
      !>        This routine was modified to incorporate the file ID word
      !>        changes which will allow runtime identification of the type of
      !>        data in a binary file SPICE binary file.
      !>
      !>        Removed the error SPICE(IDWORDNOTKNOWN) as it was no longer
      !>        relevant.
      !>
      !>        Added the error SPICE(NOTADAFFILE) if this routine is called
      !>        with a file that does not contain an ID word identifying the
      !>        file as a DAF file.
      !>
      !>-    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG)
      !>
      !>        Modified the description of the DAF encoded text file format
      !>        appearing before the program code. Changed the line:
      !>
      !>           C        < DAF ND value > < DAF NI value >
      !>
      !>        to the lines:
      !>
      !>           C        < DAF ND value >
      !>           C        < DAF NI value >
      !>
      !>        This change was necessary because the output format for the
      !>        low level routines which encode and write the data were
      !>        modified to fix a problem. See the routines WRENCD and WRENCI
      !>        for details of the modification.
      module subroutine daftb ( ctx, xfrlun, binfil )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: nextwd, nparsi, nextwd_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: xfrlun  !! Logical unit of an open DAF transfer file.
      character(*), intent(in) :: binfil  !! Name of a binary DAF file to be created.

!     Local Parameters
!




      integer(ip), parameter :: linlen = 255

      integer(ip), parameter :: errlen = 320




      integer(ip), parameter :: resrec = 0
      character(idwlen) :: idword
      character(ifnlen) :: ifname
      character(errlen) :: errmsg
      character(linlen) :: line
      character(crecl) :: name
      character(linlen) :: rest
      character(idwlen) :: tarch
      character(idwlen) :: ttype
      character(linlen) :: word

      real(wp) :: buffer( bufsiz )
      real(wp) :: dsumry( 125    )
      real(wp) :: summry( 125    )

      integer(ip) :: arrcnt
      integer(ip) :: barr
      integer(ip) :: bcnt
      integer(ip) :: binhdl
      integer(ip) :: dtacnt
      integer(ip) :: earr
      integer(ip) :: ecnt
      integer(ip) :: errptr
      integer(ip) :: iostat
      integer(ip) :: isumry( 250    )
      integer(ip) :: lftovr
      integer(ip) :: nd
      integer(ip) :: ni
      integer(ip) :: numarr
      integer(ip) :: numdta
      integer(ip) :: numlft
      integer(ip) :: snmlen

      logical :: inarr
      logical :: more
!
!     Standard/ SPICE error handling.
!
      if ( ctx%failed() ) return
!
!     A brief description of the DAF transfer file format and its
!     intended use follows. This description is intended to provide a
!     simple ``picture'' of the DAF transfer file format to aid in the
!     understanding of this routine. This description is NOT intended to
!     be a detailed specification of the file format.
!
!     A DAF transfer file contains all of the data from a binary
!     DAF file, except for the reserved record area, in an encoded
!     ASCII format. The file also contains some bookkeeping information
!     for maintaining the integrity of the data. The DAF transfer file
!     format allows the full precision of both integer and floating
!     point numeric data to be maintained in a portable fashion. The DAF
!     transfer file format is intended to provide a reliable and
!     accurate means for porting data among multiple computer systems
!     and for the archival storage of data.
!
!     A DAF transfer file is not intended to be used directly to
!     provide data to a program, the equivalent binary DAF file is
!     to be used for this purpose. In no way should any program, other
!     than a DAF binary <-> transfer conversion program, rely on the DAF
!     encoded transfer file format.
!
!     To correctly understand the DAF transfer file description
!     the reader should be familiar with the DAF file architecture.
!     Items enclosed in angle brackets, '<' and '>', are used to
!     represent the data which is to be placed at that position in
!     the file. The bookkeeping information is represented exactly
!     as it would appear in a DAF transfer file.
!
!     Let
!
!       BOF    denote the beginning of the file
!       EOF    denote the end of the file
!
!    and
!
!       n      denote the total number of arrays in a DAF file
!       NA(i)  denote the number of double precision numbers in array i
!       m(i)   denote the number of blocks of encoded data for array i
!       N(i,j) denote the number of encoded double precision numbers
!              in block j of array i
!
!     and
!
!              m(i)
!             -----
!             \
!              >   N(i,k) = NA(i),   i = 1, ..., n.
!             /
!             -----
!              k=1
!
!     A DAF encoded transfer file has the following format:
!
!        <BOF>
!        < Information line >
!        < DAF file ID word >
!        < DAF ND value >
!        < DAF NI value >
!        < DAF internal file name >
!        BEGIN_ARRAY 1 NA(1)
!        < Name for array 1 >
!        < ND double precision summary values >
!        < NI-2 integer summary values >
!        N(1,1)
!        < N(1,1) Encoded double precision numbers >
!        N(1,2)
!        < N(1,2) Encoded double precision numbers >
!                          .
!                          .
!                          .
!        N(1,m(1))
!        < N(1,m(1)) Encoded double precision numbers >
!        END_ARRAY 1 NA(1)
!        BEGIN_ARRAY 2 NA(2)
!        < Name for array 2 >
!        < ND double precision summary values >
!        < NI-2 integer summary values >
!        N(2,1)
!        < N(2,1) Encoded double precision numbers >
!        N(2,2)
!        < N(2,2) Encoded double precision numbers >
!                          .
!                          .
!                          .
!        N(2,m(2))
!        < N(2,m(2)) Encoded double precision numbers >
!        END_ARRAY 2 NA(2)
!                          .
!                          .
!                          .
!        BEGIN_ARRAY n NA(n)
!        < Name for array n >
!        < ND double precision summary values >
!        < NI-2 integer summary values >
!        N(n,1)
!        < N(n,1) Encoded double precision numbers >
!        N(n,2)
!        < N(n,2) Encoded double precision numbers >
!                          .
!                          .
!                          .
!        N(n,m(n))
!        < N(n,m(n)) Encoded double precision numbers >
!        END_ARRAY n NA(n)
!        TOTAL_ARRAYS  n
!        <EOF>
!
!     Initialize a few things.
!
      tarch  = ' '
      ttype  = ' '
      idword = ' '
!
!     We begin by reading the DAF file ID word from the DAF transfer
!     file. We should have been positioned ready to read this. If an
!     error occurs, set an appropriate error message and signal the
!     error.
!
      read (xfrlun,*,iostat=iostat) idword

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'Error reading the file ID word from the'  // &
                       ' DAF transfer file ''#''. IOSTAT = #.'     )
         call ctx%errfnm ( '#', xfrlun                                 )
         call ctx%errint ( '#', iostat                                 )
         call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                     )
         return

      end if
!
!     Separate the ID word into its components and verify that we are
!     looking at a DAF transfer file. If we're not, then this routine
!     should not be used.
!
      call idw2at ( ctx, idword, tarch, ttype )

      if ( tarch /= 'DAF' ) then

         call ctx%setmsg ( 'File architecture is not ''DAF'' for' // &
                       ' file ''#'''                           )
         call ctx%errfnm ( '#', xfrlun                             )
         call ctx%sigerr ( 'SPICE(NOTADAFFILE)'                    )
         return

      end if
!
!     The file architecture is OK, but before we can open the binary
!     DAF, we need to get the summary format and the internal file name
!     from the DAF transfer file. We begin doing this here.
!
!     Read in the ND and NI values for the DAF file.
!
      call rdenci( ctx, xfrlun, 2, isumry )

      if ( ctx%failed() ) return

      nd = isumry(1)
      ni = isumry(2)
!
!     Read the internal filename for the DAF file.
!
      read (xfrlun,*,iostat=iostat) ifname

      if ( iostat /= 0 ) then

         call ctx%setmsg ( 'Error reading the internal filename from'  // &
                       ' the DAF transfer file ''#''. IOSTAT = #.'  )
         call ctx%errfnm ( '#', xfrlun                                  )
         call ctx%errint ( '#', iostat                                  )
         call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                      )
         return

      end if
!
!     Open a new binary DAF file. Call the proper open routine,
!     depending on whether it's a new file or an old file.
!
      if ( ttype /= '?' ) then
         call dafonw ( ctx, binfil, ttype, nd, ni, ifname, resrec, binhdl )
      else
         call dafopn ( ctx, binfil, nd, ni, ifname, resrec, binhdl )
      end if

      if ( ctx%failed() ) return
!
!     Calculate the length of the segment names.
!
      snmlen = 8 * ( nd + ( ni + 1 ) / 2 )
!
!     Initialize a few things: the array counter and the data counter.
!
      arrcnt = 0
      dtacnt = 0
!
!     We currently have more to process.
!
      more  = .true.
!
!     We are currently not processing an array.
!
      inarr = .false.
!
!     Begin converting the DAF transfer file into a binary DAF file
!     here.
!
      do while ( more )

         read (xfrlun,fmt='(A)',iostat=iostat) line

         if ( iostat /= 0 ) then

            call ctx%setmsg ( 'Error reading from the DAF transfer'  // &
                          ' file ''#''. IOSTAT = #.'              )
            call ctx%errfnm ( '#', xfrlun                             )
            call ctx%errint ( '#', iostat                             )
            call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                 )
            return

         end if
!
!        At this point, we should be beginning an array, ending an
!        array, or scanning for the total number of arrays. So look
!        for the appropriate keyword.
!
         call nextwd( line, word, rest )

         if ( word == begarr ) then
!
!           Get the array number.
!
            call nextwd_inplace ( rest, word )
            call nparsi( word, barr, errmsg, errptr )

            if ( errmsg /= ' ' ) then

               call ctx%setmsg ( 'Begin array error, could not parse'  // &
                             ' array number. Error: # File: #'      )
               call ctx%errch  ( '#', errmsg                            )
               call ctx%errfnm ( '#', xfrlun                            )
               call ctx%sigerr ( 'SPICE(BADDAFTRANSFERFILE)'            )
               return

            end if
!
!           Parse the count of double precision numbers in the array.
!
            call nextwd_inplace ( rest, word )
            call nparsi( word, bcnt, errmsg, errptr )

            if ( errmsg /= ' ' ) then

               call ctx%setmsg ( 'Begin array error, could not parse' // &
                             ' the data count for array: #.'      // &
                             ' Error: # File: #'                   )
               call ctx%errint ( '#', barr                             )
               call ctx%errch  ( '#', errmsg                           )
               call ctx%errfnm ( '#', xfrlun                           )
               call ctx%sigerr ( 'SPICE(BADDAFTRANSFERFILE)'           )
               return

            end if
!
!           If we got to here, we are inside an array, so set the in
!           array flag, INARR, to .TRUE. and increment the array
!           counter.
!
            inarr  = .true.
            arrcnt = arrcnt + 1

         else if ( word == endarr ) then
!
!           Get the array number.
!
            call nextwd_inplace ( rest, word )
            call nparsi( word, earr, errmsg, errptr )

            if ( errmsg /= ' ' ) then

               call ctx%setmsg ( 'End array error, could not parse'  // &
                             ' array number. Error: # File: #'    )
               call ctx%errch  ( '#', errmsg                          )
               call ctx%errfnm ( '#', xfrlun                          )
               call ctx%sigerr ( 'SPICE(BADDAFTRANSFERFILE)'          )
               return

            end if
!
!           Parse the count of double precision numbers in the array.
!
            call nextwd_inplace ( rest, word )
            call nparsi( word, ecnt, errmsg, errptr )

            if ( errmsg /= ' ' ) then

               call ctx%setmsg ( 'End array error, could not parse' // &
                             ' the data count for array: #.'    // &
                             ' Error: # File: #'                 )
               call ctx%errint ( '#', earr                           )
               call ctx%errch  ( '#', errmsg                         )
               call ctx%errfnm ( '#', xfrlun                         )
               call ctx%sigerr ( 'SPICE(BADDAFTRANSFERFILE)'         )
               return

            end if
!
!           Check to see if the beginning and ending array numbers
!           match. If not, signal an appropriate error.
!
            if ( earr /= barr ) then

               call ctx%setmsg ( 'Data array number mismatch:'         // &
                             ' Beginning number: #; Ending'        // &
                             ' number: #. File: #'                  )
               call ctx%errint ( '#', barr                              )
               call ctx%errint ( '#', earr                              )
               call ctx%errfnm ( '#', xfrlun                            )
               call ctx%sigerr ( 'SPICE(BADDAFTRANSFERFILE)'            )
               return

            end if
!
!           Check to see if the beginning and ending array data counts
!           match. If not, signal an appropriate error.
!
            if ( ecnt /= bcnt ) then

               call ctx%setmsg ( 'Data array count mismatch:'      // &
                             ' Beginning count: #;'            // &
                             ' Ending count: #. File: #'        )
               call ctx%errint ( '#', bcnt                          )
               call ctx%errint ( '#', ecnt                          )
               call ctx%errfnm ( '#', xfrlun                        )
               call ctx%sigerr ( 'SPICE(BADDAFTRANSFERFILE)'        )
               return

            end if
!
!           If we got to here, we have successfully ended the
!           processing of an array, so set the in array flag, INARR,
!           to  .FALSE..
!
            inarr = .false.

         else if ( word == totarr ) then
!
!           We have the total arrays keyword to parse, so get
!           the total number of arrays processed.
!
            call nextwd_inplace ( rest, word )
            call nparsi( word, numarr, errmsg, errptr )

            if ( errmsg /= ' ' ) then

               call ctx%setmsg ( 'Array count error, could not parse' // &
                             ' the total number of arrays: #.'    // &
                             ' File: #'                            )
               call ctx%errch  ( '#', errmsg                           )
               call ctx%errfnm ( '#', xfrlun                           )
               call ctx%sigerr ( 'SPICE(BADDAFTRANSFERFILE)'           )
               return

            end if

            if ( arrcnt /= numarr ) then

                  call ctx%setmsg ( 'The number of data arrays'          // &
                                ' processed (#) was not equal to'    // &
                                ' the number of data arrays placed'  // &
                                ' in the DAF transfer file (#).'     // &
                                ' File: #'                            )
                  call ctx%errint ( '#', arrcnt                           )
                  call ctx%errint ( '#', numarr                           )
                  call ctx%errfnm ( '#', xfrlun                           )
                  call ctx%sigerr ( 'SPICE(BADDAFTRANSFERFILE)'           )
                  return

            end if
!
!           If we got to here, we have successfully processed the
!           entire data portion of the DAF transfer file, so there is
!           no more data.
!
            more = .false.

         else

            call ctx%setmsg ( 'Unknown keyword ''#'' encountered'      // &
                          ' while processing the DAF transfer'     // &
                          ' file #.'                                )
            call ctx%errch  ( '#', word                                 )
            call ctx%errfnm ( '#', xfrlun                               )
            call ctx%sigerr ( 'SPICE(BADDAFTRANSFERFILE)'               )
            return

         end if
!
!        If we have begun an array, then process it. Otherwise, we
!        have either ended an array or ended the file.
!
         if ( inarr ) then

            dtacnt = 0

            read (xfrlun,*,iostat=iostat) name( :snmlen)

            if ( iostat /= 0 ) then

               call ctx%setmsg ( 'Error reading the array name from the' // &
                             ' DAF transfer file #. IOSTAT = #.'      )
               call ctx%errfnm ( '#', xfrlun                              )
               call ctx%errint ( '#', iostat                              )
               call ctx%sigerr ( 'SPICE(FILEREADFAILED)'                  )
               return

            end if
!
!           Read in the double precision part of the summary.
!
            call rdencd ( ctx, xfrlun, nd, dsumry )

            if ( ctx%failed() ) return
!
!           Read in the integer part of the summary. The beginning and
!           ending addresses, ISUMRY(NI-1) and ISUMRY(NI), for the
!           array are not known currently. They will be filled in when
!           the array is actually written to the DAF file.
!
            call rdenci( ctx, xfrlun, ni-2, isumry )

            if ( ctx%failed() ) return
!
!           Pack the summary information into the DAF array summary.
!
            call dafps  ( ctx, nd, ni, dsumry, isumry, summry )
!
!           Begin a new array in the binary DAF file.
!
            call dafbna ( ctx, binhdl, summry, name( :snmlen) )

            if ( ctx%failed() ) return
!
!           Read and decode the data in the current DAF array.
!
!           First set the count of numbers yet to be decoded and placed
!           in the binary DAF file.
!
            numlft = bcnt

            do while ( numlft > 0 )
!
!              First, read in the count of encoded numbers in the
!              current data block.
!
               read (xfrlun,*,iostat=iostat) numdta

               if ( iostat /= 0 ) then

                  call ctx%setmsg ( 'Error reading array data from the' // &
                                ' DAF transfer file #. IOSTAT = #.'  )
                  call ctx%errfnm ( '#', xfrlun                          )
                  call ctx%errint ( '#', iostat                          )
                  call ctx%sigerr ( 'SPICE(FILEREADFAILED)'              )
                  return

               end if
!
!              Now read and decode the data in the current data block,
!              placing the data in the current array in the binary DAF
!              file.
!
               lftovr = numdta

               do while ( lftovr > 0 )

                  if ( lftovr >= bufsiz ) then

                     numdta = bufsiz

                  else

                     numdta = lftovr

                  end if
!
!                 Read and decode a buffer of encoded double precision
!                 data from the DAF transfer file.
!
                  call rdencd ( ctx, xfrlun, numdta, buffer )

                  if ( ctx%failed() ) return
!
!                 Write the double precision data to the current array
!                 in the binary DAF file.
!
                  call dafada ( ctx, buffer, numdta )

                  if ( ctx%failed() ) return
!
!                 Decrement the counters for the amount of data
!                 remaining to be moved from the current data block,
!                 LFTOVR, and the current array, NUMLFT.
!
                  lftovr = lftovr - numdta
                  numlft = numlft - numdta
!
!                 Increment the counter for the amount of data that
!                 has been successfully moved into the current array
!                 in the binary DAF file.
!
                  dtacnt = dtacnt + numdta

               end do
!
!              At this point, we have either finished reading in the
!              entire array, or we have just completed reading the
!              current encoded block of data for the current array
!              from the DAF transfer file.
!
            end do
!
!           If we got to here, we have successfully written an array
!           to the binary file, so we need to end it.
!
            call dafena ( ctx )

            if ( ctx%failed() ) return

         end if

      end do
!
!     Close only the binary file.
!
      call dafcls ( ctx, binhdl )

      return
      end subroutine daftb

   !> PEPPER: make room in the DAFFA state table for one more search (as
   !> [[dafbfs]] and [[dafbbs]] need when they start a search of a DAF not in the
   !> table): if no entry is free, free the entries of DAFs that are not
   !> open any more (as SPICELIB did), and if none is, double the table.
   !> Called before the ASSOCIATE blocks of [[dafbfs]] and [[dafbbs]], since the
   !> table may be reallocated.
   subroutine daffa_room(ctx)
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip) :: p, prev, nextp
      logical :: open
      if (ctx%daffa%stfptr /= nil) return
      p = ctx%daffa%sthead
      prev = nil
      do while (p /= nil)
         open = .false.
         open = ctx%spice%dafah%find(ctx%daffa%stfh(p)) > 0
         if (open) then
            prev = p
            p = ctx%daffa%stpool(p)
         else
            nextp = ctx%daffa%stpool(p)
            if (p == ctx%daffa%sthead) then
               ctx%daffa%sthead = nextp
            else
               ctx%daffa%stpool(prev) = nextp
            end if
            ctx%daffa%stpool(p) = ctx%daffa%stfptr
            ctx%daffa%stfptr = p
            p = nextp
         end if
      end do
      if (ctx%daffa%stfptr == nil) call daffa_grow(ctx%daffa)
   end subroutine daffa_room

   !> PEPPER: make room in the DAFANA state table before [[dafbna]] (which
   !> may need one more entry) enters its ASSOCIATE block: if no entry is
   !> free and [[dafbna]]'s two ways of freeing one (entries of DAFs no longer
   !> open; entries of DAFs to which no array is being added) would find
   !> none, double the table.
   subroutine dafana_room(ctx)
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip) :: p
      logical :: open
      if (ctx%dafana%stfptr /= nil) return
      p = ctx%dafana%sthead
      do while (p /= nil)
         open = .false.
         open = ctx%spice%dafah%find(ctx%dafana%stfh(p)) > 0
         if (.not. open .or. .not. ctx%dafana%staddg(p)) return
         p = ctx%dafana%stpool(p)
      end do
      call dafana_grow(ctx%dafana)
   end subroutine dafana_room

end submodule pepper_daf
