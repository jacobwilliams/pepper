!> EK files (E-kernels) at the storage level: pages, records, column entries of all classes, B*-trees of the indexes, segment descriptors, and the scratch area.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
!>
!>## ZZEKSCA (EK, scratch area)
!>
!> PEPPER: SPICELIB's umbrella routine ZZEKSCA is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzeksca. Its documentation follows.
!>
!>### Abstract
!>
!>     Manage the EK scratch area.
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
!>     None.
!>
!>### Exceptions
!>
!>     1) If this routine is called directly, the error SPICE(BOGUSENTRY)
!>        is signaled.
!>
!>     See the entry points for discussions of exceptions specific to
!>     those routines.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     The specific implementation of the EK scratch area is NOT
!>     considered part of the specification of this suite of routines:
!>     the implementation may be changed without notice.  However,
!>     some aspects of the current implementation, such as scratch
!>     file usage, are visible to users and therefore are discussed
!>     in this subroutine header.
!>
!>     The EK system, in searching for events that satisfy a query,
!>     produces intermediate results that require a potentially very
!>     large amount of storage, more than can be expected to be
!>     available in the form of memory.  On the other hand, in order
!>     to achieve reasonable query response time, these intermediate
!>     results must be capable of being accessed quickly.  The EK
!>     scratch area provides a storage location that uses a combination
!>     of memory and disk storage to give the EK system a large storage
!>     area, part of which can be rapidly accessed.
!>
!>     The logical structure of the EK scratch area is that of a large
!>     one-dimensional integer stack.  The indices of the  elements of
!>     this stack are referred to as scratch area `addresses'.  Scratch
!>     area addresses start at 1 and increase.  The maximum address is
!>     the maximum integer representable on the host computer, but the
!>     maximum usable address depends on the disk storage available
!>     to the calling program at the time the program is run.
!>
!>     The EK scratch area has access routines that allow a calling
!>     program to write to and read from it. Calling routines must
!>     coordinate their use of the scratch area:  the scratch area is
!>     effectively a global data structure.  Routines outside of the EK
!>     system should not use the scratch area.
!>
!>     The EK scratch area routines are:
!>
!>        ZZEKSCA  ( EK scratch area umbrella routine )
!>        ZZEKSTOP ( EK scratch area, return stack pointer )
!>        ZZEKSPSH ( EK scratch area, push data onto stack )
!>        ZZEKSDEC ( EK scratch area, decrement stack pointer )
!>        ZZEKSPOP ( EK scratch area, pop data from stack )
!>        ZZEKSUPD ( EK scratch area, update data )
!>        ZZEKSRD  ( EK scratch area, read data )
!>        ZZEKSCLN ( EK scratch area, clean up )
!>
!>### Examples
!>
!>     1)  Push data on the scratch area stack.
!>
!>            C
!>            C     Push N items onto the stack.
!>            C
!>                  CALL ZZEKSPSH ( N, DATA )
!>
!>     2)  Update a range of addresses that may span the stack top.
!>
!>            C
!>            C     Since we can't leave a gap between the stack top
!>            C     and the start of the range of addresses we write to,
!>            C     we'll need to know where the top is.  The address
!>            C     range to update is BEG:END.
!>            C
!>                  CALL ZZEKSTOP ( TOP )
!>
!>                  IF ( BEG .GT. TOP ) THEN
!>
!>                     [ Handle error case ]
!>
!>                  ELSE
!>
!>                     CALL ZZEKSUPD ( BEG, END, DATA )
!>
!>                  END IF
!>
!>     3)  Read from the scratch area.
!>
!>            C
!>            C     Read the contents of the scratch area address
!>            C     range BEG:END into the integer array DATA:
!>            C
!>                  CALL ZZEKSTOP ( TOP )
!>
!>                  IF ( BEG .GT. TOP ) THEN
!>
!>                     [ Handle error case ]
!>
!>                  ELSE
!>
!>                     CALL ZZEKSRD ( BEG, END, DATA )
!>
!>                  END IF
!>
!>### Restrictions
!>
!>     1)  The current implementation of this suite of routines opens
!>         a scratch file.  The logical unit connected to the scratch
!>         file counts against the total that may be used by the calling
!>         program.  Also, the scratch file, if written to, will occupy
!>         additional disk storage.
!>
!>     2)  This suite of routines should not be used by routines outside
!>         of the EK system.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman   (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 3.2.1, 03-OCT-2021 (NJB)
!>
!>        Corrected typos in comments.
!>
!>-    SPICELIB Version 3.2.0, 28-JUN-2005 (NJB)
!>
!>        Increased buffer size from 500K to 2M integers.
!>
!>-    SPICELIB Version 3.1.0, 29-JUL-2003 (NJB)
!>
!>        Added DASWBR call to entry point ZZEKCLN.  This call frees
!>        the buffer records used by the scratch file.
!>
!>-    SPICELIB Version 3.0.0, 13-DEC-2001 (NJB)
!>
!>        Added entry point ZZEKCLN.
!>
!>-    Beta Version 2.0.0, 02-NOV-1995 (NJB)
!>
!>        Updated for EK architecture 3.
!>
!>-    Beta Version 1.1.0, 01-AUG-1994 (NJB)
!>
!>        Scratch area buffer size increased to 500K integers.
!>        On 32-bit systems, this amounts to 2Mb of storage.
!>
!>-    Beta Version 1.0.1, 25-FEB-1993 (NJB)
!>
!>        Documented.
!>
!>-    Beta Version 1.0.0, 16-DEC-1992 (NJB)
!>
!>### Revisions
!>
!>-    SPICELIB Version 3.2.0, 28-JUN-2005 (NJB)
!>
!>        Increased buffer size from 500K to 2M integers.
!>
!>-    Beta Version 2.0.0, 08-SEP-1994 (NJB)
!>
!>        Updated for EK architecture 3.
!>
!>-    Beta Version 1.1.0, 01-AUG-1994 (NJB)
!>
!>        Scratch area buffer size increased to 500K integers.
!>        On 32-bit systems, this amounts to 2Mb of storage.
!>
!>### Implementation notes
!>
!>     The parameter MEMSIZ is the size of an integer array used as
!>     part of the scratch area.  The first MEMSIZ scratch area addresses
!>     refer to elements of this array.  Additional storage is supplied
!>     by the integer logical array of a scratch DAS file; the first
!>     word of the scratch DAS file corresponds to scratch area address
!>     MEMSIZ + 1.
!>
!>## ZZEKPAGE (Private: Manage EK DAS paging system)
!>
!> PEPPER: SPICELIB's umbrella routine ZZEKPAGE is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzekpage. Its documentation follows.
!>
!>### Abstract
!>
!>     SPICE Private routine intended solely for the support of SPICE
!>     routines.  Users should not call this routine directly due
!>     to the volatile nature of this routine.
!>
!>     Manage EK DAS paging system.
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
!>     None.
!>
!>### Exceptions
!>
!>     1)  If this routine is called directly, the error
!>         SPICE(BOGUSENTRY) will be signaled.
!>
!>     See the entry points for discussions of errors particular to
!>     those routines.
!>
!>### Files
!>
!>     This suite of routines provides paged access to DAS files.  Only
!>     DAS files initialized via a call to ZZEKPGIN may be written or
!>     read by these routines.
!>
!>### Particulars
!>
!>     The EK paging system provides a means for the rest of the EK
!>     system to allocate and deallocate contiguous blocks of DAS
!>     addresses of character, d.p. and integer type.  The rest of the EK
!>     system never accesses EK files directly; it only reads and writes
!>     pages allocated via this system.
!>
!>     Much of the page allocation and de-allocation performed by
!>     higher-level routines is done via the routines ZZEKAPS and
!>     ZZEKDPS; those routines should be called if applicable, rather
!>     than ZZEKPGAL, ZZEKPGAN, or ZZEKPGFR.
!>
!>### Examples
!>
!>     Initialization:               see EKOPN.
!>     Page allocation:              see EKAPS.
!>     Writing:                      see ZZEKAD01, ZZEKAD02, ZZEKAD03.
!>     Reading:                      see ZZEKRD01, ZZEKRD02, ZZEKRD03.
!>     Freeing pages:                see ZZEKDPS.
!>     Address-to-page mapping:      see EKDELR.
!>     Page number-to-base mapping:  see ZZEKAD0x
!>
!>### Restrictions
!>
!>     1) Only `empty' DAS files may be initialized for paged access.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     B.V. Semenov       (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 1.2.0, 14-OCT-2021 (BVS)
!>
!>        Bug fix: fixed routine name in CHKIN/CHKOUT calls in
!>        entry point ZZEKPGPG (ZZEKPGBS -> ZZEKPGPG).
!>
!>-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
!>
!>        Now uses ERRHAN to insert DAS file name into
!>        long error messages.
!>
!>-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
!>
!>### Implementation notes
!>
!>     Note:  the integer fill buffer should be as large as the maximum
!>     of the integer page size and the metadata area size.
!>
!>## ZZEKVADR (Compute row vector address)
!>
!> PEPPER: SPICELIB's umbrella routine ZZEKVADR is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzekvadr. Its documentation follows.
!>
!>### Abstract
!>
!>     Compute the EK scratch area base address of the row vector having
!>     a specified index, given a union of EK join row sets and a row
!>     vector index. Also return the base address of the row
!>     vector's parent segment vector.
!>
!>### Detailed Input
!>
!>     See the entry points for a discussion of their arguments.
!>
!>### Detailed Output
!>
!>     See the entry points for a discussion of their arguments.
!>
!>### Parameters
!>
!>     MXJOIN         is the maximum number of tables that can be joined.
!>
!>     MXJRS          is the maximum number of join row sets allowed in
!>                    in the input union identified by BASES and NJRS.
!>
!>### Exceptions
!>
!>     1)  This is an umbrella routine which contains declarations
!>         for its entry points.  This routine should never be called
!>         directly.  If it is, the error SPICE(BOGUSENTRY) will be
!>         signaled.
!>
!>     See the entry points for discussions of the exceptions specific
!>     to those entry points.
!>
!>### Files
!>
!>     1)  This routine uses the EK scratch area, which employs a scratch
!>         DAS file.
!>
!>### Particulars
!>
!>     In the course of query resolution, the EK system builds a set of
!>     data structures called `join row sets' that represent the rows
!>     that satisfy the query constraints.  These rows belong to a table
!>     formed by taking the Cartesian product of the tables in the FROM
!>     clause of the query.  One join row set is formed for each
!>     conjunction of join constraints; the total number of join row sets
!>     is equal to the number of conjunctions of join constraints in
!>     the query.  Join row sets are described below.
!>
!>     This group of routines allows the EK system to view the rows
!>     matching a query as a sequence of vectors, where each vector is an
!>     n-tuple of row numbers designating rows in segments of the
!>     Cartesian product of tables specified in the input query.  These
!>     vectors are called `row vectors'.  Each row vector also points to
!>     a vector of segments that contain the rows represented by the row
!>     vector.
!>
!>     These routines centralize the calculations needed to locate the
!>     nth row vector.
!>
!>     Each join row set consists of:
!>
!>         - a base address in the scratch area
!>         - a table count
!>         - a segment vector count
!>         - a set of segment vectors
!>         - a set of segment vector row vector base addresses
!>           (these are relative to the base of the join row set)
!>         - a set of segment vector row vector counts
!>         - a set of row vectors, augmented by offsets of their
!>           parent segment vectors (these offsets are at the
!>           end of each row vector)
!>
!>     The layout of a join row set in the EK scratch area is shown
!>     below:
!>
!>        +--------------------------------------------+
!>        |              join row set size             |  1 element
!>        +--------------------------------------------+
!>        |    number of row vectors in join row set   |  1 element
!>        +--------------------------------------------+
!>        |               table count (TC)             |  1 element
!>        +--------------------------------------------+
!>        |          segment vector count (SVC)        |  1 element
!>        +--------------------------------------------+
!>        |               segment vector 1             |  TC elements
!>        +--------------------------------------------+
!>                              .
!>                              .
!>                              .
!>        +--------------------------------------------+
!>        |               segment vector SVC           |  TC elements
!>        +--------------------------------------------+
!>        |   segment vector 1 row set base address    |  1 element
!>        +--------------------------------------------+
!>        |      segment vector 1 row count (RC_1)     |  1 element
!>        +--------------------------------------------+
!>                              .
!>                              .
!>                              .
!>        +--------------------------------------------+
!>        |  segment vector SVC row set base address   |  1 element
!>        +--------------------------------------------+
!>        |   segment vector SVC row count (RC_SVC)    |  1 element
!>        +--------------------------------------------+
!>        | Augmented row vectors for segment vector 1 |  TC*(RC_1 + 1 )
!>        +--------------------------------------------+  elements
!>                              .
!>                              .
!>                              .
!>        +--------------------------------------------+
!>        |Augmented row vectors for segment vector SVC|  TC*(RC_SVC + 1)
!>        +--------------------------------------------+  elements
!>
!>### Examples
!>
!>     1)  For a given join row set union, initialize the addressing
!>         routines, then look up row vectors.
!>
!>            C
!>            C     Tell the addressing routines where the join row set
!>            C     union is.  NJRS is the number of join row sets in
!>            C     the union, BASES is an array of EK scratch area base
!>            C     addresses of each join row set.  A base address is
!>            C     the predecessor of the first address actually
!>            C     occupied by a join row set.
!>            C
!>                  CALL ZZEKVSET ( NJRS, BASES )
!>
!>            C
!>            C     Find the base address of the each row vector, as well
!>            C     as the base address of the corresponding segment
!>            C     vector.
!>            C
!>                  DO I = 1, NJRS
!>
!>                     CALL EKVCAL ( I, RWVBAS, SGVBAS )
!>
!>                     [Do something with the row vector....]
!>
!>                  END DO
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
!>     N.J. Bachman   (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 1.0.2, 01-OCT-2021 (NJB)
!>
!>        Updated header Abstract section. Corrected typos in comments.
!>
!>-    SPICELIB Version 1.0.1, 06-SEP-2006 (NJB)
!>
!>        Filled in Particulars section of header in entry point
!>        ZZEKVCAL.  Changed previous version line's product from "Beta"
!>        to "SPICELIB" both here and in ZZEKVCAL.
!>
!>-    SPICELIB Version 1.0.0, 28-SEP-1994 (NJB) (WLT)
!>
!>### Implementation notes
!>
!>     Include Section:  EK Join Row Set Parameters
!>
!>        JRS$INC Version 1    17-SEP-1994 (NJB)
!>
!>     Base-relative index of join row set size
!>
!>     Index of row vector count
!>
!>     Index of table count
!>
!>     Index of segment vector count
!>
!>     Base address of first segment vector
!>
!>     End Include Section:  EK Join Row Set Parameters
submodule (pepper_instance_module) pepper_ek_storage

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: term = 1  !! tree balancing state: done
   integer(ip), parameter :: lcheck = term   + 1  !! tree balancing state: check the left neighbor
   integer(ip), parameter :: rcheck = lcheck + 1  !! tree balancing state: check the right neighbor
   integer(ip), parameter :: balnce = rcheck + 1  !! tree balancing state: balance the node
   integer(ip), parameter :: lnsize = 80  !! length of a line of text (messages, kernel lines)
   integer(ip), parameter :: maxnam = 15  !! maximum length of a query name
   integer(ip), parameter :: memsiz = 2500000  !! size of the scratch area (records)
   integer(ip), parameter :: namlen = 32  !! length of a table or column name

contains

      !> Close an E-kernel.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the file handle of an EK to be closed. Note
      !>              that EKs open for writing must be closed by this
      !>              routine in order to be valid.
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
      !>     1)  If the indicated file is not recognized, no error is
      !>         signaled.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading ek.req for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine should be used to close open EK files. EK files
      !>     open for writing *must* be closed by this routine in order to be
      !>     valid. EK files open for read access should also be closed using
      !>     this routine.
      !>
      !>     EKs open for reading won't be corrupted if closed via a FORTRAN
      !>     CLOSE statement, but the underlying bookkeeping software will
      !>     become confused if an EK is closed this way---so we recommend
      !>     closing EK files with EKCLS exclusively.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following program demonstrates how to create a new EK and
      !>        add data to a character column in a given record within the
      !>        file, how to update the data in this record, and how to read
      !>        the data from it.
      !>
      !>        The example shows the effect of the EKCLS calls when the EK
      !>        file has been opened for write or read access.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EKCLS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Include the EK Column Name Size (CNAMSZ)
      !>C
      !>      INCLUDE 'ekcnamsz.inc'
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      CHARACTER*(*)         EKNAME
      !>      PARAMETER           ( EKNAME = 'ekcls_ex1.bdb' )
      !>
      !>      CHARACTER*(*)         IFNAME
      !>      PARAMETER           ( IFNAME = 'Test EK'  )
      !>
      !>      CHARACTER*(*)         TABLE
      !>      PARAMETER           ( TABLE  = 'CHR_DATA' )
      !>
      !>      INTEGER               CVLEN
      !>      PARAMETER           ( CVLEN  = 9  )
      !>
      !>      INTEGER               DECLEN
      !>      PARAMETER           ( DECLEN = 200 )
      !>
      !>      INTEGER               MAXVAL
      !>      PARAMETER           ( MAXVAL = 4  )
      !>
      !>      INTEGER               NCOLS
      !>      PARAMETER           ( NCOLS  = 2  )
      !>
      !>      INTEGER               NROWS
      !>      PARAMETER           ( NROWS  = 6  )
      !>
      !>      INTEGER               NRESVC
      !>      PARAMETER           ( NRESVC = 0  )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(DECLEN)    CDECLS ( NCOLS  )
      !>      CHARACTER*(CNAMSZ)    CNAMES ( NCOLS  )
      !>      CHARACTER*(CVLEN)     CVALS  ( MAXVAL )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               NVALS
      !>      INTEGER               RECNO
      !>      INTEGER               SEGNO
      !>
      !>      LOGICAL               ISNULL
      !>
      !>C
      !>C     Open a new EK file.  For simplicity, we won't
      !>C     reserve space for the comment area, so the
      !>C     number of reserved comment characters is zero.
      !>C     The constant IFNAME is the internal file name.
      !>C
      !>      CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
      !>
      !>C
      !>C     Set up the table and column names and declarations
      !>C     for the CHR_DATA segment.  We'll index all of
      !>C     the columns.
      !>C
      !>      CNAMES(1) =  'CHR_COL_1'
      !>      CDECLS(1) =  'DATATYPE = CHARACTER*(*), '
      !>     .       //    'INDEXED = TRUE, NULLS_OK = TRUE'
      !>
      !>      CNAMES(2) =  'CHR_COL_2'
      !>      CDECLS(2) =  'DATATYPE = CHARACTER*(9), '
      !>     .       //    'SIZE = VARIABLE, NULLS_OK = TRUE'
      !>
      !>C
      !>C     Start the segment.
      !>C
      !>      CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
      !>     .              CNAMES, CDECLS, SEGNO )
      !>
      !>      DO I = 0, NROWS-1
      !>
      !>         CALL EKAPPR ( HANDLE, SEGNO, RECNO )
      !>
      !>         ISNULL = ( I .EQ. 1 )
      !>
      !>         CALL INTSTR ( I, CVALS(1) )
      !>         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(1),
      !>     .                 1,      CVALS, ISNULL           )
      !>
      !>C
      !>C        Array-valued columns follow.
      !>C
      !>         CALL INTSTR ( 10*I,     CVALS(1) )
      !>         CALL INTSTR ( 10*I + 1, CVALS(2) )
      !>         CALL INTSTR ( 10*I + 2, CVALS(3) )
      !>         CALL INTSTR ( 10*I + 3, CVALS(4) )
      !>         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(2),
      !>     .                 4,      CVALS, ISNULL           )
      !>
      !>      END DO
      !>
      !>C
      !>C     End the file.
      !>C
      !>      CALL EKCLS ( HANDLE )
      !>
      !>C
      !>C     Open the EK for write access.
      !>C
      !>      CALL EKOPW ( EKNAME, HANDLE )
      !>
      !>C
      !>C     Negate the values in the odd-numbered records
      !>C     using the update routines.
      !>C
      !>      DO I = 1, NROWS, 2
      !>
      !>         RECNO  = I+1
      !>
      !>         ISNULL = ( I .EQ. 1 )
      !>
      !>         CALL INTSTR ( -I, CVALS(1) )
      !>         CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(1),
      !>     .                 1,      CVALS, ISNULL           )
      !>
      !>C
      !>C        Array-valued columns follow.
      !>C
      !>         CALL INTSTR ( -10*I,       CVALS(1) )
      !>         CALL INTSTR ( -(10*I + 1), CVALS(2) )
      !>         CALL INTSTR ( -(10*I + 2), CVALS(3) )
      !>         CALL INTSTR ( -(10*I + 3), CVALS(4) )
      !>         CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(2),
      !>     .                 4,      CVALS, ISNULL           )
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the file.
      !>C
      !>      CALL EKCLS ( HANDLE )
      !>
      !>C
      !>C     Open the created file. Show the values added.
      !>C
      !>      CALL EKOPR ( EKNAME, HANDLE )
      !>
      !>      DO I = 1, NROWS
      !>
      !>         CALL EKRCEC ( HANDLE, SEGNO, I,     CNAMES(1),
      !>     .                 NVALS,  CVALS, ISNULL           )
      !>
      !>         IF ( .NOT. ISNULL ) THEN
      !>
      !>            WRITE(*,*) 'Data from column: ', CNAMES(1)
      !>            WRITE(*,*) '   record number: ', I
      !>            WRITE(*,*) '   values       : ',
      !>     .                                ( CVALS(J), J=1,NVALS )
      !>            WRITE(*,*) ' '
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) 'Record ', I, 'flag is NULL.'
      !>            WRITE(*,*) ' '
      !>
      !>         END IF
      !>
      !>C
      !>C        Array-valued columns follow.
      !>C
      !>         CALL EKRCEC ( HANDLE, SEGNO, I,     CNAMES(2),
      !>     .                 NVALS,  CVALS, ISNULL           )
      !>
      !>         IF ( .NOT. ISNULL ) THEN
      !>
      !>            WRITE(*,*) 'Data from column: ', CNAMES(2)
      !>            WRITE(*,*) '   record number: ', I
      !>            WRITE(*,*) '   values       : ',
      !>     .                                ( CVALS(J), J=1,NVALS )
      !>            WRITE(*,*) ' '
      !>
      !>         END IF
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the file.
      !>C
      !>      CALL EKCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Data from column: CHR_COL_1
      !>            record number:            1
      !>            values       : 0
      !>
      !>         Data from column: CHR_COL_2
      !>            record number:            1
      !>            values       : 0        1        2        3
      !>
      !>         Record            2 flag is NULL.
      !>
      !>         Data from column: CHR_COL_1
      !>            record number:            3
      !>            values       : 2
      !>
      !>         Data from column: CHR_COL_2
      !>            record number:            3
      !>            values       : 20       21       22       23
      !>
      !>         Data from column: CHR_COL_1
      !>            record number:            4
      !>            values       : -3
      !>
      !>         Data from column: CHR_COL_2
      !>            record number:            4
      !>            values       : -30      -31      -32      -33
      !>
      !>         Data from column: CHR_COL_1
      !>            record number:            5
      !>            values       : 4
      !>
      !>         Data from column: CHR_COL_2
      !>            record number:            5
      !>            values       : 40       41       42       43
      !>
      !>         Data from column: CHR_COL_1
      !>            record number:            6
      !>            values       : -5
      !>
      !>         Data from column: CHR_COL_2
      !>            record number:            6
      !>            values       : -50      -51      -52      -53
      !>
      !>        Note that the second record does not appear due to setting the
      !>        ISNULL flag to true for that record. The odd value record
      !>        numbers have negative values as a result of the update calls.
      !>
      !>        After run completion, a new EK exists in the output directory.
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example and removed non-applicable restriction.
      !>
      !>-    SPICELIB Version 1.0.1, 31-MAR-1998 (NJB)
      !>
      !>        Corrected $Index_Entries section.
      !>
      !>-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
      module subroutine ekcls ( ctx, handle )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! EK file handle.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Close the file as a DAS file.
!
      call dascls ( ctx, handle )

      return
      end subroutine ekcls

      !> Return shadowing status of a specified EK file. THIS IS A
      !> STUB ROUTINE.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the file handle of an EK open for writing.
      !>
      !>## Detailed Output
      !>
      !>     ISSHAD   is a logical flag that is returned .TRUE. if and
      !>              only if the EK file designated by HANDLE is
      !>              shadowed.
      !>
      !>              In this stub version of the routine, ISSHAD is
      !>              always returned .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     1)  It is not an error to supply an input handle that does not
      !>         belong to an EK that is open for write access.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine is a utility that allows a program to test the
      !>     shadowing status of a specified EK file.
      !>
      !>## Examples
      !>
      !>     See the $Examples section of the umbrella routine EKSHAD.
      !>
      !>## Restrictions
      !>
      !>     1)  This is a stub routine.
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
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB)
      module subroutine ekshdw ( ctx, handle, isshad )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle attached to EK file.
      logical, intent(out) :: isshad  !! Logical flag indicating whether EK is shadowed.

      integer(ip) :: i

      i      =  handle
      isshad = .false.
      return
      end subroutine ekshdw

      !> Look up the column descriptor for a column of a given name
      !> in a specified segment.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is an EK file handle for the file containing the
      !>                    column of interest.  The EK may be open for read
      !>                    or write access.
      !>
      !>     SEGDSC         is the descriptor of the segment containing the
      !>                    column for which a descriptor is desired.
      !>
      !>     COLUMN         is the name of the column whose descriptor is
      !>                    desired.  Case and white space are not significant.
      !>
      !>## Detailed Output
      !>
      !>     COLDSC         is the descriptor of the column belonging to the
      !>                    specified file and segment and having name COLUMN.
      !>                    See the include file ekcoldsc.inc for details
      !>                    regarding the structure of EK column descriptors.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input column name does not match any column in the
      !>         designated segment, the error SPICE(BUG) is signaled.  It
      !>         is the caller's responsibility to call this routine with
      !>         valid input arguments.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine exists for the sole purpose of centralizing code
      !>     used to perform column descriptor look-ups.
      !>
      !>## Examples
      !>
      !>     See the EKACEx routines.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN to insert DAS file name into
      !>        long error message.
      !>
      !>-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
      module subroutine zzekcdsc ( ctx, handle, segdsc, column, coldsc )
      use pepper_kinds, only: ip
      use pepper_strings, only: eqstr
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle attached to an EK file.
      integer(ip), intent(in) :: segdsc ( sdscsz )  !! Segment descriptor.
      character(*), intent(in) :: column  !! Name of column.
      integer(ip), intent(out) :: coldsc ( cdscsz )  !! Descriptor for specified column.
!
!     Local variables
!
      character(cnamsz) :: cname

      integer(ip) :: dscbas
      integer(ip) :: i
      integer(ip) :: mbase
      integer(ip) :: nambas
      integer(ip) :: ncols

      logical :: found

!
!     Get the segment's integer metadata's base address.
!
      mbase  =  segdsc ( imdidx )

!
!     Get the number of columns.
!
      ncols  =  segdsc ( ncidx )

!
!     Search linearly through the column descriptors, looking for
!     a column name match.  It's an error if we don't find the input
!     name.
!
      found = .false.
      i     =  1

      do while (  ( i <= ncols )  .and.  ( .not. found )  )

         dscbas  =  mbase + sdscsz + (i-1)*cdscsz
!
!        Get the character base address of the column name from the
!        current descriptor.
!
         call dasrdi ( ctx, handle, dscbas+1, dscbas+cdscsz, coldsc )

         nambas = coldsc( namidx )

!
!        Look up the name and compare.
!
         call dasrdc ( ctx, handle,  nambas+1,  nambas+cnamsz, &
                       1,       cnamsz,    cname         )

         if (  eqstr( cname, column )  ) then
            found = .true.
         else
            i     =  i + 1
         end if

      end do

      if ( .not. found ) then

         call ctx%setmsg ( 'Descriptor for column # was not found. ' // &
                       'Segment base = #; file = #.'              )
         call ctx%errch  ( '#',  column                               )
         call ctx%errint ( '#',  mbase                                )
         call ctx%errhan ( '#',  handle                               )
         call ctx%sigerr ( 'SPICE(BUG)'                               )
         return

      end if

      end subroutine zzekcdsc

      !> Look up the name of a column, given the column's descriptor.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for read or write
      !>                    access.
      !>
      !>     COLDSC         is the column descriptor of a column whose name is
      !>                    desired.
      !>
      !>## Detailed Output
      !>
      !>     COLNAM         is the name of the specified column.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file,
      !>         the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine centralizes the coded needed to look up the
      !>     name of a specified column.  This is a frequently
      !>     performed function.
      !>
      !>## Examples
      !>
      !>     See EKDELR.
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
      !>-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
      module subroutine zzekcnam ( ctx, handle, coldsc, column )
      use pepper_kinds, only: ip
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! EK file handle.
      integer(ip), intent(in) :: coldsc ( cdscsz )  !! Column descriptor.
      character(*), intent(out) :: column  !! Column name.
!
!     Local variables
!
      integer(ip) :: nambas

!
!     Look up the name.
!
      nambas = coldsc( namidx )

      call dasrdc ( ctx, handle,  nambas+1,  nambas+cnamsz, &
                    1,       cnamsz,    column         )

      end subroutine zzekcnam

      !> Get an encoded integer at a specified address from a character
      !> data page.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is the handle of an EK file open for read or
      !>                    write access.
      !>
      !>     ADDRSS         is the DAS character start address from which an
      !>                    integer, encoded as a string, is to be read.
      !>                    An encoded integer occupies ENCSIZ characters,
      !>                    where the parameter ENCSIZ is defined in the
      !>                    include file ekdatpag.inc.
      !>
      !>## Detailed Output
      !>
      !>     IVAL           is an integer value obtained by decoding an
      !>                    encoded integer read from the specified
      !>                    location.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If the DAS character address range
      !>
      !>            ADDRSS .. ADDRSS+ENCSIZ-1
      !>
      !>         is not a range of DAS character addresses that have been
      !>         initialized, the error wll be diagnosed by routines
      !>         called by this routine.
      !>
      !>     3)  If the character data starting at the specified address
      !>         does not represent an encoded integer, the error wll be
      !>         diagnosed by routines called by this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine should be used for all EK applications reading
      !>     integer values that have been encoded as characters.  This
      !>     routine expects the encoding to have been done by ZZEKSEI.
      !>
      !>## Examples
      !>
      !>     See ZZEKRD03.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
      module subroutine zzekgei ( ctx, handle, addrss, ival )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! EK file handle.
      integer(ip), intent(in) :: addrss  !! DAS character address to read encoded value from.
      integer(ip), intent(out) :: ival  !! Decoded integer value.
!
!     Local variables
!
      character(encsiz) :: cval

!
!     Discovery error handling should be used in this utility.
!
!     Read the encoded value.  The value is represented by a string of
!     characters.
!
      call dasrdc ( ctx, handle, addrss, addrss+encsiz-1, 1, encsiz, cval )

!
!     Decode the number.
!
      call prtdec ( ctx, cval, ival )

      return
      end subroutine zzekgei

      !> Return the status of a specified EK record.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for read or write
      !>                    access.
      !>
      !>     RECPTR         is a pointer to the record whose status is desired.
      !>
      !>## Detailed Output
      !>
      !>     STATUS         is the status word of the specified record.  See
      !>                    the include file ekrecptr.inc for values and
      !>                    meanings.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine hides details of status word access.
      !>
      !>## Examples
      !>
      !>     See EKCOMM.
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
      !>-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
      module subroutine zzekgrs ( ctx, handle, recptr, status )
      use pepper_kinds, only: ip
      use pepper_inc_ekrecptr
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: recptr  !! Record pointer.
      integer(ip), intent(out) :: status  !! Status of specified EK record.
      integer(ip) :: status_1(1)  !! one-element copy of status (array argument)
!
!     Local variables
!
      integer(ip) :: loc

!
!     Compute the status word location, and read the status.
!
      loc  =  recptr + staidx

      call dasrdi ( ctx, handle, loc, loc, status_1 )
      status = status_1(1)

      return
      end subroutine zzekgrs

      !> Determine the order of elements in a character EK column,
      !> using dictionary ordering on character data values and array
      !> indices.
      !>
      !>## Detailed Input
      !>
      !>     CVALS          is an array of character string EK column values,
      !>                    some of which may be null, if null values are
      !>                    permitted.  See the description of the input
      !>                    arguments NULLOK and NLFLGS below.
      !>
      !>     NULLOK         is a logical flag indicating whether column
      !>                    elements may be null.  If NULLOK is TRUE, then
      !>                    NLFLGS must be set to indicate the status of each
      !>                    element of CVALS.
      !>
      !>     NLFLGS         is an array of logical flags that indicate whether
      !>                    the corresponding elements of CVALS are null.
      !>                    NLFLGS is meaningful only when NULLOK is .TRUE.
      !>                    When NULLOK is .TRUE., the Ith element of CVALS is
      !>                    null if and only if the Ith element of NLFLGS
      !>                    is .TRUE.
      !>
      !>                    When NULLOK is .FALSE., all elements of CVALS are
      !>                    considered to be non-null.
      !>
      !>     NVALS          is the number of elements in the input array.
      !>
      !>## Detailed Output
      !>
      !>     IORDER         is the order vector for the input array.
      !>                    IORDER(1) is the index of the smallest element
      !>                    of CVALS; IORDER(2) is the index of the next
      !>                    smallest; and so on.  Null values, if allowed, are
      !>                    considered to be less than all non-null values.
      !>                    The order relation between equal values is
      !>                    determined by the indices of the values in the
      !>                    input array; values with lower indices are
      !>                    considered to be smaller.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     ZZEKORDC creates an order vector for an array of character
      !>     column values.  Null values are allowed.  The order relation used
      !>     is dictionary ordering on ordered pairs consisting of data
      !>     values and array indices:  if two input data values are equal,
      !>     the associated array indices determine the order relation of the
      !>     values, where the smaller index is considered to precede the
      !>     greater.
      !>
      !>## Examples
      !>
      !>     1)  Sort the following list of values, some of which are null:
      !>
      !>                    Value                      Null?
      !>             ------------------         ---------------------
      !>             CVALS(1)  =  'CAT'         NLFLGS(1)  =  .FALSE.
      !>             CVALS(2)  =  'APT'         NLFLGS(2)  =  .FALSE.
      !>             CVALS(3)  =  'DOG'         NLFLGS(3)  =  .TRUE.
      !>             CVALS(4)  =  'EAT'         NLFLGS(4)  =  .FALSE.
      !>             CVALS(5)  =  'BAD'         NLFLGS(5)  =  .TRUE.
      !>
      !>         The subroutine call
      !>
      !>             CALL ZZEKORDC ( CVALS, .TRUE., NLFLGS, 5, IORDER )
      !>
      !>         generates the output
      !>
      !>             IORDER(1)  =  3
      !>             IORDER(2)  =  5
      !>             IORDER(3)  =  2
      !>             IORDER(4)  =  1
      !>             IORDER(5)  =  4
      !>
      !>     2)  Given the same inputs values of CVALS and NLFLGS, the
      !>         subroutine call
      !>
      !>             CALL ZZEKORDC ( CVALS, .FALSE., NLFLGS, 5, IORDER )
      !>
      !>         generates the output
      !>
      !>             IORDER(1)  =  2
      !>             IORDER(2)  =  5
      !>             IORDER(3)  =  1
      !>             IORDER(4)  =  3
      !>             IORDER(5)  =  4
      !>
      !>## Restrictions
      !>
      !>      None.
      !>
      !>## Exceptions
      !>
      !>      Error free.
      !>
      !>## Files
      !>
      !>      None.
      !>
      !>## Author and Institution
      !>
      !>      N.J. Bachman    (JPL)
      !>      I.M. Underwood  (JPL)
      !>
      !>## Literature References
      !>
      !>      None.
      !>
      !>## Version
      !>
      !>-     Beta Version 3.0.0, 26-MAY-1995 (NJB)
      !>
      !>         Re-written to use dictionary ordering on values and input
      !>         array indices.
      !>
      !>-     Beta Version 2.0.0, 13-FEB-1995 (NJB)
      !>
      !>         Renamed as a private routine.
      !>
      !>-     Beta Version 1.0.0, 13-APR-1994 (NJB) (IMU)
      module subroutine zzekordc ( ctx, cvals, nullok, nlflgs, nvals, iorder )
      use pepper_kinds, only: ip
      use pepper_arrays, only: swapi
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: cvals  ( * )  !! Array of character string column values.
      logical, intent(in) :: nullok  !! Logical flag indicating whether nulls are allowed.
      logical, intent(in) :: nlflgs ( * )  !! Flags indicating whether column entries are null.
      integer(ip), intent(in) :: nvals  !! Dimension of CVALS.
      integer(ip), intent(out) :: iorder ( * )  !! Order vector for CVALS.
      integer(ip) :: gap
      integer(ip) :: i
      integer(ip) :: i1
      integer(ip) :: i2
      integer(ip) :: j
      integer(ip) :: jg

      logical :: eq1
      logical :: le1
      logical :: n1
      logical :: n2
      logical :: ok

!
!     Statement functions
!
      logical :: cmp1
      logical :: cmp2
      logical :: cmp3
      logical :: cmp4
      logical :: le

      cmp1 ( le1, eq1, i1, i2 )   =         ( le1                      ) &
                                       .or. ( eq1 .and. ( i1 < i2 ) )

      cmp2 ( i1, i2, n1, n2 )     =         ( n1 .and. .not. n2  ) &
                                       .or. (      ( n1 .and. n2 ) &
                                             .and. ( i1 <  i2 ) )

      cmp3 ( le1, eq1, i1, i2, n1, n2 )  =   ( .not. ( n1 .or. n2 ) ) &
                                               .and. &
                                             cmp1 ( le1, eq1, i1, i2 )

      cmp4 ( le1, eq1, i1, i2, n1, n2 )  =   cmp2 ( i1, i2, n1, n2 ) &
                                             .or. &
                                        cmp3 (le1, eq1, i1, i2, n1, n2 )

      le (  le1,  eq1,  i1,  i2,  ok,  n1,  n2  )  = &

              (  (.not. ok ) .and.  cmp1 ( le1, eq1, i1, i2 )          ) &
        .or.  (  (      ok ) .and.  cmp4 ( le1, eq1, i1, i2, n1, n2 )  )

!
!     Begin with the initial ordering.
!
      do i = 1, nvals
         iorder(i) = i
      end do

!
!     Find the smallest element, then the next smallest, and so on.
!     This uses the Shell Sort algorithm, but swaps the elements of
!     the order vector instead of the array itself.
!
      gap = nvals / 2

      do while ( gap > 0 )

         do i = gap+1, nvals

            j = i - gap

            do while ( j > 0 )

               jg    =   j + gap

               le1  =   lle (   cvals ( iorder(j)  ), &
                                cvals ( iorder(jg) )   )

               eq1  =   cvals ( iorder(j) ) ==  cvals ( iorder(jg) )

               if (   le (   le1, &
                             eq1, &
                             iorder(j), &
                             iorder(jg), &
                             nullok, &
                             nlflgs( iorder(j)  ), &
                             nlflgs( iorder(jg) )   )    ) then
!
!                 Getting here means that
!
!                    CVALS(IORDER(J)) .LE. CVALS(IORDER(JG))
!
!                 according to our order relation.
!
                  j = 0

               else

                  call swapi ( iorder(j), iorder(jg) )

               end if

               j = j - gap

            end do

         end do

         gap = gap / 2

      end do

      return
      end subroutine zzekordc

      !> Determine the order of elements in a double precision EK column,
      !> using dictionary ordering on d.p. data values and array
      !> indices.
      !>
      !>## Detailed Input
      !>
      !>     DVALS          is an array of double precision EK column values,
      !>                    some of which may be null, if null values are
      !>                    permitted.  See the description of the input
      !>                    arguments NULLOK and NLFLGS below.
      !>
      !>     NULLOK         is a logical flag indicating whether column
      !>                    elements may be null.  If NULLOK is TRUE, then
      !>                    NLFLGS must be set to indicate the status of each
      !>                    element of DVALS.
      !>
      !>     NLFLGS         is an array of logical flags that indicate whether
      !>                    the corresponding elements of DVALS are null.
      !>                    NLFLGS is meaningful only when NULLOK is .TRUE.
      !>                    When NULLOK is .TRUE., the Ith element of DVALS is
      !>                    null if and only if the Ith element of NLFLGS
      !>                    is .TRUE.
      !>
      !>                    When NULLOK is .FALSE., all elements of DVALS are
      !>                    considered to be non-null.
      !>
      !>     NVALS          is the number of elements in the input array.
      !>
      !>## Detailed Output
      !>
      !>     IORDER         is the order vector for the input array.
      !>                    IORDER(1) is the index of the smallest element
      !>                    of DVALS; IORDER(2) is the index of the next
      !>                    smallest; and so on.  Null values, if allowed, are
      !>                    considered to be less than all non-null values.
      !>                    The order relation between equal values is
      !>                    determined by the indices of the values in the
      !>                    input array; values with lower indices are
      !>                    considered to be smaller.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     ZZEKORDD creates an order vector for an array of double precision
      !>     column values.  Null values are allowed.  The order relation used
      !>     is dictionary ordering on ordered pairs consisting of data
      !>     values and array indices:  if two input data values are equal,
      !>     the associated array indices determine the order relation of the
      !>     values, where the smaller index is considered to precede the
      !>     greater.
      !>
      !>## Examples
      !>
      !>      1)  Sort the following list of values, some of which are
      !>          null:
      !>
      !>                  Value                         Null?
      !>             --------------             ---------------------
      !>             DVALS(1)  =  3             NLFLGS(1)  =  .FALSE.
      !>             DVALS(2)  =  1             NLFLGS(2)  =  .FALSE.
      !>             DVALS(3)  =  4             NLFLGS(3)  =  .TRUE.
      !>             DVALS(4)  =  5             NLFLGS(4)  =  .FALSE.
      !>             DVALS(5)  =  2             NLFLGS(5)  =  .TRUE.
      !>
      !>          The subroutine call
      !>
      !>              CALL ZZEKORDD ( DVALS, .TRUE., NLFLGS, 5, IORDER )
      !>
      !>          generates the output
      !>
      !>             IORDER(1)  =  3
      !>             IORDER(2)  =  5
      !>             IORDER(3)  =  2
      !>             IORDER(4)  =  1
      !>             IORDER(5)  =  4
      !>
      !>      2)  Given the same inputs values of DVALS and NLFLGS, the
      !>          subroutine call
      !>
      !>             CALL ZZEKORDD ( DVALS, .FALSE., NLFLGS, 5, IORDER )
      !>
      !>          generates the output
      !>
      !>             IORDER(1)  =  2
      !>             IORDER(2)  =  5
      !>             IORDER(3)  =  1
      !>             IORDER(4)  =  3
      !>             IORDER(5)  =  4
      !>
      !>## Restrictions
      !>
      !>      None.
      !>
      !>## Exceptions
      !>
      !>      Error free.
      !>
      !>## Files
      !>
      !>      None.
      !>
      !>## Author and Institution
      !>
      !>      N.J. Bachman    (JPL)
      !>      I.M. Underwood  (JPL)
      !>
      !>## Literature References
      !>
      !>      None.
      !>
      !>## Version
      !>
      !>-     Beta Version 3.0.0, 08-SEP-1995 (NJB)
      !>
      !>         Re-written to use dictionary ordering on values and input
      !>         array indices.
      !>
      !>-     Beta Version 2.0.0, 13-FEB-1995 (NJB)
      !>
      !>         Renamed as a private routine.
      !>
      !>-     Beta Version 1.0.0, 13-APR-1994 (NJB) (IMU)
      module subroutine zzekordd ( ctx, dvals, nullok, nlflgs, nvals, iorder )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: swapi
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: dvals  ( * )  !! Array of d.p. column values.
      logical, intent(in) :: nullok  !! Logical flag indicating whether nulls are allowed.
      logical, intent(in) :: nlflgs ( * )  !! Flags indicating whether column entries are null.
      integer(ip), intent(in) :: nvals  !! Dimension of DVALS.
      integer(ip), intent(out) :: iorder ( * )  !! Order vector for DVALS.
!
!     Local variables
!
      real(wp) :: d1
      real(wp) :: d2

      integer(ip) :: gap
      integer(ip) :: i
      integer(ip) :: i1
      integer(ip) :: i2
      integer(ip) :: j
      integer(ip) :: jg

      logical :: n1
      logical :: n2
      logical :: ok

!
!     Statement functions
!
      logical :: cmp1
      logical :: cmp2
      logical :: cmp3
      logical :: cmp4
      logical :: le

      cmp1 ( d1, d2, i1, i2 )          =          ( d1 < d2 ) &
                                          .or. (  ( d1 == d2 ) .and. &
                                                  ( i1 < i2 )       )

      cmp2 ( i1, i2, n1, n2 )          =       ( n1 .and. .not. n2  ) &
                                          .or. (      ( n1 .and. n2 ) &
                                                .and. ( i1 < i2  ) )

      cmp3 ( d1, d2, i1, i2, n1, n2 )  =        ( .not. ( n1 .or. n2 ) ) &
                                         .and. cmp1 (  d1, d2, i1, i2  )

      cmp4 ( d1, d2, i1, i2, n1, n2 )  = cmp2 ( i1, i2, n1, n2 ) &
                                         .or. &
                                         cmp3 ( d1, d2, i1, i2, n1, n2 )

      le (  d1,  d2,  i1,  i2,  ok,  n1,  n2  )  = &

              (  (.not. ok ) .and.  cmp1 ( d1, d2, i1, i2 )          ) &
        .or.  (  (      ok ) .and.  cmp4 ( d1, d2, i1, i2, n1, n2 )  )

!
!     Begin with the initial ordering.
!
      do i = 1, nvals
         iorder(i) = i
      end do

!
!     Find the smallest element, then the next smallest, and so on.
!     This uses the Shell Sort algorithm, but swaps the elements of
!     the order vector instead of the array itself.
!
      gap = nvals / 2

      do while ( gap > 0 )

         do i = gap+1, nvals

            j = i - gap

            do while ( j > 0 )

               jg = j + gap

               if (   le (   dvals ( iorder(j)  ), &
                             dvals ( iorder(jg) ), &
                             iorder(j), &
                             iorder(jg), &
                             nullok, &
                             nlflgs( iorder(j)  ), &
                             nlflgs( iorder(jg) )   )    ) then
!
!                 Getting here means that
!
!                    DVALS(IORDER(J)) .LE. DVALS(IORDER(JG))
!
!                 according to our order relation.
!
                  j = 0

               else

                  call swapi ( iorder(j), iorder(jg) )

               end if

               j = j - gap

            end do

         end do

         gap = gap / 2

      end do

      return
      end subroutine zzekordd

      !> Determine the order of elements in an integer EK column, using
      !> dictionary ordering on integer data values and array indices.
      !>
      !>## Detailed Input
      !>
      !>      IVALS          is an array of integer EK column values,
      !>                     some of which may be null, if null values are
      !>                     permitted.  See the description of the input
      !>                     arguments NULLOK and NLFLGS below.
      !>
      !>      NULLOK         is a logical flag indicating whether column
      !>                     elements may be null.  If NULLOK is TRUE, then
      !>                     NLFLGS must be set to indicate the status of each
      !>                     element of IVALS.
      !>
      !>      NLFLGS         is an array of logical flags that indicate whether
      !>                     the corresponding elements of IVALS are null.
      !>                     NLFLGS is meaningful only when NULLOK is .TRUE.
      !>                     When NULLOK is .TRUE., the Ith element of IVALS is
      !>                     null if and only if the Ith element of NLFLGS
      !>                     is .TRUE.
      !>
      !>                     When NULLOK is .FALSE., all elements of IVALS are
      !>                     considered to be non-null.
      !>
      !>      NVALS          is the number of elements in the input array.
      !>
      !>## Detailed Output
      !>
      !>      IORDER      is the order vector for the input array.
      !>                  IORDER(1) is the index of the smallest element
      !>                  of IVALS; IORDER(2) is the index of the next
      !>                  smallest; and so on.  Null values, if allowed, are
      !>                  considered to be less than all non-null values.  The
      !>                  order relation between equal values is determined
      !>                  by the indices of the values in the input array;
      !>                  values with lower indices are considered to be
      !>                  smaller.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     ZZEKORDI creates an order vector for an array of integer
      !>     column values.  Null values are allowed.  The order
      !>     relation used is dictionary ordering on ordered pairs consisting
      !>     of data values and array indices:  if two input data values
      !>     are equal, the associated array indices determine the order
      !>     relation of the values, where the smaller index is considered
      !>     to precede the greater.
      !>
      !>## Examples
      !>
      !>      1)  Sort the following list of values, some of which are
      !>          null:
      !>
      !>                  Value                         Null?
      !>             --------------             ---------------------
      !>             IVALS(1)  =  3             NLFLGS(1)  =  .FALSE.
      !>             IVALS(2)  =  1             NLFLGS(2)  =  .FALSE.
      !>             IVALS(3)  =  4             NLFLGS(3)  =  .TRUE.
      !>             IVALS(4)  =  5             NLFLGS(4)  =  .FALSE.
      !>             IVALS(5)  =  2             NLFLGS(5)  =  .TRUE.
      !>
      !>          The subroutine call
      !>
      !>              CALL ZZEKORDI ( IVALS, .TRUE., NLFLGS, 5, IORDER )
      !>
      !>          generates the output
      !>
      !>             IORDER(1)  =  3
      !>             IORDER(2)  =  5
      !>             IORDER(3)  =  2
      !>             IORDER(4)  =  1
      !>             IORDER(5)  =  4
      !>
      !>          Note that the order of the null values is determined by
      !>          their indices in the input array.
      !>
      !>      2)  Given the same inputs values of IVALS and NLFLGS, the
      !>          subroutine call
      !>
      !>             CALL ZZEKORDI ( IVALS, .FALSE., NLFLGS, 5, IORDER )
      !>
      !>          generates the output
      !>
      !>             IORDER(1)  =  2
      !>             IORDER(2)  =  5
      !>             IORDER(3)  =  1
      !>             IORDER(4)  =  3
      !>             IORDER(5)  =  4
      !>
      !>## Restrictions
      !>
      !>      None.
      !>
      !>## Exceptions
      !>
      !>      Error free.
      !>
      !>## Files
      !>
      !>      None.
      !>
      !>## Author and Institution
      !>
      !>      N.J. Bachman    (JPL)
      !>      I.M. Underwood  (JPL)
      !>
      !>## Literature References
      !>
      !>      None.
      !>
      !>## Version
      !>
      !>-     Beta Version 3.0.0, 26-MAY-1995 (NJB)
      !>
      !>         Re-written to use dictionary ordering on values and input
      !>         array indices.
      !>
      !>-     Beta Version 2.0.0, 13-FEB-1995 (NJB)
      !>
      !>         Renamed as a private routine.
      !>
      !>-     Beta Version 1.0.0, 13-APR-1994 (NJB) (IMU)
      module subroutine zzekordi ( ctx, ivals, nullok, nlflgs, nvals, iorder )
      use pepper_kinds, only: ip
      use pepper_arrays, only: swapi
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: ivals  ( * )  !! Array of integer column values.
      logical, intent(in) :: nullok  !! Logical flag indicating whether nulls are allowed.
      logical, intent(in) :: nlflgs ( * )  !! Flags indicating whether column entries are null.
      integer(ip), intent(in) :: nvals  !! Dimension of IVALS.
      integer(ip), intent(out) :: iorder ( * )  !! Order vector for IVALS.
!
!     Local variables
!
      integer(ip) :: i1
      integer(ip) :: i2
      integer(ip) :: i3
      integer(ip) :: i4

      integer(ip) :: gap
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: jg

      logical :: n1
      logical :: n2
      logical :: ok

!
!     Statement functions
!
      logical :: cmp1
      logical :: cmp2
      logical :: cmp3
      logical :: cmp4
      logical :: le

      cmp1 ( i1, i2, i3, i4 )          =          ( i1 < i2 ) &
                                          .or. (  ( i1 == i2 ) .and. &
                                                  ( i3 < i4 )       )

      cmp2 ( i3, i4, n1, n2 )          =       ( n1 .and. .not. n2  ) &
                                          .or. (      ( n1 .and. n2 ) &
                                                .and. ( i3 < i4  ) )

      cmp3 ( i1, i2, i3, i4, n1, n2 )  =        ( .not. ( n1 .or. n2 ) ) &
                                         .and. cmp1 (  i1, i2, i3, i4  )

      cmp4 ( i1, i2, i3, i4, n1, n2 )  = cmp2 ( i3, i4, n1, n2 ) &
                                         .or. &
                                         cmp3 ( i1, i2, i3, i4, n1, n2 )

      le (  i1,  i2, i3, i4, ok,   n1,   n2  )  = &

              (  (.not. ok ) .and.  cmp1 ( i1, i2, i3, i4 )          ) &
        .or.  (  (      ok ) .and.  cmp4 ( i1, i2, i3, i4, n1, n2 )  )

!
!     Begin with the initial ordering.
!
      do i = 1, nvals
         iorder(i) = i
      end do

!
!     Find the smallest element, then the next smallest, and so on.
!     This uses the Shell Sort algorithm, but swaps the elements of
!     the order vector instead of the array itself.
!
      gap = nvals / 2

      do while ( gap > 0 )

         do i = gap+1, nvals

            j = i - gap

            do while ( j > 0 )

               jg = j + gap

               if (   le (   ivals ( iorder(j)  ), &
                             ivals ( iorder(jg) ), &
                             iorder(j), &
                             iorder(jg), &
                             nullok, &
                             nlflgs( iorder(j)  ), &
                             nlflgs( iorder(jg) )   )    ) then
!
!                 Getting here means that
!
!                    IVALS(IORDER(J)) .LE. IVALS(IORDER(JG))
!
!                 according to our order relation.
!
                  j = 0

               else

                  call swapi ( iorder(j), iorder(jg) )

               end if

               j = j - gap

            end do

         end do

         gap = gap / 2

      end do

      return
      end subroutine zzekordi

      !> Parse a declaration of a new EK column.
      !>
      !>## Detailed Input
      !>
      !>     DECL           is a character string containing a column
      !>                    declaration.  Column declarations are strings that
      !>                    contain `keyword=value' assignments that define
      !>                    the attributes of the columns to which they apply.
      !>
      !>                    The attributes of a column defined by a
      !>                    declaration are:
      !>
      !>                       DATA TYPE
      !>                       <size>
      !>                       <is the column indexed?>
      !>                       <are null values allowed?>
      !>                       <is the column fixed-count?>
      !>
      !>                    The form of a column declaration is
      !>
      !>                      .'DATATYPE     = <type descriptor>,'           //
      !>                      .'[SIZE        = <size descriptor>],'          //
      !>                      .'[INDEXED     = <boolean>],'                  //
      !>                      .'[NULLS_OK    = <boolean>]'                   //
      !>                      .'[FIXED_COUNT = <boolean>]'
      !>
      !>                    The order of the assignments does not matter.
      !>
      !>                    Here <type descriptor> can be any of
      !>
      !>                       CHARACTER*<integer length>
      !>                       CHARACTER*(*)
      !>                       DOUBLE PRECISION
      !>                       INTEGER
      !>
      !>                    and the optional <size descriptor> can be either of
      !>
      !>                       <integer size>
      !>                       VARIABLE
      !>
      !>                    Character columns may not have both variable
      !>                    string length and variable size.
      !>
      !>                    The column entry size defaults to 1 if the size
      !>                    descriptor is omitted.
      !>
      !>                    The optional clauses using the INDEXED, NULLS_OK,
      !>                    and FIXED_COUNT keywords take the values
      !>
      !>                       TRUE
      !>                       FALSE
      !>
      !>                    on the right-hand-sides of the equal signs.
      !>
      !>                    The INDEXED clause indicates that the column is
      !>                    indexed.  If the clause is omitted, the column is
      !>                    not indexed.  Only scalar-valued columns can be
      !>                    indexed.
      !>
      !>                    The NULLS_OK indicates that null values are
      !>                    permitted in the column; if the clause is omitted,
      !>                    null values are not permitted in the column.
      !>
      !>                    The FIXED_COUNT clause indicates that the column
      !>                    has a fixed number of entries; no records may be
      !>                    added to or deleted from the column.  If any
      !>                    column in a segment has a fixed record count, all
      !>                    columns in the segment must have the FIXED_COUNT
      !>                    attribute.
      !>
      !>                    FIXED_COUNT columns may be loaded only by the
      !>                    fast load routines.
      !>
      !>                    Unless the FIXED_COUNT keyword is used, the column
      !>                    does not have a fixed record count.
      !>
      !>                    Commas are required to separate the assignments
      !>                    within declarations.  White space is optional.
      !>                    Case is not significant.
      !>
      !>## Detailed Output
      !>
      !>     PARDSC         is an integer array that specifies the attributes
      !>                    of the column.  PARDSC is basically a
      !>                    partially-filled-in column descriptor:  it
      !>                    doesn't contain any pointer information.  In the
      !>                    locations where a column descriptor would contain
      !>                    an index pointer or a null flag array pointer,
      !>                    PARDSC contains boolean values indicating whether
      !>                    these items are supposed to be filled in later.
      !>
      !>                    The elements of PARDSC that are filled in upon
      !>                    return from this routine are:
      !>
      !>                       -- Class.  The column class is automatically
      !>                          determined from the declared attributes.
      !>
      !>                       -- Data type.
      !>
      !>                       -- String length, if applicable.  Variable-
      !>                          length strings are represented by a length
      !>                          specification of IFALSE.
      !>
      !>                       -- Column entry size.  Variable-size entries
      !>                          are represented by a size specification of
      !>                          IFALSE.
      !>
      !>                       -- The column's index type.  This element,
      !>                          which in a normal column descriptor contains
      !>                          an index type code, takes the boolean value
      !>                          ITRUE if the column is indexed and IFALSE
      !>                          otherwise.
      !>
      !>                       -- The column's null flag.  This element takes
      !>                          the boolean value ITRUE if the column can
      !>                          contain null values and is set to IFALSE
      !>                          otherwise.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input declaration does not conform to the specification
      !>         given in $Detailed_Input, the error SPICE(BADCOLUMNDECL) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is a utility that supports EK writing subroutines.
      !>
      !>## Examples
      !>
      !>     1)  Parse a declaration of an indexed column of 80-character
      !>         strings, in which null values are allowed:
      !>
      !>            CALL ZZEKPDEC ( 'DATATYPE = CHARACTER*80, '   //
      !>           .                'SIZE     = 1,'               //
      !>           .                'INDEXED  = TRUE',            //
      !>           .                'NULLS_OK = TRUE',
      !>           .                 PARDSC                       )
      !>
      !>        When ZZEKPDEC returns, the values of its output column
      !>        descriptor will be
      !>
      !>        When ZZEKPDEC returns, the value of its output argument
      !>        PARDSC will be
      !>
      !>           +---------------+
      !>           |       3       |  Class
      !>           +---------------+
      !>           |     <CHR>     |  Data type
      !>           +---------------+
      !>           |      80       |  String length
      !>           +---------------+
      !>           |       1       |  Size
      !>           +---------------+
      !>           |       0       |  Base addres of column name (not yet set)
      !>           +---------------+
      !>           |     ITRUE     |  Index type (ITRUE means col is indexed)
      !>           +---------------+
      !>           |       0       |  Index pointer
      !>           +---------------+
      !>           |     ITRUE     |  Null flag  (ITRUE means nulls are
      !>           +---------------+  allowed)
      !>           |       0       |  Ordinal position of column in segment
      !>           +---------------+
      !>           |       0       |  Metadata pointer
      !>           +---------------+
      !>           |       0       |  (Reserved)
      !>           +---------------+
      !>
      !>     2)  Parse a declaration of a variable-size column of 80-character
      !>         strings:
      !>
      !>            CALL ZZEKPDEC ( 'DATATYPE =  CHARACTER*80, '   //
      !>           .                'SIZE     =  VARIABLE',
      !>           .                 PARDSC                        )
      !>
      !>        When ZZEKPDEC returns, the value of its output argument
      !>        PARDSC will be
      !>
      !>           +---------------+
      !>           |       3       |  Class
      !>           +---------------+
      !>           |     <CHR>     |  Data type
      !>           +---------------+
      !>           |      80       |  String length
      !>           +---------------+
      !>           |    IFALSE     |  Size (IFALSE indicates variable size)
      !>           +---------------+
      !>           |       0       |  Base addres of column name (not yet set)
      !>           +---------------+
      !>           |     IFALSE    |  Index type (IFALSE means unindexed col)
      !>           +---------------+
      !>           |       0       |  Index pointer
      !>           +---------------+
      !>           |     IFALSE    |  Null flag  (IFALSE means nulls are not
      !>           +---------------+  allowed)
      !>           |       0       |  Ordinal position of column in segment
      !>           +---------------+
      !>           |       0       |  Metadata pointer
      !>           +---------------+
      !>           |       0       |  (Reserved)
      !>           +---------------+
      !>
      !>## Restrictions
      !>
      !>     1) Currently does not diagnose extraneous keyword assignments.
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
      !>-    SPICELIB Version 1.1.0, 14-SEP-2005 (NJB)
      !>
      !>        Bug fix:  several error handling logic blocks were
      !>        missing SIGERR calls; these have been corrected.
      !>
      !>        Bug fix:  No diagnostic was issued for a declaration
      !>        of a variable-size, variable-string-length column.
      !>        This has been corrected.
      !>
      !>-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
      module subroutine zzekpdec ( ctx, decl, pardsc )
      use pepper_kinds, only: ip
      use pepper_strings, only: eqstr, lparsm, nparsi, cmprss_inplace, ucase_inplace
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ektype
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: decl  !! String containing column declaration.
      integer(ip), intent(out) :: pardsc ( * )  !! Partial column descriptor.
!
!     Local parameters
!
      integer(ip), parameter :: nattr = 5
      integer(ip), parameter :: maxtok = 20
      integer(ip), parameter :: toklen = 32
      integer(ip), parameter :: msglen = 320
      integer(ip), parameter :: nrkey = 1
!
!     Parameters naming indices of keywords in the attribute list
!     ATTKEY:
!
      integer(ip), parameter :: typpos = 1
      integer(ip), parameter :: sizpos = typpos + 1
      integer(ip), parameter :: idxpos = sizpos + 1
      integer(ip), parameter :: nflpos = idxpos + 1
      integer(ip), parameter :: fxcpos = nflpos + 1
!
!     Local variables
!
      character(msglen) :: msg
      character(toklen) :: tokens ( maxtok )
      integer(ip) :: attloc ( cdscsz )
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: n
      integer(ip) :: ptr
      integer(ip) :: tokloc
      logical :: attfnd ( cdscsz )
      logical :: found
      if ( .not. allocated ( ctx%zzekpdec_state ) ) allocate ( ctx%zzekpdec_state )
      associate ( attkey => ctx%zzekpdec_state%attkey, &
                  reqkey => ctx%zzekpdec_state%reqkey )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Start with a clean slate.
!
      pardsc(1:cdscsz) = 0

!
!     Our declaration language has been cleverly designed so that the
!     characters
!
!        ','
!        '='
!
!     act as delimiters that LPARSM can make use
!     of.  LPARSM will hand us back a token list that contains these
!     pairs of consecutive tokens:
!
!        +----------------------+
!        | CLASS                |
!        +----------------------+
!        | <integer>            |
!        +----------------------+
!
!        +----------------------+
!        | DATATYPE             |
!        +----------------------+
!        | <type>               |
!        +----------------------+
!
!        +----------------------+
!        | SIZE                 |
!        +----------------------+
!        | <size specification> |  ( 'VARIABLE' or <integer> )
!        +----------------------+
!
!        +----------------------+
!        | INDEXED              |  (fixed-size columns only, optional)
!        +----------------------+
!        | <TRUE/FALSE>         |
!        +----------------------+
!
!        +----------------------+
!        | NULLS_OK             |  (optional)
!        +----------------------+
!        | <TRUE/FALSE>         |
!        +----------------------+
!
!     The order of the token pairs is not necessarily as shown.
!
      call lparsm ( decl, ',=', maxtok, n, tokens )

!
!     Make sure the tokens are in upper case.  They are already
!     left-justified.
!
      do i = 1, n
         call ucase_inplace ( tokens(i) )
      end do

!
!     See which clauses are present in the declaration, and keep track
!     of the token indices of the keywords that start the clauses.
!
      do i = 1, nattr
         attfnd(i) = .false.
      end do

      do i = 1, n

         j     = 1
         found = .false.

         do while (  ( j <= nattr ) .and. ( .not. found )  )

            if ( tokens(i) == attkey(j) ) then
               found     = .true.
               attfnd(j) = .true.
               attloc(j) =  i
            else
               j         =  j + 1
            end if

         end do

      end do

!
!     Make sure we got the required keyword tokens we were expecting.
!
      do i = 1, nrkey

         if (  .not. attfnd( reqkey(i) ) ) then

            call ctx%setmsg ( 'Required keyword # was not found in ' // &
                          'column declaration #.'                )
            call ctx%errch  ( '#',  attkey( reqkey(i) )              )
            call ctx%errch  ( '#',  decl                             )
            call ctx%sigerr ( 'SPICE(BADCOLUMDECL)'                  )
            return

         end if

      end do

!
!     If we got this far, we can start to fill in the data type
!     descriptor.  Starting at the location of the DATATYPE keyword,
!     we should see one of the following token sequences:
!
!        DATATYPE  =  DOUBLE PRECISION
!        DATATYPE  =  INTEGER
!        DATATYPE  =  TIME
!        DATATYPE  =  CHARACTER*<integer>
!        DATATYPE  =  CHARACTER*(<integer>)
!        DATATYPE  =  CHARACTER**
!        DATATYPE  =  CHARACTER*(*)
!
!     The character declarations may have white space surrounding
!     the length specifier.
!
!     Find the location where the data type token should be.
!
      tokloc = attloc(typpos) + 1

      if ( n < tokloc ) then
         call ctx%setmsg (  'Column data type specification did not '     // &
                        'follow "DATATYPE" keyword in declaration #.'  )
         call ctx%errch  ( '#', decl                                       )
         call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                          )
         return
      end if

      if ( tokens(tokloc) == 'INTEGER' ) then

         pardsc(typidx)  =  int
         pardsc(lenidx)  =  1

      else if (  eqstr( tokens(tokloc), 'DOUBLE PRECISION' )  ) then

         pardsc(typidx)  =  dp
         pardsc(lenidx)  =  1

      else if (  eqstr( tokens(tokloc), 'TIME' )  ) then

         pardsc(typidx)  =  time
         pardsc(lenidx)  =  1

      else if (  tokens(tokloc)(1:9) == 'CHARACTER'  ) then

         pardsc(typidx)  =  chr
!
!        To simplify picking up the length specification, compress
!        out blanks and parentheses.  This should leave us with
!        a token of the form
!
!           CHARACTER*<integer>
!
!        or
!
!           CHARACTER**
!
         call cmprss_inplace ( ' ', 0, tokens(tokloc) )
         call cmprss_inplace ( '(', 0, tokens(tokloc) )
         call cmprss_inplace ( ')', 0, tokens(tokloc) )

         if ( tokens(tokloc)(10:10) /= '*' ) then

            call ctx%setmsg ( 'Required asterisk missing from character ' // &
                          'column declaration:  #  in declaration:  ' // &
                          '#'                                          )
            call ctx%errch  ( '#', tokens(tokloc)                          )
            call ctx%errch  ( '#', decl                                    )
            call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if

         if ( tokens(tokloc)(11:11)  ==  '*'  ) then
!
!           The string length is variable.
!
            pardsc(lenidx) = ifalse

         else
!
!           The portion of the token following the asterisk should be a
!           string length.
!
            msg = ' '
            call nparsi (tokens(tokloc)(11:), pardsc(lenidx), msg, ptr )

            if ( msg /= ' ' ) then

               call ctx%setmsg ( 'String length specification # didn''t ' // &
                             'parse as an integer in declaration   #'  )
               call ctx%errch  ( '#',  tokens(tokloc)(11:)                 )
               call ctx%errch  ( '#',  decl                                )
               call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                    )
               return

            end if

         end if

      else
!
!        The type specification is invalid.
!
         call ctx%setmsg ( 'Data type specification # is unrecognized in '// &
                       'declaration #.'                                )
         call ctx%errch  ( '#', tokens(tokloc)                             )
         call ctx%errch  ( '#', decl                                       )
         call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                          )
         return

      end if

!
!     Next, parse the size specification, if we have one.  If it's
!     valid, it's either the string 'VARIABLE' or it's an integer.
!
      if ( attfnd(sizpos) ) then

         tokloc  =  attloc(sizpos) + 1

         if ( n < tokloc ) then

            call ctx%setmsg (  'Column size specification did not follow '// &
                           '"SIZE" keyword in declaration #.'          )
            call ctx%errch  ( '#', decl                                    )
            call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if

         if ( tokens(tokloc) == 'VARIABLE' ) then
!
!           Variable size entries are not allowed for CHARACTER*(*)
!           columns.
!
            if ( pardsc(typidx) == chr ) then

               if ( pardsc(lenidx) == ifalse ) then

                  call ctx%setmsg (  'Column size specification was ' // &
                                 'VARIABLE for a CHARACTER*(*) '  // &
                                 'column in  declaration #.'      )
                  call ctx%errch  ( '#', decl                         )
                  call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'            )
                  return

               end if

            end if

            pardsc(sizidx) = ifalse

         else

            call nparsi ( tokens(tokloc), pardsc(sizidx), msg,  ptr )

            if ( msg /= ' ' ) then

               call ctx%setmsg ( 'Column element size  specification # '  // &
                             'didn''t parse as an integer in '        // &
                             'in declaration #'                        )
               call ctx%errch  ( '#',  tokens(tokloc)                      )
               call ctx%errch  ( '#',  decl                                )
               call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                    )
               return

            end if

         end if

      else
!
!        If the size is not specified, it defaults to 1.
!
         pardsc(sizidx) = 1

      end if

!
!     The data type and entry size determine the column's class.
!
      if ( pardsc(typidx) == chr ) then
!
!        The character classes are 3 for scalars, 6 for arrays.
!
         if ( pardsc(sizidx) == 1 ) then
            pardsc(clsidx) =  3
         else
            pardsc(clsidx) =  6
         end if

      else if ( pardsc(typidx) == int ) then
!
!        The integer classes are 1 for scalars, 4 for arrays.
!
         if ( pardsc(sizidx) == 1 ) then
            pardsc(clsidx) =  1
         else
            pardsc(clsidx) =  4
         end if

      else if (      ( pardsc(typidx) == dp   ) &
                .or. ( pardsc(typidx) == time )  ) then
!
!        The d.p. classes are 2 for scalars, 6 for arrays.  TIME
!        values are represented using d.p. classes as well.
!
         if ( pardsc(sizidx) == 1 ) then
            pardsc(clsidx) =  2
         else
            pardsc(clsidx) =  5
         end if

      end if

!
!     Parse the `NULLS_OK' clause, if we have one.
!
      if ( attfnd(nflpos) ) then

         tokloc = attloc(nflpos) + 1

         if ( n < tokloc ) then

            call ctx%setmsg (  'Boolean value did not follow '            // &
                           '"NULLS_OK" keyword in declaration #.'      )
            call ctx%errch  ( '#', decl                                    )
            call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if

         if ( tokens(tokloc) == 'TRUE' ) then

            pardsc(nflidx)  =  itrue

         else if ( tokens(tokloc) == 'FALSE' ) then

            pardsc(nflidx)  =  ifalse

         else

            call ctx%setmsg ( 'Invalid token # follows NULLS_OK keyword ' // &
                          'in declaration #. '                         )
            call ctx%errch  ( '#',  tokens(tokloc)                         )
            call ctx%errch  ( '#',  decl                                   )
            call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if

      else
!
!        As a default, nulls are not allowed.
!
         pardsc(nflidx) = ifalse

      end if

!
!     Parse the `INDEXED' clause, if we have one.
!
      if ( attfnd(idxpos) ) then

         tokloc = attloc(idxpos) + 1

         if ( n < tokloc ) then

            call ctx%setmsg (  'Boolean value did not follow '            // &
                           '"INDEXED" keyword in declaration #.'       )
            call ctx%errch  ( '#', decl                                    )
            call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if

         if ( tokens(tokloc) == 'TRUE' ) then
!
!           If we have a fixed-size column whose size is 1, then it's
!           possible to index that column.  Otherwise, we should not
!           have an `INDEXED' clause.
!
            if ( pardsc(sizidx) /= 1 ) then

               call ctx%setmsg ( 'Non-scalar columns cannot be indexed. ' // &
                             'Declaration was #.'                      )
               call ctx%errch  ( '#',  decl                                )
               call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                    )
               return

            end if

            pardsc(ixtidx)  =  itrue

         else if ( tokens(tokloc) == 'FALSE' ) then

            pardsc(ixtidx)  =  ifalse

         else

            call ctx%setmsg ( 'Invalid token # follows INDEXED keyword '  // &
                          'in declaration #. '                         )
            call ctx%errch  ( '#',  tokens(tokloc)                         )
            call ctx%errch  ( '#',  decl                                   )
            call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if

      else
!
!        As a default, the column is not indexed.
!
         pardsc(ixtidx) = ifalse

      end if

!
!     Parse the `FIXED_COUNT' clause, if we have one.
!
      if ( attfnd(fxcpos) ) then

         tokloc = attloc(fxcpos) + 1

         if ( n < tokloc ) then

            call ctx%setmsg (  'Boolean value did not follow '            // &
                           '"FIXED_COUNT" keyword in declaration #.'   )
            call ctx%errch  ( '#', decl                                    )
            call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if

         if ( tokens(tokloc) == 'TRUE' ) then
!
!           The column is a fixed-count column.  Only scalar columns
!           are permitted to have fixed count.  We adjust the column
!           class to indicate fixed-count columns.
!
            if ( pardsc(clsidx) == 1 ) then
!
!              Map scalar integers.
!
               pardsc(clsidx) =  7

            else if ( pardsc(clsidx) == 2 ) then
!
!              Map scalar d.p. numbers.
!
               pardsc(clsidx) =  8

            else if ( pardsc(clsidx) == 3 ) then
!
!              Map scalar strings.
!
               pardsc(clsidx) =  9

            else

               call ctx%setmsg ( 'FIXED_COUNT attribute used in non-'     // &
                             'scalar column declaration #. '           )
               call ctx%errch  ( '#',  decl                                )
               call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                    )
               return

            end if

         else if ( tokens(tokloc) /= 'FALSE' ) then
!
!           No action is required if the FIXED_COUNT keyword is set
!           to FALSE, but no value other than FALSE or TRUE may appear
!           on the RHS.
!
            call ctx%setmsg ( 'Invalid token # follows NULLS_OK keyword ' // &
                          'in declaration #. '                         )
            call ctx%errch  ( '#',  tokens(tokloc)                         )
            call ctx%errch  ( '#',  decl                                   )
            call ctx%sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if

      end if

      return
      end associate
      end subroutine zzekpdec

      !> Check that an EK is valid for a specified type of access by the
      !> paging system.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is an EK file handle.  The specified file is to be
      !>                    checked to see whether it is a valid paged EK and
      !>                    whether it is open for the specified type of
      !>                    access.
      !>
      !>     ACCESS         is a short string indicating the type of access
      !>                    desired.  Possible values are 'READ' and 'WRITE'.
      !>
      !>                    Leading and trailing blanks in ACCESS are ignored,
      !>                    and case is not significant.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If the EK architecture version is not current, the error
      !>         SPICE(WRONGARCHITECTURE) is signaled.
      !>
      !>     3)  If the DAS logical address ranges occupied by the EK are
      !>         not consistent with those recorded by the paging system,
      !>         the error SPICE(INVALIDFORMAT) is signaled.
      !>
      !>     4)  If the EK is not open for the specified type of access, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine centralizes a validation check performed by many
      !>     EK routines.  The EK designated by HANDLE is tested to see
      !>     whether some aspects of its structure are valid, and whether
      !>     the specified type of access (read or write) is allowed.
      !>     The tests performed are:
      !>
      !>        - Is the file a DAS file open for the specified type of access?
      !>
      !>        - Is the file's EK architecture version correct?
      !>
      !>        - Are the DAS address ranges in use consistent with those
      !>          recorded in the file by the paging system?
      !>
      !>     If the file fails any test, an error is signaled.
      !>
      !>## Examples
      !>
      !>     See EKINSR.
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
      !>-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN to insert DAS file name into
      !>        long error messages.
      !>
      !>-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
      module subroutine zzekpgch ( ctx, handle, access )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ekarch
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle attached to EK file.
      character(*), intent(in) :: access  !! Access type.
      integer(ip) :: npi_1(1)  !! one-element copy of npi (array argument)
      integer(ip) :: npd_1(1)  !! one-element copy of npd (array argument)
      integer(ip) :: npc_1(1)  !! one-element copy of npc (array argument)
      integer(ip) :: id_1(1)  !! one-element copy of id (array argument)
!
!     Local variables
!
      integer(ip) :: id
      integer(ip) :: lastc
      integer(ip) :: lastd
      integer(ip) :: lasti
      integer(ip) :: npc
      integer(ip) :: npd
      integer(ip) :: npi
      integer(ip) :: topc
      integer(ip) :: topd
      integer(ip) :: topi

!
!     Check whether the DAS is opened for the specified access method.
!
      call dassih ( ctx, handle, access )

      if ( ctx%failed() ) return

!
!     Make sure the DAS file is of the right type.
!
      call dasrdi ( ctx, handle, eparch, eparch, id_1 )
      id = id_1(1)

      if ( id /= archid ) then

         call ctx%setmsg ( 'File # has architecture #, which is invalid '// &
                       'for paged access.  You are using EK software '// &
                       'version #.'                                  )
         call ctx%errhan ( '#',   handle                                 )
         call ctx%errint ( '#',   id                                     )
         call ctx%errint ( '#',   archid                                 )
         call ctx%sigerr ( 'SPICE(WRONGARCHITECTURE)'                    )
         return

      end if

!
!     Obtain the page counts.  Set the `top' addresses.
!
      call dasrdi ( ctx, handle, epnpc, epnpc, npc_1 )
      npc = npc_1(1)
      call dasrdi ( ctx, handle, epnpd, epnpd, npd_1 )
      npd = npd_1(1)
      call dasrdi ( ctx, handle, epnpi, epnpi, npi_1 )
      npi = npi_1(1)

      topc  =  npc * pgsizc
      topd  =  npd * pgsizd
      topi  =  npi * pgsizi + pgbasi

!
!     Verify that the last addresses in use are consistent with the
!     `top' addresses known to this system.
!
      call daslla ( ctx, handle, lastc, lastd, lasti )

      if ( lastc > topc ) then

         call ctx%setmsg ( 'File # has last char address #; `top'' = #.' )
         call ctx%errhan ( '#',   handle                                 )
         call ctx%errint ( '#',   lastc                                  )
         call ctx%errint ( '#',   topc                                   )
         call ctx%sigerr ( 'SPICE(INVALIDFORMAT)'                        )
         return

      else if ( lastd > topd ) then

         call ctx%setmsg ( 'File # has last d.p. address #; `top'' = #.' )
         call ctx%errhan ( '#',   handle                                 )
         call ctx%errint ( '#',   lastd                                  )
         call ctx%errint ( '#',   topd                                   )
         call ctx%sigerr ( 'SPICE(INVALIDFORMAT)'                        )
         return

      else if ( lasti > topi ) then

         call ctx%setmsg ( 'File # has last int. address #; `top'' = #.' )
         call ctx%errhan ( '#',   handle                                 )
         call ctx%errint ( '#',   lasti                                  )
         call ctx%errint ( '#',   topi                                   )
         call ctx%sigerr ( 'SPICE(INVALIDFORMAT)'                        )
         return

      end if

      return
      end subroutine zzekpgch

      !> Initialize encoded EK query.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     ISIZE          is the size of the cell comprising the integer
      !>                    component of the encoded query.
      !>
      !>     DSIZE          is the size of the array comprising the double
      !>                    precision component of the encoded query.
      !>
      !>     EQRYI          is an integer array that is to serve as the
      !>                    integer portion of an encoded EK query.  EQRYI
      !>                    will be initialized as an integer cell having
      !>                    size ISIZE.
      !>
      !>     EQRYC          is a character string that is to serve as
      !>                    the character portion of an encoded EK query.
      !>                    EQRYC will be set to blank.
      !>
      !>     EQRYD          is a double precision array that is to serve as
      !>                    the numeric portion of an encoded EK query.
      !>
      !>## Detailed Output
      !>
      !>     EQRYI,
      !>     EQRYD,
      !>     EQRYC          are the components of an initialized EK query.
      !>                    The query's architecture code will be set to 1.
      !>                    All counts will be set to zero.  The array
      !>                    EQRYD will be zeroed out.  The character string
      !>                    EQRYC will be blank.
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) ISIZE must be large enough to accommodate a query with the
      !>        maximum number of tables, constraints, and tokens, and
      !>        indicated by the parameters in ekqlimit.inc.  If ISIZE is
      !>        too small, the error SPICE(CELLTOOSMALL) will be signaled.
      !>
      !>     2) DSIZE must be large enough to accommodate the largest number
      !>        of numeric tokens that can occur in a query.  If DSIZE is
      !>        too small, the error SPICE(CELLTOOSMALL) will be signaled.
      !>
      !>     3) EQRYC must be long enough to accommodate all of the character
      !>        data that can occur in a query.  If EQRYC is too short, the
      !>        error  SPICE(STRINGTOOSHORT) will be signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine assumes that encoded EK query architecture version
      !>     1 is to be used with the query to be initialized; this routine
      !>     will not work with any other architecture version.
      !>
      !>## Examples
      !>
      !>     See ZZEKPARS.
      !>
      !>## Restrictions
      !>
      !>     1) This routine is private to the EK library.  No routines
      !>        external to the EK library should call this routine.
      !>
      !>     2) Uses EK encoded query architecture version 1.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
      module subroutine zzekqini ( ctx, isize, dsize, eqryi, eqryc, eqryd )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekquery
      use pepper_inc_ekqlimit
      use pepper_inc_ekbool
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: isize  !! Size of integer component of encoded query.
      integer(ip), intent(in) :: dsize  !! Size of d.p. component of encoded query.
      integer(ip), intent(inout) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*), intent(inout) :: eqryc  !! Character component of query.
      real(wp), intent(inout) :: eqryd ( * )  !! D.p. component of query.

      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check sizes:
!
!     PEPPER: no minimum sizes. The callers size the encoded query from
!     the query (its integer part grows as it is written; the numbers and
!     the strings of the query fit in buffers sized from its length);
!     SPICELIB required room for the largest query (CELLTOOSMALL,
!     STRINGTOOSHORT).
!
      call ssizei ( ctx, isize, eqryi )
      eqryd(1:dsize) = 0.0_wp
      eqryc = ' '

!
!     Append enough elements to the integer cell to contain the
!     fixed-size portion of the encoded query:
!
      do i = 1, eqvbas
         call appndi ( ctx, 0, eqryi )
      end do

!
!     Clear out the fixed-size portion of the integer cell.
!
      eqryi(1:eqvbas) = 0

!
!     Fill in the architecture version.
!
      eqryi( eqarch )  =  1

!
!     Set the parse completion and name and time resolution flags to
!     indicate `not done':
!
      eqryi(eqpars)    =  ifalse
      eqryi(eqnres)    =  ifalse
      eqryi(eqtres)    =  ifalse

!
!     Set the buffer sizes:
!
      eqryi(eqcsiz)    =  len(eqryc)
      eqryi(eqnsiz)    =  dsize

!
!     Set the free pointers:
!
      eqryi(eqnptr)    =  1
      eqryi(eqcptr)    =  1

!
!     Indicate that initialization has been done:
!
      eqryi(eqinit)    =  itrue

      return
      end subroutine zzekqini

      !> Back up a modified EK record belonging to a shadowed EK.
      !> This is a stub routine.
      !>
      !>## Detailed Input
      !>
      !>     ACTION         is a short string indicating the action taken
      !>                    that necessitated backing up a record from the
      !>                    specified EK file.  Values and meanings of
      !>                    ACTION are:
      !>
      !>                       'ADD'         The indicated record is being
      !>                                     added to the input EK.  No data
      !>                                     is backed up in this case, since
      !>                                     a rollback will remove the
      !>                                     indicated record from the input
      !>                                     EK file.
      !>
      !>                       'UPDATE'      The indicated record is being
      !>                                     updated.  When ACTION indicates a
      !>                                     record update, the argument COLDSC
      !>                                     (see below) indicates the column
      !>                                     that was affected.  If the
      !>                                     specified column entry has not yet
      !>                                     been backed up, it will be.
      !>                                     Otherwise, no action is taken.
      !>
      !>                       'DELETE'      The indicated record is being
      !>                                     deleted from the input EK.  If the
      !>                                     record has been added since the
      !>                                     last commit, no data from the
      !>                                     record is backed up, but a
      !>                                     placeholder record is created.
      !>                                     If the record to be deleted
      !>                                     existed at the time of the last
      !>                                     commit, the entire original
      !>                                     record is backed up.
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     SEGDSC         is the descriptor of the segment from which to
      !>                    delete the specified column entry.
      !>
      !>     COLDSC         is the descriptor of the column from which to
      !>                    delete the specified column entry.  COLDSC is
      !>                    ignored unless ACTION is set to 'UPDATE'.
      !>
      !>     RECNO          is the number of the record containing the column
      !>                    entry to back up.
      !>
      !>## Detailed Output
      !>
      !>     None.  See the $Particulars section for a description of the
      !>     effect of this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.  The file may be corrupted.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects:  it performs record backup
      !>     functions to support EK shadowing.  If the input file is not
      !>     shadowed, this routine returns without taking any action.
      !>
      !>     This routine uses a shadow EK file to store sufficient information
      !>     to restore the data in an EK file to its state at the time of
      !>     the last commit.  The segments in the backup file are in
      !>     one-to-one correspondence with the modified segments of loaded
      !>     EK files.  If the backup file doesn't contain a backup
      !>     segment corresponding to the specified segment in the input file,
      !>     a new backup segment is started.
      !>
      !>     Whenever this routine is called, it modifies the backup segment
      !>     as necessary to reflect changes made to the source segment.
      !>     The actions taken are as follows:
      !>
      !>        Updates
      !>        -------
      !>
      !>        The first time an OLD record is updated, a backup record is
      !>        created for that record.  The old value of the updated column
      !>        entry is saved in the backup record.  The status of the
      !>        source record becomes UPDATE.  The status of the backup
      !>        record is OLD.
      !>
      !>        Updates to unmodified entries in an UPDATEd record cause the
      !>        original values of those entries to be stored in the backup
      !>        record.  Unmodified entries are not backed up.  Once an entry
      !>        has been updated, further updates to that entry do not cause
      !>        any backup action to be taken.
      !>
      !>        Updates to NEW records do not result in any action.
      !>
      !>        Additions
      !>        ---------
      !>
      !>        When a new record is added to the source segment, an empty
      !>        record is appended to the backup segment.  The backup record
      !>        has status NEW and points back to the new source record.  Note
      !>        that this backward pointer is guaranteed to be valid only when
      !>        the source record occupies its current ordinal position in the
      !>        source segment.
      !>
      !>        Deletions
      !>        ---------
      !>
      !>        When any record is deleted from the source segment, a backup
      !>        record is appended to the backup segment.  The backup record
      !>        has a pointer to the corresponding source record.  The pointer
      !>        is the record number of the deleted record at the time of
      !>        deletion. If the deleted record had NEW status, the backup
      !>        record is empty and has DELNEW status.  If the deleted record
      !>        had OLD or UPDATE status, the backup record is a copy of the
      !>        original state of the deleted record and has DELOLD status.
      !>
      !>     When a rollback is performed, the set of backup records that
      !>     denote additions and deletions is processed in LIFO order.  Each
      !>     record with DELETE status is copied to the source segment and
      !>     inserted at the ordinal position indicated by its backward
      !>     pointer.  Records with NEW status signal that the corresponding
      !>     source records are to be deleted.  Backup records having OLD
      !>     status are ignored during this step.  The inversion of additions
      !>     and deletions performed on the source segment ensures that
      !>     the backup records' pointers to source records are valid at the
      !>     time they are referenced.  After all insertions and deletions
      !>     are processed, all records having UPDATE status in the source
      !>     segment are returned to their original status by copying values
      !>     from their backup records into the corresponding column entries.
      !>     Forward pointers in the source records are used to identify the
      !>     corresponding backup records.
      !>
      !>     The changes made by this routine to the target EK file become
      !>     permanent when the file is closed.  Failure to close the file
      !>     properly will leave it in an indeterminate state.
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     1) This is a stub version of the routine.
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
      !>-    Beta Version 1.0.0, 25-OCT-1995 (NJB)
      module subroutine zzekrbck ( ctx, action, handle, segdsc, coldsc, recno )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekglimit
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: action  !! Action necessitating backup.
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip), intent(in) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip), intent(in) :: recno  !! Record number.
      character(1) :: tmpchr
      integer(ip) :: i

      tmpchr = action(1:1)
      i      = handle
      i      = segdsc(1)
      i      = coldsc(1)
      i      = recno

      return
      end subroutine zzekrbck

      !> Read scalar integer value from encoded EK query.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     EQRYI          is the integer portion of an encoded EK query.
      !>
      !>     NAME           is the name of the item whose value is to be read.
      !>                    This item is some element of the integer portion
      !>                    of an encoded query.
      !>
      !>## Detailed Output
      !>
      !>     VALUE          is the integer value designated by NAME.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input name is not recognized, the error
      !>         SPICE(INVALIDNAME) is signaled.  The encoded query is not
      !>         modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is the inverse of ZZEKWEQI.
      !>
      !>## Examples
      !>
      !>     See EKSRCH.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
      module subroutine zzekreqi ( ctx, eqryi, name, value )
      use pepper_kinds, only: ip
      use pepper_strings, only: ucase_inplace
      use pepper_inc_ekquery
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*), intent(in) :: name  !! Name of scalar item to read.
      integer(ip), intent(out) :: value  !! Value of item.
!
!     Initial values
!
      character(namlen), parameter :: namlst ( maxnam ) = [ character(namlen) :: &
         'ARCHITECTURE', 'INITIALIZED', 'PARSED', 'NAMES_RESOLVED',&
         'TIMES_RESOLVED', 'SEM_CHECKED', 'NUM_TABLES', 'NUM_CONJUNCTIONS',&
         'NUM_CONSTRAINTS', 'NUM_SELECT_COLS', 'NUM_ORDERBY_COLS', 'NUM_BUF_SIZE',&
         'FREE_NUM', 'CHR_BUF_SIZE', 'FREE_CHR' ]
      integer(ip), parameter :: namidx ( maxnam ) = [ &
         eqarch, eqinit, eqpars, eqnres,&
         eqtres, eqschk, eqntab, eqncnj,&
         eqncns, eqnsel, eqnord, eqnsiz,&
         eqnptr, eqcsiz, eqcptr ]
!
!     Local variables
!
      character(namlen) :: tmpnam
      integer(ip) :: i
      if ( .not. allocated ( ctx%zzekreqi_state ) ) allocate ( ctx%zzekreqi_state )
!
!     Find the location of the named item.
!
      tmpnam = adjustl(name)
      call ucase_inplace ( tmpnam )

      i  =  findloc(namlst, tmpnam, dim=1)

      if ( i == 0 ) then

         call ctx%setmsg ( 'Item # not found.'  )
         call ctx%errch  ( '#',  name           )
         call ctx%sigerr ( 'SPICE(INVALIDNAME)' )
         return

      end if

!
!     Do the deed.
!
      value  =  eqryi( namidx(i) )

      return
      end subroutine zzekreqi

      !> Obtain last address in use in EK scratch area.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     TOP            is the last address of the EK scratch area stack
      !>                    top.  This is the highest EK scratch area address
      !>                    currently in use.
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
      !>     The EK scratch area stack top ranges from zero to, theoretically,
      !>     the largest integer representable on the host system.
      !>     and never decreases during a program run.  Data pushed on the
      !>     EK stack is inserted at address TOP+1 and occupies a contiguous
      !>     range of addresses that extends upwards from this address.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZEKSCA for an example
      !>     of use of this routine.
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
      !>-    Beta Version 2.0.0, 08-SEP-1994 (NJB)
      !>
      !>        Updated for EK architecture 3.
      !>
      !>-    Beta Version 1.0.0, 25-FEB-1993 (NJB)
      module subroutine zzekstop ( ctx, top )
      use pepper_kinds, only: ip
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: top  !! EK scratch area stack top.
      if ( .not. allocated ( ctx%zzeksca_state ) ) then
         allocate ( ctx%zzeksca_state )
         call init_zzeksca_state ( ctx%zzeksca_state )
      end if
      associate ( t => ctx%zzeksca_state%t )
      top  =  t
      return
      end associate
      end subroutine zzekstop

      !> Push the contents of an integer array onto the EK scratch area
      !> stack.
      !>
      !>## Detailed Input
      !>
      !>     N              is the number of integers in the array IDATA to
      !>                    append to the EK scratch area.  The data is
      !>                    stored in scratch area addresses T+1:T+N,
      !>                    where T is the EK scratch area stack top prior to
      !>                    the call to ZZEKSPSH.
      !>
      !>     IDATA          is an integer array containing data to append to
      !>                    the EK scratch area.  The first N elements of
      !>                    IDATA are appended to the EK scratch area, in
      !>                    order.
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
      !>     1)  If N is non-positive, this routine simply returns.  No error
      !>         is signaled.
      !>
      !>     2)  If an I/O error occurs during the data addition, the error
      !>         will be diagnosed by routines called by this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Let TOP be the EK scratch area stack top prior to a call to this
      !>     routine.  Data that is appended to the EK scratch area by this
      !>     routine is inserted at address TOP+1 and occupies a contiguous
      !>     range of addresses that extends upwards from this address.
      !>
      !>     As a side effect of calling this routine, TOP is set to TOP + N.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZEKSCA for an example
      !>     of use of this routine.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine must execute quickly.  Therefore, it checks in
      !>         only if it detects an error.  If an error is signaled by a
      !>         routine called by this routine, this routine will not appear
      !>         in the SPICELIB traceback display.  Also, in the interest
      !>         of speed, this routine does not test the value of the SPICELIB
      !>         function RETURN upon entry.
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 15-JAN-1995 (NJB)
      module subroutine zzekspsh ( ctx, n, idata )
      use pepper_kinds, only: ip
      use pepper_containers, only: grow
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! Number of integers to push.
      integer(ip), intent(in) :: idata ( * )  !! Integer data.
      integer(ip) :: remain
      integer(ip) :: numadd
      integer(ip) :: rt
      integer(ip) :: lasti
      integer(ip) :: start
      integer(ip) :: lastd
      integer(ip) :: e
      integer(ip) :: lastc
      integer(ip) :: i
      integer(ip) :: b
      if ( .not. allocated ( ctx%zzeksca_state ) ) then
         allocate ( ctx%zzeksca_state )
         call init_zzeksca_state ( ctx%zzeksca_state )
      end if
!
!     PEPPER: the in-memory part grows to hold the stack, up to MEMSIZ.
!
      if ( n > 0 .and. ctx%zzeksca_state%t < memsiz ) then
         call grow ( ctx%zzeksca_state%scrtch, &
                     min ( memsiz, ctx%zzeksca_state%t + n ) )
      end if
      associate ( scrhan => ctx%zzeksca_state%scrhan, &
                  scrtch => ctx%zzeksca_state%scrtch, &
                  t => ctx%zzeksca_state%t, &
                  first => ctx%zzeksca_state%first )

!
!     Go back if there's no data to write.
!
      if ( n < 1 ) return

!
!     Add as much data as possible to our big array.
!
      if ( t < memsiz ) then

         numadd = min ( n, memsiz-t )

         do i = 1, numadd
            scrtch(t+i) = idata(i)
         end do

         t      = t + numadd

         if ( numadd == n ) return

         remain = n - numadd
         start  = 1 + numadd

         if ( remain == 0 ) return

      else

         remain = n
         start  = 1

      end if
!
!     At this point, REMAIN and START are set, and T reflects the
!     amount of data we've pushed so far.  If we got this far,
!     we'll need to put the rest of the data in the scratch DAS.
!
!     The DAS system requires separate operations for updating
!     an existing range of addresses and for appending data.
!     We need to know the last integer address in use in the DAS
!     file in order to determine which part of the data will
!     be written to addresses previously written to, and which
!     part will be appended.
!
!     PEPPER: the scratch DAS is opened when the stack first goes past
!     the in-memory part (SPICELIB: on the first push).
!
      if ( first ) then
         first = .false.
         call dasops ( ctx, scrhan )
         if ( ctx%failed() ) return
      end if

      call daslla ( ctx, scrhan, lastc, lastd, lasti )

!
!     To simplify our arithmetic, we'll work with a variable RT
!     that represents the stack top measured relative to the base
!     of the DAS integer array.  At this point, RT is greater than
!     or equal to zero.
!
      rt  =  t - memsiz

      if ( rt < lasti ) then
!
!        Some data can be added by updating DAS addresses.  The
!        available range for updating is B:E, where B and E are
!        calculated below.  This case can occur only when LASTI > 0.
!
         b       =  rt + 1
         e       =  min ( lasti,  rt + remain )

         call dasudi ( ctx, scrhan, b, e, idata(start) )

         numadd  =  e      - b + 1
         start   =  start  + numadd
         remain  =  remain - numadd
         t       =  t      + numadd

         if ( remain == 0 ) return

      end if

!
!     At this point, START and REMAIN are set, and T reflects the
!     amount of data we've pushed so far..  The remaining data
!     must be appended to the scratch DAS file.
!
      call dasadi ( ctx, scrhan, remain, idata(start) )

      t = t + remain

      return
      end associate
      end subroutine zzekspsh

      !> Pop a specified number of elements from the top of the EK scratch
      !> area stack, transferring this data to an integer array.
      !>
      !>## Detailed Input
      !>
      !>     N              is the number of integers to pop from the
      !>                    EK scratch area stack.  The data is
      !>                    read from the scratch area addresses T-N+1:T,
      !>                    where T is the stack top prior to the call to
      !>                    ZZEKSPOP.
      !>
      !>## Detailed Output
      !>
      !>     IDATA          is an integer array containing data read from
      !>                    the EK scratch area.  The first N elements of
      !>                    IDATA assigned the values occupying the top N
      !>                    elements of the EK stack.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If N is non-positive or if N is greater than the number of
      !>         items on the stack, the error SPICE(INVALIDCOUNT) is
      !>         signaled.
      !>
      !>     2)  If an I/O error occurs during the data read, the error
      !>         will be diagnosed by routines called by this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Let TOP be the EK scratch area stack top prior to a call to this
      !>     routine.  Data that is read from the EK scratch area by this
      !>     routine is transferred from addresses TOP-N+1 to TOP and occupies
      !>     to the range of addresses 1 to N in the array IDATA.
      !>
      !>     As a side effect of calling this routine, TOP is set to TOP - N.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZEKSCA for an example
      !>     of use of this routine.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine must execute quickly.  Therefore, it checks in
      !>         only if it detects an error.  If an error is signaled by a
      !>         routine called by this routine, this routine will not appear
      !>         in the SPICELIB traceback display.  Also, in the interest
      !>         of speed, this routine does not test the value of the SPICELIB
      !>         function RETURN upon entry.
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 10-SEP-1994 (NJB)
      module subroutine zzekspop ( ctx, n, idata )
      use pepper_kinds, only: ip
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! Number of integers to pop.
      integer(ip), intent(out) :: idata ( * )  !! Integer data.
      integer(ip) :: remain
      integer(ip) :: start
      integer(ip) :: e
      integer(ip) :: numrd
      integer(ip) :: base
      integer(ip) :: i
      integer(ip) :: b
      integer(ip) :: rb
      if ( .not. allocated ( ctx%zzeksca_state ) ) then
         allocate ( ctx%zzeksca_state )
         call init_zzeksca_state ( ctx%zzeksca_state )
      end if
      associate ( scrhan => ctx%zzeksca_state%scrhan, &
                  scrtch => ctx%zzeksca_state%scrtch, &
                  t => ctx%zzeksca_state%t, &
                  first => ctx%zzeksca_state%first )
!
!     PEPPER: the scratch DAS is opened by ZZEKSPSH when the stack first
!     goes past the in-memory part, and only read beyond it.
!

!
!     You can't pop a negative number of elements.
!
      if ( n < 0 ) then

         call ctx%setmsg ( 'Pop count must be non-negative; '    // &
                       'call requests popping # elements.'    )
         call ctx%errint ( '#', n                                 )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                  )
         return

!
!     It's an error to try to pop more data than we have on the
!     stack.
!
      else if ( n > t ) then

         call ctx%setmsg ( 'EK stack pointer = #; call requests popping ' // &
                       '# items.'                                      )
         call ctx%errint ( '#', t                                          )
         call ctx%errint ( '#', n                                          )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                           )
         return

      end if

!
!     Read as much data as possible from our big array.
!
      base  = t - n

      if ( base < memsiz ) then

         numrd = min ( n, memsiz-base )

         do i = 1, numrd
            idata(i) = scrtch(base+i)
         end do

         if ( numrd == n ) then

            t  =  t - numrd
            return

         end if

         remain = n      -  numrd
         base   = memsiz
         start  = numrd  +  1

      else

         remain = n
         start  = 1

      end if

!
!     At this point, REMAIN, START and BASE are set.  If we got this
!     far, we'll need to read the rest of the data from the scratch DAS.
!     Compute the base address to read from relative to the start of
!     the DAS array.
!
      rb     =  base - memsiz
      b      =  rb   + 1
      e      =  rb   + remain

      call dasrdi ( ctx, scrhan, b, e, idata(start) )

      t      =  t - n

      return
      end associate
      end subroutine zzekspop

      !> Decrement the EK scratch area stack pointer by a specified count.
      !>
      !>## Detailed Input
      !>
      !>     N              is the number to subtract from the EK scratch
      !>                    area stack pointer.
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
      !>     1)  If N is non-positive or if N is greater than the number of
      !>         items on the stack, the error SPICE(INVALIDCOUNT) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Let TOP be the EK scratch area stack top prior to a call to this
      !>     routine.  The  effect of calling this routine is that TOP is set
      !>     to TOP - N.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZEKSCA for an example
      !>     of use of this routine.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine must execute quickly.  Therefore, it checks in
      !>         only if it detects an error.  If an error is signaled by a
      !>         routine called by this routine, this routine will not appear
      !>         in the SPICELIB traceback display.  Also, in the interest
      !>         of speed, this routine does not test the value of the SPICELIB
      !>         function RETURN upon entry.
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 10-SEP-1994 (NJB)
      module subroutine zzeksdec ( ctx, n )
      use pepper_kinds, only: ip
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! Decrement count.
      if ( .not. allocated ( ctx%zzeksca_state ) ) then
         allocate ( ctx%zzeksca_state )
         call init_zzeksca_state ( ctx%zzeksca_state )
      end if
      associate ( scrhan => ctx%zzeksca_state%scrhan, &
                  t => ctx%zzeksca_state%t, &
                  first => ctx%zzeksca_state%first )
!
!     PEPPER: the scratch DAS is opened by ZZEKSPSH when the stack first
!     goes past the in-memory part.
!

!
!     Catch non-positive decrement requests.
!
      if ( n < 0 ) then

         call ctx%setmsg ( 'Decrement value must be non-negative; ' // &
                       'call requests decrement by #.'          )
         call ctx%errint ( '#', n                                   )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                    )
         return

!
!     It's an error to try to decrement the pointer by more than
!     the current stack depth.
!
      else if ( n > t ) then

         call ctx%setmsg ( 'EK stack pointer = #; call requests  ' // &
                       'decrement by #.'                        )
         call ctx%errint ( '#', t                                   )
         call ctx%errint ( '#', n                                   )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                    )
         return

      end if

      t  =  t  -  n

      return
      end associate
      end subroutine zzeksdec

      !> Update the contents of a range of addresses already in use in the
      !> EK scratch area.
      !>
      !>## Detailed Input
      !>
      !>     BEG,
      !>     END            are the first and last of a range of EK scratch
      !>                    area addresses to write to.  BEG and END must
      !>                    satisfy the relations
      !>
      !>                       1  <  BEG  <  END  <  TOP
      !>                          -       -       -
      !>
      !>                    where TOP is the last EK scratch area stack top
      !>                    at the time this routine is called.
      !>
      !>     IDATA          is an integer array containing data to write to
      !>                    the specified range of addresses in the EK scratch
      !>                    area.  The first END-BEG+1 elements of IDATA are
      !>                    written to the specified range in the EK scratch
      !>                    area, in order.
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
      !>     1)  If either of BEG or END are outside of the range 1:TOP,
      !>         where TOP is the EK scratch area stack top, the error
      !>         SPICE(INVALIDADDRESS) is signaled.
      !>
      !>     2)  If END < BEG, this routine simply returns.  No error
      !>         is signaled.
      !>
      !>     3)  If an I/O error occurs during the data addition, the error
      !>         will be diagnosed by routines called by this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Let TOP be the EK scratch area stack top prior to a call to this
      !>     routine.  This routine is used to modify values in the scratch
      !>     area that lie in the address range 1:TOP.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZEKSCA for an example
      !>     of use of this routine.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine must execute quickly.  Therefore, it checks in
      !>         only if it detects an error.  If an error is signaled by a
      !>         routine called by this routine, this routine will not appear
      !>         in the SPICELIB traceback display.  Also, in the interest
      !>         of speed, this routine does not test the value of the SPICELIB
      !>         function RETURN upon entry.
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
      !>-    SPICELIB Version 2.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 2.0.0, 23-FEB-1995 (NJB)
      !>
      !>        Updated for EK architecture 3.
      !>
      !>-    Beta Version 1.0.0, 25-FEB-1993 (NJB)
      module subroutine zzeksupd ( ctx, beg, end, idata )
      use pepper_kinds, only: ip
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: beg  !! Begin and end addresses of range to update.
      integer(ip), intent(in) :: end  !! Begin and end addresses of range to update.
      integer(ip), intent(in) :: idata ( * )  !! Integer data.
      integer(ip) :: i
      if ( .not. allocated ( ctx%zzeksca_state ) ) then
         allocate ( ctx%zzeksca_state )
         call init_zzeksca_state ( ctx%zzeksca_state )
      end if
      associate ( scrhan => ctx%zzeksca_state%scrhan, &
                  scrtch => ctx%zzeksca_state%scrtch, &
                  t => ctx%zzeksca_state%t )
      if (  ( beg < 1 ) .or. ( beg > t )  ) then

         call ctx%setmsg ( 'Start address BEG was #; valid range is 1:#' )
         call ctx%errint ( '#',  beg                                     )
         call ctx%errint ( '#',  t                                       )
         call ctx%sigerr ( 'SPICE(INVALIDADDRESS)'                       )
         return

      else if (  ( end < 1 ) .or. ( end > t  )  ) then

         call ctx%setmsg ( 'End address END was #; valid range is 1:#'   )
         call ctx%errint ( '#',  end                                     )
         call ctx%errint ( '#',  t                                       )
         call ctx%sigerr ( 'SPICE(INVALIDADDRESS)'                       )
         return

      else if ( beg > end ) then
         return
      end if

      if ( end <= memsiz ) then
!
!        If the entire range is in memory, fine.  Update the range
!        now.
!
         do i = beg, end
            scrtch(i)  =  idata(i-beg+1)
         end do

      else if ( beg <= memsiz ) then
!
!        Update the portion of the address range that's in memory.
!
         do i = beg, memsiz
            scrtch(i)  =  idata(i-beg+1)
         end do

!
!        Now update the rest of the range, which is in the scratch
!        DAS file.
!
         call dasudi  ( ctx, scrhan, &
                        1, &
                        end-memsiz, &
                        idata( memsiz-beg+2 )  )

      else
!
!        The whole range is in the DAS file.
!
         call dasudi ( ctx, scrhan,  beg-memsiz,  end-memsiz,  idata )

      end if

      return
      end associate
      end subroutine zzeksupd

      !> Read from a range of addresses in the EK scratch area.
      !>
      !>## Detailed Input
      !>
      !>     BEG,
      !>     END            are the first and last of a range of EK scratch
      !>                    area addresses to read from. BEG and END must
      !>                    satisfy the relations
      !>
      !>                       1  <  BEG  <  END  <  LAST
      !>                          -       -       -
      !>
      !>                    where LAST is the last EK scratch area address
      !>                    in use at the time this routine is called.
      !>
      !>## Detailed Output
      !>
      !>     IDATA          is an integer array containing data read from the
      !>                    range of addresses BEG:END in the EK scratch area.
      !>                    The first END-BEG+1 elements of IDATA are assigned
      !>                    in order using the contents of this address range.
      !>                    IDATA must have dimension at least END-BEG+1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If either of BEG or END are outside of the range 1:LAST,
      !>         where LAST is the last address already in use in the EK
      !>         scratch area, the error SPICE(INVALIDADDRESS) is signaled.
      !>
      !>     2)  If END < BEG, this routine simply returns.  No error
      !>         is signaled.
      !>
      !>     3)  If an I/O error occurs during the read, the error will be
      !>         diagnosed by routines called by this routine.
      !>
      !>     4)  If IDATA has dimension less than END-BEG+1, the results of
      !>         a call to this routine will be unpredictable, except that
      !>         you can safely predict they'll be wrong.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Let LAST be the last address in use in the EK scratch area prior
      !>     to a call to this routine.  This routine is used to read values
      !>     in the scratch area that lie in the address range 1:LAST.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZEKSCA for an example
      !>     of use of this routine.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine must execute quickly.  Therefore, it checks in
      !>         only if it detects an error.  If an error is signaled by a
      !>         routine called by this routine, this routine will not appear
      !>         in the SPICELIB traceback display.  Also, in the interest
      !>         of speed, this routine does not test the value of the SPICELIB
      !>         function RETURN upon entry.
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 23-FEB-1995 (NJB)
      module subroutine zzeksrd ( ctx, beg, end, idata )
      use pepper_kinds, only: ip
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: beg  !! Begin and end addresses of range to read from.
      integer(ip), intent(in) :: end  !! Begin and end addresses of range to read from.
      integer(ip), intent(out) :: idata ( * )  !! Integer data.
      integer(ip) :: i
      if ( .not. allocated ( ctx%zzeksca_state ) ) then
         allocate ( ctx%zzeksca_state )
         call init_zzeksca_state ( ctx%zzeksca_state )
      end if
      associate ( scrhan => ctx%zzeksca_state%scrhan, &
                  scrtch => ctx%zzeksca_state%scrtch, &
                  t => ctx%zzeksca_state%t )
      if (  ( beg < 1 ) .or. ( beg > t  )  ) then

         call ctx%setmsg ( 'Start address BEG was #; valid range is 1:#' )
         call ctx%errint ( '#',  beg                                     )
         call ctx%errint ( '#',  t                                       )
         call ctx%sigerr ( 'SPICE(INVALIDADDRESS)'                       )
         return

      else if (  ( end < 1 ) .or. ( end > t  )  ) then

         call ctx%setmsg ( 'End address END was #; valid range is 1:#'   )
         call ctx%errint ( '#',  end                                     )
         call ctx%errint ( '#',  t                                       )
         call ctx%sigerr ( 'SPICE(INVALIDADDRESS)'                       )
         return

      else if ( beg > end ) then
         return
      end if

      if ( end <= memsiz ) then
!
!        If the entire range is in memory, fine.  Read from the range
!        now.
!
         do i = beg, end
            idata(i-beg+1)  =  scrtch(i)
         end do

      else if ( beg <= memsiz ) then
!
!        Read from the portion of the address range that's in memory.
!
         do i = beg, memsiz
            idata(i-beg+1)  =  scrtch(i)
         end do

!
!        Now read the rest of the range, which is in the scratch
!        DAS file.
!
         call dasrdi  ( ctx, scrhan, &
                        1, &
                        end-memsiz, &
                        idata( memsiz-beg+2 )  )

      else
!
!        The whole range is in the DAS file.
!
         call dasrdi ( ctx, scrhan,  beg-memsiz,  end-memsiz,  idata )

      end if

      return
      end associate
      end subroutine zzeksrd

      !> Clean up:  re-initialize the EK scratch area; unload the
      !> scratch file.
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
      !>     None.
      !>
      !>## Files
      !>
      !>     This routine unloads the scratch DAS used by this system.
      !>
      !>## Particulars
      !>
      !>     This routine is intended to enable test software to unload
      !>     the scratch DAS file used by the EK scratch area routines.
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     1) Many EK routines operate by side effects on the EK scratch
      !>        area, so this routine must be used with caution.
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
      !>-    SPICELIB Version 3.1.0, 29-JUL-2003 (NJB)
      !>
      !>        Added DASWBR call.  This call frees the buffer records used by
      !>        the scratch file.
      !>
      !>-    SPICELIB Version 3.0.0, 27-DEC-2001 (NJB)
      module subroutine zzekscln ( ctx )
      use pepper_kinds, only: ip
      class(pepper_context), intent(inout) :: ctx  !! the context
      if ( .not. allocated ( ctx%zzeksca_state ) ) then
         allocate ( ctx%zzeksca_state )
         call init_zzeksca_state ( ctx%zzeksca_state )
      end if
      associate ( scrhan => ctx%zzeksca_state%scrhan, &
                  scrtch => ctx%zzeksca_state%scrtch, &
                  t => ctx%zzeksca_state%t, &
                  first => ctx%zzeksca_state%first )
!
!     PEPPER: only the part in use is cleared (SPICELIB: all MEMSIZ
!     integers, on every clean-up).
!
      scrtch(1:min(t, size(scrtch, kind=ip))) = 0
      t  = 0

!
!     If FIRST has been set to .FALSE., we've an open scratch DAS
!     to dispose of.
!
      if ( .not. first ) then
!
!        Write out the buffered records belonging to the scratch file;
!        this will cause them to be returned to the free list.
!
         call daswbr ( ctx, scrhan )

!
!        Dump the scratch DAS.
!
         call dasllc ( ctx, scrhan )

      end if

!
!     Tell the system to re-initialize on the next pass.
!
      first = .true.

      return
      end associate
      end subroutine zzekscln

      !> Scan tokens in an EK query.
      !>
      !>## Detailed Input
      !>
      !>     QUERY          is character string containing an EK query.  See
      !>                    the header of the subroutine EKFIND for a
      !>                    detailed description of the EK query language.
      !>
      !>     MAXNTK         is the maximum number of tokens that may occur
      !>                    in QUERY.
      !>
      !>     MAXNUM         is the maximum number of tokens representing
      !>                    numeric values that may occur in QUERY.
      !>
      !>## Detailed Output
      !>
      !>     NTOKEN         is the number of tokens found in the input QUERY.
      !>                    This number will be less than or equal to MAXNTK.
      !>
      !>     TOKENS         is an array of codes for the tokens found in QUERY.
      !>                    The parameter values for these codes are not part
      !>                    of the EKSCAN specification; however, these values
      !>                    must be kept consistent with those used by EKPARS.
      !>                    The caller of EKSCAN should declare TOKENS with
      !>                    dimension MAXNTK.
      !>
      !>     LXBEGS,
      !>     LXENDS         are, respectively, arrays of begin and end pointers
      !>                    for the lexemes occurring in QUERY. Lexemes are
      !>                    the strings in QUERY that correspond to tokens. For
      !>                    example, '4.9D0' and '3' are both lexemes that map
      !>                    to the token <number>.
      !>
      !>     VALUES         is an array of token values.  The Ith element of
      !>                    VALUES refers to the Ith token.
      !>
      !>                    If the Ith token is a number, the Ith element of
      !>                    VALUES is a pointer into the NUMVLS array where
      !>                    the value of the number is stored.  The Ith token
      !>                    code indicates whether the number was a signed
      !>                    integer or d.p. number.
      !>
      !>                    If the Ith token is a keyword, the Ith element of
      !>                    VALUES is the code for that keyword.
      !>
      !>                    If the Ith token is a quoted string, the Ith
      !>                    element of VALUES is the common index in the arrays
      !>                    CHBEGS and CHENDS where the begin and end positions
      !>                    in CHRBUF of the parsed identifier are stored.
      !>                    Identifiers are converted to upper case when they
      !>                    are scanned.
      !>
      !>                    If the Ith token is an identifier, the Ith element
      !>                    of VALUES has the same role as in the case of a
      !>                    quoted string.
      !>
      !>                    If the Ith token is a special character, the Ith
      !>                    element of values is undefined; the value of
      !>                    TOKENS is the value of ICHAR() applied to the
      !>                    character.
      !>
      !>                    The caller of EKSCAN should declare VALUES with
      !>                    dimension MAXNTK.
      !>
      !>     NUMVLS         is an array of numeric values of parsed numeric
      !>                    tokens.  The caller of EKSCAN should declare
      !>                    NUMVLS with dimension at least MAXNUM.
      !>
      !>     CHRBUF         is a character string used to contain the values
      !>                    of literal string tokens and identifiers.  The
      !>                    value MAXQRY is guaranteed to be a safe length for
      !>                    CHRBUF, though the caller of EKSCAN can probably
      !>                    get away with less.
      !>
      !>                    The reason for the existence of CHRBUF is that
      !>                    the lexemes representing quoted strings may contain
      !>                    doubled quote characters representing embedded
      !>                    quotes; these characters are undoubled when the
      !>                    lexemes are parsed.  Hence the parsed quoted
      !>                    strings are not necessarily substrings of the
      !>                    original lexemes from which they are derived.
      !>
      !>     CHBEGS,
      !>     CHENDS         are, respectively, arrays of begin and end pointers
      !>                    for parsed quoted strings and identifiers stored in
      !>                    CHRBUF.
      !>
      !>     SCNERR         is a logical flag which is set to .TRUE. if a
      !>                    scanning error is detected, and is set to .FALSE.
      !>                    otherwise.  If SCNERR is returned .TRUE., all
      !>                    outputs save ERRMSG are undefined.
      !>
      !>     ERRMSG         is an error message that describes the cause of
      !>                    a scanning error, if such an error is detected.
      !>                    When SCNERR is returned .FALSE., ERRMSG is set to
      !>                    blank.
      !>
      !>## Parameters
      !>
      !>     See the include files.
      !>
      !>## Exceptions
      !>
      !>     Error free.
      !>
      !>     This routine set the error flag ERROR to .TRUE. and returns an
      !>     error message in the event that a syntax error precludes scanning
      !>     the input string.  Note that incorrect queries may scan
      !>     successfully; it is the responsibility of the caller to ensure
      !>     syntactic and semantic correctness of queries.
      !>
      !>     The following error messages are returned by this routine:
      !>
      !>        'No table list preceded first keyword.'
      !>        'Column clause and WHERE keyword are missing.'
      !>        'WHERE keyword is missing.'
      !>        'Too many tokens in query; max allowed is #.'
      !>        'Column list was empty.'
      !>        'Quoted string in positions #:# is empty.'
      !>        'Unexpected token found in query: #'
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine breaks up a valid EK query into an array of
      !>     individual tokens in order to facilitate parsing.
      !>
      !>     Time values and quoted strings are treated as single tokens.
      !>
      !>## Examples
      !>
      !>     1)  Examples of strings containing lexically valid queries
      !>         are:
      !>
      !>            FROM TIME * WHERE TIME LT 'MO SCLK 15328997.121'
      !>
      !>            from time, event_type where event_type eq "MOC_EVENT"
      !>
      !>            FROM * WHERE TIME GE "1994 MAR 1" AND  IDCODE EQ -94030
      !>
      !>            FROM  *   WHERE
      !>                  TIME        GE   "1994 MAR 1"
      !>            AND   TIME        LE   '1-MAR-1994 18:4:1'
      !>            AND   EVENT_TYPE  LIKE '*PMIRR*'
      !>
      !>            FROM * WHERE TIME LT "MO SCLK 15328997.121" ORDER BY TIME
      !>
      !>            from col_1 col_2 col_3 where time lt '2010'
      !>
      !>            from col_1 col_2 col_3
      !>
      !>            from *
      !>
      !>            from * order by event_type
      !>
      !>         For a query to be semantically valid, all of the column names
      !>         referenced in the query must be present in at least one
      !>         loaded E-kernel.
      !>
      !>     2)  Examples of lexically invalid queries are:
      !>
      !>            from time where time lt
      !>            1991 jan 1                         {time string is not
      !>                                                quoted}
      !>
      !>            from time * where time
      !>             .lt. 1991 jan 1                   {operator should be lt}
      !>
      !>            from event_type * where
      !>            event_type eq ""                   {quoted string is empty}
      !>
      !>            from event_type ^ where
      !>            event_type eq "cmd"                {unexpected token}
      !>
      !>            from column1 where
      !>            column1 eq  3c                     {invalid numeric token}
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
      !>-    SPICELIB Version 3.0.2, 02-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    SPICELIB Version 3.0.1, 22-OCT-1996 (NJB)
      !>
      !>        Corrected miscellaneous errors in the header.
      !>
      !>-    SPICELIB Version 3.0.0, 14-NOV-1995 (NJB)
      !>
      !>        Complete re-write for architecture 3.
      !>
      !>## Revisions
      !>
      !>-    Beta Version 3.0.0, 14-NOV-1995 (NJB)
      !>
      !>        Complete re-write for architecture 3.
      module subroutine zzekscan ( ctx, query, maxntk, maxnum, ntoken, tokens, lxbegs, lxends, values, numvls, chrbuf, chbegs, chends, scnerr, errmsg )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchc
      use pepper_strings, only: beint, frstpc, lxqstr, nparsd, parsqs, prefix, rtrim, suffix, &
         ucase, repmc_inplace, repmi_inplace
      use pepper_inc_ekkeyw
      use pepper_inc_ektokn
      use pepper_inc_ekqlimit
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: query  !! Query specifying events to be found.
      integer(ip), intent(in) :: maxntk  !! Maximum number of tokens to return.
      integer(ip), intent(in) :: maxnum  !! Maximum number of numeric tokens allowed.
      integer(ip), intent(out) :: ntoken  !! Number of tokens returned.
      integer(ip), intent(out) :: tokens ( * )  !! Code numbers of identified tokens.
      integer(ip), intent(out) :: lxbegs ( * )  !! Start and end locations of lexemes in query.
      integer(ip), intent(out) :: lxends ( * )  !! Start and end locations of lexemes in query.
      integer(ip), intent(out) :: values ( * )  !! Token values or value pointers, as needed.
      real(wp), intent(out) :: numvls ( * )  !! Buffer containing values of numeric tokens.
      character(*), intent(out) :: chrbuf  !! Buffer containing string tokens and identifiers.
      integer(ip), intent(out) :: chbegs ( * )  !! Begin locations of string tokens in CHRBUF.
      integer(ip), intent(out) :: chends ( * )  !! End locations of string tokens in CHRBUF.
      logical, intent(out) :: scnerr  !! Flag indicating whether query parsed correctly.
      character(*), intent(out) :: errmsg  !! Scan error description.
!
!     Local parameters
!
      character(*), parameter :: dquote = '"'
      character(*), parameter :: squote = ''''
      integer(ip), parameter :: nxttok = 1
      integer(ip), parameter :: newtok = nxttok + 1
      integer(ip), parameter :: term = newtok + 1
      integer(ip), parameter :: qstr = term   + 1
      integer(ip), parameter :: dot = qstr   + 1
      integer(ip), parameter :: number = dot    + 1
      integer(ip), parameter :: ident = number + 1
      integer(ip), parameter :: spcial = ident + 1
      integer(ip), parameter :: mxspec = 512
!
!     Number of tokens made up of special characters:
!
      integer(ip), parameter :: nspec = 13
!
!     Max length of any such token:
!
      integer(ip), parameter :: spclen = 2
!
!     Local variables
!
      character(1) :: chr
!
!     The following tokens are sequences of special characters.  Some
!     of these are synonyms for keywords; some have other meanings.  In
!     this data statement, the longer sequences must precede the shorter
!     ones, in order for the matching algorithm to work properly.
!
      character(spclen), parameter :: spcstr ( nspec ) = [ character(spclen) :: &
         '!=', '^=', '<>', '<=',&
         '>=', '<', '>', '=',&
         '(', ')', ',', '.',&
         '*' ]
      character(kwlen), parameter :: keywds ( nkeywd ) = [ character(kwlen) :: &
         'ALL', 'AND', 'ASC ', 'AVG ',&
         'BETWEEN', 'BY', 'COUNT', 'DESC',&
         'DISTINCT', 'EQ', 'FROM', 'GE',&
         'GROUP', 'GT', 'HAVING', 'IS',&
         'LE', 'LIKE', 'LT', 'MAX',&
         'MIN', 'NE', 'NOT', 'NULL',&
         'OR', 'ORDER', 'SELECT', 'SUM',&
         'WHERE' ]
      integer(ip), parameter :: kwvals ( nkeywd ) = [ &
         kwall, kwand, kwasnd, kwavg,&
         kwbetw, kwby, kwcnt, kwdsnd,&
         kwdstn, kweq, kwfrom, kwge,&
         kwgrp, kwgt, kwhav, kwis,&
         kwle, kwlike, kwlt, kwmax,&
         kwmin, kwne, kwnot, kwnull,&
         kwor, kwordr, kwsel, kwsum,&
         kwwher ]
      integer(ip), parameter :: spctok ( nspec ) = [ &
         tkkey, tkkey, tkkey, tkkey,&
         tkkey, tkkey, tkkey, tkkey,&
         tklpar, tkrpar, tkcoma, tkdot,&
         tkstar ]
      integer(ip), parameter :: spcval ( nspec ) = [ &
         kwne, kwne, kwne, kwle,&
         kwge, kwlt, kwgt, kweq,&
         0, 0, 0, 0,&
         0 ]
      logical :: isalph
      logical :: isdigt
      logical :: isdot
      logical :: issign
      logical :: isnum
      logical :: isquot
      isalph ( chr )  =       (      ( ichar(chr) >= ichar('A') ) &
                               .and. ( ichar(chr) <= ichar('Z') ) ) &
                        .or.  (      ( ichar(chr) >= ichar('a') ) &
                               .and. ( ichar(chr) <= ichar('z') ) )

      isdigt ( chr )  =       (      ( ichar(chr) >= ichar('0') ) &
                               .and. ( ichar(chr) <= ichar('9') ) )

      isdot  ( chr )  =     ( chr == '.' )

      issign ( chr )  =     ( chr == '+' )    .or. ( chr == '-' )

      isnum  ( chr )  =       isdigt( chr )     .or.   issign ( chr ) &
                                                .or.  ( chr == '.' )

      isquot ( chr )  =     ( chr == squote ) .or. ( chr == dquote )
      integer(ip) :: cptr
      integer(ip) :: j
      character(:), allocatable :: tquery
      integer(ip) :: room
      integer(ip) :: i
      character(lnsize) :: hdchrs
      integer(ip) :: l
      integer(ip) :: nstrs
      integer(ip) :: nchars
      integer(ip) :: ptr
      character(lnsize) :: tlchrs
      integer(ip) :: last
      integer(ip) :: state
      integer(ip) :: chcard
      integer(ip) :: nnums
      integer(ip) :: length
      if ( .not. allocated ( ctx%zzekscan_state ) ) allocate ( ctx%zzekscan_state )
      associate ( idspec => ctx%zzekscan_state%idspec, &
                  pass1 => ctx%zzekscan_state%pass1 )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     The first time through, set up our identifier character set.
!
      if ( pass1 ) then
!
!        Each identifier must start with a letter (of either case).
!        The subsequent characters must be letters, numbers, dollar
!        signs or underscores.
!
         hdchrs = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' // &
                  'abcdefghijklmnopqrstuvwxyz'

         tlchrs = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' // &
                  'abcdefghijklmnopqrstuvwxyz' // &
                  '0123456789'                 // &
                  '$_'

         call ssizei ( ctx, mxspec,         idspec )
         call lxcsid ( ctx, hdchrs, tlchrs, idspec )

         pass1  =  .false.

      end if

!
!     We'll work with a local copy of the query.
!
      l       =  rtrim ( query )
!
!     PEPPER: the query is copied whole, with a blank after it as the
!     padding of SPICELIB's MAXQRY-character copy gave (which cut longer
!     queries without an error).
!
      tquery  =  query(:l) // ' '

!
!     Initialize pointers and counts.
!
      cptr   =  1
      nnums  =  0
      nstrs  =  0
      chcard =  0
      ntoken =  0

!
!     Start out in the token search state.
!
      state  =  nxttok

      do while ( state /= term )

         if ( state == nxttok ) then
!
!           In our initial state, we're looking for a new token.
!           We stop when we have enough characters to determine
!           which kind of token we have, or if we run out of
!           characters.
!
!           Set our character pointer to the beginning of the next
!           token.
!
            if ( ntoken > 0 ) then
              cptr =  lxends(ntoken) + 1
            end if

            if ( cptr > l ) then

               state  =  term

            else

               do while (       ( tquery(cptr:cptr) == ' ' ) &
                          .and. ( cptr              < l   )  )

                  cptr  =  cptr + 1
               end do

               if ( tquery(cptr:cptr) == ' ' ) then
!
!                 We're out of non-blank characters to look at.
!
                  state  =  term

               else

                  chr    =  tquery(cptr:cptr)
                  state  =  newtok

               end if

            end if
!
!           STATE is in the set {NEWTOK, TERM}.
!

         else if ( state == newtok ) then
!
!           If we got this far, we have the initial character of
!           something that could be a valid token.  We test for
!
!              - quoted strings
!              - numbers
!              - identifiers
!              - special symbols
!
!           in that order.  Of course, we must have room in our output
!           arrays for the token.
!
            if ( ntoken == maxntk ) then

               errmsg  =  'Maximum allowed number of tokens is #; at '// &
                          'least # tokens are present in QUERY.'
               call repmi_inplace ( errmsg, '#', maxntk )
               call repmi_inplace ( errmsg, '#', maxntk+1 )

               scnerr = .true.
               return

            end if

            if ( isquot(chr) ) then

               state = qstr

            else if ( isdot(chr) ) then

               state = dot

            else if ( isnum(chr) ) then

               state = number

            else if ( isalph(chr) ) then

               state = ident

            else

               state = spcial

            end if

!
!           At this point, the next value of STATE has been determined.
!           STATE is in the set
!
!              {QSTR, NUMBER, IDENT, SPCIAL}
!

         else if ( state == qstr ) then
!
!           Look for a quoted string starting at location CPTR.
!           Use the current character as the quote character.
!
            call lxqstr ( tquery, chr, cptr, last, nchars )

            if ( nchars == 0 ) then

               errmsg = 'Invalid quoted string at location #.'
               call repmi_inplace ( errmsg, '#', cptr )

               scnerr = .true.
               return

            end if

!
!           We've located a quoted string lexeme.  Parse the lexeme
!           and obtain the corresponding string value.  First make
!           sure we have enough room for the parsed string.
!
            room  =  len(chrbuf)-chcard

            if ( nchars > room )  then

               errmsg  =  'Insufficient space to store quoted '    // &
                          'string at location #; # chars needed; ' // &
                          'only # are available.'

               call repmi_inplace ( errmsg, '#', cptr )
               call repmi_inplace ( errmsg, '#', nchars )
               call repmi_inplace ( errmsg, '#', room )

               scnerr = .true.
               return

            end if

            call parsqs ( tquery(cptr:cptr+nchars-1), &
                          chr, &
                          chrbuf(chcard+1:), &
                          length, &
                          scnerr, &
                          errmsg, &
                          ptr              )

            if ( scnerr ) then

               call prefix ( '#', 2, errmsg )

               call repmc_inplace ( errmsg, &
                             '#', &
                             'Error occurred while parsing '    // &
                             'quoted string token at location #:' )

               call repmi_inplace ( errmsg, '#', cptr )

               scnerr = .true.

               return

            end if

!
!           We've found a valid quoted string.  Set our outputs.
!
            ntoken          =  ntoken + 1
            tokens(ntoken)  =  tkqstr
            nstrs           =  nstrs  + 1
            values(ntoken)  =  nstrs
            chbegs(nstrs)   =  chcard + 1
            chends(nstrs)   =  chcard + length
            chcard          =  chends(nstrs)
            lxbegs(ntoken)  =  cptr
            lxends(ntoken)  =  last

            state           =  nxttok

!
!           STATE is now NXTTOK.
!

         else if ( state == dot ) then
!
!           The token begins with a period.  We could be looking at
!           a floating point number, or we could be looking at a
!           period in a compound identifier.
!
!           Look for a number starting at location CPTR.
!
            call lx4num ( ctx, tquery, cptr, last, nchars )

            if ( nchars > 0 ) then
               state  =  number
            else
               state  =  spcial
            end if

!
!           STATE has been set to NUMBER or SPCIAL.  CPTR and NTOKEN
!           remain unchanged.
!

         else if ( state == number ) then
!
!           Look for a number starting at location CPTR.
!
            call lx4num ( ctx, tquery, cptr, last, nchars )

            if ( nchars == 0 ) then

               errmsg = 'Invalid numeric token at location #.'
               call repmi_inplace ( errmsg, '#', cptr )

               scnerr = .true.
               return

            end if

!
!           Parse the token, but only do so if there's enough
!           room to store the result.
!
            room  =  maxnum - nnums

            if ( room < 1 ) then

               errmsg  =  'Insufficient space to store value of ' // &
                          'number at location #; # elements are ' // &
                          'available in the NUMVLS array; # are ' // &
                          'required.'

               call repmi_inplace ( errmsg, '#', cptr )
               call repmi_inplace ( errmsg, '#', maxnum )
               call repmi_inplace ( errmsg, '#', maxnum+1 )

               scnerr = .true.
               return

            end if

            call nparsd ( tquery(cptr:last), &
                          numvls(nnums+1), &
                          errmsg, &
                          ptr                 )

            if ( errmsg /= ' ' ) then
!
!              This check is done for safety; by construction, we
!              should always have a valid number if LX4NUM
!              thinks we have a valid number, so in fact ERRMSG
!              should always be blank.
!

               call prefix ( '#', 2, errmsg )

               call repmc_inplace ( errmsg, &
                             '#', &
                             'Error found in numeric ' // &
                             'token at location #:' )

               call repmi_inplace ( errmsg, '#', cptr+ptr-1 )

               scnerr = .true.
               return

            end if

!
!           We found a valid numeric token.  We distinguish
!           between integers and d.p. numbers; set the token
!           to the most restrictive category possible.
!
            ntoken  =  ntoken + 1

            if (   beint( tquery(cptr:last) )    ) then
               tokens(ntoken) =  tkint
            else
               tokens(ntoken) =  tkdp
            end if

!
!           Set the rest of our outputs.
!
            nnums           =  nnums  + 1
            values(ntoken)  =  nnums
            lxbegs(ntoken)  =  cptr
            lxends(ntoken)  =  last

            state           =  nxttok

!
!           STATE is now NXTTOK.
!

         else if ( state == ident ) then
!
!           Look for an identifier starting at location CPTR.
!
            call lxidnt ( ctx, idspec, tquery, cptr, last, nchars )

            if ( nchars == 0 ) then
!
!              This check is done for safety; by construction, we
!              should always have a valid identifier of at least one
!              character if we get to the IDENT state, so in fact
!              NCHARS should never equal zero.
!
               errmsg = 'Invalid identifier at location #.'
               call repmi_inplace ( errmsg, '#', cptr )

               scnerr = .true.
               return

            end if

!
!           We've located an identifier lexeme.  Make sure we have
!           enough room for the string.
!
            room  =  len(chrbuf)-chcard

            if ( nchars > room )  then

               errmsg  =  'Insufficient space to store identifier '// &
                          'string at location #; # chars needed; ' // &
                          'only # are available.'

               call repmi_inplace ( errmsg, '#', cptr )
               call repmi_inplace ( errmsg, '#', nchars )
               call repmi_inplace ( errmsg, '#', room )

               scnerr = .true.
               return

            end if

!
!           We've found a valid identifier or keyword.  Set our
!           outputs.  Convert the string to upper case.
!
            ntoken =  ntoken + 1

            call ucase (  tquery( cptr:last ), &
                          chrbuf( chcard+1:chcard+nchars )  )

            i      =  bsrchc ( chrbuf( chcard+1 : chcard+nchars ), &
                               nkeywd, &
                               keywds                            )

            if ( i > 0 ) then
!
!              It's a keyword.
!
               tokens(ntoken)  =  tkkey
               values(ntoken)  =  kwvals(i)
               lxbegs(ntoken)  =  cptr
               lxends(ntoken)  =  last

               state           =  nxttok

            else
!
!              It's an identifier.
!
               nstrs           =  nstrs  + 1
               chbegs(nstrs)   =  chcard + 1
               chends(nstrs)   =  chcard + nchars
               chcard          =  chends(nstrs)
               tokens(ntoken)  =  tkid
               values(ntoken)  =  nstrs
               lxbegs(ntoken)  =  cptr
               lxends(ntoken)  =  last

               state           =  nxttok

!
!              We finished scanning an identifier.
!
!              STATE is set to NXTTOK.
!
            end if
!
!           We scanned a keyword or an identifier.
!
!           STATE is set to NXTTOK.
!

         else if ( state == spcial ) then
!
!           Look for a valid token starting with a special character at
!           location CPTR. We attempt to match the longest possible
!           special token.
!
            i  =  min ( spclen,   l - cptr + 1 )
            j  =  0

            do while (  ( i >= 1 ) .and. ( j == 0 ) )

               last =  cptr + i - 1
               j    =  findloc(spcstr, tquery(cptr:last), dim=1)

               if ( j == 0 ) then
                  i  =  i - 1
               end if

            end do

            if ( j > 0 ) then
!
!              We've identified a valid token.
!
               ntoken =  ntoken + 1

               tokens(ntoken)  =  spctok(j)
               values(ntoken)  =  spcval(j)
               lxbegs(ntoken)  =  cptr
               lxends(ntoken)  =  cptr - 1 + rtrim( spcstr(j) )

               state           =  nxttok

            else

               errmsg = 'Invalid character found at location #. '
               call repmi_inplace ( errmsg, '#', cptr )
!
!              If the offending character is printable, include it
!              in the error message.  Otherwise, include the integer
!              code for the character.
!
               if ( frstpc(chr) > 0 ) then

                  call suffix ( '<character> = ''#''',   2, errmsg )
                  call repmc_inplace ( errmsg, '#', chr )

               else

                  call suffix ( 'ICHAR(<character>) = #', 2, errmsg )
                  call repmi_inplace ( errmsg, '#', ichar(chr) )

               end if

               scnerr = .true.
               return

            end if
!
!           STATE is now NXTTOK.
!

         end if

      end do

!
!     If we got this far, we've found the tokens in the query.
!
      scnerr  =  .false.
      errmsg  =  ' '

      return
      end associate
      end subroutine zzekscan

      !> Set an encoded integer at a specified address from a character
      !> data page.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is the handle of an EK file open for write access.
      !>
      !>     ADDRSS         is the DAS character start address at which an
      !>                    integer, encoded as a string, is to be written.
      !>                    An encoded integer occupies ENCSIZ characters,
      !>                    where the parameter ENCSIZ is defined in the
      !>                    include file ekdatpag.inc.
      !>
      !>     IVAL           is an integer value to be written in encoded form.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If the DAS character address range
      !>
      !>            ADDRSS .. ADDRSS+ENCSIZ-1
      !>
      !>         is not a range of DAS character addresses that have been
      !>         initialized, the error wll be diagnosed by routines
      !>         called by this routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine should be used for all EK applications requiring
      !>     storage of encoded integer values as characters.  Use of this
      !>     routine should ensure consistent encoding across the library.
      !>
      !>     Encoded integers written by this routine should be read using
      !>     ZZEKGEI.
      !>
      !>## Examples
      !>
      !>     See ZZEKAD03.
      !>
      !>## Restrictions
      !>
      !>     1)  Portability dictates that the base used for encoding be
      !>         no greater than 128.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
      module subroutine zzeksei ( ctx, handle, addrss, ival )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! EK file handle.
      integer(ip), intent(in) :: addrss  !! DAS character address.
      integer(ip), intent(in) :: ival  !! Integer value to write.
!
!     Local variables
!
      character(encsiz) :: cval

!
!     Discovery error handling should be used in this utility.
!
!     Encode the number.
!
      call prtenc ( ctx, ival, cval )

!
!     Write the encoded value.
!
      call dasudc ( ctx, handle, addrss, addrss+encsiz-1, 1, encsiz, cval )

      return
      end subroutine zzeksei

      !> Determine the type of segment required to support a specified
      !> set of columns.
      !>
      !> The function returns the type of segment that is compatible with
      !> the input column descriptors.
      !>
      !>## Detailed Input
      !>
      !>     NCOLS          is the number of columns in a new segment.
      !>
      !>     CDSCRS         is an array of column descriptors:  the Ith
      !>                    descriptor applies to the Ith column in the
      !>                    segment.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the type of segment that is compatible with
      !>     the input column descriptors.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If NCOLS is non-positive or greater than the maximum allowed
      !>         number MXCLSG, the error SPICE(INVALIDCOUNT) is signaled.
      !>
      !>     2)  If the input column descriptors do not contain compatible
      !>         attributes, the error SPICE(BADATTRIBUTES) will be signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine determines the appropriate segment type to contain
      !>     a specified set of columns.  Currently, there are two segment
      !>     types.  The first type accommodates column classes 1 through 6;
      !>     the second type accommodates column classes 7 through 9.  The
      !>     latter set of column classes are `fixed_count' classes:  a column
      !>     in one of these classes may not have entries added or deleted
      !>     after the column is created.  Fixed and variable count columns
      !>     may not coexist in the same segment.
      !>
      !>## Examples
      !>
      !>     See EKBSEG.
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 06-NOV-1995 (NJB)
      module integer(ip) function zzekstyp ( ctx, ncols, cdscrs )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekglimit
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: ncols  !! Number of columns in the segment.
      integer(ip), intent(in) :: cdscrs ( cdscsz, * )  !! Descriptors of columns.
!
!     Local variables
!
      integer(ip) :: i

      logical :: fixed
      logical :: var

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         zzekstyp = 0
         return
      end if

!
!     FIXED and VAR indicate whether we've seen any fixed or variable
!     column classes so far.
!
      fixed  =  .false.
      var    =  .false.

      do i = 1, ncols

         if (       ( cdscrs(clsidx,i) >= 1 ) &
              .and. ( cdscrs(clsidx,i) <= 6 )  ) then

            var   = .true.

         else if (       ( cdscrs(clsidx,i) >= 7 ) &
                   .and. ( cdscrs(clsidx,i) <= 9 )  ) then

            fixed = .true.

         end if

      end do

      if (  var  .and. ( .not. fixed )  ) then

         zzekstyp = 1

      else if (  fixed  .and. ( .not. var )  ) then

         zzekstyp = 2

      else

         zzekstyp = 0

         call ctx%setmsg ( 'Column set contains a mixture of variable ' // &
                       'and fixed-count columns.  Segments must '   // &
                       'contain all variable or all fixed count '   // &
                       'columns.'                                    )
         call ctx%sigerr ( 'SPICE(BADATTRIBUTES)'                        )
         return

      end if

      return
      end function zzekstyp

      !> Return the size of a specified entry in a class 4 column.
      !>
      !> The function returns the number of elements in the specified
      !> column entry.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is an EK file handle.  The file may be open for
      !>                    reading or writing.
      !>
      !>     SEGDSC         is the segment descriptor of the segment
      !>                    containing the column specified by COLDSC.
      !>
      !>     COLDSC         is the column descriptor of the column containing
      !>                    the entry whose size is requested.  The column
      !>                    must be class 4.
      !>
      !>     RECPTR         is a pointer to the EK record containing the
      !>                    column entry of interest.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of elements in the specified
      !>     column entry.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file,
      !>         the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the column index contained in the input column descriptor
      !>         is out of range, the error SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This utility supports the commonly performed function of
      !>     determining the element count of a column entry.
      !>
      !>## Examples
      !>
      !>     See ZZEKESIZ.
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module integer(ip) function zzeksz04 ( ctx, handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip), intent(in) :: recptr  !! Record pointer.
      integer(ip), intent(in) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: datptr_1(1)  !! one-element copy of datptr (array argument)
      integer(ip) :: value_1(1)  !! one-element copy of the result (array argument)

      integer(ip) :: colidx
      integer(ip) :: datptr
      integer(ip) :: ncols
      integer(ip) :: nrec
      integer(ip) :: ptrloc

!
!     Initialize the function's return value.
!
      zzeksz04  =  0

      nrec    =  segdsc ( nridx  )
      colidx  =  coldsc ( ordidx )

!
!     Make sure the column exists.
!
      ncols  =  segdsc ( ncidx )

      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         call ctx%setmsg ( 'Column index = #; valid range is 1:#.' )
         call ctx%errint ( '#',  colidx                            )
         call ctx%errint ( '#',  nrec                              )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                   )
         return

      end if

!
!     If the column has fixed-size entries, just return the declared
!     size.
!
      if ( coldsc(sizidx) /= ifalse ) then

         zzeksz04  =  coldsc(sizidx)
         return

      end if

!
!     Compute the data pointer location.  Read the data pointer.
!
      ptrloc  =  recptr + dptbas + colidx

      call dasrdi ( ctx, handle, ptrloc, ptrloc, datptr_1 )
      datptr = datptr_1(1)

      if ( datptr < 1 ) then
!
!        The value is null.  Null entries are always considered to have
!        size 1.
!
         zzeksz04  =  1

      else
!
!        DATPTR points to the element count.
!
         call dasrdi ( ctx, handle, datptr, datptr, value_1 )
         zzeksz04 = value_1(1)

      end if

      return
      end function zzeksz04

      !> Return the size of a specified entry in a class 5 column.
      !>
      !> The function returns the number of elements in the specified
      !> column entry.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is an EK file handle.  The file may be open for
      !>                    reading or writing.
      !>
      !>     SEGDSC         is the segment descriptor of the segment
      !>                    containing the column specified by COLDSC.
      !>
      !>     COLDSC         is the column descriptor of the column containing
      !>                    the entry whose size is requested.  The column
      !>                    must be class 5.
      !>
      !>     RECPTR         is a pointer to the EK record containing the
      !>                    column entry of interest.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of elements in the specified
      !>     column entry.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file,
      !>         the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the column index contained in the input column descriptor
      !>         is out of range, the error SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This utility supports the commonly performed function of
      !>     determining the element count of a column entry.
      !>
      !>## Examples
      !>
      !>     See ZZEKESIZ.
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module integer(ip) function zzeksz05 ( ctx, handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip), intent(in) :: recptr  !! Record pointer.
      integer(ip), intent(in) :: coldsc ( cdscsz )  !! Column descriptor.
      real(wp) :: dpcnt_1(1)  !! one-element copy of dpcnt (array argument)
      integer(ip) :: datptr_1(1)  !! one-element copy of datptr (array argument)
!
!     Local variables
!
      real(wp) :: dpcnt

      integer(ip) :: colidx
      integer(ip) :: datptr
      integer(ip) :: ncols
      integer(ip) :: nrec
      integer(ip) :: ptrloc

!
!     Initialize the function's return value.
!
      zzeksz05  =  0

      nrec    =  segdsc ( nridx  )
      colidx  =  coldsc ( ordidx )

!
!     Make sure the column exists.
!
      ncols  =  segdsc ( ncidx )

      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         call ctx%setmsg ( 'Column index = #; valid range is 1:#.' )
         call ctx%errint ( '#',  colidx                            )
         call ctx%errint ( '#',  nrec                              )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                   )
         return

      end if

!
!     If the column has fixed-size entries, just return the declared
!     size.
!
      if ( coldsc(sizidx) /= ifalse ) then

         zzeksz05  =  coldsc(sizidx)
         return

      end if

!
!     Compute the data pointer location.  Read the data pointer.
!
      ptrloc  =  recptr + dptbas + colidx

      call dasrdi ( ctx, handle, ptrloc, ptrloc, datptr_1 )
      datptr = datptr_1(1)

      if ( datptr < 1 ) then
!
!        The value is null.  Null entries are always considered to have
!        size 1.
!
         zzeksz05  =  1

      else
!
!        DATPTR points to the element count.
!
         call dasrdd ( ctx, handle, datptr, datptr, dpcnt_1 )
         dpcnt = dpcnt_1(1)

         zzeksz05  =  nint ( dpcnt )

      end if

      return
      end function zzeksz05

      !> Convert time strings from EK query to ephemeris time.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     TIMSTR         is a string representing a time value.  The value
      !>                    make be an SCLK string in the form
      !>
      !>                       <clock name> SCLK <clock string>
      !>
      !>                    or may be any string acceptable to ST2ET.
      !>
      !>## Detailed Output
      !>
      !>     ET             is the ephemeris time equivalent to the input
      !>                    time.
      !>
      !>     ERROR          is a logical flag indicating whether an error was
      !>                    detected.  Note that a time string might be
      !>                    syntactically valid, but incapable of being
      !>                    converted to ET if the appropriate time kernels
      !>                    (Leapseconds or SCLK) are not loaded.
      !>
      !>     ERRMSG         is an error message describing an error in the
      !>                    input query, if one was detected.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If any sort of time conversion error occurs, the output flag
      !>         ERROR is set, and an error message is returned.
      !>
      !>         The routine attempts to avoid causing errors that must
      !>         be trapped by SPICELIB error handling.  Time string syntax
      !>         errors or missing kernel files, for example, should not trip
      !>         SPICELIB error handling.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Strings representing time values are interpreted as follows:
      !>
      !>        1)  The string is first examined to see whether it's an
      !>            SCLK string for a recognized clock; if it is, the
      !>            string is converted to the equivalent ET.
      !>
      !>        2)  If the string is not a SCLK string, it is expected
      !>            to be some sort of standard time representation.
      !>            The string is checked to see whether it's in a format
      !>            that TPARTV can handle.  If TPARTV can't deal with it,
      !>            the string is considered to be invalid.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRES.
      !>
      !>## Restrictions
      !>
      !>     1) A leapseconds kernel must be loaded at the time this routine
      !>        is called.
      !>
      !>     2) In order to convert SCLK strings, an appropriate SCLK kernel
      !>        must be loaded at the time this routine is called.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.0, 12-AUG-2001 (NJB)
      !>
      !>        Now converts standard time strings to ET via STR2ET instead
      !>        of the less general routines ISO2UTC and UTC2ET.
      !>
      !>        Bug fix:  modified algorithm to handle case where string
      !>        "SCLK" appears in SCLK name.
      !>
      !>        Bug fix:  construction of error messages in case where
      !>        FAILED() returns .TRUE. has been changed so that REPMC is
      !>        not called.  Instead, the error-free routine SUFFIX is
      !>        used.
      !>
      !>-    SPICELIB Version 1.0.0, 11-OCT-1995 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.0.0, 12-AUG-2001 (NJB)
      !>
      !>        Now converts standard time strings to ET via STR2ET instead
      !>        of the less general routines ISO2UTC and UTC2ET.
      !>
      !>        Bug fix:  modified algorithm to handle case where string
      !>        "SCLK" appears in SCLK name.
      !>
      !>        Bug fix:  construction of error messages in case where
      !>        FAILED() returns .TRUE. has been changed so that REPMC is
      !>        not called.  Instead, the error-free routine SUFFIX is
      !>        used.
      module subroutine zzektcnv ( ctx, timstr, et, error, errmsg )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: cmprss, posr, rtrim, suffix, repmc_inplace, ucase_inplace
      use pepper_inc_ektype
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: timstr  !! Time string.
      real(wp), intent(out) :: et  !! Ephemeris time in seconds past J2000, TDB.
      logical, intent(out) :: error  !! Error flag.
      character(*), intent(out) :: errmsg  !! Error message.

!
!     Local parameters
!
      integer(ip), parameter :: msglen = 160

      integer(ip), parameter :: ncomp = 10

      integer(ip), parameter :: maxmod = 10

      integer(ip), parameter :: short = 32

      character(lnsize) :: locstr
      character(short) :: modify ( maxmod )
      character(lnsize) :: pictur
      character(msglen) :: sclmsg
      character(short) :: type

      real(wp) :: sclkdp
      real(wp) :: tvec   ( ncomp )

      integer(ip) :: clkid
      integer(ip) :: loc
      integer(ip) :: ntvec

      logical :: fnd
      logical :: mods
      logical :: succes
      logical :: yabbrv

      if ( ctx%failed() ) return

!
!     No error to start with.
!
      error  =  .false.
      errmsg =  ' '

!
!     Get a left-justified, compressed, upper-case copy of
!     the string, so we can easily search it for substrings
!     that would identify it as SCLK.  If we do find a
!     match, remove the identifying substring (of the form
!     'MO SCLK', 'VGR1 SCLK', etc.).
!
      call cmprss ( ' ',   1,  timstr,  locstr )
      locstr = adjustl(locstr)
      call ucase_inplace ( locstr )

      loc  =  posr ( locstr, 'SCLK', rtrim(locstr) )

      if ( loc > 0 ) then
!
!        It's a SCLK string.  Find the ID code, if we can.
!
         call scn2id ( ctx, locstr(:loc+3), clkid, fnd )

         if ( .not. fnd ) then
!
!           We don't recognize this SCLK type.
!
            error  = .true.

            if ( loc > 1 ) then

               errmsg = 'Time conversion failed; SCLK type '// &
                        '<#> was not recognized.'

               call repmc_inplace ( errmsg, '#', timstr(:loc-1) )

            else

               errmsg = 'Time conversion failed; SCLK name was not ' // &
                        'supplied.'
            end if

            return

         end if

!
!        If we got this far, we recognized the SCLK type.
!        Convert the time to ET.
!
         call scpars ( ctx, clkid, locstr(loc+4:), error, sclmsg, sclkdp )

         if ( ctx%failed() ) then
!
!           We'll arrive here if the required SCLK kernel hasn't
!           been loaded.
!
            error  =  .true.
            errmsg =  'Unexpected SPICELIB error encountered '  // &
                      'while attempting to parse the string <'
            call suffix ( timstr, 0, errmsg )
            call suffix ( '>',    0, errmsg )
            return

         else if ( error ) then

            errmsg =  'The string <#> didn''t parse '                 // &
                      'as a spacecraft clock string.'
            call repmc_inplace ( errmsg, '#', timstr )
            call suffix ( sclmsg,  3,          errmsg )
            return

         else

            call sct2e ( ctx, clkid, sclkdp, et )

            if ( ctx%failed() ) then

               error  =  .true.
               errmsg =  'Unexpected SPICELIB error encountered '  // &
                         'while attempting to parse the string <'
               call suffix ( timstr, 0, errmsg )
               call suffix ( '>',    0, errmsg )
               return

            end if

         end if

      else
!
!        We could have a standard time string.  Make sure that the
!        time string is acceptable before actually calling STR2ET.
!
         call tpartv ( ctx, locstr, &
                       tvec,   ntvec, type, &
                       modify, mods,  yabbrv, succes, &
                       pictur, errmsg                 )

         if ( succes ) then
!
!           It's safe to pass the string to STR2ET.
!
            call str2et ( ctx, locstr, et )

            if ( ctx%failed() ) then

               error  =  .true.
               errmsg =  'Unexpected SPICELIB error encountered '  // &
                         'while attempting to parse the string <'
               call suffix ( timstr, 0, errmsg )
               call suffix ( '>',    0, errmsg )
               return

            end if

         else
!
!           The string cannot be parsed by STR2ET.  The error message
!           was set by TPARTV.
!
            error  =  .true.
            return

         end if
!
!        We're done with the standard time string case.
!
      end if
!
!     We've parsed a time string, if it was an SCLK or standard string.
!
      return
      end subroutine zzektcnv

      !> Locate the first occurrence of a specified token in a tokenized
      !> EK query.  The input may actually be any subset of token codes
      !> and corresponding keyword codes from a tokenized query.
      !>
      !>## Detailed Input
      !>
      !>     TOKID          is a token code identifying the type of token
      !>                    sought.
      !>
      !>     KWCODE         is a code that specifies the desired keyword,
      !>                    if the desired token is a keyword.  KWCODE is
      !>                    ignored if the desired token is not a keyword.
      !>
      !>     NTOKEN         is the number of tokens in the input query.
      !>
      !>     TOKENS         is an array of token codes.  This array normally
      !>                    represents a tokenized EK query or a sublist of
      !>                    such a query.
      !>
      !>     VALUES         is a list of values associated with the codes
      !>                    contained in TOKENS.  When the Ith element of
      !>                    TOKENS indicates that the Ith token is a keyword,
      !>                    the Ith element of VALUES contains the code
      !>                    specifying which keyword is meant.
      !>
      !>## Detailed Output
      !>
      !>     LOC            is the index in the input token list at which
      !>                    the desired token was first encountered.  LOC
      !>                    is meaningful only if FOUND is .TRUE.
      !>
      !>     FOUND          is a logical flag indicating whether the desired
      !>                    token was found.
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
      !>     This is a utility that simplifies parsing of tokenized EK
      !>     queries.
      !>
      !>## Examples
      !>
      !>     See ZZEKPARS.
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
      !>-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comment.
      !>
      !>-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
      module subroutine zzektloc ( ctx, tokid, kwcode, ntoken, tokens, values, loc, found )
      use pepper_kinds, only: ip
      use pepper_inc_ektokn
      use pepper_inc_ekkeyw
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: tokid  !! Token ID.
      integer(ip), intent(in) :: kwcode  !! Keyword code.
      integer(ip), intent(in) :: ntoken  !! Number of tokens in query.
      integer(ip), intent(in) :: tokens ( * )  !! Token codes.
      integer(ip), intent(in) :: values ( * )  !! Pointers to numeric and string token values.
      integer(ip), intent(out) :: loc  !! Location of first occurrence of token.
      logical, intent(out) :: found  !! Flag indicating whether token was found.
!
!     Error free.
!
      found  = .false.
      loc    =  1

      do while ( loc <= ntoken )

         if ( tokens(loc) == tokid ) then

            if ( tokid == tkkey ) then
!
!              To get a match, the keyword codes must match.
!
               if ( values(loc) == kwcode ) then
                  found  =  .true.
                  return
               end if

            else
!
!              For non-keyword tokens, we're done at this point.
!
               found  =  .true.
               return

            end if

         end if

         loc = loc + 1

      end do

      return
      end subroutine zzektloc

      !> Write scalar integer value to encoded EK query.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     NAME           is the name of the item whose value is to be set.
      !>                    This item is some element of the integer portion
      !>                    of an encoded query.  The currently supported set
      !>                    of names is:
      !>
      !>                       ARCHITECTURE
      !>                       INITIALIZED
      !>                       PARSED
      !>                       NAMES_RESOLVED
      !>                       TIMES_RESOLVED
      !>                       SEM_CHECKED
      !>                       NUM_TABLES
      !>                       NUM_CONJUNCTIONS
      !>                       NUM_CONSTRAINTS
      !>                       NUM_SELECT_COLS
      !>                       NUM_ORDERBY_COLS
      !>                       NUM_BUF_SIZE
      !>                       FREE_NUM
      !>                       CHR_BUF_SIZE
      !>                       FREE_CHR
      !>
      !>     VALUE          is an integer value to assign to the quantity
      !>                    designated by NAME.
      !>
      !>     EQRYI          is the integer portion of an encoded EK query.
      !>
      !>## Detailed Output
      !>
      !>     EQRYI          is the integer portion of an encoded EK query,
      !>                    updated to reflect the requested assignment.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input name is not recognized, the error
      !>         SPICE(INVALIDNAME) is signaled.  The encoded query is not
      !>         modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is intended to hide from calling routines the
      !>     specifics of the EK encoded query structure.  See the include
      !>     file ekquery.inc if details of this structure are desired.
      !>
      !>## Examples
      !>
      !>     See ZZEKNRES.
      !>
      !>## Restrictions
      !>
      !>     1) Uses EK encoded query architecture version 2.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 01-AUG-1995 (NJB)
      module subroutine zzekweqi ( ctx, name, value, eqryi )
      use pepper_kinds, only: ip
      use pepper_strings, only: ucase_inplace
      use pepper_inc_ekquery
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of scalar item to write.
      integer(ip), intent(in) :: value  !! Value to write.
      integer(ip), intent(inout) :: eqryi ( lbcell : * )  !! Integer component of query.
!
!     Initial values
!
      character(namlen), parameter :: namlst ( maxnam ) = [ character(namlen) :: &
         'ARCHITECTURE', 'INITIALIZED', 'PARSED', 'NAMES_RESOLVED',&
         'TIMES_RESOLVED', 'SEM_CHECKED', 'NUM_TABLES', 'NUM_CONJUNCTIONS',&
         'NUM_CONSTRAINTS', 'NUM_SELECT_COLS', 'NUM_ORDERBY_COLS', 'NUM_BUF_SIZE',&
         'FREE_NUM', 'CHR_BUF_SIZE', 'FREE_CHR' ]
      integer(ip), parameter :: namidx ( maxnam ) = [ &
         eqarch, eqinit, eqpars, eqnres,&
         eqtres, eqschk, eqntab, eqncnj,&
         eqncns, eqnsel, eqnord, eqnsiz,&
         eqnptr, eqcsiz, eqcptr ]
!
!     Local variables
!
      character(namlen) :: tmpnam
      integer(ip) :: i
      if ( .not. allocated ( ctx%zzekweqi_state ) ) allocate ( ctx%zzekweqi_state )
!
!     Find the location of the named item.
!
      tmpnam = adjustl(name)
      call ucase_inplace ( tmpnam )

      i  =  findloc(namlst, tmpnam, dim=1)

      if ( i == 0 ) then

         call ctx%setmsg ( 'Item # not found.'  )
         call ctx%errch  ( '#',  name           )
         call ctx%sigerr ( 'SPICE(INVALIDNAME)' )
         return

      end if

!
!     Do the deed.
!
      eqryi( namidx(i) ) = value

      return
      end subroutine zzekweqi

      !> Open an existing E-kernel file for reading.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of an existing E-kernel file to be
      !>              opened for read access.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the EK file handle of the file designated by
      !>              FNAME. This handle is used to identify the file
      !>              to other EK routines.
      !>
      !>## Parameters
      !>
      !>     FTSIZE   is the maximum number of DAS files that a user can
      !>              have open simultaneously. This includes any files used
      !>              by the DAS system.
      !>
      !>              See the include file das.inc for the actual value of
      !>              this parameter.
      !>
      !>## Exceptions
      !>
      !>     1)  If the indicated file cannot be opened, an error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>     2)  If the indicated file has the wrong architecture version, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     3)  If an I/O error occurs while reading the indicated file, the
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading ek.req for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine should be used to open an EK file for read access.
      !>     EKs opened for read access may not be modified.
      !>
      !>     Opening an EK file with this routine makes the EK accessible to
      !>     the SPICELIB EK readers
      !>
      !>        EKRCEC
      !>        EKRCED
      !>        EKRCEI
      !>
      !>     all of which expect an EK file handle as an input argument. These
      !>     readers allow a caller to read individual EK column entries.
      !>
      !>     To make an EK available to the EK query system, the file must be
      !>     loaded via EKLEF, rather than by this routine. See the EK
      !>     Required Reading for further information.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Open an EK for read access and find the number of segments in
      !>        it.
      !>
      !>        Use the EK kernel below as test input file for loading the
      !>        experiment database. This kernel contains the Deep
      !>        Impact spacecraft sequence data based on the integrated
      !>        Predicted Events File covering the whole primary mission,
      !>        split into two segments.
      !>
      !>           dif_seq_050112_050729.bes
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EKOPR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               EKNSEG
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER               HANDLE
      !>      INTEGER               NSEG
      !>
      !>C
      !>C     Open the EK file, returning the file handle
      !>C     associated with the open file to the variable named
      !>C     HANDLE.
      !>C
      !>      CALL EKOPR ( 'dif_seq_050112_050729.bes', HANDLE )
      !>
      !>C
      !>C     Return the number of segments in the EK.
      !>C
      !>      NSEG = EKNSEG( HANDLE )
      !>      WRITE(*,'(A,I3)') 'Number of segments =', NSEG
      !>
      !>C
      !>C     Close the file.
      !>C
      !>      CALL EKCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Number of segments =  2
      !>
      !>## Restrictions
      !>
      !>     1)  No more than FTSIZE DAS files may be opened simultaneously.
      !>         See the include file das.inc for the value of FTSIZE.
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example and updated $Parameters section.
      !>
      !>        Corrected $Exceptions #1: this routine does not delete the
      !>        input file if the file cannot be opened.
      !>
      !>-    SPICELIB Version 1.0.0, 26-AUG-1995 (NJB)
      module subroutine ekopr ( ctx, fname, handle )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of EK file.
      integer(ip), intent(out) :: handle  !! Handle attached to EK file.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Open the file as a DAS file.
!
      call dasopr ( ctx, fname, handle )

      if ( ctx%failed() ) return

!
!     Nothing doing unless the architecture is correct.  This file
!     should be a paged DAS EK.
!
      call zzekpgch ( ctx, handle, 'READ' )

      return
      end subroutine ekopr

      !> Open an existing E-kernel file for writing.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of an existing E-kernel file to be
      !>              opened for write access.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the DAS file handle of the EK designate by
      !>              FNAME. This handle is used to identify the file
      !>              to other EK routines.
      !>
      !>## Parameters
      !>
      !>     FTSIZE   is the maximum number of DAS files that a user can
      !>              have open simultaneously. This includes any files used
      !>              by the DAS system.
      !>
      !>              See the include file das.inc for the actual value of
      !>              this parameter.
      !>
      !>## Exceptions
      !>
      !>     1)  If the indicated file cannot be opened, an error is signaled
      !>         by a routine in the call tree of this routine.
      !>
      !>     2)  If the indicated file has the wrong architecture version, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine.
      !>
      !>     3)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading ek.req for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine should be used to open an EK existing file for write
      !>     access.
      !>
      !>     Opening an EK file with this routine makes the EK accessible to
      !>     the following SPICELIB EK access routines, all of which modify
      !>     the target EK file:
      !>
      !>        Begin segment:
      !>
      !>           EKBSEG
      !>
      !>        Append, insert, delete records:
      !>
      !>           EKAPPR
      !>           EKINSR
      !>           EKDELR
      !>
      !>        Add column entries:
      !>
      !>           EKACEC
      !>           EKACED
      !>           EKACEI
      !>
      !>        Update existing column entries:
      !>
      !>           EKUCEC
      !>           EKUCED
      !>           EKUCEI
      !>
      !>        Execute fast write:
      !>
      !>           EKIFLD
      !>           EKFFLD
      !>           EKACLC
      !>           EKACLD
      !>           EKACLI
      !>
      !>     An EK opened for write access is also accessible for reading.
      !>     The file may be accessed by the SPICELIB EK readers
      !>
      !>           EKRCEC
      !>           EKRCED
      !>           EKRCEI
      !>
      !>        and summary routines:
      !>
      !>           EKNSEG
      !>           EKSSUM
      !>
      !>     An EK opened for write access cannot be queried. To make an EK
      !>     available to the EK query system, the file must be loaded via
      !>     FURNSH or EKLEF, rather than by this routine. See the EK Required
      !>     Reading for further information.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) The following program demonstrates how to create a new EK and
      !>        add data to a character column in a given record within the
      !>        file, how to re-open the file for write access and update the
      !>        data, and how to read the data from it.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EKOPW_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Include the EK Column Name Size (CNAMSZ)
      !>C
      !>      INCLUDE 'ekcnamsz.inc'
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      CHARACTER*(*)         EKNAME
      !>      PARAMETER           ( EKNAME = 'ekopw_ex1.bdb' )
      !>
      !>      CHARACTER*(*)         IFNAME
      !>      PARAMETER           ( IFNAME = 'Test EK'  )
      !>
      !>      CHARACTER*(*)         TABLE
      !>      PARAMETER           ( TABLE  = 'CHR_DATA' )
      !>
      !>      INTEGER               CVLEN
      !>      PARAMETER           ( CVLEN  = 9  )
      !>
      !>      INTEGER               DECLEN
      !>      PARAMETER           ( DECLEN = 200 )
      !>
      !>      INTEGER               MAXVAL
      !>      PARAMETER           ( MAXVAL = 4  )
      !>
      !>      INTEGER               NCOLS
      !>      PARAMETER           ( NCOLS  = 2  )
      !>
      !>      INTEGER               NROWS
      !>      PARAMETER           ( NROWS  = 6  )
      !>
      !>      INTEGER               NRESVC
      !>      PARAMETER           ( NRESVC = 0  )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(DECLEN)    CDECLS ( NCOLS  )
      !>      CHARACTER*(CNAMSZ)    CNAMES ( NCOLS  )
      !>      CHARACTER*(CVLEN)     CVALS  ( MAXVAL )
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               J
      !>      INTEGER               NVALS
      !>      INTEGER               RECNO
      !>      INTEGER               SEGNO
      !>
      !>      LOGICAL               ISNULL
      !>
      !>C
      !>C     Open a new EK file.  For simplicity, we won't
      !>C     reserve space for the comment area, so the
      !>C     number of reserved comment characters is zero.
      !>C     The constant IFNAME is the internal file name.
      !>C
      !>      CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
      !>
      !>C
      !>C     Set up the table and column names and declarations
      !>C     for the CHR_DATA segment.  We'll index all of
      !>C     the columns.
      !>C
      !>      CNAMES(1) =  'CHR_COL_1'
      !>      CDECLS(1) =  'DATATYPE = CHARACTER*(*), '
      !>     .       //    'INDEXED = TRUE, NULLS_OK = TRUE'
      !>
      !>      CNAMES(2) =  'CHR_COL_2'
      !>      CDECLS(2) =  'DATATYPE = CHARACTER*(9), '
      !>     .       //    'SIZE = VARIABLE, NULLS_OK = TRUE'
      !>
      !>C
      !>C     Start the segment.
      !>C
      !>      CALL EKBSEG ( HANDLE, TABLE,  NCOLS,
      !>     .              CNAMES, CDECLS, SEGNO )
      !>
      !>      DO I = 0, NROWS-1
      !>
      !>         CALL EKAPPR ( HANDLE, SEGNO, RECNO )
      !>
      !>         ISNULL = ( I .EQ. 1 )
      !>
      !>         CALL INTSTR ( I, CVALS(1) )
      !>         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(1),
      !>     .                 1,      CVALS, ISNULL           )
      !>
      !>C
      !>C        Array-valued columns follow.
      !>C
      !>         CALL INTSTR ( 10*I,     CVALS(1) )
      !>         CALL INTSTR ( 10*I + 1, CVALS(2) )
      !>         CALL INTSTR ( 10*I + 2, CVALS(3) )
      !>         CALL INTSTR ( 10*I + 3, CVALS(4) )
      !>         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(2),
      !>     .                 4,      CVALS, ISNULL           )
      !>
      !>      END DO
      !>
      !>C
      !>C     End the file.
      !>C
      !>      CALL EKCLS ( HANDLE )
      !>
      !>C
      !>C     Open the EK for write access.
      !>C
      !>      CALL EKOPW ( EKNAME, HANDLE )
      !>
      !>C
      !>C     Negate the values in the even-numbered records
      !>C     using the update routines.
      !>C
      !>      DO I = 1, NROWS, 2
      !>
      !>         RECNO  = I+1
      !>
      !>         ISNULL = ( I .EQ. 1 )
      !>
      !>         CALL INTSTR ( -I, CVALS(1) )
      !>         CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(1),
      !>     .                 1,      CVALS, ISNULL           )
      !>
      !>C
      !>C        Array-valued columns follow.
      !>C
      !>         CALL INTSTR ( -10*I,       CVALS(1) )
      !>         CALL INTSTR ( -(10*I + 1), CVALS(2) )
      !>         CALL INTSTR ( -(10*I + 2), CVALS(3) )
      !>         CALL INTSTR ( -(10*I + 3), CVALS(4) )
      !>         CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(2),
      !>     .                 4,      CVALS, ISNULL           )
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the file.
      !>C
      !>      CALL EKCLS ( HANDLE )
      !>
      !>C
      !>C     Open the created file. Show the values added.
      !>C
      !>      CALL EKOPR ( EKNAME, HANDLE )
      !>
      !>      DO I = 1, NROWS
      !>
      !>         CALL EKRCEC ( HANDLE, SEGNO, I,     CNAMES(1),
      !>     .                 NVALS,  CVALS, ISNULL           )
      !>
      !>         IF ( .NOT. ISNULL ) THEN
      !>
      !>            WRITE(*,*) 'Data from column: ', CNAMES(1)
      !>            WRITE(*,*) '   record number: ', I
      !>            WRITE(*,*) '   values       : ',
      !>     .                                ( CVALS(J), J=1,NVALS )
      !>            WRITE(*,*) ' '
      !>
      !>         ELSE
      !>
      !>            WRITE(*,*) 'Record ', I, 'flag is NULL.'
      !>            WRITE(*,*) ' '
      !>
      !>         END IF
      !>
      !>C
      !>C        Array-valued columns follow.
      !>C
      !>         CALL EKRCEC ( HANDLE, SEGNO, I,     CNAMES(2),
      !>     .                 NVALS,  CVALS, ISNULL           )
      !>
      !>         IF ( .NOT. ISNULL ) THEN
      !>
      !>            WRITE(*,*) 'Data from column: ', CNAMES(2)
      !>            WRITE(*,*) '   record number: ', I
      !>            WRITE(*,*) '   values       : ',
      !>     .                                ( CVALS(J), J=1,NVALS )
      !>            WRITE(*,*) ' '
      !>
      !>         END IF
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the file.
      !>C
      !>      CALL EKCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Data from column: CHR_COL_1
      !>            record number:            1
      !>            values       : 0
      !>
      !>         Data from column: CHR_COL_2
      !>            record number:            1
      !>            values       : 0        1        2        3
      !>
      !>         Record            2 flag is NULL.
      !>
      !>         Data from column: CHR_COL_1
      !>            record number:            3
      !>            values       : 2
      !>
      !>         Data from column: CHR_COL_2
      !>            record number:            3
      !>            values       : 20       21       22       23
      !>
      !>         Data from column: CHR_COL_1
      !>            record number:            4
      !>            values       : -3
      !>
      !>         Data from column: CHR_COL_2
      !>            record number:            4
      !>            values       : -30      -31      -32      -33
      !>
      !>         Data from column: CHR_COL_1
      !>            record number:            5
      !>            values       : 4
      !>
      !>         Data from column: CHR_COL_2
      !>            record number:            5
      !>            values       : 40       41       42       43
      !>
      !>         Data from column: CHR_COL_1
      !>            record number:            6
      !>            values       : -5
      !>
      !>         Data from column: CHR_COL_2
      !>            record number:            6
      !>            values       : -50      -51      -52      -53
      !>
      !>        Note that the second record does not appear due to setting the
      !>        ISNULL flag to .TRUE. for that record. The odd value record
      !>        numbers have negative values as a result of the update calls.
      !>
      !>        After run completion, a new EK exists in the output directory.
      !>
      !>## Restrictions
      !>
      !>     1)  No more than FTSIZE DAS files may be opened simultaneously.
      !>         See the include file das.inc for the value of FTSIZE.
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added complete
      !>        code example.
      !>
      !>        Updated exception #1 to remove the statement about deleting the
      !>        opened file upon failure.
      !>
      !>        Updated "fast write" list of API, which was listing the wrong
      !>        APIs for adding data.
      !>
      !>        Added FTSIZE parameter description. Updated index entry.
      !>
      !>-    SPICELIB Version 1.0.1, 09-JAN-2002 (NJB)
      !>
      !>        Documentation change: instances of the phrase "fast load"
      !>        were replaced with "fast write."
      !>
      !>-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
      module subroutine ekopw ( ctx, fname, handle )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of EK file.
      integer(ip), intent(out) :: handle  !! Handle attached to EK file.

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Open the file as a DAS file.
!
      call dasopw ( ctx, fname, handle )

      if ( ctx%failed() ) return

!
!     Nothing doing unless the architecture is correct.  This file
!     should be a paged DAS EK.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      return
      end subroutine ekopw

      !> Get the companion pointer of a specified EK record.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for read or write
      !>                    access.
      !>
      !>     RECPTRC        is a pointer to the record whose companion pointer
      !>                    is desired.
      !>
      !>## Detailed Output
      !>
      !>     PTR            is a pointer to the companion record of the
      !>                    specified input record.  If the input record
      !>                    belongs to a shadowed EK, the companion record
      !>                    is the corresponding backup record.  If the input
      !>                    record is a backup record, the companion record
      !>                    is the corresponding source record.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine sets the companion record pointer of a specified EK
      !>     record.
      !>
      !>## Examples
      !>
      !>     See EKROLL.
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
      !>-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
      module subroutine zzekgrcp ( ctx, handle, recptr, ptr )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      use pepper_inc_ekrecptr
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: recptr  !! Record pointer.
      integer(ip), intent(out) :: ptr  !! Pointer to companion of specified EK record.
      integer(ip) :: ptr_1(1)  !! one-element copy of ptr (array argument)
!
!     Local variables
!
      integer(ip) :: loc

!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Compute the companion pointer and set the pointer.
!
      loc  =  recptr + rcpidx

      call dasrdi ( ctx, handle, loc, loc, ptr_1 )
      ptr = ptr_1(1)

      return
      end subroutine zzekgrcp

      !> Insert a character value into a specified encoded EK query, and
      !> obtain a descriptor for the stored value.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     VALUE          is a character value to be inserted into an
      !>                    encoded query.
      !>
      !>     LENGTH         indicates the length of the input character value.
      !>                    If LENGTH exceeds LEN(VALUE), the stored value
      !>                    is padded with trailing blanks.  This allows
      !>                    faithful representation of literal strings.
      !>
      !>     LEXBEG,
      !>     LEXEND         are the begin and end character positions in the
      !>                    original query of the lexeme that generated the
      !>                    input value.  These indices may be used for error
      !>                    correction.
      !>
      !>     EQRYI          is the integer portion of an encoded EK query
      !>
      !>     EQRYC          is the character portion of an encoded EK query.
      !>
      !>## Detailed Output
      !>
      !>     EQRYI          is the integer portion of an encoded EK query,
      !>                    updated to reflect the addition of a value to the
      !>                    encoded query's character buffer.
      !>
      !>     EQRYC          is the character portion of an encoded EK query,
      !>                    with the input value added.
      !>
      !>     DESCR          is a descriptor for the input value.  The
      !>                    descriptor contains EQVDSZ elements.
      !>
      !>## Parameters
      !>
      !>     See the INCLUDE files.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input query is uninitialized, the error
      !>         SPICE(NOTINITIALIZED) will be signaled.
      !>
      !>     2)  If the input character count LENGTH is non-positive, the
      !>         error SPICE(INVALIDCOUNT) is signaled.
      !>
      !>     3)  If there is insufficient space in the encoded query's
      !>         character component, the error SPICE(BUFFERTOOSMALL) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine should always be used to insert character values
      !>     into an encoded query; the insertion should never be done
      !>     directly.
      !>
      !>## Examples
      !>
      !>     See ZZEKNRML.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
      module subroutine zzekinqc ( ctx, value, length, lexbeg, lexend, eqryi, eqryc, descr )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      use pepper_inc_ektype
      use pepper_inc_ekbool
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: value  !! Character value.
      integer(ip), intent(in) :: length  !! Length of item to insert.
      integer(ip), intent(in) :: lexbeg  !! Begin and end positions of value's lexeme.
      integer(ip), intent(in) :: lexend  !! Begin and end positions of value's lexeme.
      integer(ip), intent(inout) :: eqryi  ( lbcell : * )  !! Integer portion of encoded query.
      character(*), intent(inout) :: eqryc  !! Character portion of encoded query.
      integer(ip), intent(out) :: descr  ( * )  !! Descriptor for value.

      integer(ip) :: free
      integer(ip) :: init
      integer(ip) :: l
      integer(ip) :: room
      integer(ip) :: size

      call zzekreqi ( ctx, eqryi, 'INITIALIZED', init )

      if ( init /= itrue ) then

         call ctx%setmsg ( 'Encoded query must be initialized before it ' // &
                       'may be written.'                             )
         call ctx%sigerr ( 'SPICE(NOTINITIALIZED)'                       )
         return

      end if

!
!     Check the input length value.
!
      if ( length < 1 ) then

         call ctx%setmsg ( 'Length of string value was #; must be > 0.'  )
         call ctx%errint ( '#',  length                                  )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                         )
         return

      end if

!
!     Get the character free pointer; make sure there's enough room.
!
      call zzekreqi ( ctx, eqryi, 'FREE_CHR',     free )
      call zzekreqi ( ctx, eqryi, 'CHR_BUF_SIZE', size )

      room  =  size - free + 1

      if ( length > room ) then

         call ctx%setmsg ( 'Out of room in character portion of encoded ' // &
                       'query; only # elements were available; # '    // &
                       'are needed.'                                 )
         call ctx%errint ( '#',  room                                    )
         call ctx%errint ( '#',  length                                  )
         call ctx%sigerr ( 'SPICE(BUFFERTOOSMALL)'                       )
         return

      end if

!
!     Insert the value into the character portion of the encoded query.
!
      l                =  min ( length, len(value) )
      eqryc(free:)     =  value(:l)

!
!     Fill in the descriptor.
!
      descr(1:eqvdsz) = 0

      descr( eqdtyp )  =  chr
      descr( eqblex )  =  lexbeg
      descr( eqelex )  =  lexend
      descr( eqbstr )  =  free
      descr( eqestr )  =  free + length - 1

!
!     Update the character free pointer.
!
      call zzekweqi ( ctx, 'FREE_CHR', free+length, eqryi )

      return
      end subroutine zzekinqc

      !> Insert a numeric value into a specified encoded EK query, and
      !> obtain a descriptor for the stored value.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     VALUE          is a numeric value to be inserted into an
      !>                    encoded query.
      !>
      !>     TYPE           indicates the data type of the numeric value.
      !>                    TYPE may be INT, DP, or TIME.
      !>
      !>     LEXBEG,
      !>     LEXEND         are the begin and end character positions in the
      !>                    original query of the lexeme that generated the
      !>                    input value.  These indices may be used for error
      !>                    correction.
      !>
      !>     EQRYI          is the integer portion of an encoded EK query
      !>
      !>     EQRYD          is the numeric portion of an encoded EK query.
      !>
      !>## Detailed Output
      !>
      !>     EQRYI          is the integer portion of an encoded EK query,
      !>                    updated to reflect the addition of a value to the
      !>                    encoded query's numeric buffer.
      !>
      !>     EQRYD          is the numeric portion of an encoded EK query,
      !>                    with the input numeric value added.
      !>
      !>     DESCR          is a descriptor for the input value.  The
      !>                    descriptor contains EQVDSZ elements.
      !>
      !>## Parameters
      !>
      !>     See the INCLUDE files.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input query is uninitialized, the error
      !>         SPICE(NOTINITIALIZED) will be signaled.
      !>
      !>     2)  If there is insufficient space in the encoded query's
      !>         numeric component, the error SPICE(BUFFERTOOSMALL) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine should always be used to insert numeric values
      !>     into an encoded query; the insertion should never be done
      !>     directly.
      !>
      !>## Examples
      !>
      !>     See ZZEKNRML.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
      module subroutine zzekinqn ( ctx, value, type, lexbeg, lexend, eqryi, eqryd, descr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekquery
      use pepper_inc_ektype
      use pepper_inc_ekbool
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: value  !! Numeric value.
      integer(ip), intent(in) :: type  !! Data type.  May be INT, DP, or TIME.
      integer(ip), intent(in) :: lexbeg  !! Begin and end positions of value's lexeme.
      integer(ip), intent(in) :: lexend  !! Begin and end positions of value's lexeme.
      integer(ip), intent(inout) :: eqryi  ( lbcell : * )  !! Integer portion of encoded query.
      real(wp), intent(inout) :: eqryd  ( * )  !! Numeric portion of encoded query.
      integer(ip), intent(out) :: descr  ( * )  !! Descriptor for value.

      integer(ip) :: free
      integer(ip) :: init
      integer(ip) :: room
      integer(ip) :: size

      call zzekreqi ( ctx, eqryi, 'INITIALIZED', init )

      if ( init /= itrue ) then

         call ctx%setmsg ( 'Encoded query must be initialized before it ' // &
                       'may be written.'                             )
         call ctx%sigerr ( 'SPICE(NOTINITIALIZED)'                       )
         return

      end if

!
!     Get the numeric free pointer; make sure there's enough room.
!
      call zzekreqi ( ctx, eqryi, 'FREE_NUM',     free )
      call zzekreqi ( ctx, eqryi, 'NUM_BUF_SIZE', size )

      room  =  size - free + 1

      if ( room <= 0 ) then

         call ctx%setmsg ( 'Out of room in numeric portion of encoded ' // &
                       'query; only # elements were available.'      )
         call ctx%errint ( '#',  size                                    )
         call ctx%sigerr ( 'SPICE(BUFFERTOOSMALL)'                       )
         return

      end if

!
!     Insert the value into the double precision portion of the encoded
!     query.
!
      eqryd(free)      =  value

!
!     Fill in the descriptor.
!
      descr(1:eqvdsz) = 0

      descr( eqdtyp )  =  type
      descr( eqblex )  =  lexbeg
      descr( eqelex )  =  lexend
      descr( eqvptr )  =  free

!
!     Update the numeric free pointer.
!
      call zzekweqi ( ctx, 'FREE_NUM', free+1, eqryi )

      return
      end subroutine zzekinqn

      !> Compress a join row set by eliminating segment vectors for
      !> which there are no corresponding row vectors.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     JRSBAS         is the base address, in the scratch area, of a
      !>                    join row set to be compressed.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If JRSBAS is not the base address of a structurally valid
      !>         join row set, the results of this routine will be
      !>         unpredictable.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects:  it modifies the join row
      !>     set designated by the input argument JRSBAS.  Every row vector
      !>     marked for deletion is removed.  Every empty segment vector is
      !>     removed, along with the row count and row vector base for that
      !>     segment vector.  The join row set is compressed to remove all
      !>     gaps.   All counts are updated to reflect the updated join row
      !>     set.
      !>
      !>     The purpose of the compression performed by this routine is to
      !>     save work during joins by reducing the size of the cartesian
      !>     products of sets of segment vectors.  Also, special cases
      !>     involving null segment vectors can be avoided by this clean-up
      !>     mechanism.  Finally, it may be possible to save space in the EK
      !>     scratch area freed by the compression.
      !>
      !>## Examples
      !>
      !>     See EKSRCH.
      !>
      !>## Restrictions
      !>
      !>     1) Relies on the EK scratch area.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.1, 02-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    SPICELIB Version 1.1.0, 07-AUG-2006 (NJB)
      !>
      !>        Bug fix:  added initialization of variable NRVDEL to support
      !>                  operation under the Macintosh Intel Fortran
      !>                  compiler. Note that this bug did not affect
      !>                  operation of this routine on other platforms.
      !>
      !>-    SPICELIB Version 1.0.0, 10-OCT-1995 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 07-AUG-2006 (NJB)
      !>
      !>        Bug fix:  added initialization of variable NRVDEL to support
      !>                  operation under the Macintosh Intel Fortran
      !>                  compiler. Note that this bug did not affect
      !>                  operation of this routine on other platforms. The
      !>                  statement referencing the uninitialized variable
      !>                  was:
      !>
      !>           IF (  ( RC .EQ. 0 ) .OR. ( NRVDEL .EQ. RC )  ) THEN
      !>
      !>        In the previous version of the code, NRVDEL is uninitialized
      !>        when NRVDEL is 0.  NRVDEL *is* initialized when RC is
      !>        non-zero, so the logical value of the IF expression is not
      !>        affected by the lack of proper initialization.
      !>
      !>        However, the Intel Fortran compiler for the Mac flags a runtime
      !>        error when the above code is exercised.  So NRVDEL is now
      !>        initialized prior to the above IF statement.
      module subroutine zzekjsqz ( ctx, jrsbas )
      use pepper_kinds, only: ip
      use pepper_inc_ekjrs
      use pepper_inc_ekqlimit
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: jrsbas  !! Scratch area base address of join row set.
      integer(ip) :: setbas_1(1)  !! one-element copy of setbas (array argument)
      integer(ip) :: rc_1(1)  !! one-element copy of rc (array argument)
      integer(ip) :: nsv_1(1)  !! one-element copy of nsv (array argument)
      integer(ip) :: ntab_1(1)  !! one-element copy of ntab (array argument)
      integer(ip) :: size_1(1)  !! one-element copy of size (array argument)
!
!     Local variables
!
      integer(ip) :: cntloc
      integer(ip) :: delta
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: nsvdel
      integer(ip) :: nrvdel
      integer(ip) :: newnsv
      integer(ip) :: nr
      integer(ip) :: nrloc
      integer(ip) :: nsv
      integer(ip) :: nsvloc
      integer(ip) :: ntab
      integer(ip) :: ntloc
      integer(ip) :: pcpair ( 2 )
      integer(ip) :: ptarg
      integer(ip) :: ptbase
      integer(ip) :: ptrloc
      integer(ip) :: rc
      integer(ip) :: rbase
      integer(ip), allocatable :: rowvec ( : )
      integer(ip) :: rtarg
      integer(ip) :: rvsize
      integer(ip), allocatable :: segvec ( : )
      integer(ip) :: setbas
      integer(ip) :: size
      integer(ip) :: sizloc
      integer(ip) :: svbase
      integer(ip) :: svsize
      integer(ip) :: vtarg

!
!     Look up the counts that are of interest:
!
!       -- The table count
!       -- The segment vector count
!       -- The join row set size
!
!     Save the address of each count.
!
      sizloc  =  jrsbas + jszidx
      nsvloc  =  jrsbas + jscidx
      ntloc   =  jrsbas + jtcidx

      call zzeksrd ( ctx, sizloc, sizloc, size_1 )
      size = size_1(1)
      call zzeksrd ( ctx, ntloc, ntloc, ntab_1 )
      ntab = ntab_1(1)
      allocate ( rowvec(ntab+1), segvec(ntab) )
      call zzeksrd ( ctx, nsvloc, nsvloc, nsv_1 )
      nsv = nsv_1(1)

      if ( ctx%failed() ) return

!
!     Set the sizes of segment and row vectors.
!
      svsize  =  ntab
      rvsize  =  ntab + 1

!
!     For each segment vector, obtain the row count.  Clean up after
!     null segment vectors:  compress out the space allocated for their
!     row vector pointers.  Keep track of the number of deletions.
!
      nsvdel  =  0
      nrvdel  =  0
      vtarg   =  jrsbas + jsvbas

      do i = 1, nsv
!
!        The location of the row count is CNTLOC.  The row vector base
!        pointer precedes the row count.
!
         cntloc  =  jrsbas + jsvbas + nsv*svsize + 2*(i-1) + 2
         ptrloc  =  cntloc - 1

         call zzeksrd ( ctx, cntloc, cntloc, rc_1 )
         rc = rc_1(1)

         if ( rc > 0 ) then
!
!           The row vector set for this segment vector is non-empty.
!           scan the rows, looking for those marked for deletion, and
!           update the row count to reflect the number of rows that
!           we're going to keep.
!
            call zzeksrd ( ctx, ptrloc, ptrloc, setbas_1 )
            setbas = setbas_1(1)

            nrvdel = 0

            do j = 1, rc

               rbase  =  jrsbas + setbas + (j-1)*rvsize

               call zzeksrd ( ctx, rbase+1, rbase+1, rowvec(1) )

               if ( rowvec(1) == 0 ) then
                  nrvdel = nrvdel + 1
               end if

            end do

         end if

!
!        Compute the base address of the current segment vector.
!
         svbase  =  jrsbas + jsvbas + (i-1)*svsize

         if (  ( rc == 0 ) .or. ( nrvdel == rc )  ) then
!
!           We're going to delete the current segment vector.  We'll
!           just skip over it without advancing our target pointers.
!
            nsvdel   =  nsvdel + 1

         else if ( nsvdel > 0 ) then
!
!           We need to shift the current segment vector to its
!           destination.
!
            call zzeksrd  ( ctx, svbase+1, svbase+svsize, segvec )
            call zzeksupd ( ctx, vtarg+1,  vtarg+svsize,  segvec )

            vtarg = vtarg + svsize

         else
!
!           No segment vectors have been deleted yet.  We still must
!           update the target in case we shift vectors later on in this
!           loop.
!
            vtarg = vtarg + svsize

         end if

      end do

!
!     At this point, we've compressed out the null segment vectors.
!     The next step is to compress out the row vector counts and row
!     vector pointers that corresponded to those segment vectors.  We
!     also want to remove the gap between the segment vectors and the
!     row vector pointer/count pairs.
!
!     We need to do this only if we deleted some segment vectors.
!
      if ( nsvdel > 0 ) then

         newnsv =  nsv    - nsvdel
         ptarg  =  jrsbas + jsvbas + (newnsv)*svsize

         do i = 1, nsv
!
!           The row count is RC.
!
            svsize  =  ntab
            cntloc  =  jrsbas + jsvbas + nsv*svsize + 2*(i-1) + 2

            call zzeksrd ( ctx, cntloc, cntloc, rc_1 )
            rc = rc_1(1)

            ptbase  =  cntloc - 2

            if ( rc > 0 ) then
!
!              Shift the current row vector pointer and row vector
!              count.
!
               call zzeksrd  ( ctx, ptbase+1, ptbase+2, pcpair )
               call zzeksupd ( ctx, ptarg+1,  ptarg+2,  pcpair )

               ptarg =  ptarg + 2

            end if

         end do

      else

         newnsv = nsv

      end if

!
!     Update the segment vector count.
!
      call zzeksupd ( ctx, nsvloc, nsvloc, [ newnsv ] )

!
!     Remove any gaps that may exist between any of the row vectors,
!     or between the end of the segment vector's row vector counts
!     and base addresses and the first row vector.
!
!     The initial target location is the first element following the
!     last segment vector's row vector count.  RTARG is used as a base
!     address; it precedes this location by 1.
!
!     If we deleted any segment vectors, the segment vector pointers
!     embedded in the row vectors must change.  Make these updates
!     if necessary.
!
      rtarg   =  jrsbas + jsvbas + newnsv*(svsize+2)

      do i = 1, newnsv
!
!        Find the row count and row pointer for the current segment
!        vector.
!
         cntloc  =  jrsbas + jsvbas + newnsv*svsize + 2*(i-1) + 2

         call zzeksrd ( ctx, cntloc, cntloc, rc_1 )
         rc = rc_1(1)

         ptrloc  =  cntloc - 1

!
!        Get the row vector set base pointer.  After capturing the
!        current value, we'll update this pointer to account for
!        the shifting of row vectors.
!
         call zzeksrd ( ctx, ptrloc, ptrloc, setbas_1 )
         setbas = setbas_1(1)

         rbase   =  jrsbas + setbas
         delta   =  rtarg  - rbase

         call zzeksupd ( ctx, ptrloc, ptrloc, [ setbas+delta ] )

!
!        Shift the row vectors for the current segment vector,
!        leaving behind the row vectors marked for deletion.
!
         nrvdel = 0

         do j = 1, rc

            call zzeksrd  ( ctx, rbase+1, rbase+rvsize, rowvec )

            if ( rowvec(1) == 0 ) then
!
!              This row vector is to be deleted; don't copy it.
!
               rbase  = rbase  + rvsize
               nrvdel = nrvdel + 1

            else
!
!              The segment vector pointer is base-relative.
!
               rowvec( rvsize )  =  jsvbas + (i-1)*svsize

               call zzeksupd ( ctx, rtarg+1, rtarg+rvsize, rowvec )

               rbase  =  rbase + rvsize
               rtarg  =  rtarg + rvsize

            end if

         end do

!
!        Update the row count for the current segment vector, if
!        necessary.  Note that no segment vector will become empty
!        as a result of the row vector deletions we've done; we
!        already eliminated any segment vectors for which that
!        could happen, before we entered this loop.
!
         if ( nrvdel > 0 ) then
            call zzeksupd ( ctx, cntloc, cntloc, [ rc - nrvdel ] )
         end if

      end do

!
!     Update the total row count and size of the join row set.
!
      nr  =  0

      do i = 1, newnsv

         cntloc  =  jrsbas + jsvbas + newnsv*svsize + (i-1)*2 + 2

         call zzeksrd ( ctx, cntloc, cntloc, rc_1 )
         rc = rc_1(1)

         nr      =  nr     + rc

      end do

      nrloc  =  jrsbas   +   jrcidx
      size   =  jsvbas   +   newnsv * ( svsize + 2 )  +  nr * rvsize

      call zzeksupd ( ctx, nrloc, nrloc, [ nr ] )
      call zzeksupd ( ctx, sizloc, sizloc, [ size ] )

      return
      end subroutine zzekjsqz

      !> Initialize an open DAS file for paged access.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a handle of a DAS file open for write access.
      !>                    The file must be empty:  the last address of
      !>                    each type (character, d.p. and integer) must be
      !>                    zero.
      !>
      !>## Detailed Output
      !>
      !>     None.  This routine operates by side effects; see $Particulars
      !>     for a description of the effect of this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the DAS file designated by HANDLE is not empty, the error
      !>         SPICE(DASNOTEMPTY) is signaled.
      !>
      !>     2)  Any read or write errors detected during reading or writing
      !>         the DAS file will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine initializes a DAS file for paged access.
      !>     Initialization consists of:
      !>
      !>        - Setting up the metadata area.  This structure is defined in
      !>          the include file ekpage.inc.  For each data type, there is
      !>          a free list pointer and an allocated page count.
      !>
      !>        - Writing the architecture code to the file.  This code is
      !>          defined in the include file ekarch.inc.
      !>
      !>## Examples
      !>
      !>     See EKOPN.
      !>
      !>## Restrictions
      !>
      !>     1) Only `empty' DAS files may be initialized for paged access.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN to insert DAS file name into
      !>        long error messages.
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgin ( ctx, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! DAS file handle.
      integer(ip) :: lastd
      integer(ip) :: lasti
      integer(ip) :: lastc
      if ( .not. allocated ( ctx%zzekpage_state ) ) allocate ( ctx%zzekpage_state )
      associate ( cfill => ctx%zzekpage_state%cfill, &
                  dfill => ctx%zzekpage_state%dfill, &
                  ifill => ctx%zzekpage_state%ifill )
!
!     The file must be open for write access.
!
      call dassih ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Find out which addresses are already in use.  A file containing
!     data cannot be initialized.
!
      call daslla ( ctx, handle, lastc, lastd, lasti )

      if (      ( lastc > 0 ) &
           .or. ( lastd > 0 ) &
           .or. ( lasti > 0 ) ) then

         call ctx%setmsg ( 'File # contains data; LASTC = #; LASTD = #; '// &
                       'LASTI = #.'                                  )
         call ctx%errhan ( '#',  handle                                  )
         call ctx%errint ( '#',  lastc                                   )
         call ctx%errint ( '#',  lastd                                   )
         call ctx%errint ( '#',  lasti                                   )
         call ctx%sigerr ( 'SPICE(DASNOTEMPTY)'                          )
         return

      end if

!
!     Initialize our fill buffers.
!
      cfill = ' '
      dfill = 0.0_wp
      ifill = 0

!
!     Initialize enough integer addresses to hold the metadata area.
!
      call dasadi ( ctx, handle, pgbasi, ifill )

!
!     Set the architecture code.
!
      call dasudi ( ctx, handle, eparch, eparch, [ archid ] )

!
!     Set the page sizes and base addresses.
!
      call dasudi ( ctx, handle, eppszc, eppszc, [ pgsizc ] )
      call dasudi ( ctx, handle, eppszd, eppszd, [ pgsizd ] )
      call dasudi ( ctx, handle, eppszi, eppszi, [ pgsizi ] )

      call dasudi ( ctx, handle, epbasc, epbasc, [ pgbasc ] )
      call dasudi ( ctx, handle, epbasd, epbasd, [ pgbasd ] )
      call dasudi ( ctx, handle, epbasi, epbasi, [ pgbasi ] )

!
!     Since the integer fill value is zero, and since zero is
!     interpreted as null pointer, all pointers are initialized.
!
      return
      end associate
      end subroutine zzekpgin

      !> Allocate a new page of a specified data type.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a handle of a paged EK file.  The file must
      !>                    be open for write access.
      !>
      !>     TYPE           is the data type of the page to allocate.  The
      !>                    type may be CHR, DP, or INT.  Values of these
      !>                    parameters are defined in ektype.inc.
      !>
      !>## Detailed Output
      !>
      !>     P              is the number of an allocated page.  The returned
      !>                    page is never taken from the free list; it is
      !>                    the lowest-addressed page of the specified type
      !>                    that has never been allocated.
      !>
      !>     BASE           is the base DAS address of the page.  This address
      !>                    is the predecessor of the first DAS word of the
      !>                    page.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the DAS file designated by HANDLE is not open for paged
      !>         write access, the error will be diagnosed by routines called
      !>         by this routine.
      !>
      !>     2)  Any read or write errors detected during reading or writing
      !>         the DAS file will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the requested data type is not recognized, the error
      !>         SPICE(INVALIDTYPE) is signaled.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     The pages returned by this routine lie on DAS record boundaries.
      !>     Successive requests for pages of the same data type will return
      !>     pages that are adjacent in the DAS address space of that type.
      !>     In fact, the main reason to call this routine rather than
      !>     ZZEKPGAL is to allocate adjacent pages.
      !>
      !>     Use ZZEKPGAL for normal allocation.
      !>
      !>## Examples
      !>
      !>     See EKAPS.
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
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgan ( ctx, handle, type, p, base )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Paged EK file handle.
      integer(ip), intent(in) :: type  !! Data type of page to allocate.
      integer(ip), intent(out) :: p  !! Page number.
      integer(ip), intent(out) :: base  !! DAS base address of page.
      integer(ip) :: npi_1(1)  !! one-element copy of npi (array argument)
      integer(ip) :: npd_1(1)  !! one-element copy of npd (array argument)
      integer(ip) :: npc_1(1)  !! one-element copy of npc (array argument)
      integer(ip) :: npd
      integer(ip) :: npc
      integer(ip) :: npi
      if ( .not. allocated ( ctx%zzekpage_state ) ) allocate ( ctx%zzekpage_state )
      associate ( cfill => ctx%zzekpage_state%cfill, &
                  dfill => ctx%zzekpage_state%dfill, &
                  ifill => ctx%zzekpage_state%ifill )
!
!     Validate the file.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

      if ( type == chr ) then
!
!        The new page follows the last character address.
!
         call dasadc ( ctx, handle, pgsizc, 1, pgsizc, cfill )

!
!        Update the character page count.
!
         call dasrdi ( ctx, handle, epnpc, epnpc, npc_1 )
         npc = npc_1(1)
         call dasudi ( ctx, handle, epnpc, epnpc, [ npc+1 ] )

!
!        Set the page number and base address.
!
         p     =  npc + 1
         base  =  npc * pgsizc

      else if ( type == dp ) then

         call dasadd ( ctx, handle, pgsizd, dfill )

         call dasrdi ( ctx, handle, epnpd, epnpd, npd_1 )
         npd = npd_1(1)
         call dasudi ( ctx, handle, epnpd, epnpd, [ npd+1 ] )

         p     =  npd + 1
         base  =  npd * pgsizd

      else if ( type == int ) then

         call dasadi ( ctx, handle, pgsizi, ifill )

         call dasrdi ( ctx, handle, epnpi, epnpi, npi_1 )
         npi = npi_1(1)
         call dasudi ( ctx, handle, epnpi, epnpi, [ npi+1 ] )

         p     =  npi + 1
         base  =  npi * pgsizi  +  pgbasi

      else

         call ctx%setmsg ( 'The data type code # was not recognized.' )
         call ctx%errint ( '#', type                                  )
         call ctx%sigerr ( 'SPICE(INVALIDTYPE)'                       )
         return

      end if

      return
      end associate
      end subroutine zzekpgan

      !> Allocate a page of a specified data type.  The page need not
      !> be new:  free pages are returned if possible.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a handle of a paged EK file.  The file must
      !>                    be open for write access.
      !>
      !>     TYPE           is the data type of the page to allocate.  The
      !>                    type may be CHR, DP, or INT.  Values of these
      !>                    parameters are defined in ektype.inc.
      !>
      !>## Detailed Output
      !>
      !>     P              is the number of an allocated page.  The returned
      !>                    page is taken from the free list if the free list
      !>                    is non-empty; otherwise, a new page is returned.
      !>
      !>     BASE           is the base DAS address of the page.  This address
      !>                    is the predecessor of the first DAS word of the
      !>                    page.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the DAS file designated by HANDLE is not open for paged
      !>         write access, the error will be diagnosed by routines called
      !>         by this routine.
      !>
      !>     2)  Any read or write errors detected during reading or writing
      !>         the DAS file will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the requested data type is not recognized, the error
      !>         SPICE(INVALIDTYPE) is signaled.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine should be used for page allocation, except for
      !>     applications requiring allocation of contiguous pages.  If
      !>     contiguous pages are required, use ZZEKPGAN.
      !>
      !>## Examples
      !>
      !>     See EKAPS.
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
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgal ( ctx, handle, type, p, base )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Paged EK file handle.
      integer(ip), intent(in) :: type  !! Data type of page to allocate.
      integer(ip), intent(out) :: p  !! Page number.
      integer(ip), intent(out) :: base  !! DAS base address of page.
      integer(ip) :: npi_1(1)  !! one-element copy of npi (array argument)
      integer(ip) :: nfreei_1(1)  !! one-element copy of nfreei (array argument)
      integer(ip) :: freei_1(1)  !! one-element copy of freei (array argument)
      integer(ip) :: npd_1(1)  !! one-element copy of npd (array argument)
      integer(ip) :: nfreed_1(1)  !! one-element copy of nfreed (array argument)
      real(wp) :: dpptr_1(1)  !! one-element copy of dpptr (array argument)
      integer(ip) :: freed_1(1)  !! one-element copy of freed (array argument)
      integer(ip) :: npc_1(1)  !! one-element copy of npc (array argument)
      integer(ip) :: nfreec_1(1)  !! one-element copy of nfreec (array argument)
      integer(ip) :: freec_1(1)  !! one-element copy of freec (array argument)
      integer(ip) :: npd
      integer(ip) :: freed
      integer(ip) :: freec
      integer(ip) :: addr
      integer(ip) :: freei
      integer(ip) :: nfreei
      real(wp) :: dpptr
      integer(ip) :: npc
      integer(ip) :: npi
      character(5) :: encpag
      integer(ip) :: nfreed
      integer(ip) :: forwrd
      integer(ip) :: nfreec
      if ( .not. allocated ( ctx%zzekpage_state ) ) allocate ( ctx%zzekpage_state )
      associate ( cfill => ctx%zzekpage_state%cfill, &
                  dfill => ctx%zzekpage_state%dfill, &
                  ifill => ctx%zzekpage_state%ifill )
!
!     Validate the file.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

      if ( type == chr ) then
!
!        If the character free list is non-empty, take a page from
!        that list.
!
         call dasrdi ( ctx, handle, epfpc, epfpc, freec_1 )
         freec = freec_1(1)

         if ( freec > 0 ) then
!
!           We'll return the first free page.
!
            p       =     freec
!
!           The new head of the list is the successor of FREEC, if
!           any.  Obtain the forward pointer from the page.
!
            addr  =   ( freec - 1 ) * pgsizc  + 1

            call dasrdc ( ctx, handle,  addr,  addr+4,  1,  5,  encpag )
            call prtdec ( ctx, encpag,  forwrd  )

            freec =   forwrd

!
!           Decrement the free page count, and write the free pointer
!           back to the file.
!
            call dasrdi ( ctx, handle, epnfpc, epnfpc, nfreec_1 )
            nfreec = nfreec_1(1)
            call dasudi ( ctx, handle, epnfpc, epnfpc, [ nfreec-1 ] )
            call dasudi ( ctx, handle, epfpc, epfpc, [ freec ] )

!
!           Set base address.
!
            base  =  (p-1) * pgsizc

         else
!
!           The new page follows the last character address.
!
            call dasadc ( ctx, handle, pgsizc, 1, pgsizc, cfill )

!
!           Update the character page count.
!
            call dasrdi ( ctx, handle, epnpc, epnpc, npc_1 )
            npc = npc_1(1)
            call dasudi ( ctx, handle, epnpc, epnpc, [ npc+1 ] )

!
!           Set the page number and base address.
!
            p     =  npc + 1
            base  =  npc * pgsizc

         end if

      else if ( type == dp ) then
!
!        If the d.p. free list is non-empty, take a page from
!        that list.
!
         call dasrdi ( ctx, handle, epfpd, epfpd, freed_1 )
         freed = freed_1(1)

         if ( freed > 0 ) then
!
!           We'll return the first free page.
!
            p       =     freed
!
!           The new head of the list is the successor of FREED, if
!           any.  Obtain the forward pointer from the page.
!
            addr  =   ( freed - 1 ) * pgsizd   +  1

            call dasrdd ( ctx, handle, addr, addr, dpptr_1 )
            dpptr = dpptr_1(1)
            freed =   nint(dpptr)

!
!           Decrement the free page count, and write the free pointer
!           back to the file.
!
            call dasrdi ( ctx, handle, epnfpd, epnfpd, nfreed_1 )
            nfreed = nfreed_1(1)
            call dasudi ( ctx, handle, epnfpd, epnfpd, [ nfreed-1 ] )
            call dasudi ( ctx, handle, epfpd, epfpd, [ freed ] )

!
!           Set base address.
!
            base  =  (p-1) * pgsizd

         else
!
!           The new page follows the last d.p. address.
!
            call dasadd ( ctx, handle, pgsizd, dfill )

!
!           Update the d.p. page count.
!
            call dasrdi ( ctx, handle, epnpd, epnpd, npd_1 )
            npd = npd_1(1)
            call dasudi ( ctx, handle, epnpd, epnpd, [ npd+1 ] )

!
!           Set the page number and base address.
!
            p     =  npd + 1
            base  =  npd * pgsizd

         end if

      else if ( type == int ) then
!
!        If the integer free list is non-empty, take a page from
!        that list.
!
         call dasrdi ( ctx, handle, epfpi, epfpi, freei_1 )
         freei = freei_1(1)

         if ( freei > 0 ) then
!
!           We'll return the first free page.
!
            p     =     freei
!
!           The new head of the list is the successor of FREEI, if
!           any.  Obtain the forward pointer from the page.
!
            addr  =   ( freei - 1 ) * pgsizi   +   pgbasi  +  1

            call dasrdi ( ctx, handle, addr, addr, freei_1 )
            freei = freei_1(1)

!
!           Decrement the free page count, and write the free pointer
!           back to the file.
!
            call dasrdi ( ctx, handle, epnfpi, epnfpi, nfreei_1 )
            nfreei = nfreei_1(1)
            call dasudi ( ctx, handle, epnfpi, epnfpi, [ nfreei-1 ] )
            call dasudi ( ctx, handle, epfpi, epfpi, [ freei ] )

!
!           Set base address.
!
            base  =  (p-1) * pgsizi  +  pgbasi

         else

            call dasadi ( ctx, handle, pgsizi, ifill )

            call dasrdi ( ctx, handle, epnpi, epnpi, npi_1 )
            npi = npi_1(1)
            call dasudi ( ctx, handle, epnpi, epnpi, [ npi+1 ] )

            p     =  npi + 1
            base  =  npi * pgsizi  +  pgbasi

         end if

      else

         call ctx%setmsg ( 'The data type code # was not recognized.' )
         call ctx%errint ( '#', type                                  )
         call ctx%sigerr ( 'SPICE(INVALIDTYPE)'                       )
         return

      end if

      return
      end associate
      end subroutine zzekpgal

      !> Free a specified page.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a handle of a paged EK file.  The file must
      !>                    be open for write access.
      !>
      !>     TYPE           is the data type of the page to allocate.  The
      !>                    type may be CHR, DP, or INT.  Values of these
      !>                    parameters are defined in ektype.inc.
      !>
      !>     P              is the number of an allocated page to be freed.
      !>
      !>## Detailed Output
      !>
      !>    None.  See $Particulars for a description of the effect of this
      !>    routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the DAS file designated by HANDLE is not open for paged
      !>         write access, the error will be diagnosed by routines called
      !>         by this routine.
      !>
      !>     2)  Any read or write errors detected during reading or writing
      !>         the DAS file will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the requested data type is not recognized, the error
      !>         SPICE(INVALIDTYPE) is signaled.
      !>
      !>     4)  If the number of the page to be freed is not that of an
      !>         allocated page of the specified type, the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine should be used for page deallocation.  The input
      !>     page is placed at the head of the free list of the specified
      !>     data type.
      !>
      !>## Examples
      !>
      !>     See EKDPS.
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
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgfr ( ctx, handle, type, p )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Paged EK file handle.
      integer(ip), intent(in) :: type  !! Data type of page to allocate.
      integer(ip), intent(in) :: p  !! Page number.
      integer(ip) :: nfreei_1(1)  !! one-element copy of nfreei (array argument)
      integer(ip) :: freei_1(1)  !! one-element copy of freei (array argument)
      integer(ip) :: npi_1(1)  !! one-element copy of npi (array argument)
      integer(ip) :: nfreed_1(1)  !! one-element copy of nfreed (array argument)
      integer(ip) :: freed_1(1)  !! one-element copy of freed (array argument)
      integer(ip) :: npd_1(1)  !! one-element copy of npd (array argument)
      integer(ip) :: nfreec_1(1)  !! one-element copy of nfreec (array argument)
      integer(ip) :: freec_1(1)  !! one-element copy of freec (array argument)
      integer(ip) :: npc_1(1)  !! one-element copy of npc (array argument)
      integer(ip) :: npd
      integer(ip) :: freed
      integer(ip) :: freec
      integer(ip) :: addr
      integer(ip) :: freei
      integer(ip) :: nfreei
      integer(ip) :: npc
      integer(ip) :: npi
      character(5) :: encpag
      integer(ip) :: nfreed
      integer(ip) :: nfreec
      if ( .not. allocated ( ctx%zzekpage_state ) ) allocate ( ctx%zzekpage_state )
!
!     Check the file.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

      if ( type == chr ) then
!
!        Validate the page number.  Find out how many pages are
!        out there.
!
         call dasrdi ( ctx, handle, epnpc, epnpc, npc_1 )
         npc = npc_1(1)

         if (  ( p < 1 ) .or. ( p > npc )  ) then

            call ctx%setmsg ( 'Attempt to free non-existent CHR page. ' // &
                          'Page number = #; valid range is 1:#'     )
            call ctx%errint ( '#',  p                                   )
            call ctx%errint ( '#',  npc                                 )
            call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                     )
            return

         end if

!
!        Get the current character free pointer and free page count.
!
         call dasrdi ( ctx, handle, epfpc, epfpc, freec_1 )
         freec = freec_1(1)
         call dasrdi ( ctx, handle, epnfpc, epnfpc, nfreec_1 )
         nfreec = nfreec_1(1)

!
!        Insert into the freed page a pointer to the head of the
!        free list.
!
         call prtenc ( ctx, freec, encpag )
         addr  =  (p-1)*pgsizc + 1

         call dasudc ( ctx, handle, addr, addr+4, 1, 5, encpag )

!
!        Update the current character free pointer and free page count.
!
         call dasudi ( ctx, handle, epfpc, epfpc, [ p ] )
         call dasudi ( ctx, handle, epnfpc, epnfpc, [ nfreec+1 ] )

      else if ( type == dp ) then
!
!        Validate the page number.  Find out how many pages are
!        out there.
!
         call dasrdi ( ctx, handle, epnpd, epnpd, npd_1 )
         npd = npd_1(1)

         if (  ( p < 1 ) .or. ( p > npd )  ) then

            call ctx%setmsg ( 'Attempt to free non-existent DP page. ' // &
                          'Page number = #; valid range is 1:#'     )
            call ctx%errint ( '#',  p                                   )
            call ctx%errint ( '#',  npd                                 )
            call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                     )
            return

         end if

!
!        Get the current d.p. free pointer and free page count.
!
         call dasrdi ( ctx, handle, epfpd, epfpd, freed_1 )
         freed = freed_1(1)
         call dasrdi ( ctx, handle, epnfpd, epnfpd, nfreed_1 )
         nfreed = nfreed_1(1)

!
!        Insert into the freed page a pointer to the head of the
!        free list.
!
         addr  =  (p-1)*pgsizd + 1

         call dasudd ( ctx, handle, addr, addr, [ real(freed, wp) ] )

!
!        Update the current d.p. free pointer and free page count.
!
         call dasudi ( ctx, handle, epfpd, epfpd, [ p ] )
         call dasudi ( ctx, handle, epnfpd, epnfpd, [ nfreed+1 ] )

      else if ( type == int ) then
!
!        Validate the page number.  Find out how many pages are
!        out there.
!
         call dasrdi ( ctx, handle, epnpi, epnpi, npi_1 )
         npi = npi_1(1)

         if (  ( p < 1 ) .or. ( p > npi )  ) then

            call ctx%setmsg ( 'Attempt to free non-existent INT page. ' // &
                          'Page number = #; valid range is 1:#'     )
            call ctx%errint ( '#',  p                                   )
            call ctx%errint ( '#',  npi                                 )
            call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                     )
            return

         end if

!
!        Get the current integer free pointer and free page count.
!
         call dasrdi ( ctx, handle, epfpi, epfpi, freei_1 )
         freei = freei_1(1)
         call dasrdi ( ctx, handle, epnfpi, epnfpi, nfreei_1 )
         nfreei = nfreei_1(1)

!
!        Insert into the freed page a pointer to the head of the
!        free list.
!
         addr  =  (p-1)*pgsizi  +  pgbasi  +  1

         call dasudi ( ctx, handle, addr, addr, [ freei ] )

!
!        Update the current integer free pointer and free page count.
!
         call dasudi ( ctx, handle, epfpi, epfpi, [ p ] )
         call dasudi ( ctx, handle, epnfpi, epnfpi, [ nfreei+1 ] )

      else

         call ctx%setmsg ( 'The data type code # was not recognized.' )
         call ctx%errint ( '#', type                                  )
         call ctx%sigerr ( 'SPICE(INVALIDTYPE)'                       )
         return

      end if

      return
      end subroutine zzekpgfr

      !> Read a specified character page.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a handle of a paged EK file.  The file may
      !>                    be open for read or write access.
      !>
      !>     P              is the number of a character page to read.
      !>
      !>## Detailed Output
      !>
      !>     PAGEC          is a string containing the contents of the
      !>                    specified page.  PAGEC should be declared with
      !>                    length at PGSIZC characters.  This parameter is
      !>                    declared in the include file ekpage.inc.
      !>
      !>                    If PAGEC has length less than PGSIZC characters,
      !>                    the output will be truncated on the right.  If
      !>                    PAGEC is longer than PGSIZC characters, the output
      !>                    will be padded with trailing blanks.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  Any errors detected during reading the DAS file will be
      !>         diagnosed by routines called by this routine.
      !>
      !>     2)  If the number of the page to read is not that of an
      !>         allocated character page, the error SPICE(INVALIDINDEX) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine should be used to read character pages.
      !>
      !>## Examples
      !>
      !>     See ZZEKRD03.
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
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgrc ( ctx, handle, p, pagec )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Paged EK file handle.
      integer(ip), intent(in) :: p  !! Page number.
      character(*), intent(out) :: pagec  !! Character page.
      integer(ip) :: npc_1(1)  !! one-element copy of npc (array argument)
      integer(ip) :: addr
      integer(ip) :: e
      integer(ip) :: npc
      integer(ip) :: l
      if ( .not. allocated ( ctx%zzekpage_state ) ) allocate ( ctx%zzekpage_state )
!
!     Use discovery check-in.
!
!     Find out how many character pages are in use.
!
      call dasrdi ( ctx, handle, epnpc, epnpc, npc_1 )
      npc = npc_1(1)

      if (  ( p < 1 )  .or.  ( p > npc )  ) then

         call ctx%setmsg ( 'CHR page = #; valid range is [1:#]' )
         call ctx%errint ( '#', p                               )
         call ctx%errint ( '#', npc                             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      l      =   len( pagec )
      e      =   min( l, pgsizc )

      addr   =   (p-1)*pgsizc + 1

      call dasrdc ( ctx, handle, addr, addr+pgsizc-1, 1, e, pagec )

      if ( l > e ) then
         pagec ( e+1: ) = ' '
      end if

      return
      end subroutine zzekpgrc

      !> Read a specified double precision page.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a handle of a paged EK file.  The file may
      !>                    be open for read or write access.
      !>
      !>     P              is the number of a double precision page to read.
      !>
      !>## Detailed Output
      !>
      !>     PAGED          is a double precision array containing the contents
      !>                    of the specified page.  PAGED should be declared
      !>                    with dimension PGSIZD.  This parameter is
      !>                    declared in the include file ekpage.inc.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  Any errors detected during reading the DAS file will be
      !>         diagnosed by routines called by this routine.
      !>
      !>     2)  If the number of the page to read is not that of an
      !>         allocated double precision page, the error SPICE(INVALIDINDEX)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine should be used to read double precision pages.
      !>
      !>## Examples
      !>
      !>     See ZZEKRD02.
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
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgrd ( ctx, handle, p, paged )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Paged EK file handle.
      integer(ip), intent(in) :: p  !! Page number.
      real(wp), intent(out) :: paged ( * )  !! Double precision page.
      integer(ip) :: npd_1(1)  !! one-element copy of npd (array argument)
      integer(ip) :: npd
      integer(ip) :: addr
      if ( .not. allocated ( ctx%zzekpage_state ) ) allocate ( ctx%zzekpage_state )
!
!     Use discovery check-in.
!
!     Find out how many d.p. pages are in use.
!
      call dasrdi ( ctx, handle, epnpd, epnpd, npd_1 )
      npd = npd_1(1)

      if (  ( p < 1 )  .or.  ( p > npd )  ) then

         call ctx%setmsg ( 'DP page = #; valid range is [1:#]'  )
         call ctx%errint ( '#', p                               )
         call ctx%errint ( '#', npd                             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      addr   =   (p-1)*pgsizd + 1

      call dasrdd ( ctx, handle, addr, addr+pgsizd-1, paged )

      return
      end subroutine zzekpgrd

      !> Read a specified integer page.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a handle of a paged EK file.  The file may
      !>                    be open for read or write access.
      !>
      !>     P              is the number of an integer page to read.
      !>
      !>## Detailed Output
      !>
      !>     PAGEI          is an integer array containing the contents
      !>                    of the specified page.  PAGEI should be declared
      !>                    with dimension PGSIZI.  This parameter is
      !>                    declared in the include file ekpage.inc.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  Any errors detected during reading the DAS file will be
      !>         diagnosed by routines called by this routine.
      !>
      !>     2)  If the number of the page to read is not that of an
      !>         allocated double precision page, the error SPICE(INVALIDINDEX)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine should be used to read integer pages.
      !>
      !>## Examples
      !>
      !>     See ZZEKRD01.
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
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgri ( ctx, handle, p, pagei )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Paged EK file handle.
      integer(ip), intent(in) :: p  !! Page number.
      integer(ip), intent(out) :: pagei ( * )  !! Integer page.
      integer(ip) :: npi_1(1)  !! one-element copy of npi (array argument)
      integer(ip) :: addr
      integer(ip) :: npi
      if ( .not. allocated ( ctx%zzekpage_state ) ) allocate ( ctx%zzekpage_state )
!
!     Use discovery check-in.
!
!     Find out how many integer pages are in use.
!
      call dasrdi ( ctx, handle, epnpi, epnpi, npi_1 )
      npi = npi_1(1)

      if (  ( p < 1 )  .or.  ( p > npi )  ) then

         call ctx%setmsg ( 'INT page = #; valid range is [1:#]' )
         call ctx%errint ( '#', p                               )
         call ctx%errint ( '#', npi                             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      addr   =   pgbasi  +  (p-1)*pgsizi + 1

      call dasrdi ( ctx, handle, addr, addr+pgsizi-1, pagei )

      return
      end subroutine zzekpgri

      !> Write a specified character page.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a handle of a paged EK file.  The file must
      !>                    be open for write access.
      !>
      !>     P              is the number of an allocated character page to
      !>                    write.
      !>
      !>     PAGEC          is a string to be written to the specified page.
      !>                    PAGEC must be declared with length at PGSIZC
      !>                    characters.  This parameter is declared in the
      !>                    include file ekpage.inc.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  Any errors detected during reading or writing the DAS file
      !>         will be diagnosed by routines called by this routine.
      !>
      !>     2)  If the number of the page to write is not that of an
      !>         allocated character page, the error SPICE(INVALIDINDEX) is
      !>         signaled.
      !>
      !>     3)  If the input string has length less than PGSIZC characters,
      !>         the error SPICE(STRINGTOOSHORT) is signaled.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine writes the input string to the DAS address range
      !>     corresponding to the specified page.  The file must be closed
      !>     properly (via EKCLS) in order to make the change permanent.
      !>
      !>## Examples
      !>
      !>     See ZZEKAD03.
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
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgwc ( ctx, handle, p, pagec )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Paged EK file handle.
      integer(ip), intent(in) :: p  !! Page number.
      character(*), intent(in) :: pagec  !! Character page.
      integer(ip) :: npc_1(1)  !! one-element copy of npc (array argument)
      integer(ip) :: addr
      integer(ip) :: npc
      integer(ip) :: l
      if ( .not. allocated ( ctx%zzekpage_state ) ) allocate ( ctx%zzekpage_state )
!
!     Use discovery check-in.
!
!     Validate the file.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Find out how many character pages are in use.
!
      call dasrdi ( ctx, handle, epnpc, epnpc, npc_1 )
      npc = npc_1(1)

      if (  ( p < 1 )  .or.  ( p > npc )  ) then

         call ctx%setmsg ( 'CHR page = #; valid range is [1:#]' )
         call ctx%errint ( '#', p                               )
         call ctx%errint ( '#', npc                             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      l  =  len( pagec )

      if ( l < pgsizc ) then

         call ctx%setmsg ( 'Input CHR page size = #; valid size is [#:]' )
         call ctx%errint ( '#', l                                        )
         call ctx%errint ( '#', pgsizc                                   )
         call ctx%sigerr ( 'SPICE(STRINGTOOSHORT)'                       )
         return

      end if

      addr   =   (p-1)*pgsizc + 1

      call dasudc ( ctx, handle, addr, addr+pgsizc-1, 1, pgsizc, pagec )

      return
      end subroutine zzekpgwc

      !> Write a specified double precision page.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a handle of a paged EK file.  The file must
      !>                    be open for write access.
      !>
      !>     P              is the number of an allocated double precision
      !>                    page to write.
      !>
      !>     PAGED          is a double precision array to be written to
      !>                    the specified page.  PAGED must be declared with
      !>                    dimension at PGSIZD.  This parameter is
      !>                    declared in the include file ekpage.inc.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  Any errors detected during reading or writing the DAS file
      !>         will be diagnosed by routines called by this routine.
      !>
      !>     2)  If the number of the page to write is not that of an
      !>         allocated d.p. page, the error SPICE(INVALIDINDEX) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine writes the input array to the DAS address range
      !>     corresponding to the specified page.  The file must be closed
      !>     properly (via EKCLS) in order to make the change permanent.
      !>
      !>## Examples
      !>
      !>     See ZZEKAD02.
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
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgwd ( ctx, handle, p, paged )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Paged EK file handle.
      integer(ip), intent(in) :: p  !! Page number.
      real(wp), intent(in) :: paged ( * )  !! Double precision page.
      integer(ip) :: npd_1(1)  !! one-element copy of npd (array argument)
      integer(ip) :: npd
      integer(ip) :: addr
      if ( .not. allocated ( ctx%zzekpage_state ) ) allocate ( ctx%zzekpage_state )
!
!     Use discovery check-in.
!
!     Validate the file.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Find out how many d.p. pages are in use.
!
      call dasrdi ( ctx, handle, epnpd, epnpd, npd_1 )
      npd = npd_1(1)

      if (  ( p < 1 )  .or.  ( p > npd )  ) then

         call ctx%setmsg ( 'DP page = #; valid range is [1:#]'  )
         call ctx%errint ( '#', p                               )
         call ctx%errint ( '#', npd                             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      addr   =  (p-1)*pgsizd + 1

      call dasudd ( ctx, handle, addr, addr+pgsizd-1, paged )

      return
      end subroutine zzekpgwd

      !> Write a specified integer page.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a handle of a paged EK file.  The file must
      !>                    be open for write access.
      !>
      !>     P              is the number of an allocated integer
      !>                    page to write.
      !>
      !>     PAGEI          is an integer array to be written to
      !>                    the specified page.  PAGEI must be declared with
      !>                    dimension at PGSIZI.  This parameter is
      !>                    declared in the include file ekpage.inc.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  Any errors detected during reading or writing the DAS file
      !>         will be diagnosed by routines called by this routine.
      !>
      !>     2)  If the number of the page to write is not that of an
      !>         allocated integer page, the error SPICE(INVALIDINDEX) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine writes the input array to the DAS address range
      !>     corresponding to the specified page.  The file must be closed
      !>     properly (via EKCLS) in order to make the change permanent.
      !>
      !>## Examples
      !>
      !>     See ZZEKAD01.
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
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgwi ( ctx, handle, p, pagei )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Paged EK file handle.
      integer(ip), intent(in) :: p  !! Page number.
      integer(ip), intent(in) :: pagei ( * )  !! Integer page.
      integer(ip) :: npi_1(1)  !! one-element copy of npi (array argument)
      integer(ip) :: addr
      integer(ip) :: npi
      if ( .not. allocated ( ctx%zzekpage_state ) ) allocate ( ctx%zzekpage_state )
!
!     Use discovery check-in.
!
!     Validate the file.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Find out how many integer pages are in use.
!
      call dasrdi ( ctx, handle, epnpi, epnpi, npi_1 )
      npi = npi_1(1)

      if (  ( p < 1 )  .or.  ( p > npi )  ) then

         call ctx%setmsg ( 'INT page = #; valid range is [1:#]' )
         call ctx%errint ( '#', p                               )
         call ctx%errint ( '#', npi                             )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                )
         return

      end if

      addr   =   pgbasi + (p-1)*pgsizi + 1

      call dasudi ( ctx, handle, addr, addr+pgsizi-1, pagei )

      return
      end subroutine zzekpgwi

      !> Map a page to its base address.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     TYPE           is the data type of the page whose base address
      !>                    is requested.  The type may be CHR, DP, or INT.
      !>                    Values of these parameters are defined in
      !>                    ektype.inc.
      !>
      !>     P              is the number of the page of interest.
      !>
      !>## Detailed Output
      !>
      !>     BASE           is the base DAS address of the page.  This address
      !>                    is the predecessor of the first DAS word of the
      !>                    page.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the requested data type is not recognized, the error
      !>         SPICE(INVALIDTYPE) is signaled.
      !>
      !>     2)  Range checking is not performed on the input page number P.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine provides translation from page numbers to DAS
      !>     addresses.
      !>
      !>## Examples
      !>
      !>     See EKDELR.
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
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgbs ( ctx, type, p, base )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: type  !! Data type of page.
      integer(ip), intent(in) :: p  !! Page number.
      integer(ip), intent(out) :: base  !! DAS base address of page.
      if ( type == chr ) then

         base  =   ( p - 1 ) * pgsizc

      else if ( type == dp ) then

         base  =   ( p - 1 ) * pgsizd

      else if ( type == int ) then

         base  =   ( p - 1 ) * pgsizi   +   pgbasi

      else

         call ctx%setmsg ( 'The data type code # was not recognized.' )
         call ctx%errint ( '#', type                                  )
         call ctx%sigerr ( 'SPICE(INVALIDTYPE)'                       )
         return

      end if

      return
      end subroutine zzekpgbs

      !> Map a DAS address to the number of the page containing it.  Also
      !> return the base address of the page.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     TYPE           is the data type of a DAS address to be mapped to
      !>                    a page number.  The type may be CHR, DP, or INT.
      !>                    Values of these parameters are defined in
      !>                    ektype.inc.
      !>
      !>     ADDRSS         is a DAS address to be mapped to a page number.
      !>
      !>## Detailed Output
      !>
      !>     P              is the number of the page containing the input
      !>                    address.
      !>
      !>     BASE           is the base DAS address of the page.  This address
      !>                    is the predecessor of the first DAS word of the
      !>                    page.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the requested data type is not recognized, the error
      !>         SPICE(INVALIDTYPE) is signaled.
      !>
      !>     2)  Range checking is not performed on the input address.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine provides translation from DAS addresses to page
      !>     numbers.
      !>
      !>## Examples
      !>
      !>     See ZZEKAD01.
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
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 14-OCT-2021 (BVS)
      !>
      !>        Bug fix: fixed routine name in CHKIN/CHKOUT calls (ZZEKPGBS
      !>        -> ZZEKPGPG).
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgpg ( ctx, type, addrss, p, base )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: type  !! Data type of address.
      integer(ip), intent(in) :: addrss  !! DAS address to be mapped.
      integer(ip), intent(out) :: p  !! Page number.
      integer(ip), intent(out) :: base  !! DAS base address of page.
      if ( type == chr ) then

         p     =   ( addrss  + pgsizc - 1 ) / pgsizc
         base  =   ( p - 1 ) * pgsizc

      else if ( type == dp ) then

         p     =   ( addrss  + pgsizd - 1 ) / pgsizd
         base  =   ( p - 1 ) * pgsizd

      else if ( type == int ) then

         p     =   ( ( addrss-pgbasi )  +   pgsizi - 1 ) / pgsizi
         base  =   ( p - 1 ) * pgsizi   +   pgbasi

      else

         call ctx%setmsg ( 'The data type code # was not recognized.' )
         call ctx%errint ( '#', type                                  )
         call ctx%sigerr ( 'SPICE(INVALIDTYPE)'                       )
         return

      end if

      return
      end subroutine zzekpgpg

      !> Return paging statistics.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a handle of a paged EK file.  The file may
      !>                    be open for read or write access.
      !>
      !>     STAT           is the name of the requested statistic.  Possible
      !>                    values and meanings of STAT are:
      !>
      !>                      'N_C_ALLOC'     Number of character pages
      !>                                      allocated.  Pages on the free
      !>                                      list are not included.
      !>
      !>                      'N_D_ALLOC'     Number of d.p. pages allocated.
      !>
      !>                      'N_I_ALLOC'     Number of integer pages
      !>                                      allocated.
      !>
      !>                      'N_C_FREE'      Number of pages in character free
      !>                                      list.
      !>
      !>                      'N_D_FREE'      Number of pages in d.p. free
      !>                                      list.
      !>
      !>                      'N_I_FREE'      Number of pages in integer free
      !>                                      list.
      !>
      !>## Detailed Output
      !>
      !>     VALUE          is the value of the requested statistic.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the requested statistic is not recognized, the error
      !>         SPICE(INVALIDOPTION) is signaled.
      !>
      !>## Files
      !>
      !>     This suite of routines provides paged access to DAS files.  Only
      !>     DAS files initialized via a call to ZZEKPGIN may be written or
      !>     read by these routines.
      !>
      !>## Particulars
      !>
      !>     This routine provides translation from DAS addresses to page
      !>     numbers.
      !>
      !>## Examples
      !>
      !>     1)  Find the number of pages on the integer free list of the
      !>         paged EK designated by HANDLE:
      !>
      !>            CALL ZZEKPGST ( HANDLE, 'N_I_FREE', NFREE )
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
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzekpgst ( ctx, handle, stat, value )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: eqstr
      use pepper_inc_ekarch
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Paged EK file handle.
      character(*), intent(in) :: stat  !! Name of requested statistic.
      integer(ip), intent(out) :: value  !! Value of requested statistic.
      integer(ip) :: value_1(1)  !! one-element copy of value (array argument)
      if (  eqstr( stat, 'N_C_ALLOC' )  ) then

         call dasrdi ( ctx, handle, epnpc, epnpc, value_1 )
         value = value_1(1)

      else if (  eqstr( stat, 'N_D_ALLOC' )  ) then

         call dasrdi ( ctx, handle, epnpd, epnpd, value_1 )
         value = value_1(1)

      else if (  eqstr( stat, 'N_I_ALLOC' )  ) then

         call dasrdi ( ctx, handle, epnpi, epnpi, value_1 )
         value = value_1(1)

      else if (  eqstr( stat, 'N_C_FREE' )  ) then

         call dasrdi ( ctx, handle, epnfpc, epnfpc, value_1 )
         value = value_1(1)

      else if (  eqstr( stat, 'N_D_FREE' )  ) then

         call dasrdi ( ctx, handle, epnfpd, epnfpd, value_1 )
         value = value_1(1)

      else if (  eqstr( stat, 'N_I_FREE' )  ) then

         call dasrdi ( ctx, handle, epnfpi, epnfpi, value_1 )
         value = value_1(1)

      else

         call ctx%setmsg ( 'Statistic # is not supported.' )
         call ctx%errch  ( '#',  stat                      )
         call ctx%sigerr ( 'SPICE(INVALIDOPTION)'          )
         return

      end if

      return
      end subroutine zzekpgst

      !> Read conjunction sizes from an encoded EK query.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     EQRYI          is the integer portion of an encoded EK query.
      !>                    The query must have been parsed.
      !>
      !>     N              is the index, within the FROM clause of the query,
      !>                    of the table whose name is to be fetched.
      !>
      !>## Detailed Output
      !>
      !>     SIZE           is the size of the Nth conjunction of
      !>                    constraints in the input encoded query.  The size
      !>                    applies to the constraints after `normalization'.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input query is not initialized, the error will be
      !>         diagnosed by routines called by this routine.  The outputs
      !>         will not be modified.
      !>
      !>     2)  If the input query has not been parsed, the error
      !>         SPICE(UNPARSEDQUERY) will be signaled.  The outputs
      !>         will not be modified.
      !>
      !>     3)  If the index N is less than 1 or greater than the number of
      !>         conjunctions in the query, the error SPICE(INVALIDINDEX)
      !>         will be signaled.  The outputs will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The call
      !>
      !>        CALL ZZEKREQI (  EQRYI,  'NUM_CONJUNCTIONS',  N  )
      !>
      !>     may be used to get the conjunction count from an encoded query.
      !>
      !>     This routine assumes that encoded EK query architecture version
      !>     1 is to be used with the query to be initialized; this routine
      !>     will not work with any other architecture version.
      !>
      !>## Examples
      !>
      !>     See EKSRCH.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
      module subroutine zzekqcnj ( ctx, eqryi, n, size )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      use pepper_inc_ekbool
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: eqryi ( lbcell : * )  !! Integer component of query.
      integer(ip), intent(in) :: n  !! Index within FROM clause of table name to read.
      integer(ip), intent(out) :: size  !! Size of Nth conjunction in WHERE clause.

      integer(ip) :: iparse
      integer(ip) :: loc
      integer(ip) :: ncnj
      integer(ip) :: ncns
      integer(ip) :: ntab

      call zzekreqi ( ctx, eqryi, 'PARSED', iparse )

      if ( ctx%failed() ) return

      if ( iparse == ifalse ) then

         call ctx%setmsg ( 'Encoded query has not yet been parsed.'  )
         call ctx%sigerr ( 'SPICE(UNPARSEDQUERY)'                    )
         return

      end if

      call zzekreqi ( ctx, eqryi, 'NUM_TABLES',         ntab   )
      call zzekreqi ( ctx, eqryi, 'NUM_CONJUNCTIONS',   ncnj   )
      call zzekreqi ( ctx, eqryi, 'NUM_CONSTRAINTS',    ncns   )

      if (  ( n < 1 ) .or. ( n > ncnj )  ) then

         call ctx%setmsg ( 'Table index # is out of valid range 1:#.'  )
         call ctx%errint ( '#',  n                                     )
         call ctx%errint ( '#',  ncnj                                  )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                       )
         return

      end if

!
!     Compute the location of the requested conjunction size value.
!
      loc    =     eqvbas &
                 + ntab  *  2  *  eqvdsz &
                 + ncns        *  eqcdsz &
                 + n

      size   =  eqryi ( loc )

      return
      end subroutine zzekqcnj

      !> Return elements of a specified constraint from an encoded EK
      !> query.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     EQRYI,
      !>     EQRYC,
      !>     EQRYD          are, respectively, the integer, character, and
      !>                    numeric components of an encoded EK query.
      !>                    The query must have names and values resolved and
      !>                    must have been semantically checked.
      !>
      !>     N              is the index, within the WHERE clause of the query,
      !>                    of the constraint to be fetched.
      !>
      !>## Detailed Output
      !>
      !>     CNSTYP         is the constraint type.  Possible values are
      !>
      !>                       EQCOL  ...  constraint compares two columns
      !>                       EQVAL  ...  constraint compares column and value
      !>
      !>     LTNAME         is the table name for the LHS of the constraint.
      !>                    If an alias was supplied in the query, that
      !>                    alias is returned.  If the column was unqualified,
      !>                    LTNAME is returned blank.
      !>
      !>     LTIDX          is the index of the LHS table in the FROM clause.
      !>
      !>     LCNAME         is the name of the LHS column.
      !>
      !>     LCIDX          is the index of the LHS column in the virtual
      !>                    table containing the column.
      !>
      !>     OPCODE         is the operator code used in the constraint.
      !>
      !>     RTNAME         is the table name for the RHS of the constraint.
      !>                    RTNAME is meaningful only if the constraint
      !>                    compares two columns, as indicated by CNSTYP.
      !>                    If an alias was supplied in the query, that
      !>                    alias is returned.  If the column was unqualified,
      !>                    RTNAME is returned blank.
      !>
      !>     RTIDX          is the index of the RHS table in the FROM clause.
      !>                    RTIDX is meaningful only if the constraint
      !>                    compares two columns, as indicated by CNSTYP.
      !>
      !>     RCNAME         is the name of the RHS column.  RCNAME is
      !>                    meaningful only if the constraint compares two
      !>                    columns, as indicated by CNSTYP.
      !>
      !>     RCIDX          is the index of the RHS column in the virtual
      !>                    table containing the column.  RCIDX is
      !>                    meaningful only if the constraint compares two
      !>                    columns, as indicated by CNSTYP.
      !>
      !>     DTYPE          is the data type of the value on the RHS of the
      !>                    constraint.  DTYPE is meaningful only if the
      !>                    constraint compares a column against a value,
      !>                    as indicated by CNSTYP.
      !>
      !>     CBEG,
      !>     CEND           are, respectively, begin and end character pointers
      !>                    into the EQRYC array; these pointers give the
      !>                    location of a character value on the RHS of a
      !>                    query constraint.  CBEG and CEND are meaningful
      !>                    only if the constraint compares a column against a
      !>                    value, as indicated by CNSTYP, and if the value's
      !>                    data type is CHR, as indicated by DTYPE.
      !>
      !>     IVAL           is an integer value on the RHS of the constraint.
      !>                    IVAL is meaningful only if the constraint compares
      !>                    a column against a value, as indicated by CNSTYP,
      !>                    and if the value's data type is INT, as indicated
      !>                    by DTYPE.
      !>
      !>     DVAL           is a double precision value on the RHS of the
      !>                    constraint.  DVAL is meaningful only if the
      !>                    constraint compares a column against a value, as
      !>                    indicated by CNSTYP, and if the value's data type
      !>                    is DP or TIME, as indicated by DTYPE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input query is not initialized, the error will be
      !>         diagnosed by routines called by this routine.  The outputs
      !>         will not be modified.
      !>
      !>     2)  If the input query has not been semantically checked, the
      !>         error SPICE(NOTSEMCHECKED) will be signaled.  The outputs
      !>         will not be modified.
      !>
      !>     3)  If the index N is less than 1 or greater than the number of
      !>         constraints in the query, the error SPICE(INVALIDINDEX)
      !>         will be signaled.  The outputs
      !>         will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The call
      !>
      !>        CALL ZZEKREQI (  EQRYI,  'NUM_CONSTRAINTS',  N  )
      !>
      !>     may be used to get the constraint count from an encoded query.
      !>
      !>## Examples
      !>
      !>     See EKSRCH.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 29-APR-2009 (NJB)
      !>
      !>        Bug fix: this routine now does not attempt to
      !>        read constraint RHS value parameters from the
      !>        encoded query when the RHS value is NULL, as
      !>        indicated by the opcode.
      !>
      !>-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
      module subroutine zzekqcon ( ctx, eqryi, eqryc, eqryd, n, cnstyp, ltname, ltidx, lcname, lcidx, opcode, rtname, rtidx, rcname, rcidx, dtype, cbeg, cend, dval, ival )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekquery
      use pepper_inc_ekbool
      use pepper_inc_ekopcd
      use pepper_inc_ektype
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*), intent(in) :: eqryc  !! Character component of query.
      real(wp), intent(in) :: eqryd ( * )  !! Numeric component of query.
      integer(ip), intent(in) :: n  !! Index of constraint to read.
      integer(ip), intent(out) :: cnstyp  !! Type of constraint (column or value comparison).
      character(*), intent(out) :: ltname  !! LHS table name.
      integer(ip), intent(out) :: ltidx  !! LHS table index in FROM clause.
      character(*), intent(out) :: lcname  !! LHS column name.
      integer(ip), intent(out) :: lcidx  !! LHS column index in virtual parent table.
      integer(ip), intent(out) :: opcode  !! Operation code.
      character(*), intent(out) :: rtname  !! RHS table name.
      integer(ip), intent(out) :: rtidx  !! RHS table index in FROM clause.
      character(*), intent(out) :: rcname  !! RHS column name.
      integer(ip), intent(out) :: rcidx  !! RHS column index in virtual parent table.
      integer(ip), intent(out) :: dtype  !! Data type of RHS value.
      integer(ip), intent(out) :: cbeg  !! Character begin pointer for RHS value.
      integer(ip), intent(out) :: cend  !! Character end pointer for RHS value.
      real(wp), intent(out) :: dval  !! RHS double precision value.
      integer(ip), intent(out) :: ival  !! RHS integer value.


      integer(ip) :: base
      integer(ip) :: cb
      integer(ip) :: ce
      integer(ip) :: icheck
      integer(ip) :: ncns
      integer(ip) :: ntab
      integer(ip) :: ptr
      integer(ip) :: tb
      integer(ip) :: te

      call zzekreqi ( ctx, eqryi, 'SEM_CHECKED', icheck )

      if ( ctx%failed() ) return

      if ( icheck == ifalse ) then

         call ctx%setmsg ( 'Encoded query has not been semantically '// &
                       'checked.'                                )
         call ctx%sigerr ( 'SPICE(NOTSEMCHECKED)'                    )
         return

      end if

      call zzekreqi ( ctx, eqryi, 'NUM_CONSTRAINTS',   ncns   )
      call zzekreqi ( ctx, eqryi, 'NUM_TABLES',        ntab   )

      if (  ( n < 1 ) .or. ( n > ncns )  ) then

         call ctx%setmsg ( 'Constraint index # is out of valid range 1:#.' )
         call ctx%errint ( '#',  n                                         )
         call ctx%errint ( '#',  ncns                                      )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                           )
         return

      end if

!
!     Compute the base address of the Nth constraint.
!
      base   =      eqvbas &
                  + ntab  * 2 * eqvdsz &
                  + (n-1)     * eqcdsz

!
!     Get the constraint type.
!
      cnstyp  =  eqryi ( base + eqctyp )

!
!     Get the LHS items.
!
      ltidx  =  eqryi ( base + eqltab - 1 + eqtord )
      tb     =  eqryi ( base + eqltab - 1 + eqbstr )
      te     =  eqryi ( base + eqltab - 1 + eqestr )

      if ( tb /= 0 ) then
         ltname =  eqryc ( tb:te )
      else
         ltname = ' '
      end if

      lcidx  =  eqryi ( base + eqlcol - 1 + eqcidx )
      cb     =  eqryi ( base + eqlcol - 1 + eqbstr )
      ce     =  eqryi ( base + eqlcol - 1 + eqestr )
      lcname =  eqryc ( cb:ce )

!
!     Next, the opcode.
!
      opcode =  eqryi ( base + eqopcd )

!
!     If the constraint compares two columns, get the RHS table and
!     column info.
!
      if ( cnstyp == eqcol ) then

         rtidx  =  eqryi ( base + eqrtab - 1 + eqtord )
         tb     =  eqryi ( base + eqrtab - 1 + eqbstr )
         te     =  eqryi ( base + eqrtab - 1 + eqestr )

         if ( tb /= 0 ) then
            rtname =  eqryc ( tb:te )
         else
            rtname = ' '
         end if

         rcidx  =  eqryi ( base + eqrcol - 1 + eqcidx )
         cb     =  eqryi ( base + eqrcol - 1 + eqbstr )
         ce     =  eqryi ( base + eqrcol - 1 + eqestr )
         rcname =  eqryc ( cb:ce )

!
!        ...and clear out the scalar outputs.
!
         cbeg   =  1
         cend   =  1
         dval   =  0.0_wp
         ival   =  0

      else
!
!        The constraint compares a column and a value.  Set the
!        appropriate scalar output, and clear out the other outputs.
!
         if ( ( opcode == isnull ) .or. ( opcode == notnul ) ) then
!
!           There's no output value; the opcode implies the value NULL.
!           Set the outputs to innocuous defaults.
!
            cbeg = 1
            cend = 1
            dval = 0.0_wp
            ival = 0

         else
!
!           This is the normal case; set the scalar output values
!           according to the RHS data type.
!
            dtype  =  eqryi ( base + eqbval - 1 + eqdtyp )

            if ( dtype == chr ) then

               cbeg   =  eqryi ( base + eqbval - 1 + eqbstr )
               cend   =  eqryi ( base + eqbval - 1 + eqestr )
               dval   =  0.0_wp
               ival   =  0

            else if ( dtype == int ) then

               ptr    =  eqryi ( base + eqbval - 1 + eqvptr )
               ival   =  nint  ( eqryd(ptr) )
               dval   =  0.0_wp
               cbeg   =  1
               cend   =  1

            else
!
!              The data type is DP or TIME.
!
               ptr    =  eqryi ( base + eqbval - 1 + eqvptr )
               dval   =  eqryd ( ptr )
               ival   =  0
               cbeg   =  1
               cend   =  1

            end if

         end if

!
!        Set the RHS table and column outputs.
!
         rtidx  =  0
         rtname =  ' '
         rcidx  =  0
         rtname =  ' '

      end if

      return
      end subroutine zzekqcon

      !> Read a specified order-by table and column name, along with the
      !> corresponding order sense, from an encoded EK query.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     EQRYI          is the integer portion of an encoded EK query.
      !>                    The query must have been parsed.
      !>
      !>     EQRYC          is the character portion of an encoded EK query.
      !>
      !>     N              is the index, within the ORDER BY clause of the
      !>                    query, of the table whose name is to be fetched.
      !>
      !>## Detailed Output
      !>
      !>     TABLE          is the table name or alias associated with the Nth
      !>                    column in the ORDER BY clause of an the input
      !>                    encoded query.  If the Nth column is unqualified,
      !>                    TABLE is returned blank.
      !>
      !>     TABIDX         is the ordinal position in the FROM clause of the
      !>                    input query of the table containing the Nth order-
      !>                    by column.  TABIDX is meaningful only if name
      !>                    resolution has not been performed on the input
      !>                    query; otherwise, TABIDX is returned as zero.
      !>
      !>     COLUMN         is the name of the Nth column in the ORDER BY
      !>                    clause of an the input encoded query.
      !>
      !>     COLIDX         is the ordinal position of the Nth column in the
      !>                    ORDER BY clause with respect to the virtual table
      !>                    designated by TABLE.  This index is available only
      !>                    if the query has already had names resolved;
      !>                    otherwise, COLIDX is returned as zero.
      !>
      !>     SENSE          is an integer code giving the ordering sense to
      !>                    use with the specified column.  The possible values
      !>                    of SENSE are EQASND, which indicates that the
      !>                    order sense is ascending, and EQDSND, which
      !>                    indicates that the order sense is descending.
      !>                    `Ascending order' means that the order relation
      !>                    defined by the indicated column orders rows
      !>                    according to the order of elements in the
      !>                    indicated order-by column; `descending order' means
      !>                    that the order relation orders columns in the
      !>                    reverse of ascending order.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input query is not initialized, the error will be
      !>         diagnosed by routines called by this routine. The outputs
      !>         will not be modified.
      !>
      !>     2)  If the input query has not been parsed, the error
      !>         SPICE(UNPARSEDQUERY) will be signaled. The outputs
      !>         will not be modified.
      !>
      !>     3)  If the index N is less than 1 or greater than the number of
      !>         columns in the ORDER BY clause, the error SPICE(INVALIDINDEX)
      !>         will be signaled. The outputs will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The call
      !>
      !>        CALL ZZEKREQI (  EQRYI,  'NUM_ORDERBY_COLS',  N  )
      !>
      !>     may be used to get the ORDER BY column count from an encoded
      !>     query.
      !>
      !>## Examples
      !>
      !>     See EKSRCH.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
      module subroutine zzekqord ( ctx, eqryi, eqryc, n, table, tabidx, column, colidx, sense )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      use pepper_inc_ekbool
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*), intent(in) :: eqryc  !! Character component of query.
      integer(ip), intent(in) :: n  !! Index within FROM clause of table name to read.
      character(*), intent(out) :: table  !! Name of table qualifying Nth ORDER BY column.
      integer(ip), intent(out) :: tabidx  !! Index of TABLE in FROM clause, if known.
      character(*), intent(out) :: column  !! Nth column in ORDER BY clause of query.
      integer(ip), intent(out) :: colidx  !! Index of column in TABLE, if known.
      integer(ip), intent(out) :: sense  !! Code giving order sense for Nth column.

      integer(ip) :: base
      integer(ip) :: buflen
      integer(ip) :: cb
      integer(ip) :: ce
      integer(ip) :: iparse
      integer(ip) :: ncnj
      integer(ip) :: ncns
      integer(ip) :: nord
      integer(ip) :: ntab
      integer(ip) :: resolv
      integer(ip) :: tb
      integer(ip) :: te

      call zzekreqi ( ctx, eqryi, 'PARSED', iparse )

      if ( ctx%failed() ) return

      if ( iparse == ifalse ) then

         call ctx%setmsg ( 'Encoded query has not yet been parsed.'  )
         call ctx%sigerr ( 'SPICE(UNPARSEDQUERY)'                    )
         return

      end if

      call zzekreqi ( ctx, eqryi,  'NUM_ORDERBY_COLS',   nord   )

      if (  ( n < 1 ) .or. ( n > nord )  ) then

         call ctx%setmsg ( 'Column index # is out of valid range 1:#.' )
         call ctx%errint ( '#',  n                                     )
         call ctx%errint ( '#',  nord                                  )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                       )
         return

      end if

      call zzekreqi ( ctx, eqryi,  'NUM_TABLES',         ntab   )
      call zzekreqi ( ctx, eqryi,  'NUM_CONJUNCTIONS',   ncnj   )
      call zzekreqi ( ctx, eqryi,  'NUM_CONSTRAINTS',    ncns   )
      call zzekreqi ( ctx, eqryi,  'CHR_BUF_SIZE',       buflen )

!
!     Get the Nth table and column from the query.  The table
!     descriptor lies beyond the fixed-size portion of the query, the
!     conjunction size list, and the constraint descriptors, as
!     well as the (N-1) previous order-by column descriptors.
!
      base   =        eqvbas &
                  +   ntab * 2 * eqvdsz &
                  +   ncnj &
                  +   ncns     * eqcdsz &
                  +   (n-1)    * eqodsz

!
!     Pick up the column name first.
!
      cb     =  eqryi ( base + eqvdsz + eqbstr )
      ce     =  eqryi ( base + eqvdsz + eqestr )

      if (       ( cb > 0      ) &
           .and. ( ce > 0      ) &
           .and. ( cb <= buflen ) &
           .and. ( ce <= buflen ) &
           .and. ( cb <= ce     )  ) then

         column = eqryc(cb:ce)

      else
!
!        We should never see invalid pointers in a parsed, encoded
!        query, but let's not take chances.
!
         call ctx%setmsg ( 'Invalid string bounds #:# for column #.' )
         call ctx%errint ( '#',  cb                                  )
         call ctx%errint ( '#',  ce                                  )
         call ctx%errint ( '#',  n                                   )
         call ctx%sigerr ( 'SPICE(BUG)'                              )
         return

      end if

!
!     Same deal for the qualifying table or alias, except that the begin
!     pointer is set to zero if there's no name.
!
      tb     =  eqryi ( base + eqbstr )
      te     =  eqryi ( base + eqestr )

      if ( tb > 0 ) then

         if (       ( te > 0      ) &
              .and. ( tb <= buflen ) &
              .and. ( te <= buflen ) &
              .and. ( tb <= te     )  ) then

            table  = eqryc(tb:te)

         else
!
!           If the first pointer is non-zero, both pointers should have
!           been valid.
!
            call ctx%setmsg ( 'Invalid string bounds #:# for the table '  // &
                          'qualifying column #.'                       )
            call ctx%errint ( '#',  tb                                     )
            call ctx%errint ( '#',  te                                     )
            call ctx%errint ( '#',  n                                      )
            call ctx%sigerr ( 'SPICE(BUG)'                                 )
            return

         end if

      else
!
!        No table was supplied.
!
         table  =  ' '

      end if

!
!     Set the order sense.
!
      sense  =  eqryi ( base + eqodir )

!
!     If names have been resolved already, we can determine the index
!     of the table to which the specified order-by column belongs.
!
      call zzekreqi ( ctx, eqryi,  'NAMES_RESOLVED',   resolv )

      if ( resolv == itrue ) then
!
!        The qualifying table's index in the FROM clause is available.
!        So is the index of the column within the table.
!
         tabidx  =  eqryi( base + eqtord              )
         colidx  =  eqryi( base + eqocol - 1 + eqcidx )

      else

         tabidx  =  0
         colidx  =  0

      end if

      return
      end subroutine zzekqord

      !> Read a specified SELECT clause table and column name from an
      !> encoded EK query.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     EQRYI          is the integer portion of an encoded EK query.
      !>                    The query must have been parsed.
      !>
      !>     EQRYC          is the character portion of an encoded EK query.
      !>
      !>     N              is the index, within the SELECT clause of the
      !>                    query, of the table whose name is to be fetched.
      !>
      !>## Detailed Output
      !>
      !>     LXBEG,
      !>     LXEND          are, respectively, the begin and end indices in
      !>                    the original query of the selected item.
      !>
      !>     TABLE          is the table name or alias associated with the Nth
      !>                    column in the SELECT clause of an the input
      !>                    encoded query.  If the Nth column is unqualified,
      !>                    TABLE is returned blank.
      !>
      !>     TABIDX         is the ordinal position in the FROM clause of the
      !>                    input query of the table containing the Nth order-
      !>                    by column.  TABIDX is meaningful only if name
      !>                    resolution has not been performed on the input
      !>                    query; otherwise, TABIDX is returned as zero.
      !>
      !>     COLUMN         is the name of the Nth column in the SELECT
      !>                    clause of an the input encoded query.
      !>
      !>     COLIDX         is the ordinal position of the Nth column in the
      !>                    SELECT clause with respect to the virtual table
      !>                    designated by TABLE.  This index is available only
      !>                    if the query has already had names resolved;
      !>                    otherwise, COLIDX is returned as zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input query is not initialized, the error will be
      !>         diagnosed by routines called by this routine.  The outputs
      !>         will not be modified.
      !>
      !>     2)  If the input query has not been parsed, the error
      !>         SPICE(UNPARSEDQUERY) will be signaled.  The outputs
      !>         will not be modified.
      !>
      !>     3)  If the index N is less than 1 or greater than the number of
      !>         columns in the SELECT clause, the error SPICE(INVALIDINDEX)
      !>         will be signaled.  The outputs
      !>         will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The call
      !>
      !>        CALL ZZEKREQI (  EQRYI,  'NUM_SELECT_COLS',  N  )
      !>
      !>     may be used to get the SELECT column count from an encoded
      !>     query.
      !>
      !>## Examples
      !>
      !>     See EKSRCH.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
      module subroutine zzekqsel ( ctx, eqryi, eqryc, n, lxbeg, lxend, table, tabidx, column, colidx )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      use pepper_inc_ekbool
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*), intent(in) :: eqryc  !! Character component of query.
      integer(ip), intent(in) :: n  !! Index within SELECT clause of table name to read.
      integer(ip), intent(out) :: lxbeg  !! Begin index in query of selected item.
      integer(ip), intent(out) :: lxend  !! End index in query of selected item.
      character(*), intent(out) :: table  !! Name of table qualifying Nth SELECT column.
      integer(ip), intent(out) :: tabidx  !! Index of TABLE in FROM clause, if known.
      character(*), intent(out) :: column  !! Nth column in SELECT clause of query.
      integer(ip), intent(out) :: colidx  !! Index of column in TABLE, if known.

      integer(ip) :: base
      integer(ip) :: buflen
      integer(ip) :: cb
      integer(ip) :: ce
      integer(ip) :: iparse
      integer(ip) :: ncnj
      integer(ip) :: ncns
      integer(ip) :: nord
      integer(ip) :: nsel
      integer(ip) :: ntab
      integer(ip) :: resolv
      integer(ip) :: tb
      integer(ip) :: te

      call zzekreqi ( ctx, eqryi, 'PARSED', iparse )

      if ( ctx%failed() ) return

      if ( iparse == ifalse ) then

         call ctx%setmsg ( 'Encoded query has not yet been parsed.'  )
         call ctx%sigerr ( 'SPICE(UNPARSEDQUERY)'                    )
         return

      end if

      call zzekreqi ( ctx, eqryi,  'NUM_SELECT_COLS',   nsel   )

      if (  ( n < 1 ) .or. ( n > nsel )  ) then

         call ctx%setmsg ( 'Column index # is out of valid range 1:#.' )
         call ctx%errint ( '#',  n                                     )
         call ctx%errint ( '#',  nsel                                  )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                       )
         return

      end if

      call zzekreqi ( ctx, eqryi,  'NUM_TABLES',         ntab   )
      call zzekreqi ( ctx, eqryi,  'NUM_CONJUNCTIONS',   ncnj   )
      call zzekreqi ( ctx, eqryi,  'NUM_CONSTRAINTS',    ncns   )
      call zzekreqi ( ctx, eqryi,  'NUM_ORDERBY_COLS',   nord   )
      call zzekreqi ( ctx, eqryi,  'CHR_BUF_SIZE',       buflen )

!
!     The lexeme begin and end values start out as invalid values.
!     We'll set these when we discover what form the SELECT item has.
!

      lxbeg  =  0
      lxend  =  0

!
!     Get the Nth table and column from the query.  The table
!     descriptor lies beyond the fixed-size portion of the query, the
!     conjunction size list, the constraint descriptors, the order-by
!     column descriptors, as well as the (N-1) previous SELECT column
!     descriptors.
!
      base   =        eqvbas &
                  +   ntab * 2 * eqvdsz &
                  +   ncnj &
                  +   ncns     * eqcdsz &
                  +   nord     * eqodsz &
                  +   (n-1)    * eqsdsz

!
!     Pick up the column name first.
!
      cb     =  eqryi ( base + eqvdsz + eqbstr )
      ce     =  eqryi ( base + eqvdsz + eqestr )

      if (       ( cb > 0      ) &
           .and. ( ce > 0      ) &
           .and. ( cb <= buflen ) &
           .and. ( ce <= buflen ) &
           .and. ( cb <= ce     )  ) then

         column = eqryc(cb:ce)

!
!        The end of the column name is always the end of the SELECT
!        item, at least until we handle more general expressions.
!
         lxend  =  eqryi ( base + eqvdsz + eqelex )

      else
!
!        We should never see invalid pointers in a parsed, encoded
!        query, but let's not take chances.
!
         call ctx%setmsg ( 'Invalid string bounds #:# for column #.' )
         call ctx%errint ( '#',  cb                                  )
         call ctx%errint ( '#',  ce                                  )
         call ctx%errint ( '#',  n                                   )
         call ctx%sigerr ( 'SPICE(BUG)'                              )
         return

      end if

!
!     Same deal for the qualifying table or alias, except that the begin
!     pointer is set to zero if there's no name.
!
      tb     =  eqryi ( base + eqbstr )
      te     =  eqryi ( base + eqestr )

      if ( tb > 0 ) then

         if (       ( te > 0      ) &
              .and. ( tb <= buflen ) &
              .and. ( te <= buflen ) &
              .and. ( tb <= te     )  ) then

            table  = eqryc(tb:te)

!
!           The start position of the table name is the start of
!           the SELECT item.
!
            lxbeg  =  eqryi ( base + eqblex )

         else
!
!           If the first pointer is non-zero, both pointers should have
!           been valid.
!
            call ctx%setmsg ( 'Invalid string bounds #:# for the table '  // &
                          'qualifying column #.'                       )
            call ctx%errint ( '#',  tb                                     )
            call ctx%errint ( '#',  te                                     )
            call ctx%errint ( '#',  n                                      )
            call ctx%sigerr ( 'SPICE(BUG)'                                 )
            return

         end if

      else
!
!        No table was supplied.
!
         table  =  ' '

!
!        The start position of the column name is the start of
!        the SELECT item.
!
         lxbeg  =  eqryi ( base + eqvdsz + eqblex )

      end if

!
!     If names have been resolved already, we can determine the index
!     of the table to which the specified order-by column belongs.
!
      call zzekreqi ( ctx, eqryi,  'NAMES_RESOLVED',   resolv )

      if ( resolv == itrue ) then
!
!        The qualifying table's index in the FROM clause is available.
!        So is the index of the column within the table.
!
         tabidx  =  eqryi( base + eqtord              )
         colidx  =  eqryi( base + eqscol - 1 + eqcidx )

      else

         tabidx  =  0
         colidx  =  0

      end if

      return
      end subroutine zzekqsel

      !> Read table names and aliases from an encoded EK query.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     EQRYI          is the integer portion of an encoded EK query.
      !>                    The query must have been parsed.
      !>
      !>     EQRYC          is the character portion of an encoded EK query.
      !>
      !>     N              is the index, within the FROM clause of the query,
      !>                    of the table whose name is to be fetched.
      !>
      !>## Detailed Output
      !>
      !>     TABLE          is the name of the Nth table in the FROM clause of
      !>                    the input encoded query.
      !>
      !>     ALIAS          is the alias of TABLE.  If no alias for TABLE is
      !>                    present, ALIAS is returned blank.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input query is not initialized, the error will be
      !>         diagnosed by routines called by this routine.  The outputs
      !>         will not be modified.
      !>
      !>     2)  If the input query has not been parsed, the error
      !>         SPICE(UNPARSEDQUERY) will be signaled.  The outputs
      !>         will not be modified.
      !>
      !>     3)  If the index N is less than 1 or greater than the number of
      !>         tables in the FROM clause, the error SPICE(INVALIDINDEX)
      !>         will be signaled.  The outputs
      !>         will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The call
      !>
      !>        CALL ZZEKREQI (  EQRYI,  'NUM_TABLES',  N  )
      !>
      !>     may be used to get the FROM table count from an encoded query.
      !>
      !>## Examples
      !>
      !>     See EKSRCH.
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
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
      module subroutine zzekqtab ( ctx, eqryi, eqryc, n, table, alias )
      use pepper_kinds, only: ip
      use pepper_inc_ekquery
      use pepper_inc_ekbool
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: eqryi ( lbcell : * )  !! Integer component of query.
      character(*), intent(in) :: eqryc  !! Character component of query.
      integer(ip), intent(in) :: n  !! Index within FROM clause of table name to read.
      character(*), intent(out) :: table  !! Name of Nth table in FROM clause.
      character(*), intent(out) :: alias  !! Alias of Nth table in FROM clause.

      integer(ip) :: ab
      integer(ip) :: ae
      integer(ip) :: base
      integer(ip) :: buflen
      integer(ip) :: iparse
      integer(ip) :: ntab
      integer(ip) :: tb
      integer(ip) :: te

      call zzekreqi ( ctx, eqryi, 'PARSED', iparse )

      if ( ctx%failed() ) return

      if ( iparse == ifalse ) then

         call ctx%setmsg ( 'Encoded query has not yet been parsed.'  )
         call ctx%sigerr ( 'SPICE(UNPARSEDQUERY)'                    )
         return

      end if

      call zzekreqi ( ctx, eqryi, 'CHR_BUF_SIZE', buflen )
      call zzekreqi ( ctx, eqryi, 'NUM_TABLES',   ntab   )

      if (  ( n < 1 ) .or. ( n > ntab )  ) then

         call ctx%setmsg ( 'Table index # is out of valid range 1:#.'  )
         call ctx%errint ( '#',  n                                     )
         call ctx%errint ( '#',  ntab                                  )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                       )
         return

      end if

!
!     Get the Nth table and alias from the query.  The table
!     descriptor lies beyond the fixed-size portion of the query, as
!     well as the (N-1) previous descriptors, each one of which has
!     size 2*EQVDSZ.
!
      base   =  eqvbas + (n-1)*2*eqvdsz

      tb     =  eqryi ( base + eqbstr )
      te     =  eqryi ( base + eqestr )

      if (       ( tb > 0      ) &
           .and. ( te > 0      ) &
           .and. ( tb <= buflen ) &
           .and. ( te <= buflen ) &
           .and. ( tb <= te     )  ) then

         table = eqryc(tb:te)

      else
!
!        We should never see invalid pointers in a parsed, encoded
!        query, but let's not take chances.
!
         call ctx%setmsg ( 'Invalid string bounds #:# for table #.'  )
         call ctx%errint ( '#',  tb                                  )
         call ctx%errint ( '#',  te                                  )
         call ctx%errint ( '#',  n                                   )
         call ctx%sigerr ( 'SPICE(BUG)'                              )
         return

      end if

!
!     Same deal for the alias, except that the begin pointer is
!     set to zero if there's no alias.
!
      ab     =  eqryi ( base + eqvdsz + eqbstr )
      ae     =  eqryi ( base + eqvdsz + eqestr )

      if ( ab > 0 ) then

         if (       ( ae > 0      ) &
              .and. ( ab <= buflen ) &
              .and. ( ae <= buflen ) &
              .and. ( ab <= ae     )  ) then

            alias  = eqryc(ab:ae)

         else
!
!           If the first pointer is non-zero, both pointers should have
!           been valid.
!
            call ctx%setmsg ( 'Invalid string bounds #:# for the alias '  // &
                          'of table #.'                                )
            call ctx%errint ( '#',  ab                                     )
            call ctx%errint ( '#',  ae                                     )
            call ctx%errint ( '#',  n                                      )
            call ctx%sigerr ( 'SPICE(BUG)'                                 )
            return

         end if

      else
!
!        No alias was supplied.
!
         alias  =  ' '

      end if

      return
      end subroutine zzekqtab

      !> Read a column entry from a specified record in a class 9 column.
      !> Class 9 columns contain fixed record count, fixed-length,
      !> scalar character values.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is an EK file handle.
      !>
      !>     SEGDSC         is the descriptor of the segment from which data is
      !>                    to be read.
      !>
      !>     COLDSC         is the descriptor of the column from which data is
      !>                    to be read.
      !>
      !>     RECNO          is the number of the record containing the column
      !>                    entry to be written.
      !>
      !>## Detailed Output
      !>
      !>     CVLEN          is the length of the returned string value.  This
      !>                    is the declared string length of the column being
      !>                    read.  Note this definition differs from that used
      !>                    for class 3 columns.  In the class 9 case, no
      !>                    string length is stored in the file, so extra work
      !>                    at run time would be required to determine whether
      !>                    truncation would occur.
      !>
      !>     CVAL           is the value read from the specified column entry.
      !>                    CVAL must have sufficient length to hold the
      !>                    returned string value.  Entries that are shorter
      !>                    than the string length of CVAL are padded with
      !>                    trailing blanks.
      !>
      !>     ISNULL         is a logical flag indicating whether the entry is
      !>                    null.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If the ordinal position of the column specified by COLDSC
      !>         is out of range, the error SPICE(INVALIDINDEX) is signaled.
      !>
      !>     3)  If the output string CVAL is too short to accommodate the
      !>         returned string value, the error SPICE(STRINGTRUNCATED)
      !>         is signaled.  CVAL must be at least as long as the declared
      !>         length of the column being read.
      !>
      !>     4)  If an I/O error occurs while reading the indicated file,
      !>         the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine is a utility for reading data from class 9 columns.
      !>
      !>## Examples
      !>
      !>     See EKRCEC.
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
      !>-    SPICELIB Version 1.2.0, 07-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN to insert DAS file name into
      !>        long error messages.
      !>
      !>        Bug fix: changed max column index in long error
      !>        message from NREC to NCOLS.
      !>
      !>-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
      !>
      !>        Error check for string truncation on output was added.
      !>        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
      !>        to SPICE(UNINITIALIZED).
      !>
      !>        The argument RECPTR was renamed to RECNO.  The reference to
      !>        ZZEKRP2N was removed.
      !>
      !>        Miscellaneous header corrections were made.
      !>
      !>-    SPICELIB Version 1.0.0, 09-NOV-1995 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB)
      !>
      !>        Error check for string truncation on output was added.
      !>        SHORT error message SPICE(UNINITIALIZEDVALUE) was shortened
      !>        to SPICE(UNINITIALIZED).
      !>
      !>        The argument RECPTR was renamed to RECNO.  The reference to
      !>        ZZEKRP2N was removed.
      module subroutine zzekrd09 ( ctx, handle, segdsc, coldsc, recno, cvlen, cval, isnull )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekclas09
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      use pepper_inc_ektype

      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle attached to EK file.
      integer(ip), intent(in) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip), intent(in) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip), intent(in) :: recno  !! Record number.
      integer(ip), intent(out) :: cvlen  !! Length of returned character value.
      character(*), intent(out) :: cval  !! Character value in column entry.
      logical, intent(out) :: isnull  !! Flag indicating whether column entry is null.
!
!     Local variables
!
      character(1) :: cflag
      character(cnamsz) :: column

      integer(ip) :: addrss
      integer(ip) :: colidx
      integer(ip) :: datbas
      integer(ip) :: l
      integer(ip) :: mdat   ( mdsz09 )
      integer(ip) :: metloc
      integer(ip) :: ncols
      integer(ip) :: nflbas
      integer(ip) :: offset
      integer(ip) :: q
      integer(ip) :: r
      integer(ip) :: spp

      logical :: nullok

!
!     Make sure the column exists.
!
      ncols   =  segdsc ( ncidx  )
      colidx  =  coldsc ( ordidx )
      metloc  =  coldsc ( metidx )
      nullok  =  coldsc ( nflidx ) == itrue
      l       =  coldsc ( lenidx )

      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         call ctx%setmsg ( 'Column index = #; valid range is 1:#.' // &
                       'SEGNO = #; RECNO = #; EK = #'          )
         call ctx%errint ( '#',  colidx                            )
         call ctx%errint ( '#',  ncols                             )
         call ctx%errint ( '#',  segdsc(snoidx)                    )
         call ctx%errint ( '#',  recno                             )
         call ctx%errhan ( '#',  handle                            )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                   )
         return

      end if

!
!     Since class 9 columns have fixed-length strings, we already
!     know the string length.
!
      cvlen  =  l

      if ( cvlen > len(cval) ) then
!
!        We have a string truncation error.  Look up the column
!        name, record number, and file name before signaling an
!        error.
!
         call zzekcnam ( ctx, handle, coldsc, column )

         call ctx%setmsg ( 'String value has length #; output string '// &
                       'can hold only # characters.  COLUMN = #; '// &
                       'SEGNO = #; RECNO = #; EK = #'              )
         call ctx%errint ( '#',  cvlen                                 )
         call ctx%errint ( '#',  len(cval)                             )
         call ctx%errch  ( '#',  column                                )
         call ctx%errint ( '#',  segdsc(snoidx)                        )
         call ctx%errint ( '#',  recno                                 )
         call ctx%errhan ( '#',  handle                                )
         call ctx%sigerr ( 'SPICE(STRINGTRUNCATED)'                    )
         return

      end if

!
!     Read the metadata block.  There are two items in the block:
!
!        1) The base address of the first page of the data
!        2) The base address of the null flag array, if nulls are
!           permitted.
!
      call dasrdi ( ctx, handle, metloc+1, metloc+mdsz09, mdat )

      datbas = mdat ( dbix09 )
      nflbas = mdat ( nfix09 )

!
!     If null values are permitted, the first step is to get
!     the null flag for the value of interest.  Compute the
!     address of this flag.
!
!     There are CPSIZE null flags per page, and each page has size
!     PGSIZC.  The null flags start at the beginning of the page.
!
      if ( nullok ) then

         q       =  (recno-1)  / cpsize
         r       =  recno  - q * cpsize
         offset  =  r      + q * pgsizc
         addrss  =  nflbas + offset

         call dasrdc ( ctx, handle, addrss, addrss, 1, 1, cflag )

         isnull  =  cflag == ctrue

         if ( isnull ) return

      end if

!
!     If we're still here, we'll read the data value.
!
      isnull  =  .false.

!
!     The address calculation for the value is similar to that
!     for the null flag.  However, the string length must be
!     taken into account.
!
      spp     =  cpsize     /   l
      q       =  (recno-1)  /   spp
      r       =  recno      -   q*spp
      addrss  =  datbas     +   q*pgsizc  +  (r-1)*l  +  1

      call dasrdc ( ctx, handle, addrss, addrss+l-1, 1, l, cval )

!
!     Blank-pad CVAL if required.
!
      if ( len(cval) > l ) then
         cval( l+1 : )  =  ' '
      end if

      return
      end subroutine zzekrd09

      !> Set the companion pointer of a specified EK record.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     RECPTR         is a pointer to the record whose companion pointer
      !>                    is to be set.
      !>
      !>     RECNO          is the `companion record pointer' of the record
      !>                    designated by RECPTR.  This name is a little
      !>                    misleading:  we identify the companion record
      !>                    by its record number, not by an address.  So RECNO
      !>                    is a pointer only in an abstract sense.
      !>
      !>                    If the input record belongs to a shadowed EK, the
      !>                    companion record is the corresponding backup
      !>                    record.  If the input record is a backup record,
      !>                    the companion record is the corresponding source
      !>                    record.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the action of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine sets the companion record pointer of a specified EK
      !>     record.  As noted in $Detailed_Input, the `pointer' is simply
      !>     the number of the companion record.
      !>
      !>## Examples
      !>
      !>     See ZZEKRBCK.
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
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module subroutine zzeksrcp ( ctx, handle, recptr, recno )
      use pepper_kinds, only: ip
      use pepper_inc_ekrecptr
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: recptr  !! Record pointer.
      integer(ip), intent(in) :: recno  !! Record number of companion of specified EK record.
!
!     Local variables
!
      integer(ip) :: loc

!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Compute the companion pointer and set the pointer.
!
      loc  =  recptr + rcpidx

      call dasudi ( ctx, handle, loc, loc, [ recno ] )

      return
      end subroutine zzeksrcp

      !> Set the status of a specified EK record.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     RECPTR         is a pointer to the record whose status is to be
      !>                    set.
      !>
      !>     STATUS         is the status word of the specified record.  See
      !>                    the include file ekrecptr.inc for values and
      !>                    meanings.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the action of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine sets the status word of a specified EK record.
      !>
      !>## Examples
      !>
      !>     See EKCOMM.
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
      !>-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
      module subroutine zzeksrs ( ctx, handle, recptr, status )
      use pepper_kinds, only: ip
      use pepper_inc_ekrecptr
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: recptr  !! Record pointer.
      integer(ip), intent(in) :: status  !! Status of specified EK record.
!
!     Local variables
!
      integer(ip) :: loc

!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Compute the status word location and set the status.
!
      loc  =  recptr + staidx

      call dasudi ( ctx, handle, loc, loc, [ status ] )

      return
      end subroutine zzeksrs

      !> Return the size of a specified entry in a class 6 column.
      !>
      !> The function returns the number of elements in the specified
      !> column entry.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is an EK file handle.  The file may be open for
      !>                    reading or writing.
      !>
      !>     SEGDSC         is the segment descriptor of the segment
      !>                    containing the column specified by COLDSC.
      !>
      !>     COLDSC         is the column descriptor of the column containing
      !>                    the entry whose size is requested.  The column
      !>                    must be class 6.
      !>
      !>     RECPTR         is a pointer to the EK record containing the
      !>                    column entry of interest.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of elements in the specified
      !>     column entry.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file,
      !>         the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the column index contained in the input column descriptor
      !>         is out of range, the error SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This utility supports the commonly performed function of
      !>     determining the element count of a column entry.
      !>
      !>## Examples
      !>
      !>     See ZZEKESIZ.
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
      module integer(ip) function zzeksz06 ( ctx, handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip), intent(in) :: recptr  !! Record pointer.
      integer(ip), intent(in) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip) :: datptr_1(1)  !! one-element copy of datptr (array argument)
!
!     Local variables
!
      integer(ip) :: colidx
      integer(ip) :: datptr
      integer(ip) :: ncols
      integer(ip) :: nrec
      integer(ip) :: ptrloc

!
!     Initialize the function's return value.
!
      zzeksz06  =  0

      nrec    =  segdsc ( nridx  )
      colidx  =  coldsc ( ordidx )

!
!     Make sure the column exists.
!
      ncols  =  segdsc ( ncidx )

      if (  ( colidx < 1 ) .or. ( colidx > ncols )  ) then

         call ctx%setmsg ( 'Column index = #; valid range is 1:#.' )
         call ctx%errint ( '#',  colidx                            )
         call ctx%errint ( '#',  nrec                              )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                   )
         return

      end if

!
!     If the column has fixed-size entries, just return the declared
!     size.
!
      if ( coldsc(sizidx) /= ifalse ) then

         zzeksz06  =  coldsc(sizidx)
         return

      end if

!
!     Compute the data pointer location.  Read the data pointer.
!
      ptrloc  =  recptr + dptbas + colidx

      call dasrdi ( ctx, handle, ptrloc, ptrloc, datptr_1 )
      datptr = datptr_1(1)

      if ( datptr < 1 ) then
!
!        The value is null.  Null entries are always considered to have
!        size 1.
!
         zzeksz06  =  1

      else
!
!        DATPTR points to the element count.
!
         call zzekgei ( ctx, handle, datptr, zzeksz06 )

      end if

      return
      end function zzeksz06

      !> Given a union of EK join row sets, prepare EKVCAL to
      !> compute addresses of row vectors in that union.
      !>
      !>## Detailed Input
      !>
      !>     NJRS           is the number of join row sets in a join row set
      !>                    for which address calculations will be performed.
      !>
      !>     BASES          is an array of base addresses of the join row sets
      !>                    comprising the union.  These addresses are the
      !>                    predecessors of the addresses actually occupied by
      !>                    the join row sets.  There are NJRS base addresses
      !>                    in the array.  The order in which addresses are
      !>                    listed in BASES determines the order of the union
      !>                    of the row vectors:  the first row vector in the
      !>                    join row set whose base address is BASES(1) has
      !>                    index 1, and so on.  The last row vector in the
      !>                    join row set whose base address is BASES(NJRS) has
      !>                    the highest index of any row vector in the union.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a discussion of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the join row set count is less than 1 or greater than
      !>         MXJRS, the error SPICE(INVALIDCOUNT) is signaled.
      !>
      !>     2)  If any base address is less than zero or greater than TOP,
      !>         the EK scratch area stack top, the error
      !>         SPICE(BADADDRESS) is signaled.
      !>
      !>     3)  If the table count for any join row set is less than 1 or
      !>         greater than MXJOIN, the error SPICE(INVALIDCOUNT) is
      !>         signaled.
      !>
      !>     4)  If the table count for any join row set unequal to the count
      !>         for the first join row set, the error SPICE(INVALIDCOUNT) is
      !>         signaled.
      !>
      !>     5)  If any join row set has a row vector count that is less than
      !>         zero or greater than TOP, the EK scratch area stack top, the
      !>         error SPICE(BADADDRESS) is signaled.
      !>
      !>     6)  If any join row set has a segment vector count that is less
      !>         than zero or greater than TOP, the EK scratch area stack top,
      !>         the error SPICE(BADADDRESS) is signaled.
      !>
      !>## Files
      !>
      !>     1)  This routine uses the EK scratch area, which employs a scratch
      !>         DAS file.
      !>
      !>## Particulars
      !>
      !>     This routine speeds up EK row vectors address calculations by
      !>     centralizing the activities that need be performed only once
      !>     for a series of address  calculations for a given join row set
      !>     union.
      !>
      !>## Examples
      !>
      !>     See the $Examples section of ZZEKVADR.
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
      !>-    Beta Version 1.0.0, 28-SEP-1994 (NJB) (WLT)
      module subroutine zzekvset ( ctx, njrs, bases )
      use pepper_kinds, only: ip
      use pepper_containers, only: grow
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: njrs  !! Number of join row sets in union.
      integer(ip), intent(in) :: bases  ( * )  !! EK scratch area base addresses of join row sets.
      integer(ip) :: nsv_1(1)  !! one-element copy of nsv (array argument)
      integer(ip) :: j_1(1)  !! one-element copy of j (array argument)
      integer(ip) :: ntabs_1(1)  !! one-element copy of ntabs (array argument)
      integer(ip), parameter :: jrcidx = 2
      integer(ip), parameter :: jtcidx = 3
      integer(ip), parameter :: jscidx = 4
      integer(ip) :: addrss
      integer(ip) :: nsv
      integer(ip) :: i
      integer(ip) :: top
      if ( .not. allocated ( ctx%zzekvadr_state ) ) allocate ( ctx%zzekvadr_state )
!
!     PEPPER: the arrays of the join row sets grow to NJRS (SPICELIB: MXJRS
!     = 200, a conjunction of the WHERE clause each; also no maximum number
!     of tables, SPICELIB: MXJOIN = 10).
!
      call grow ( ctx%zzekvadr_state%begidx, max ( njrs, 1_ip ) )
      call grow ( ctx%zzekvadr_state%rbas, max ( njrs, 1_ip ) )
      call grow ( ctx%zzekvadr_state%svbas, max ( njrs, 1_ip ) )
      associate ( begidx => ctx%zzekvadr_state%begidx, &
                  j => ctx%zzekvadr_state%j, &
                  maxrwv => ctx%zzekvadr_state%maxrwv, &
                  ntabs => ctx%zzekvadr_state%ntabs, &
                  rbas => ctx%zzekvadr_state%rbas, &
                  svbas => ctx%zzekvadr_state%svbas, &
                  svnjrs => ctx%zzekvadr_state%svnjrs )
      if ( ctx%failed() ) return

!
!     Validate join row set count.
!
      if ( njrs < 1 ) then

         call ctx%setmsg ( 'Number of join row sets was #; valid ' // &
                       'it must be at least 1.'                           )
         call ctx%errint ( '#',  njrs                               )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                    )
         return

      end if

!
!     Validate the join row set bases.
!
      call zzekstop ( ctx, top )

      do i = 1, njrs

         if (  ( bases(i) < 0 ) .or. ( bases(i) > top )  ) then

            call ctx%setmsg ( 'Base address # was #; valid range is ' // &
                          '1:#'                                    )
            call ctx%errint ( '#',  i                                  )
            call ctx%errint ( '#',  bases(i)                           )
            call ctx%errint ( '#',  top                                )
            call ctx%sigerr ( 'SPICE(BADADDRESS)'                      )
            return

         end if

         svbas(i) = bases(i)

      end do

!
!     Validate and save the table count.  It's an error for this
!     count not to be identical for all of the join row sets in the
!     union.
!
      addrss  =  bases(1) + jtcidx
      call zzeksrd ( ctx, addrss, addrss, ntabs_1 )
      ntabs = ntabs_1(1)

      if ( ntabs < 1 ) then

         call ctx%setmsg ( 'Table count for first join row set was #; '  // &
                       'it must be at least 1.'                           )
         call ctx%errint ( '#',  ntabs                                    )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                          )
         return

      end if

      do i = 2, njrs

         addrss  =  bases(i) + jtcidx
         call zzeksrd ( ctx, addrss, addrss, j_1 )
         j = j_1(1)

         if ( j /= ntabs ) then

            call ctx%setmsg ( 'Join row set # contains # tables; first ' // &
                          'join row set contains # tables.  These '  // &
                          'counts are supposed to match.'             )
            call ctx%errint ( '#',  i                                     )
            call ctx%errint ( '#',  j                                     )
            call ctx%errint ( '#',  ntabs                                 )
            call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                       )
            return

         end if

      end do

!
!     Validate the row vector counts for each join row set.
!     These counts must be in range.  Save the start indices of
!     the row vectors in each join row set.
!
      begidx = 0
      begidx(1) = 1

      do i = 1, njrs

         addrss  =  bases(i) + jrcidx
         call zzeksrd ( ctx, addrss, addrss, j_1 )
         j = j_1(1)

         if (  ( j < 0 ) .or. ( j > top )  ) then

            call ctx%setmsg ( 'Join row set # has row count #; valid ' // &
                          'range is 0:#'                            )
            call ctx%errint ( '#',  i                                   )
            call ctx%errint ( '#',  j                                   )
            call ctx%errint ( '#',  top                                 )
            call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                     )
            return

         end if

         if ( i < njrs ) then
            begidx(i+1)  =  begidx(i)  +  j
         end if

      end do

!
!     Retain the index of the last row vector.
!
      maxrwv  =  begidx(njrs) + j

!
!     Save the base addresses of the row vectors in each join row set.
!     Validate the segment vector counts while we're at it.
!
      do i = 1, njrs

         addrss  =  bases(i) + jscidx
         call zzeksrd ( ctx, addrss, addrss, nsv_1 )
         nsv = nsv_1(1)

         if ( nsv < 0 ) then

            call ctx%setmsg ( 'Join row set # has segment vector '     // &
                          'count #; count must be non-negative.'    )
            call ctx%errint ( '#',  i                                   )
            call ctx%errint ( '#',  nsv                                 )
            call ctx%errint ( '#',  top                                 )
            call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                     )
            return

         end if

         rbas(i) =  addrss  +  nsv * ( ntabs + 2 )

      end do

!
!     Retain the count of join row sets in the union.
!
      svnjrs = njrs

      return
      end associate
      end subroutine zzekvset

      !> Find the EK scratch area base address of a row vector and the
      !> corresponding segment vector, where the row vector has a
      !> specified index within a union of join row sets.
      !>
      !>## Detailed Input
      !>
      !>     RWVIDX         is the index of a row vector in a join row set
      !>                    union.  The union is presumed to have been
      !>                    specified by a call to ZZEKVSET.
      !>
      !>## Detailed Output
      !>
      !>     RWVBAS         is the EK scratch area base address of the row
      !>                    vector specified by RWVIDX.  This address is
      !>                    the predecessor of the first address occupied by
      !>                    the row vector.  The row vector occupies NTAB
      !>                    consecutive addresses, where NTAB is the common
      !>                    table count for all join row sets in the union
      !>                    containing the specified row vector.
      !>
      !>     SGVBAS         is the EK scratch area base address of the segment
      !>                    vector corresponding to the specified row vector.
      !>                    The segment vector also occupies NTAB consecutive
      !>                    addresses.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input index is less than 1 or greater than
      !>         the highest index in the join row set union being addressed,
      !>         the error SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     1)  This routine uses the EK scratch area, which employs a scratch
      !>         DAS file.
      !>
      !>## Particulars
      !>
      !>     See header of umbrella routine ZZEKVADR.
      !>
      !>## Examples
      !>
      !>     See the $Examples section of ZZEKVADR.
      !>
      !>## Restrictions
      !>
      !>     1)  ZZEKVSET must be called before this routine is called for the
      !>         first time.
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
      !>-    SPICELIB Version 1.0.1, 06-SEP-2006 (NJB)
      !>
      !>        Filled in Particulars section of header.  Changed
      !>        previous version line's product from "Beta" to "SPICELIB."
      !>
      !>-    SPICELIB Version 1.0.0, 22-SEP-1994 (NJB) (WLT)
      module subroutine zzekvcal ( ctx, rwvidx, rwvbas, sgvbas )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlei
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: rwvidx  !! Index of row vector.
      integer(ip), intent(out) :: rwvbas  !! EK scratch area base address of row vector.
      integer(ip), intent(out) :: sgvbas  !! Base address of parent segment vector.
      integer(ip) :: sgvbas_1(1)  !! one-element copy of sgvbas (array argument)
      logical :: return
      integer(ip) :: reloff
      integer(ip) :: jrsidx
      if ( .not. allocated ( ctx%zzekvadr_state ) ) allocate ( ctx%zzekvadr_state )
      if ( .not. allocated ( ctx%zzekvadr_state%begidx ) ) then
         allocate ( ctx%zzekvadr_state%begidx(1), ctx%zzekvadr_state%rbas(1), &
                    ctx%zzekvadr_state%svbas(1), source = 0_ip )
      end if
      associate ( begidx => ctx%zzekvadr_state%begidx, &
                  maxrwv => ctx%zzekvadr_state%maxrwv, &
                  ntabs => ctx%zzekvadr_state%ntabs, &
                  rbas => ctx%zzekvadr_state%rbas, &
                  svbas => ctx%zzekvadr_state%svbas, &
                  svnjrs => ctx%zzekvadr_state%svnjrs )
      if (  ( rwvidx < 1 ) .or. ( rwvidx > maxrwv )  ) then

         call ctx%setmsg ( 'Row vector index was #; valid range is 0:#' )
         call ctx%errint ( '#',  rwvidx                                 )
         call ctx%errint ( '#',  maxrwv                                 )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                        )
         return

      end if

!
!     Identify the join row set containing the indicated row.  Our error
!     check guarantees a non-zero result.
!
      jrsidx  =  lstlei ( rwvidx, svnjrs, begidx )

!
!     Compute the offset of the indicated row vector relative to the
!     first row vector in the parent join row set.  This offset is one
!     less than the relative index of the row vector, multiplied by
!     the augmented row vector size.
!
      reloff  =  ( rwvidx - begidx(jrsidx) ) * ( ntabs + 1 )

!
!     Find the base address of the row vector.
!
      rwvbas  =  rbas(jrsidx) + reloff

!
!     Compute the base address of the parent segment vector.  The base-
!     relative address of the segment vector is stored at the end of the
!     row vector.
!
      call zzeksrd ( ctx, rwvbas+ntabs+1, rwvbas+ntabs+1, sgvbas_1 )
      sgvbas = sgvbas_1(1)

      sgvbas  =  svbas(jrsidx) + sgvbas

      return
      end associate
      end subroutine zzekvcal

      !> Return the forward data pointer for a specified EK data page.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TYPE           is the data type of the desired page.
      !>
      !>     P              is the page number of the allocated page.  This
      !>                    number is recognized by the EK paged access
      !>                    routines.
      !>
      !>## Detailed Output
      !>
      !>     FWARD          is a forward data pointer.  This is the number
      !>                    of a data page on which the last column entry
      !>                    on page P is continued.
      !>
      !>                    FWARD may overwrite P.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If TYPE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     3)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine returns the forward data pointer of the specified EK
      !>     data page.  The value of the pointer is a page number.
      !>
      !>## Examples
      !>
      !>     See ZZEKDE03.
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
      !>-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
      module subroutine zzekgfwd ( ctx, handle, type, p, fward )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekdatpag
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: type  !! Data type of page.
      integer(ip), intent(in) :: p  !! Page number.
      integer(ip), intent(out) :: fward  !! Forward data pointer.
      integer(ip) :: fward_1(1)  !! one-element copy of fward (array argument)
      real(wp) :: dpptr_1(1)  !! one-element copy of dpptr (array argument)
!
!     Local variables
!
      real(wp) :: dpptr
      integer(ip) :: base

!
!     Look up the base address of the page.
!
      call zzekpgbs ( ctx, type, p, base )

      if ( ctx%failed() ) return

      if ( type == chr ) then
!
!        Get the encoded pointer.
!
         call zzekgei ( ctx, handle, base+cfpidx, fward )

      else if ( type == dp ) then
!
!        Convert the d.p. pointer to integer type.
!
         call dasrdd ( ctx, handle, base+dfpidx, base+dfpidx, dpptr_1 )
         dpptr = dpptr_1(1)

         fward  =  nint ( dpptr )

      else
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, ZZEKPGBS would have complained.
!
         call dasrdi ( ctx, handle, base+ifpidx, base+ifpidx, fward_1 )
         fward = fward_1(1)

      end if

      return
      end subroutine zzekgfwd

      !> Return the link count for a specified EK data page.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TYPE           is the data type of the desired page.
      !>
      !>     P              is the page number of the allocated page.  This
      !>                    number is recognized by the EK paged access
      !>                    routines.
      !>
      !>## Detailed Output
      !>
      !>     NLINKS         is the currently held number of links to the
      !>                    specified data page.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If TYPE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     3)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine centralizes EK data page link count accesses.
      !>
      !>## Examples
      !>
      !>     See EKDELR.
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
      !>-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
      module subroutine zzekglnk ( ctx, handle, type, p, nlinks )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekdatpag
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: type  !! Data type of page.
      integer(ip), intent(in) :: p  !! Page number.
      integer(ip), intent(out) :: nlinks  !! Number of links to page.
      integer(ip) :: nlinks_1(1)  !! one-element copy of nlinks (array argument)
      real(wp) :: dplnk_1(1)  !! one-element copy of dplnk (array argument)
!
!     Local variables
!
      real(wp) :: dplnk
      integer(ip) :: base

!
!     Look up the base address of the page.
!
      call zzekpgbs ( ctx, type, p, base )

      if ( ctx%failed() ) return

      if ( type == chr ) then
!
!        Look up the encoded count.
!
         call zzekgei ( ctx, handle, base+clcidx, nlinks )

      else if ( type == dp ) then
!
!        Convert the encoded count to integer type.
!
         call dasrdd ( ctx, handle, base+dlcidx, base+dlcidx, dplnk_1 )
         dplnk = dplnk_1(1)

         nlinks  =  nint ( dplnk )

      else
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, ZZEKPGBS would have complained.
!
         call dasrdi ( ctx, handle, base+ilcidx, base+ilcidx, nlinks_1 )
         nlinks = nlinks_1(1)

      end if

      return
      end subroutine zzekglnk

      !> Convert the WHERE clause of an EK query to a normalized form.
      !>
      !>## Detailed Input
      !>
      !>     QUERY          is an EK query to be parsed.  The tokens of the
      !>                    query have been found already.  See the header
      !>                    of the subroutine EKFIND for a detailed
      !>                    description of the EK query language.
      !>
      !>     NTOKEN         is the number of tokens in the input query.
      !>
      !>     LXBEGS,
      !>     LXENDS         are lexeme begin and end pointers; the Ith
      !>                    lexeme in the query is
      !>
      !>                       QUERY ( LXBEGS(I) : LXENDS(I) )
      !>
      !>                    (Lexemes are strings that correspond to tokens
      !>                     in the language.)
      !>
      !>     TOKENS         is an array of token codes.  The Ith element of
      !>                    TOKENS represents the Ith token in the scanned
      !>                    query.
      !>
      !>     VALUES         is an array of values associated with tokens; the
      !>                    Ith element of VALUES corresponds to the Ith
      !>                    token.  Keywords, for example, are distinguished
      !>                    by codes in the VALUES array.  Literal numeric
      !>                    and string tokens use the VALUES array to point
      !>                    to elements of NUMVLS or CHBEGS and CHENDS,
      !>                    respectively.  Some tokens don't need to use
      !>                    VALUES, but to simplify indexing, each token gets
      !>                    an element of this array.
      !>
      !>     NUMVLS         is an array of double precision numbers used to
      !>                    store the values corresponding to literal numeric
      !>                    tokens.
      !>
      !>     CHRBUF         is a string used to store the values of literal
      !>                    string tokens.
      !>
      !>     CHBEGS,
      !>     CHENDS         are pairs of begin and end pointers into CHRBUF.
      !>                    These pointers delimit character values
      !>                    associated with literal string tokens.
      !>
      !>## Detailed Output
      !>
      !>     EQRYI,
      !>     EQRYC,
      !>     EQRYD          are the integer, character, and numeric portions
      !>                    of an encoded form of the input query.  The WHERE
      !>                    clause of the input query is represented in this
      !>                    encoding.  The WHERE clause constraints have been
      !>                    normalized.
      !>
      !>                    Normalized queries have their constraints grouped
      !>                    into a disjunction of conjunctions of relational
      !>                    expressions, as symbolized below:
      !>
      !>                          ( <rel_exp_1_1> and <rel_exp_1_2> and ... )
      !>                       or ( <rel_exp_2_1> and <rel_exp_2_2> and ... )
      !>                                     .
      !>                                     .
      !>                                     .
      !>                       or ( <rel_exp_N_1> and <rel_exp_N_2> and ... )
      !>
      !>     ERROR,
      !>     PRSERR         are, respectively, a flag indicating whether the
      !>                    input query parsed correctly, and a message
      !>                    describing the parse error, if one occurred.  If
      !>                    no error occurred, ERROR is .FALSE. and PRSERR
      !>                    is blank.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     No matter how ridiculous the query passed to ZZEKNRML, the
      !>     routine diagnoses errors via the output arguments ERROR and
      !>     PRSERR.  No errors are signaled.  The possible error messages
      !>     returned by this routine are:
      !>
      !>        Conjunction table is full.
      !>
      !>        Disjunction table is full.
      !>
      !>        Empty WHERE clause.
      !>
      !>        Missing WHERE keyword.
      !>
      !>        More tokens expected.
      !>
      !>        NULL values are not allowed in BETWEEN or NOT BETWEEN clauses.
      !>
      !>        NULL values can only be used with the operators
      !>        "IS NULL", "NOT NULL" or equivalents.
      !>
      !>        Relation table is full.
      !>
      !>        Stack is full.
      !>
      !>        Syntax error:  badly formed WHERE clause.
      !>
      !>        Token following BETWEEN operator is invalid.
      !>
      !>        Token following NOT operator was invalid.
      !>
      !>        Token must be followed by a comparison operator.
      !>
      !>        Token must be followed by the AND operator.
      !>
      !>        Token sequence must be followed by a value.
      !>
      !>        Tokens were missing from comparison relation.
      !>
      !>        Tokens were missing from logical expression.
      !>
      !>        Too few tokens in WHERE clause.
      !>
      !>        Too many tokens in query; max allowed is #.
      !>
      !>        Unexpected keyword # found at location #.
      !>
      !>        Unexpected right parenthesis found.
      !>
      !>        Unexpected token # found at location #.
      !>
      !>        Unexpected token found following valid expression.
      !>
      !>        Unexpected token found.
      !>
      !>        WHERE clause ran out of tokens unexpectedly.
      !>        This may be due to an extra left parenthesis.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Here is the grammar for the EK WHERE clause:
      !>
      !>        <WHERE clause>           =>   WHERE <relational expression>
      !>
      !>        <relational expression>  =>   <simple expression>
      !>
      !>                                      <NULL value expression>
      !>
      !>                                    | NOT <relational expression>
      !>
      !>                                    |   ( <relational expression> )
      !>
      !>                                    |     <relational expression>
      !>                                      AND <relational expression>
      !>
      !>                                    |     <relational expression>
      !>                                      OR  <relational expression>
      !>
      !>        <simple expression>      =>   <LHS> <operator> <RHS>
      !>
      !>                                    | <LHS> BETWEEN     <RHS> AND <RHS>
      !>
      !>                                    | <LHS> NOT BETWEEN <RHS> AND <RHS>
      !>
      !>        <NULL value expression>  =>   <LHS> <Null operator> NULL
      !>
      !>        <LHS>                    =>   <name>
      !>
      !>        <RHS>                    =>   <name>
      !>                                    | <value>
      !>
      !>        <name>                   =>   <identifier> . <identifier>
      !>                                    | <identifier>
      !>
      !>        <operator>               =>   EQ
      !>                                    | GE
      !>                                    | GT
      !>                                    | LE
      !>                                    | LT
      !>                                    | NE
      !>                                    | LIKE
      !>                                    | NOT LIKE
      !>                                    | =
      !>                                    | >=
      !>                                    | >
      !>                                    | <=
      !>                                    | <
      !>                                    | !=
      !>                                    | <>
      !>
      !>        <NULL operator>         =>    IS
      !>                                    | IS NOT
      !>                                    | EQ
      !>                                    | NE
      !>                                    | =
      !>                                    | !=
      !>                                    | <>
      !>
      !>        <value>                 =>    <character value>
      !>                                    | <d.p. value>
      !>                                    | <integer value>
      !>
      !>## Examples
      !>
      !>     1)  This routine breaks down the constraints of the WHERE clause
      !>
      !>             WHERE        ( ( COL1 EQ VAL1 ) OR ( COL2 NE VAL2 ) )
      !>                     AND  ( ( COL3 LE VAL3 ) OR ( COL4 GT VAL4 ) )
      !>
      !>         as
      !>
      !>                          (  ( COL1 EQ VAL1 ) AND ( COL3 LE VAL3 ) )
      !>                     OR   (  ( COL1 EQ VAL1 ) AND ( COL4 GT VAL4 ) )
      !>                     OR   (  ( COL2 NE VAL2 ) AND ( COL3 LE VAL3 ) )
      !>                     OR   (  ( COL2 NE VAL2 ) AND ( COL4 GT VAL4 ) )
      !>
      !>     2)  This routine breaks down the constraints of the WHERE clause
      !>
      !>             WHERE  NOT ( ( COL1 EQ VAL1 ) OR ( COL2 NE VAL2 ) )
      !>
      !>          as
      !>                          ( COL1 NE VAL1 ) AND ( COL3 EQ VAL3 )
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
      !>-    SPICELIB Version 3.0.1, 02-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 3.0.0, 17-NOV-1995 (NJB)
      !>
      !>        Significantly re-written for architecture 3.
      module subroutine zzeknrml ( ctx, query, ntoken, lxbegs, lxends, tokens, values, numvls, chrbuf, chbegs, chends, eqryi, eqryc, eqryd, error, prserr )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: repmc_inplace, repmi_inplace
      use pepper_inc_ekkeyw
      use pepper_inc_ekopcd
      use pepper_inc_ekqlimit
      use pepper_inc_ekquery
      use pepper_inc_ektokn
      use pepper_inc_ektype
      use pepper_containers, only: grow, grow_cell, grown_capacity, lnkgrow
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: query  !! Input EK query.
      integer(ip), intent(in) :: ntoken  !! Number of tokens in query.
      integer(ip), intent(in) :: lxbegs ( * )  !! Start positions of lexemes comprising WHERE clause.
      integer(ip), intent(in) :: lxends ( * )  !! End positions of lexemes comprising WHERE clause.
      integer(ip), intent(in) :: tokens ( * )  !! Tokens comprising query.
      integer(ip), intent(in) :: values ( * )  !! Values associated with tokens.
      real(wp), intent(in) :: numvls ( * )  !! Buffer containing numeric token values.
      character(*), intent(in) :: chrbuf  !! Buffer containing string token values.
      integer(ip), intent(in) :: chbegs ( * )  !! String token begin and end character positions.
      integer(ip), intent(in) :: chends ( * )  !! String token begin and end character positions.
      integer(ip), allocatable, intent(inout) :: eqryi ( : )  !! Parsed query and string and number value buffers. PEPPER: allocatable, lower bound LBCELL; grows as the query is encoded.
      character(*), intent(out) :: eqryc  !! Parsed query and string and number value buffers.
      real(wp), intent(out) :: eqryd  ( * )  !! Parsed query and string and number value buffers.
      logical, intent(out) :: error  !! Parse error flag.
      character(*), intent(out) :: prserr  !! Parse error message.
!
!     Local parameters
!
      integer(ip), parameter :: nil = 0
!
!     LBPOOL is the lower bound of the second index of a linked list
!     pool array.
!
!
!     Operator parameters:
!
!     NLOGOP is the number of recognized logical operators.  These
!     are AND, OR, and NOT.
!
      integer(ip), parameter :: nlogop = 3
!
!     NRELOP is the number of arithmetic and character comparison
!     operators.
!
      integer(ip), parameter :: nrelop = 8
!
!     Meta-token codes, excluding codes for relational operators:
!
      integer(ip), parameter :: rgroup = -1
      integer(ip), parameter :: lgroup = rgroup - 1
      integer(ip), parameter :: btween = lgroup - 1
      integer(ip), parameter :: notbtw = btween - 1
      integer(ip), parameter :: btwexp = notbtw - 1
      integer(ip), parameter :: name = btwexp - 1
      integer(ip), parameter :: ident = name   - 1
      integer(ip), parameter :: value = ident  - 1
      integer(ip), parameter :: period = value  - 1
      integer(ip), parameter :: and = period - 1
      integer(ip), parameter :: or = and    - 1
      integer(ip), parameter :: not = or     - 1
      integer(ip), parameter :: expr = not    - 1
!
!     Number of keywords that can terminate a WHERE clause.
!
      integer(ip), parameter :: nendkw = 3
!
!     State parameters:
!
      integer(ip), parameter :: push = 0
      integer(ip), parameter :: pop = push   + 1
      integer(ip), parameter :: parse = pop    + 1
      integer(ip), parameter :: reducd = parse  + 1
      integer(ip), parameter :: term = reducd + 1
!
!     'Pop condition' codes:
!
      integer(ip), parameter :: none = 0
      integer(ip), parameter :: reduce = 1
      integer(ip), parameter :: endgrp = reduce + 1
!
!     Token descriptor size:
!
      integer(ip), parameter :: dscsiz = eqvdsz + 1
!
!     Note:  there is no "UNLIKE" keyword, but there is an UNLIKE
!     operator, which is the complement of the LIKE operator.
!
      integer(ip), parameter :: endkw  ( nendkw ) = [ kwfrom, kwordr, kwsel ]
      integer(ip), parameter :: cmpcde ( nrelop     ) = [ eq, ge, gt, le, lt, ne, like, unlike ]
      integer(ip), parameter :: cmpneg ( nrelop     ) = [ ne, lt, le, gt, ge, eq, unlike, like ]
      integer(ip), parameter :: cmpops ( nrelop - 1 ) = [ kweq, kwge, kwgt, kwle, kwlt, kwne, kwlike ]
!
!     Initial values
!
      integer(ip), parameter :: logcde ( nlogop     ) = [ &
         and, or, not ]
      integer(ip), parameter :: logops ( nlogop     ) = [ &
         kwand, kwor, kwnot ]
!
!     Other local variables
!
      integer(ip) :: colptr
      integer(ip) :: metahd
      logical :: donow
      integer(ip) :: fourth
      integer(ip) :: j
      integer(ip) :: lxe
      integer(ip) :: e
      integer(ip) :: whrend
      integer(ip) :: i
      integer(ip) :: head1
      integer(ip) :: tail
      integer(ip) :: head2
      integer(ip) :: third
      integer(ip) :: prev
      integer(ip) :: whrsiz
      integer(ip) :: cjnode
      integer(ip) :: newrel
      integer(ip) :: djnode
      integer(ip) :: rhsptr
      integer(ip) :: b
      integer(ip) :: op
      integer(ip) :: k
      integer(ip) :: tabptr
      integer(ip) :: whrbeg
      integer(ip) :: endloc
      integer(ip) :: nrels
      integer(ip) :: djtail
      integer(ip) :: type
      integer(ip) :: nconj
      integer(ip) :: next
!
!     Stack variables
!
!     These variables have the following meanings:
!
!        MSTART is the node number of the first meta-token of
!        the current expression being parsed.
!
!        NMETA is the number of meta-tokens in the query.
!
!        POPCND is the `pop condition'.  This is a code indicating
!        what event must occur to trigger popping the current state.
!        The two events that can cause the state to be popped are
!        the execution of a reduction and encountering a right grouper.
!
      integer(ip) :: nmeta
      integer(ip) :: lxb
      logical :: qual
      integer(ip) :: relptr
      integer(ip) :: newcj
      integer(ip) :: node
      logical :: fnd
      integer(ip) :: exprhd
      integer(ip) :: skip
!
!     Each comparison relation is expressed by three tokens, so the
!     comparison relations are represented by a 3 x MAXREL array.  The
!     first and third elements of each row of RELS are array indices
!     that point into the input array TOKENS; the middle element
!     of each row is an operator code.  The set of triples representing
!     comparison relations is indexed by a doubly linked list pool.
!     Each conjunction of comparison relations is represented by a
!     linked list of pointers to entries in the RELS array.  These
!     pointers are contained in the CJPTRS array.  The pointers are
!     linked via entries in the double linked list pool CJPOOL.
!
!     PEPPER: the work arrays are local and sized from the number of
!     tokens (SPICELIB: saved arrays for MAXTOK = 500 tokens and 10*MAXTOK
!     relations, conjunctions, disjunctions and descriptors, 0.4 MB per
!     context); the linked list pools grow when they are full (SPICELIB:
!     the parse errors "Relation table is full.", "Conjunction table is
!     full.", "Disjunction table is full."), and the stack has room for
!     the deepest nesting the tokens allow (SPICELIB: 500).
!
      integer(ip), allocatable :: rels ( :, : )  !! each comparison relation: token pointer, operator code, token
      integer(ip), allocatable :: rlpool ( :, : )  !! linked list pool of the relations (rels)
      integer(ip), allocatable :: cjpool ( :, : )  !! linked list pool of the conjunctions (one list per conjunction)
      integer(ip), allocatable :: cjptrs ( : )  !! relation (index in rels) of each conjunction list node
!
!     Each normalized expression is a disjunction of conjunctions.  Each
!     such disjunction is represented by a linked list of nodes
!     associated with pointers to entries in the CJPOOL array.  DJPTRS
!     is the parallel array used to associate each node of a disjunction
!     with the head node of a conjunction list in CJPOOL.
!
      integer(ip), allocatable :: djpool ( :, : )  !! linked list pool of the disjunctions (one per expression)
      integer(ip), allocatable :: djptrs ( : )  !! head of the conjunction list (cjpool) of each disjunction node
!
!     Meta-tokens are groups of tokens that comprise syntactic units
!     in a query.  Each symbol that appears on the left hand side of
!     a production rule in the grammar corresponds to a type of
!     meta-token.
!
!     Throughout the parsing process, the meta-tokens representing the
!     query are organized as a linked list.  Each meta-token is also
!     associated with a more detailed classification MTCODE.
!
!     For each meta-token that represents an identifier, a value,
!     a name, or an expression, there is a corresponding element of
!     MTEXPR.  This element contains a pointer to a token or to a
!     normalized expression.  In the latter case, the pointer is the
!     head node of a list in the disjunction table.
!
      integer(ip), allocatable :: mtpool ( :, : )  !! linked list pool of the meta-tokens of the query
      integer(ip), allocatable :: mtcode ( : )  !! classification code of each meta-token
      integer(ip), allocatable :: mtexpr ( : )  !! token, descriptor or disjunction list pointer of each meta-token
      integer(ip), allocatable :: mstart ( : )  !! the first meta-token node at each level of the normalization stack
      integer(ip), allocatable :: popcnd ( : )  !! pop condition (reduction or right grouper) of each stack level
      integer(ip), allocatable :: dspool ( :, : )  !! linked list pool of the descriptors in dscbuf
      integer(ip), allocatable :: dscbuf ( :, : )  !! descriptor and type code (value or ident) of each value/identifier
      integer(ip), allocatable :: relset ( : )  !! set of the relations of an expression to negate, so each is negated once
      integer(ip), allocatable :: sizes ( : )  !! number of relations of each conjunction of the normalized expression
      integer(ip) :: cj (4)  !! conjunction list nodes made for a BETWEEN or NOT BETWEEN relation
      integer(ip) :: dj (2)  !! disjunction list nodes being made or joined
      integer(ip) :: rel (4)  !! relations made for a BETWEEN or NOT BETWEEN relation
      integer(ip) :: first  !! node of the first meta-token of the current expression
      integer(ip) :: level  !! current level of the parser's state stack
      integer(ip) :: newdj  !! disjunction list node just made
      integer(ip) :: retcnd  !! pop condition of the stack level to push
      integer(ip) :: second  !! node of the second meta-token of the current expression
      integer(ip) :: start  !! node of the first meta-token of the subexpression to push
      integer(ip) :: state  !! state of the parser (push, pop, parse, reduced or terminated)
!
!     Stack parameters:
!
      integer(ip) :: maxstk  !! size of the stack
!
!     MAXREL is the maximum number of relations that can be handled
!     by this routine.
!
      integer(ip) :: maxrel  !! initial size of the relation, conjunction, disjunction and descriptor pools
      integer(ip) :: mtsize  !! size of the meta-token pool

      mtsize = max ( ntoken, 1_ip )
      maxrel = 10 * mtsize
      maxstk = mtsize + 2
      allocate ( rels(3, maxrel), cjptrs(maxrel), djptrs(maxrel), &
                 mtcode(mtsize), mtexpr(mtsize), mstart(maxstk), popcnd(maxstk), &
                 dscbuf(dscsiz, maxrel), source = 0_ip )
      allocate ( rlpool(2, lbpool:maxrel), cjpool(2, lbpool:maxrel), djpool(2, lbpool:maxrel), &
                 dspool(2, lbpool:maxrel), mtpool(2, lbpool:mtsize), source = 0_ip )
      allocate ( relset(lbcell:maxrel), source = 0_ip )
      allocate ( sizes(0) )
      cj = 0
      dj = 0
      rel = 0
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     No error at this point.
!
      error  = .false.
      prserr = ' '

!
!     Find out the start and end indices of the tokens comprising the
!     WHERE clause.  If there are no tokens in the WHERE clause, we may
!     as well go home.
!
      call zzektloc ( ctx, tkkey,   kwwher,  ntoken,  tokens,  values, &
                      whrbeg,  fnd                                )

      whrbeg  =  whrbeg + 1

      if ( .not. fnd ) then
         error   =  .true.
         prserr  =  'Missing WHERE keyword.'
         return
      end if

!
!     The WHERE clause is terminated by the end of the query or by
!     the first keyword of the set {SELECT, FROM, ORDER} that follows
!     the WHERE keyword.
!
      whrend  =  ntoken

      do i = 1, 3

         call zzektloc ( ctx, tkkey,   endkw(i),  ntoken,  tokens,  values, &
                         endloc,  fnd                                 )

         if ( fnd ) then

            if (       ( endloc < whrend ) &
                 .and. ( endloc > whrbeg )  ) then

               whrend  =  endloc-1
            end if

         end if

      end do

      whrsiz  =  whrend  -  whrbeg  +  1

      if ( whrsiz == 0 ) then
         error   =  .true.
         prserr  =  'Empty WHERE clause.'
         return
      end if

!
!     Initialize the pools.
!
      call lnkini ( ctx, maxrel, rlpool )
      call lnkini ( ctx, maxrel, cjpool )
      call lnkini ( ctx, maxrel, djpool )
      call lnkini ( ctx, mtsize, mtpool )
      call lnkini ( ctx, maxrel, dspool )

!
!     Loop through our token list and classify the tokens.  Initialize
!     the meta-token list.
!
      nmeta  =  0
      tail   =  nil

      i      =  whrbeg

      do while ( i <= whrend )
!
!        Allocate a node and link it in at the tail of the meta-token
!        list.
!
         call lnkan  ( ctx, mtpool, node         )
         call lnkila ( ctx, tail,   node, mtpool )
         tail         = node

!
!        Each meta-token's expression pointer points to its original
!        token index, by default.
!
         mtexpr(node) = i

         if ( tokens(i) == tklpar  ) then

            mtcode(node) = lgroup

         else if ( tokens(i) == tkrpar ) then

            mtcode(node) = rgroup

         else if (       ( tokens(i) == tkint  ) &
                   .or.  ( tokens(i) == tkdp   )  ) then
!
!           Numeric values must be added to the encoded query.  We
!           allocate a descriptor from the descriptor pool for
!           each identifier.  The expression pointer for the
!           identifier points to the descriptor.  Note:  the
!           allocation should be safe, since we've checked the total
!           number of tokens in the query.
!
            mtcode(node) = value

            if ( lnknfn( ctx,dspool) < 1 ) call more_descriptors ( )
            call lnkan ( ctx, dspool, mtexpr(node) )

            if ( tokens(i) == tkint ) then
               type = int
            else
               type = dp
            end if

            call zzekinqn ( ctx, numvls(values(i)), &
                            type, &
                            lxbegs(i), &
                            lxends(i), &
                            eqryi, &
                            eqryd, &
                            dscbuf( 1, mtexpr(node) )  )

!
!           Set the descriptor to indicate that it represents a value.
!
            dscbuf( dscsiz, mtexpr(node) )  =  value

         else if ( tokens(i) == tkqstr ) then
!
!           The treatment of strings is analogous to that of numbers.
!
            mtcode(node) = value

            if ( lnknfn( ctx,dspool) < 1 ) call more_descriptors ( )
            call lnkan ( ctx, dspool, mtexpr(node) )

            b  =  chbegs( values(i) )
            e  =  chends( values(i) )

            call zzekinqc ( ctx,  chrbuf(b:e), &
                             e - b + 1, &
                             lxbegs(i), &
                             lxends(i), &
                             eqryi, &
                             eqryc, &
                             dscbuf( 1, mtexpr(node) )  )

!
!           Set the descriptor to indicate that it represents a value.
!
            dscbuf( dscsiz, mtexpr(node) )  =  value

         else if ( tokens(i) == tkid ) then
!
!           Identifiers must be added to the encoded query.  We
!           allocate a descriptor from the descriptor pool for
!           each identifier.  The expression pointer for the
!           identifier points to the descriptor.
!
            mtcode(node) = ident

            if ( lnknfn( ctx,dspool) < 1 ) call more_descriptors ( )
            call lnkan ( ctx, dspool, mtexpr(node) )

            b  =  chbegs( values(i) )
            e  =  chends( values(i) )

            call zzekinqc ( ctx,  chrbuf(b:e), &
                             e - b + 1, &
                             lxbegs(i), &
                             lxends(i), &
                             eqryi, &
                             eqryc, &
                             dscbuf( 1, mtexpr(node) )  )

!
!           Set the descriptor to indicate that it represents an
!           identifier.
!
            dscbuf( dscsiz, mtexpr(node) )  =  ident

         else if ( tokens(i) == tkdot ) then

            mtcode(node) = period

         else if ( tokens(i) == tkkey  ) then
!
!           We have a keyword.  Identify it and locate the corresponding
!           code.
!
            j  =  findloc(logops, values(i), dim=1)
            k  =  findloc(cmpops(1:nrelop-1), values(i), dim=1)

            if ( j > 0 ) then
!
!              We have a logical operator, unless we have the NOT LIKE
!               or NOT BETWEEN sequence.
!
               if ( logcde(j) /= not ) then

                  mtcode(node) = logcde(j)

               else

                  if ( i <= whrend ) then

                     if (       ( tokens(i+1) == tkkey  ) &
                          .and. ( values(i+1) == kwlike )  ) then
!
!                       Replace the NOT LIKE sequence with the
!                       UNLIKE operator.  Skip over the LIKE token.
!
                        mtcode(node)  =  unlike
                        i             =  i + 1

                     else if (       ( tokens(i+1) == tkkey  ) &
                               .and. ( values(i+1) == kwbetw )  ) then
!
!                       Replace the NOT BETWEEN sequence with the
!                       NOTBTW operator.  Skip over the BETWEEN token.
!
                        mtcode(node)  =  notbtw
                        i             =  i + 1

                     else
                        mtcode(node)  =  not
                     end if

                  else
                     mtcode(node) = not
                  end if

               end if

            else if ( k > 0 ) then
               mtcode(node) = cmpcde(k)

            else if ( values(i) == kwbetw ) then
               mtcode(node) = btween

            else if ( values(i) == kwis ) then
!
!              The token IS translates to EQ; the token sequence
!              IS NOT translates to NE.
!
               if ( i < whrend ) then

                  if (       ( tokens(i+1) == tkkey ) &
                       .and. ( values(i+1) == kwnot )  ) then
!
!                    We have an IS NOT sequence.  Skip over the NOT
!                    token; indicate the sequence with a single NE
!                    meta-token.
!
                     mtcode(node) = ne
                     i            = i + 1
                  else
                     mtcode(node) = eq
                  end if

               else
                  mtcode(node) = eq
               end if

            else if ( values(i) == kwnull ) then
!
!              The expression pointer for null values is NIL.
!
               mtcode(node) = value
               mtexpr(node) = nil

            else
!
!              Sorry, that was the last chance for valid keywords.
!
               lxb     =  lxbegs(i)
               lxe     =  lxends(i)
               error   =  .true.
               prserr  =  'Unexpected keyword # found at location #.'
               call repmc_inplace ( prserr, '#', query(lxb:lxe) )
               call repmi_inplace ( prserr, '#', lxb )
               return

            end if

         else
!
!           Sorry, that was the last chance, period.
!
            lxb     =  lxbegs(i)
            lxe     =  lxends(i)
            error   =  .true.
            prserr  =  'Unexpected token # found at location #.'
            call repmc_inplace ( prserr, '#', query(lxb:lxe) )
            call repmi_inplace ( prserr, '#', lxb )
            return

         end if
!
!        At this point, we've classified the Ith token.  MTCODE(NODE)
!        is the meta-token code for this token.
!
         i     = i     + 1
         nmeta = nmeta + 1

      end do

!
!     Initialize the head of the meta-token list.
!
      metahd = lnkhl( ctx, tail, mtpool )

!
!     Filter out extraneous parentheses around column names or
!     values.
!
      node = metahd

      do while ( node > 0 )

         if (      ( mtcode(node) == name  ) &
              .or. ( mtcode(node) == value )  )  then
!
!           If the current metatoken is bracketed by parentheses,
!           remove them and update the metatoken count accordingly.
!
            prev  =  lnkprv( ctx, node, mtpool )
            next  =  lnknxt( ctx, node, mtpool )

            if (  ( prev > 0 ) .and. ( next > 0 )  ) then

               if (       ( mtcode( prev ) == lgroup ) &
                    .and. ( mtcode( next ) == rgroup )   ) then

                  call lnkfsl ( ctx, prev, prev, mtpool )
                  call lnkfsl ( ctx, next, next, mtpool )
                  metahd  =   lnkhl ( ctx, node, mtpool )
                  nmeta   =   nmeta - 2
!
!                 We don't advance the current token in this case
!                 because there may be more parentheses to remove.
!
               else
!
!                 This token is not bracketed by parentheses; look at
!                 the next metatoken.
!
                  node  =  next

               end if

            else
!
!              This token is not bracketed by tokens on both sides; look
!              at the next metatoken.  It's ok for the next token to be
!              NIL.
!
               node  =  next

            end if

         else
!
!           The current token is not a name or value; look at the next
!           token.
!
            node  =  lnknxt( ctx, node, mtpool )

         end if

      end do

!
!     Now it's time to parse our expression.  We will validate the
!     expression by using our grammar rules to condense groups of
!     meta-tokens that correspond to the right-hand sides of grammatical
!     rules into meta-tokens that correspond to the left-hand sides
!     of those same rules.  Each such application of a grammar rule
!     is called a `reduction.'  When we're left with a single
!     meta-token of type <relational expression>, we're done.
!
!     If, before reaching the desired final state, we get to a point
!     where no reductions can be performed, we have a syntax error.
!
!     As parsing advances, we'll start to get meta-tokens that are
!     logical expressions.  Each logical expression will be represented
!     by a data structure that organizes the expression in a way that
!     we'll refer to as `normalized':  the expression will be
!     represented as a disjunction of conjunctions, for example
!
!        ( A AND B AND C ) OR ( D AND E ) OR ( F ) OR ( G AND H AND I )
!
!     Each metatoken that represents a logical expression will
!     refer to it through a pointer which is a member of the MTEXPR
!     array.
!

      if ( whrsiz < 3 ) then

         error  = .true.
         prserr = 'Too few tokens in WHERE clause.'
         return

      else

         level          =  1
         mstart(level)  =  metahd
         popcnd(level)  =  none
         state          =  parse

      end if

      do while ( state /= term )

         if ( state == parse ) then
!
!           If the input query is valid, we're looking at the leftmost
!           meta-token of an expression that matches the right-hand
!           side of one of the grammar rules.  Referring back to the
!           rules, we see that there are only a few meta-tokens that are
!           valid as the first token of such an expression:
!
!              - A left grouper
!              - An identifier
!              - A name
!              - An expression
!              - A unary operator (`NOT' )
!
!           We'll see if we can perform a reduction.  The reductions
!           that are possible depend on how many meta-tokens are
!           present in the expression we're looking at.
!
!           FIRST is the node number of the first meta-token to look
!           at, in an attempt to perform a reduction.  SECOND, THIRD,
!           and FOURTH have the obvious meanings; some of these may
!           be 0.
!
            first   =  mstart ( level )

            if ( first > 0 ) then
               second  =  lnknxt ( ctx, first,  mtpool )
            else
               second  =  nil
            end if

            if ( second > 0 ) then
               third   =  lnknxt ( ctx, second, mtpool )
            else
               third   =  nil
            end if

            if ( third > 0 ) then
               fourth  =  lnknxt ( ctx, third,  mtpool )
            else
               fourth  =  nil
            end if

            if ( first <= 0 ) then
!
!              This never happens to good commands.
!
               error  = .true.
               prserr = 'WHERE clause ran out of tokens '            // &
                        'unexpectedly.  This may be due to an '      // &
                        'extra left parenthesis.'
               return

            end if

!
!           We have at least one meta-token to work with.  We'll
!           take different actions depending on its type.
!

            if ( mtcode(first) == ident ) then
!
!              This is a simple case to deal with:  in valid queries,
!              we have either the sequence
!
!                 <identifier> . <identifier>
!
!              or
!
!                 <identifier>
!
!              Both of these token sequences represent a column name;
!              in the former case, the name is qualified by a table
!              name, in the latter, the column name is unqualified.
!              If the table name is absent, we'll simply save a null
!              descriptor for it.  The descriptors will be linked, with
!              the table descriptor coming first, and the NAME token
!              resulting from reducing this token sequence will point to
!              the list of descriptors via the MTEXPR pointer.
!
               if ( third > 0 ) then
!
!                 We can look at the following two tokens.
!
                  if (       ( mtcode(second) == period ) &
                       .and. ( mtcode(third)  == ident  )  ) then

                     qual  =  .true.
                  else
                     qual  =  .false.
                  end if

               else
!
!                 There aren't enough tokens for this name to be
!                 qualified.
!
                  qual  =  .false.

               end if

               if ( qual ) then
!
!                 We have a fully qualified column name.  Hook up the
!                 table and column name descriptors.
!
                  tabptr  =  mtexpr(first)
                  colptr  =  mtexpr(third)

                  call lnkila ( ctx, tabptr, colptr, dspool )

!
!                 Reduce the expression to a <name> metatoken.
!
                  mtcode(first)  =  name

                  call lnkfsl( ctx, second, third, mtpool )

                  nmeta          =  nmeta - 2

               else
!
!                 We have an unqualified column name.  Allocate a table
!                 descriptor.  Set the table descriptor to indicate a
!                 null character descriptor.  Link this descriptor in
!                 before the column descriptor.
!
                  if ( lnknfn( ctx,dspool) < 1 ) call more_descriptors ( )
                  call lnkan ( ctx, dspool, tabptr )

                  dscbuf(1:dscsiz,tabptr) = 0
                  dscbuf(eqctyp,tabptr)  =  chr
                  dscbuf(dscsiz,tabptr)  =  ident

                  colptr  =  mtexpr(first)

                  call lnkila ( ctx, tabptr, colptr, dspool )

!
!                 Reduce the expression to a <name> metatoken.
!                 The reduction doesn't change the number of metatokens.
!
                  mtexpr(first)  =  tabptr
                  mtcode(first)  =  name

               end if

!
!              Decide the next state.
!
               state  =  reducd

            else if ( mtcode(first) == value ) then
!
!              If the query is valid, the sequence of meta-tokens
!              should be one of
!
!                 <value>  AND  <name>
!                 <value>  AND  <value>
!
!              Both of these reduce to the symbol <BETWEEN expr>.
!
               if (  third <= 0 ) then

                  error  = .true.
                  prserr = 'Tokens were missing from comparison '   // &
                           'relation.'
                  return

               end if

!
!              Null values are not allowed in BETWEEN expressions.
!
               if (      ( mtexpr(first) == nil ) &
                    .or. ( mtexpr(third) == nil )  )  then

                  error  = .true.
                  prserr = 'NULL values are not allowed in BETWEEN ' // &
                           'or NOT BETWEEN clauses.'
                  return

               end if

               if ( mtcode(third)  == ident ) then
!
!                 We'll need to reduce the IDENT before proceeding.
!
                  start   =  third
                  retcnd  =  reduce
                  state   =  push

               else if (      (       mtcode(second) == and      ) &
                        .and. (     ( mtcode(third)  == name  ) &
                               .or. ( mtcode(third)  == value )  )   ) &
               then
!
!                 This sequence of tokens, when seen in the PARSE
!                 state, is a set of value bounds for a BETWEEN or
!                 NOT BETWEEN expression.  Note that this token sequence
!                 can occur elsewhere, but not in the PARSE state.
!                 This is because the meta-token sequences
!
!                    <value>  AND  <name>
!                    <value>  AND  <value>
!
!                 occur at the start of the RHS of only two
!                 productions, namely
!
!                    <BETWEEN expr>  =>  <value>  AND  <name>
!                    <BETWEEN expr>  =>  <value>  AND  <value>
!
!                 Hook up the name or value descriptors.
!
                  call lnkilb ( ctx, mtexpr(first), mtexpr(third), dspool )

                  mtcode(first)  =  btwexp

                  call lnkfsl ( ctx, second, third, mtpool )

                  nmeta          =  nmeta - 2

!
!                 Decide the next state.
!
                  state  =  reducd

               else if ( mtcode(second) > 0 ) then
!
!                 The third meta-token is in the wrong place at the
!                 wrong time.
!
                  error  =  .true.
                  prserr =  'Token sequence must be followed '      // &
                            'by a value.'

                  return

               else
!
!                 The second meta-token is supposed to be the AND token,
!                 but it's actually something else.
!
                  error  =  .true.
                  prserr =  'Token must be followed by the AND ' // &
                            'operator.'

                  return

               end if

            else if ( mtcode(first) == name ) then
!
!              If the query is valid, the sequence of meta-tokens
!              should be any of
!
!                 <name>  <comparison operator> <value>
!                 <name>  <comparison operator> <name>
!                 <name>  <comparison operator> <ident>
!
!              or
!
!                 <name>  AND  <name>
!                 <name>  AND  <value>
!                 <name>  AND  <ident>
!
!              or
!
!                 <name>  BETWEEN  <BETWEEN expr>
!                 <name>  BETWEEN  <name>
!                 <name>  BETWEEN  <value>
!                 <name>  BETWEEN  <ident>
!
!              or
!
!                 <name>  <NOT BETWEEN>  <BETWEEN expr>
!                 <name>  <NOT BETWEEN>  <name>
!                 <name>  <NOT BETWEEN>  <value>
!                 <name>  <NOT BETWEEN>  <ident>
!
!              There must be at least three meta-tokens here.
!
               if (  third <= 0 ) then

                  error  = .true.
                  prserr = 'Tokens were missing from comparison '   // &
                           'relation.'
                  return

               end if

               if ( mtcode(third)  == ident ) then
!
!                 We'll need to reduce the IDENT before proceeding.
!
                  start   =  third
                  retcnd  =  reduce
                  state   =  push

               else if (      (       mtcode(second) == and      ) &
                        .and. (     ( mtcode(third)  == name  ) &
                               .or. ( mtcode(third)  == value )  )   ) &
               then
!
!                 This sequence of tokens, when seen in the PARSE
!                 state, is a set of value bounds for a BETWEEN or
!                 NOT BETWEEN expression.  Note that this token sequence
!                 can occur elsewhere, but not in the PARSE state.
!                 This is because the meta-token sequences
!
!                    <name>  AND  <name>
!                    <name>  AND  <value>
!
!                 occur at the start of the RHS of only two
!                 productions, namely
!
!                    <BETWEEN expr>  =>  <name>   AND  <name>
!                    <BETWEEN expr>  =>  <name>   AND  <value>
!
!                 Null values are not allowed in BETWEEN expressions.
!
                  if (      ( mtexpr(first) == nil ) &
                       .or. ( mtexpr(third) == nil )  )  then

                     error  = .true.
                     prserr = 'NULL values are not allowed in ' // &
                              'BETWEEN or NOT BETWEEN clauses.'
                     return

                  end if

!
!                 Hook up the name or value descriptors.
!
                  call lnkilb ( ctx, mtexpr(first), mtexpr(third), dspool )

                  mtcode(first)  =  btwexp

                  call lnkfsl ( ctx, second, third, mtpool )

                  nmeta          =  nmeta - 2

!
!                 Decide the next state.
!
                  state  =  reducd

               else if (      (        mtcode(second) > 0       ) &
                        .and. (      ( mtcode(third)  == name  ) &
                                .or. ( mtcode(third)  == value ) )   ) &
               then
!
!                 Positive meta-token codes denote comparison
!                 operators.
!
!                 We have an arithmetic, string, or column comparison
!                 expression.  This is a trivial normalized
!                 relational expression.  All we have to do
!                 is store the expression in the relation table,
!                 and free the second and third meta-tokens.
!
                  if ( lnknfn( ctx,rlpool) < 1 ) call more_relations ( )

                  call lnkan ( ctx, rlpool, newrel )

                  rels(1,newrel) = mtexpr(first )
                  rels(2,newrel) = mtcode(second)
                  rels(3,newrel) = mtexpr(third )

                  call lnkfsl ( ctx, second, third, mtpool )
                  nmeta  =  nmeta - 2

!
!                 Now allocate an entry in the conjunction pool
!                 and make this entry point to the relation table
!                 entry.
!
                  if ( lnknfn( ctx,cjpool) < 1 ) call more_conjunctions ( )

                  call lnkan ( ctx, cjpool, newcj )

                  cjptrs(newcj)  =  newrel

!
!                 Now allocate an entry in the disjunction pool
!                 and make this entry point to the conjunction pool
!                 entry.
!
                  if ( lnknfn( ctx,djpool) < 1 ) call more_disjunctions ( )

                  call lnkan ( ctx, djpool, newdj )

                  djptrs(newdj)  =  newcj

!
!                 Change the type of the first meta-token to EXPR and
!                 have that meta-token point to this table entry.  Bag
!                 the other two meta-tokens.
!
                  mtcode(first)  =  expr
                  mtexpr(first)  =  newdj

!
!                 Decide the next state.
!
                  state  =  reducd

               else if (      ( mtcode(second) == btween ) &
                         .or. ( mtcode(second) == notbtw )  ) then
!
!                 If the command is syntactically correct, the
!                 meta-token sequence should be one of:
!
!                    <name>  <BETWEEN>      <BETWEEN expr>
!                    <name>  <BETWEEN>      <value>
!                    <name>  <BETWEEN>      <name>
!                    <name>  <NOT BETWEEN>  <BETWEEN expr>
!                    <name>  <NOT BETWEEN>  <value>
!                    <name>  <NOT BETWEEN>  <name>
!
                  if (       ( mtcode(second) == btween ) &
                       .and. ( mtcode(third)  == btwexp )  ) then
!
!                    It's a BETWEEN comparison.  We treat this as a
!                    disjunction of conjunctions of comparison
!                    relations:
!                                      <name>   >=   <item1>
!                              AND     <name>   <=   <item2>
!
!                         OR
!                                      <name>   <=   <item1>
!                              AND     <name>   >=   <item2>
!
!                    where item1 and item2 are specified by the
!                    descriptors belonging to the third meta-token.
!
                     do i = 1, 4

                        if (  ( i == 1 ) .or. ( i == 3 )  ) then

                           k  =  mtexpr(third)

                        else
!
!                          We need the descriptor pointer for the RHS
!                          item.  This descriptor is linked to the tail
!                          of the descriptor for the LHS item.  The
!                          number of nodes to skip over depends on
!                          whether the LHS item is a name or value.
!
                           k  =  mtexpr(third)

                           if ( dscbuf(dscsiz,k) == ident ) then
                              skip = 1
                           else
                              skip = 0
                           end if

                           do j = 1, skip + 1
                              k  =  lnknxt( ctx, k, dspool )
                           end do

                        end if

                        if ( lnknfn( ctx,rlpool) < 1 ) call more_relations ( )

                        call lnkan ( ctx, rlpool, rel(i) )

                        rels( 1, rel(i) ) = mtexpr(first)

                        if ( ( i == 1 ) .or. ( i == 4 )  ) then
                           rels( 2, rel(i) ) = ge
                        else
                           rels( 2, rel(i) ) = le
                        end if

                        rels( 3, rel(i) ) = k

                        if ( lnknfn( ctx,cjpool) < 1 ) call more_conjunctions ( )

                        call lnkan ( ctx, cjpool, cj(i) )

                        cjptrs(cj(i))  =  rel(i)

                     end do

!
!                    Link the conjunction nodes to form the two
!                    conjunctions shown above.
!
                     call lnkila ( ctx, cj(1), cj(2), cjpool )
                     call lnkila ( ctx, cj(3), cj(4), cjpool )

!
!                    Allocate disjunction pool entries and make them
!                    point to the two respective conjunctions.
!
                     do i = 1, 2

                        if ( lnknfn( ctx,djpool) < 1 ) call more_disjunctions ( )

                        call lnkan ( ctx, djpool, dj(i) )
                        djptrs(dj(i))  =  cj( 2*i - 1 )

                     end do

!
!                    Finally, link the disjunction pool entries, and
!                    create an <expression> meta-token.  Free the unused
!                    meta-tokens.
!
                     call lnkila ( ctx, dj(1), dj(2), djpool )

                     mtcode(first)  =  expr
                     mtexpr(first)  =  dj(1)

                     call lnkfsl ( ctx, second, third, mtpool )
                     nmeta  =  nmeta - 2

!
!                    Decide the next state.
!
                     state  =  reducd

                  else if (       ( mtcode(second) == notbtw ) &
                            .and. ( mtcode(third)  == btwexp )  ) then
!
!                    It's a NOT BETWEEN comparison.  We treat
!                    this as a disjunction of conjunctions of comparison
!                    relations:
!
!                                      <name>   <   <item1>
!                              AND     <name>   <   <item2>
!
!                         OR
!                                      <name>   >   <item1>
!                              AND     <name>   >   <item2>
!
!                    where item1 and item2 are specified by the
!                    descriptors belonging to the third meta-token.
!
!                    The actions here are closely analogous to those
!                    for the BETWEEN case.
!
                     do i = 1, 4

                        if (  ( i == 1 ) .or. ( i == 3 )  ) then

                           k  =  mtexpr(third)

                        else
!
!                          We need the descriptor pointer for the RHS
!                          item.  This descriptor is linked to the tail
!                          of the descriptor for the LHS item.  The
!                          number of nodes to skip over depends on
!                          whether the LHS item is a name or value.
!
                           k  =  mtexpr(third)

                           if ( dscbuf(dscsiz,k) == ident ) then
                              skip = 1
                           else
                              skip = 0
                           end if

                           do j = 1, skip + 1
                              k  =  lnknxt( ctx, k, dspool )
                           end do

                        end if

                        if ( lnknfn( ctx,rlpool) < 1 ) call more_relations ( )

                        call lnkan ( ctx, rlpool, rel(i) )

                        rels( 1, rel(i) ) = mtexpr(first)

                        if ( i <= 2 ) then
                           rels( 2, rel(i) ) = lt
                        else
                           rels( 2, rel(i) ) = gt
                        end if

                        rels( 3, rel(i) ) = k

                        if ( lnknfn( ctx,cjpool) < 1 ) call more_conjunctions ( )

                        call lnkan ( ctx, cjpool, cj(i) )

                        cjptrs(cj(i))  =  rel(i)

                     end do

!
!                    Link the conjunction nodes to form the two
!                    conjunctions shown above.
!
                     call lnkila ( ctx, cj(1), cj(2), cjpool )
                     call lnkila ( ctx, cj(3), cj(4), cjpool )

!
!                    Allocate disjunction pool entries and make them
!                    point to the two respective conjunctions.
!
                     do i = 1, 2

                        if ( lnknfn( ctx,djpool) < 1 ) call more_disjunctions ( )

                        call lnkan ( ctx, djpool, dj(i) )
                        djptrs(dj(i))  =  cj( 2*i - 1 )

                     end do

!
!                    Finally, link the disjunction pool entries, and
!                    create an <expression> meta-token.  Free the unused
!                    meta-tokens.
!
                     call lnkila ( ctx, dj(1), dj(2), djpool )

                     mtcode(first)  =  expr
                     mtexpr(first)  =  dj(1)

                     call lnkfsl ( ctx, second, third, mtpool )
                     nmeta  =  nmeta - 2
!
!                    Decide the next state.
!
                     state  =  reducd

                  else if (      ( mtcode(third) == name  ) &
                            .or. ( mtcode(third) == value )  )  then
!
!                    If the third meta-token is anything other than
!                    <BETWEEN expr>, we'll have to parse the portion of
!                    the query following the BETWEEN keyword before
!                    reducing the <BETWEEN> or <NOT BETWEEN> expression.
!
                     start   =  third
                     retcnd  =  reduce
                     state   =  push

                  else

                     error  =  .true.
                     prserr = 'Token following BETWEEN operator is ' // &
                              'invalid.'
                     return

                  end if

               else if ( mtcode(second) > 0 ) then
!
!                 The third meta-token is in the wrong place at the
!                 wrong time.
!
                  error  =  .true.
                  prserr =  'Token sequence must be followed '      // &
                            'by a value.'

                  return

               else
!
!                 The second meta-token is supposed to be a comparison
!                 operator, but it's actually something else.
!
                  error  =  .true.
                  prserr =  'Token must be followed by a '          // &
                            'comparison operator.'

                  return

               end if

            else if ( mtcode(first) == expr ) then
!
!              If the query is valid, the sequence of meta-tokens
!              should be one of
!
!                 <expression>
!                 <expression>   )
!                 <expression>   OR   <expression>
!                 <expression>   OR   NAME
!                 <expression>   OR   IDENT
!                 <expression>   OR   NOT
!                 <expression>   OR   (
!                 <expression>   AND  <expression>
!                 <expression>   AND  NAME
!                 <expression>   AND  IDENT
!                 <expression>   AND  NOT
!                 <expression>   AND  (
!
               if ( second <= 0 ) then
!
!                 This is the last state we pass through
!                 before exiting the loop.  However, some syntax errors
!                 can get us here as well.
!
                  if (  ( level > 1 ) .or. ( nmeta > 1 )  ) then

                     error  = .true.
                     prserr = 'More tokens expected.'
                     return

                  end if

                  state = term

               else if ( mtcode(second) == rgroup ) then
!
!                 We've reached the end of a `parenthesized'
!                 expression.
!
                  if (       ( level         > 1      ) &
                       .and. ( popcnd(level) == endgrp )  ) then
!
!                    Time to pop the state.
!
                     state = pop

                  else
!
!                    There should not be a right grouper here.
!
                     error  =  .true.
                     prserr = 'Unexpected right parenthesis '      // &
                              'found.'
                     return

                  end if

!
!              In all other cases, there must be at least three
!              meta-tokens here.  Make sure there are.
!
               else if ( third <= 0 ) then

                  error  =  .true.
                  prserr =  'More tokens expected.'
                  return

!
!              Take care of the cases that will require reducing a sub-
!              expression before reducing the current expression.
!
               else if (      ( mtcode(third) == ident  ) &
                         .or. ( mtcode(third) == name   ) &
                         .or. ( mtcode(third) == not    )  ) then

                  start   =  third
                  retcnd  =  reduce
                  state   =  push

               else if ( mtcode(third) == lgroup ) then
!
!                 We'll have to push our state before continuing.
!
                  start   =  fourth
                  retcnd  =  endgrp
                  state   =  push

!
!              Now continue with the interesting cases.
!
               else if (       ( mtcode(first)  == expr ) &
                         .and. ( mtcode(second) == or   ) &
                         .and. ( mtcode(third)  == expr )  )  then
!
!                 We have a disjunction of two normalized
!                 expressions.  We're not ready to perform a
!                 reduction yet; we need to see whether there's
!                 a higher priority operator, namely AND, on the
!                 right of the second expression.
!
                  donow = .true.

                  if ( fourth > 0 ) then

                     if ( mtcode(fourth) == and ) then
!
!                       The third token is already spoken for:
!                       the expression involving the operator
!                       to its right must be processed first.
!
                        donow = .false.

                     end if

                  end if

                  if ( donow ) then
!
!                    This is an easy case to handle:
!                    we can form the resulting normalized
!                    expression by just linking together the two
!                    lists in the disjunction table.
!
                     dj(1) = mtexpr(first)
                     dj(2) = mtexpr(third)

                     call lnkilb ( ctx, dj(1), dj(2), djpool )
!
!                    The first meta-token will point to the resulting
!                    normalized expression; we'll discard the other
!                    two meta-tokens.
!
                     call lnkfsl ( ctx, second, third, mtpool )
                     nmeta = nmeta - 2

!
!                    MTEXPR(FIRST) and MTCODE(FIRST) are already
!                    set correctly.  All we need to do is determine
!                    our next state.  The next state defaults to
!                    PARSE; the other possibility is POP.
!
                     state  =  reducd

                  else
!
!                    We'll have to reduce the expression on the right
!                    of the third meta-token before coming back to
!                    this expression.  Get ready to push our state.
!
!                    The condition that must be met in order to pop our
!                    state will be that we've performed a reduction.
!
                     retcnd = reduce
                     start  = third
                     state  = push

                  end if
!
!                 Either we've reduced an OR expression, in which case
!                 the state has been set to PARSE or POP, or we've
!                 found a sub-expression that must be reduced before
!                 we attack the current expression, in which case the
!                 state has been set to PUSH.
!

               else if (       ( mtcode(first)  == expr ) &
                         .and. ( mtcode(second) == and  ) &
                         .and. ( mtcode(third)  == expr )  )  then
!
!                 We have the conjunction of two normalized
!                 expressions.  This case requires application of
!                 DeMorgan's laws to convert the expression to a
!                 normalized form.
!
!                 If we have two normalized expressions, say
!
!                    EXPR1 =        ( A11 and A12 and ... )
!                                or ( A21 and A22 and ... )
!                                              .
!                                              .
!                                              .
!                                or ( AM1 and AM2 and ... )
!
!                    EXPR2 =        ( B11 and B12 and ... )
!                                or ( B21 and B22 and ... )
!                                              .
!                                              .
!                                              .
!                                or ( BN1 and BN2 and ... )
!
!                 Then ( EXPR1 and EXPR2 ) =
!
!                        or       {  (     ( AI1 and AI2 and ... )
!                    I = 1,...,M       and ( BJ1 and BJ2 and ... ) )  }
!                    J = 1,...,N
!
!                 We have the conjunction of two normalized
!                 So, to represent the normalized expression resulting
!                 from the conjuction of the expressions represented by
!                 the meta-tokens FIRST and THIRD, we will loop through
!                 each disjunction list and form the disjunction of all
!                 conjunctions of pairs of conjunctions, one of which is
!                 from the first expression and one of which is from the
!                 second.  After doing this, we'll clean up the
!                 conjunction and disjunction pools by freeing the
!                 elements in those pools used by the original two
!                 meta-tokens FIRST and THIRD.
!
                  dj(1)    = mtexpr(first)
                  djtail = nil

                  do while ( dj(1) > 0 )

                     dj(2) = mtexpr(third)

                     do while ( dj(2) > 0 )
!
!                       Allocate a new disjunction table entry,
!                       and create a new conjunction that represents
!                       the conjunction of the conjunction lists
!                       pointed to by DJ(1) and DJ(2).
!
                        if ( lnknfn( ctx,djpool) < 1 ) call more_disjunctions ( )

                        call lnkan ( ctx, djpool, newdj )

!
!                       Make copies of the conjunction lists pointed
!                       to by DJ(1) and DJ(2).
!
                        cj(1) = djptrs(dj(1))
                        tail  = nil

                        do while ( cj(1) > 0 )

                           if ( lnknfn( ctx,cjpool) < 1 ) call more_conjunctions ( )

                           call lnkan  ( ctx,  cjpool, newcj          )
                           call lnkila ( ctx,  tail,   newcj, cjpool  )
                           tail         = newcj
                           cjptrs(tail) = cjptrs(cj(1))

                           cj(1)        = lnknxt ( ctx, cj(1), cjpool )

                        end do

                        head1 = lnkhl( ctx, tail, cjpool )

                        cj(2) = djptrs(dj(2))
                        tail  = nil

                        do while ( cj(2) > 0 )

                           if ( lnknfn( ctx,cjpool) < 1 ) call more_conjunctions ( )

                           call lnkan  ( ctx,  cjpool, newcj          )
                           call lnkila ( ctx,  tail,   newcj, cjpool  )
                           tail         = newcj
                           cjptrs(tail) = cjptrs(cj(2))

                           cj(2)        = lnknxt ( ctx, cj(2), cjpool )

                        end do

                        head2 = lnkhl( ctx, tail, cjpool )

!
!                       Now link these copies and make NEWDJ point to
!                       the resulting list.
!
                        call lnkilb ( ctx, head1, head2, cjpool )

                        djptrs(newdj) = head1

!
!                       Link NEWDJ in at the tail of the disjunction
!                       list.
!
                        call lnkila  ( ctx, djtail, newdj, djpool )
                        djtail = newdj

                        dj(2)  = lnknxt ( ctx, dj(2),   djpool )

                     end do

                     dj(1) = lnknxt ( ctx, dj(1), djpool )

                  end do

!
!                 We've now created the new normalized expression that
!                 represents the conjunction of our original two
!                 expressions.
!
!                 Before continuing, we should clean up the entries in
!                 the disjunction and conjunction pools used by the
!                 original expressions.  We can save a little work
!                 by linking those entries before freeing them.
!
                  call lnkilb ( ctx, mtexpr(first), mtexpr(third), djpool )

                  djnode = mtexpr(first)

                  do while ( djnode > 0 )
!
!                    Free the conjunction list pointed to by DJNODE.
!
                     cjnode = djptrs(djnode)

                     call lnkfsl ( ctx,  cjnode, &
                                    lnktl( ctx,cjnode,cjpool), &
                                    cjpool                )

                     djnode = lnknxt( ctx, djnode, djpool )

                  end do
!
!                 Free the disjunction list that starts with
!                 MTEXPR(FIRST).
!
                  call lnkfsl ( ctx,  mtexpr(first), &
                                 lnktl( ctx, mtexpr(first), djpool ), &
                                 djpool                          )

!
!                 NEWDJ is the tail node of the list of disjunctions
!                 we've just finished.  The first meta-token should
!                 point to the head of this disjunction list.
!
                  mtexpr(first)  =  lnkhl ( ctx, newdj, djpool )

!
!                 We no longer need the other two meta-tokens.
!
                  call lnkfsl ( ctx, second, third, mtpool )
                  nmeta = nmeta - 2

!
!                 Decide the next state.
!
                  state  =  reducd

               else
!
!                 There are no other valid cases in which the first
!                 meta-token is an expression.
!
                  error  =  .true.
                  prserr =  'Unexpected token found following '       // &
                            'valid expression.'
                  return

               end if

            else if ( mtcode(first) == not ) then
!
!              There are four valid token sequences that we could
!              see here:
!
!                 NOT  <expression>
!                 NOT  IDENT
!                 NOT  NAME
!                 NOT  NOT
!                 NOT  (
!
               if ( second <= 0 ) then

                  error  = .true.
                  prserr = 'Tokens were missing from logical '        // &
                           'expression.'
                  return

               else if ( mtcode(second) == lgroup ) then
!
!                 We'll have to push our state before continuing.
!
                  start   =  third
                  retcnd  =  endgrp
                  state   =  push

               else if (      ( mtcode(second) == not   ) &
                         .or. ( mtcode(second) == ident ) &
                         .or. ( mtcode(second) == name  )  )  then

                  start   =  second
                  retcnd  =  reduce
                  state   =  push

               else if ( mtcode(second) == expr ) then
!
!                 We have the negation of a normalized expression. Since
!                 the NOT operator has higher precedence than any other,
!                 we need not concern ourselves with the token on the
!                 right of the expression.
!
!                 This case requires application of DeMorgan's laws to
!                 convert the expression to a normalized form.
!
!                 If we have a normalized expression, say
!
!                    EXPR  =         ( A11 and A12 and ... )
!                                 or ( A21 and A22 and ... )
!                                              .
!                                              .
!                                              .
!                                 or ( AM1 and AM2 and ... )
!
!                 Then (using the tilde to express negation):
!
!                    ~EXPR =          ( ~A11 or ~A12 or ... )
!                                 and ( ~A21 or ~A22 or ... )
!                                              .
!                                              .
!                                              .
!                                 and ( ~AM1 or ~AM2 or ... )
!
!                 Since each parenthesized expression above is a
!                 normalized expression, we can convert the conjunction
!                 of any of these expressions and a second normalized
!                 expression to normalized form using the method of the
!                 AND case above.
!
!                 We'll first build the expression
!
!                    ( ~A11 or ~A12 or ... )
!
!                 and then combine the others with it, one by one.
!                 When we're all done, we'll negate the operators used
!                 in the comparison relations.
!
!                 The pointer EXPRHD will denote the head of the
!                 combined normalized expression.
!
                  djnode = mtexpr(second)

                  cjnode = djptrs(djnode)
                  tail   = nil

                  do while ( cjnode > 0 )
!
!                    Create a new singleton disjunction list
!                    that points to the relation pointed to by
!                    CJNODE.
!
                     if ( lnknfn( ctx,djpool) < 1 ) call more_disjunctions ( )

                     call lnkan ( ctx, djpool, newdj )

                     if ( lnknfn( ctx,cjpool) < 1 ) call more_conjunctions ( )

                     call lnkan ( ctx, cjpool, newcj )

                     cjptrs(newcj) = cjptrs(cjnode)
                     djptrs(newdj) = newcj
!
!                    Now link the new singleton disjunction list in
!                    at the tail of the disjunction list that
!                    parallels the conjunction list we're currently
!                    traversing.
!
                     call lnkila ( ctx, tail, newdj, djpool )
                     tail   = newdj

                     cjnode = lnknxt ( ctx, cjnode, cjpool )

                  end do

!
!                 Keep track of the head of the new normalized
!                 expression.
!
                  exprhd = lnkhl ( ctx, tail, djpool )

!
!                 Now, for every remaining conjunction in the original
!                 expression, we'll form the normalized expression
!                 resulting from the conjunction of its negation and
!                 of our cumulative normalized expression.  As mentioned
!                 before, we won't negate the comparison operators
!                 just yet.
!
                  djnode = lnknxt ( ctx, djnode, djpool )

                  do while ( djnode > 0 )
!
!                    Loop through our existing cumulative
!                    expression and the latest conjunction, forming
!                    all pairwise conjunctions.
!
                     dj(1)    = exprhd
                     djtail = nil

                     do while ( dj(1) > 0 )

                        cj(2) = djptrs(djnode)

                        do while ( cj(2) > 0 )
!
!                          Make a copy of the conjunction list pointed
!                          to by DJPTRS(DJ(1)).
!
                           cjnode = djptrs(dj(1))
                           tail   = nil

                           do while ( cjnode > 0 )

                              if ( lnknfn( ctx,cjpool) < 1 ) call more_conjunctions ( )

                              call lnkan  ( ctx, cjpool, newcj         )
                              call lnkila ( ctx, tail,   newcj, cjpool )

                              cjptrs(newcj)  = cjptrs(cjnode)
                              tail           = newcj
                              cjnode         = lnknxt ( ctx, cjnode, cjpool )

                           end do

                           cj(1) = lnkhl ( ctx, tail, cjpool )

!
!                          Allocate a new conjunction table entry for
!                          the conjunction of the expressions
!                          pointed to by CJ(1) and CJ(2).  Allocate a
!                          new disjunction table entry to point to this
!                          new conjunction.
!
                           if ( lnknfn( ctx,cjpool) < 1 ) call more_conjunctions ( )

                           call lnkan ( ctx, cjpool, newcj )
                           cjptrs(newcj)  =  cjptrs(cj(2))

                           if ( lnknfn( ctx,djpool) < 1 ) call more_disjunctions ( )

                           call lnkan ( ctx, djpool, newdj )

!
!                          Hook everything up.
!
                           call lnkilb ( ctx, cj(1), newcj, cjpool )

                           djptrs(newdj) =  cj(1)

                           call lnkila ( ctx, djtail, newdj, djpool )
                           djtail        =  newdj

                           cj(2)         =  lnknxt ( ctx, cj(2), cjpool )

                        end do

                        dj(1)    = lnknxt ( ctx, dj(1), djpool )

                     end do

!
!                    Before going on, clean up the conjunction and
!                    disjunction pool entries used by our last
!                    version of the cumulative expression.
!
                     dj(1) = exprhd

                     do while ( dj(1) > 0 )

                        cj(1) = djptrs(dj(1))
                        cj(2) = lnktl ( ctx, cj(1), cjpool )

                        call lnkfsl ( ctx, cj(1), cj(2), cjpool )

                        dj(1) = lnknxt ( ctx, dj(1), djpool )

                     end do

                     call lnkfsl ( ctx,  exprhd, &
                                    lnktl( ctx, exprhd, djpool ), &
                                    djpool                    )

!
!                    Set EXPRHD to be the head of our updated,
!                    cumulative expression.  Start to work on the
!                    next conjunction.
!
                     exprhd = lnkhl  ( ctx, djtail, djpool )
                     djnode = lnknxt ( ctx, djnode, djpool )

                  end do

!
!                 EXPRHD now points to a new expression that will
!                 represent the negation of the expression pointed
!                 to by MTEXPR(SECOND), as soon as we negate the
!                 comparison operators referenced in the expression.
!                 Take care of this last step now.  To make sure that
!                 we negate each operator exactly once, we build a set
!                 of relations to be negated, then negate each relation
!                 in the set.
!
                  call ssizei ( ctx, ubound ( relset, 1, kind=ip ), relset )

                  djnode = mtexpr(second)

                  do while ( djnode > 0 )

                     cjnode = djptrs(djnode)

                     do while ( cjnode > 0 )

                        relptr = cjptrs(cjnode)
                        call insrti ( ctx, relptr, relset )

                        cjnode = lnknxt( ctx, cjnode, cjpool )

                     end do

                     djnode = lnknxt ( ctx, djnode, djpool )

                  end do

                  do i = 1, cardi( ctx,relset)

                     relptr          =  relset( i )
                     j               =  findloc(cmpcde, rels(2,relptr), dim=1)
                     rels(2,relptr)  =  cmpneg( j )

                  end do

!
!                 Set the pointer of the first meta-token to point
!                 to our normalized expression, and change the
!                 meta-token's code to <expr>.
!
                  mtexpr(first) = exprhd
                  mtcode(first) = expr

!
!                 Get rid of the second meta-token, and determine the
!                 next state.
!
                  call lnkfsl ( ctx, second, second, mtpool )
                  nmeta  =  nmeta - 1

                  state  =  reducd

               else
!
!                 The second token is invalid in this context.
!
                  error  = .true.
                  prserr = 'Token following NOT operator was invalid.'
                  return

               end if
!
!              This is the end of the NOT case.
!

            else if ( mtcode(first) == lgroup ) then
!
!              We're looking at the start of a `parenthesized'
!              sub-expression.
!
!              Push our state, and start parsing at meta-token
!              SECOND.  The condition for popping our state will be
!              that we encounter a right grouper.
!
               retcnd = endgrp
               start  = second
               state  = push

            else
!
!              Only a syntax error could get us here.
!
               error  =  .true.
               prserr = 'Unexpected token found.'
               return

            end if

!
!           This is the end of the code for the PARSE state.  We've
!           determined the next parsing state at this point.
!

         else if ( state == reducd ) then
!
!           A reduction has been done.  Decide the next state.
!
            state  =  reducd

            if ( popcnd(level) == reduce ) then
               state         = pop
            else
               mstart(level) = first
               state         = parse
            end if

         else if ( state == push ) then
!
!           Increment the stack level, and save the current
!           starting point and pop condition.
!
            level  =  level + 1

            if ( level > maxstk ) then
               error          = .true.
               prserr         = 'Stack is full'
               state          =  term
            else
               mstart(level)  =  start
               popcnd(level)  =  retcnd
               state          =  parse
            end if

         else if ( state == pop ) then
!
!           If we can, pop the state.
!
            if ( level > 1 ) then

               if ( popcnd(level) == endgrp ) then
!
!                 If we're popping the state because we encountered a
!                 right grouper, we have a meta-token sequence that
!                 looks like this:
!
!                    (  EXPR  )
!
!                        ^    ^
!                     FIRST  SECOND
!
!                 We need to remove the grouping tokens, taking care to
!                 update the starting token at the next lower level, if
!                 the left grouper was the starting token.
!
                  prev   =  lnkprv ( ctx, first, mtpool )

                  if ( mstart(level-1) == prev ) then
                     mstart(level-1) = first
                  end if

                  if ( metahd == prev ) then
                     metahd = first
                  end if

                  call lnkfsl ( ctx, prev,   prev,   mtpool )
                  call lnkfsl ( ctx, second, second, mtpool )

                  nmeta  =  nmeta - 2

               end if

               level = level - 1
               state = parse

            else
               error  =  .true.
               prserr =  'Syntax error:  badly formed WHERE clause.'
               return
            end if

         end if

!
!        We've considered all states.
!

      end do

!
!     At this point, there should be a single meta-token of type EXPR.
!     This meta-token should point to a normalized expression.  We'll
!     set the encoded query to represent this expression.  For each
!     constraint, we'll add a constraint descriptor to the encoded
!     query.  We'll also update the count of constraints, the count of
!     conjunctions, and we'll add a list of conjunction sizes.
!
      djnode = mtexpr(first)
      nconj  = 0
      nrels  = 0

      do while ( djnode > 0 )

         nconj         =  nconj + 1
         call grow ( sizes, nconj )
         sizes(nconj)  =  0
         cjnode        =  djptrs(djnode)

         do while ( cjnode > 0 )

            nrels          =  nrels        + 1
            sizes (nconj)  =  sizes(nconj) + 1
            relptr         =  cjptrs(cjnode)

            tabptr         =  rels(1,relptr)
            op             =  rels(2,relptr)
            rhsptr         =  rels(3,relptr)

!
!           Add a constraint descriptor to the encoded query.  The
!           structure of these descriptors is documented in the include
!           file for encoded query parameters.
!
!           First, save space for the constraint type.  We'll fill this
!           in after finding out what's on the right hand side.
!
            call append_query ( ctx, 0, eqryi )
            k  =  cardi ( ctx, eqryi )

!
!           Next, add name descriptors for the table and column on
!           the left-hand side.  These descriptors are linked and
!           pointed to by NAMPTR.
!
            do i = 1, eqvdsz
               call append_query ( ctx,  dscbuf( i, tabptr ),  eqryi  )
            end do

            colptr  =  lnknxt( ctx, tabptr, dspool )

            do i = 1, eqvdsz
               call append_query ( ctx,  dscbuf( i, colptr ),  eqryi  )
            end do

!
!           What happens next depends on whether the query has a null
!           value on the right hand side.  This is indicated by the
!           relation's value pointer being NIL.
!
            if ( rhsptr == nil ) then
!
!              For constraints involving null values, we change the
!              operator to ISNULL or NOTNUL as appropriate.
!
               if ( op == eq ) then
                  op  =  isnull

               else if ( op == ne ) then
                  op  =  notnul

               else

                  error  =  .true.
                  prserr =  'NULL values can only be used with the ' // &
                            'operators "IS NULL", "NOT NULL", '      // &
                            'or equivalents.'
                  return

               end if

!
!              Set the operator code.
!
               call append_query ( ctx, op, eqryi )

!
!              Pad the constraint descriptor up to the full length.
!
               do i = 1, 2*eqvdsz
                  call append_query ( ctx, 0, eqryi )
               end do

!
!              Set the descriptor's type by updating the reserved
!              location.
!
               eqryi(k)  =  eqval

            else
!
!              For `normal' constraints, that is, constraints that don't
!              involve null values, we set the operator code, then
!              fill in the information describing the RHS of the
!              constraint.
!
               call append_query ( ctx, op, eqryi )

               if (  dscbuf( dscsiz, rhsptr ) == value ) then
!
!                 The RHS contains a value.  Append the descriptor
!                 for the value, then pad the constraint descriptor.
!
                  do i = 1, eqvdsz
                     call append_query ( ctx,  dscbuf( i, rhsptr ),  eqryi  )
                  end do

                  do i = 1, eqvdsz
                     call append_query ( ctx,  0,  eqryi  )
                  end do

!
!                 Set the descriptor's type by updating the reserved
!                 location.
!
                  eqryi(k)  =  eqval

               else
!
!                 The RHS contains a column name.  Append the
!                 descriptors for the table and column.
!
                  do i = 1, eqvdsz
                     call append_query ( ctx,  dscbuf( i, rhsptr ),  eqryi  )
                  end do

                  colptr  =  lnknxt( ctx, rhsptr, dspool )

                  do i = 1, eqvdsz
                     call append_query ( ctx,  dscbuf( i, colptr ),  eqryi  )
                  end do

!
!                 Set the descriptor's type by updating the reserved
!                 location.
!
                  eqryi(k)  =  eqcol

               end if

            end if
!
!           We've updated the encoded query to reflect the current
!           constraint relation.
!
            cjnode = lnknxt ( ctx, cjnode, cjpool )

         end do
!
!        We've set the array element SIZES(NCONJ).
!
         djnode = lnknxt ( ctx, djnode, djpool )

      end do

!
!     Set the counts of constraints and conjunctions in the encoded
!     query.
!
      call zzekweqi ( ctx, 'NUM_CONSTRAINTS',  nrels, eqryi )
      call zzekweqi ( ctx, 'NUM_CONJUNCTIONS', nconj, eqryi )

!
!     Add the conjunction size list to the encoded query.
!
      do i = 1, nconj
         call append_query ( ctx, sizes(i), eqryi )
      end do

      return

      contains

!
!        PEPPER: grow the pools (and their parallel arrays) when full.
!
         !> Grow the relation pool, the relations and the relation set.
         subroutine more_relations ( )
            integer(ip) :: n
            n = grown_capacity ( ubound ( rlpool, 2, kind=ip ), ubound ( rlpool, 2, kind=ip ) + 1 )
            call lnkgrow ( rlpool, n )
            call grow ( rels, n, 3_ip )
            call grow_cell ( relset, n )
         end subroutine more_relations

         !> Grow the conjunction pool and its relation pointers.
         subroutine more_conjunctions ( )
            integer(ip) :: n
            n = grown_capacity ( ubound ( cjpool, 2, kind=ip ), ubound ( cjpool, 2, kind=ip ) + 1 )
            call lnkgrow ( cjpool, n )
            call grow ( cjptrs, n )
         end subroutine more_conjunctions

         !> Grow the disjunction pool and its conjunction list pointers.
         subroutine more_disjunctions ( )
            integer(ip) :: n
            n = grown_capacity ( ubound ( djpool, 2, kind=ip ), ubound ( djpool, 2, kind=ip ) + 1 )
            call lnkgrow ( djpool, n )
            call grow ( djptrs, n )
         end subroutine more_disjunctions

         !> Grow the descriptor pool and the descriptor buffer.
         subroutine more_descriptors ( )
            integer(ip) :: n
            n = grown_capacity ( ubound ( dspool, 2, kind=ip ), ubound ( dspool, 2, kind=ip ) + 1 )
            call lnkgrow ( dspool, n )
            call grow ( dscbuf, n, dscsiz )
         end subroutine more_descriptors
      end subroutine zzeknrml

      !> Set the forward data pointer for a specified EK data page.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TYPE           is the data type of the desired page.
      !>
      !>     P              is the page number of the allocated page.  This
      !>                    number is recognized by the EK paged access
      !>                    routines.
      !>
      !>     FWARD          is a forward data pointer.  This is the number
      !>                    of a data page on which the last column entry
      !>                    on page P is continued.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If TYPE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     3)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine sets the forward data pointer of the specified EK
      !>     data page.  The value of the pointer is a page number.
      !>
      !>## Examples
      !>
      !>     See ZZEKAPS.
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
      !>-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
      module subroutine zzeksfwd ( ctx, handle, type, p, fward )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekdatpag
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: type  !! Data type of page.
      integer(ip), intent(in) :: p  !! Page number.
      integer(ip), intent(in) :: fward  !! Forward data pointer.
!
!     Local variables
!
      real(wp) :: dpptr
      integer(ip) :: base

!
!     Look up the base address of the page.
!
      call zzekpgbs ( ctx, type, p, base )

      if ( ctx%failed() ) return

      if ( type == chr ) then
!
!        Set the encoded count.
!
         call zzeksei ( ctx, handle, base+cfpidx, fward )

      else if ( type == dp ) then
!
!        Convert the input count to d.p. type.
!
         dpptr  =  fward

         call dasudd ( ctx, handle, base+dfpidx, base+dfpidx, [ dpptr ] )

      else
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, ZZEKPGBS would have complained.
!
         call dasudi ( ctx, handle, base+ifpidx, base+ifpidx, [ fward ] )

      end if

      return
      end subroutine zzeksfwd

      !> Set the link count for a specified EK data page.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TYPE           is the data type of the desired page.
      !>
      !>     P              is the page number of the allocated page.  This
      !>                    number is recognized by the EK paged access
      !>                    routines.
      !>
      !>     NLINKS         is the new number of links to the specified data
      !>                    page.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If TYPE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     3)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine updates the link count of the specified EK data page.
      !>
      !>     Link counts are used to indicate how many `users' of a page
      !>     there are.  When the link count of a page drops to zero, that
      !>     page is eligible to be freed.
      !>
      !>## Examples
      !>
      !>     See EKDELR.
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
      !>-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
      module subroutine zzekslnk ( ctx, handle, type, p, nlinks )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ekdatpag
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: type  !! Data type of page.
      integer(ip), intent(in) :: p  !! Page number.
      integer(ip), intent(in) :: nlinks  !! Number of links to page.
!
!     Local variables
!
      real(wp) :: dplnk
      integer(ip) :: base

!
!     Look up the base address of the page.
!
      call zzekpgbs ( ctx, type, p, base )

      if ( ctx%failed() ) return

      if ( type == chr ) then
!
!        Set the encoded count.
!
         call zzeksei ( ctx, handle, base+clcidx, nlinks )

      else if ( type == dp ) then
!
!        Convert the input count to d.p. type.
!
         dplnk  =  nlinks

         call dasudd ( ctx, handle, base+dlcidx, base+dlcidx, [ dplnk ] )

      else
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, ZZEKPGBS would have complained.
!
         call dasudi ( ctx, handle, base+ilcidx, base+ilcidx, [ nlinks ] )

      end if

      return
      end subroutine zzekslnk

      !> Execute a 1-3 split:  split the root node to create two new
      !> children, leaving a single key in the root.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>     3)  If the number of keys in the root does not correspond to an
      !>         overflow of exactly 1 key, the error SPICE(BUG) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     Insertions into an EK tree start at a leaf node.  If the node
      !>     overflows, the EK system attempts to shuffle keys at the leaf
      !>     level to resolve the overflow.  That attempt failing, the system
      !>     delegates the problem upward to the next higher level.  Overflow
      !>     may occur there as well; if it does, the problem gets passed
      !>     upward again.  If the root overflows, the system makes room by
      !>     executing what's called a `1-3' split:  the root gets two new
      !>     children, and all but one of the keys in the root are moved into
      !>     the new children.  The former children of the root become
      !>     children of the two new children of the root.
      !>
      !>     After the 1-3 split, the tree is balanced and all invariants
      !>     relating to key counts are restored.
      !>
      !>     The tree grows taller by one level as a result of a 1-3 split;
      !>     this is the only circumstance under which the tree grows taller.
      !>
      !>     Below are the gory details concerning the actions of this routine.
      !>     All of the parameters referred to here (in capital letters) are
      !>     defined in the include file ektree.inc.
      !>
      !>     In a 1-3 split:
      !>
      !>        - The leftmost MNKEYC keys of the root are moved into the
      !>          new left child.
      !>
      !>        - The data values associated with the first MNKEYC keys of the
      !>          root are moved along with the keys.
      !>
      !>        - The left child pointers associated with the first MNKEYC keys
      !>          of the root are moved along with the keys.
      !>
      !>        - The right child pointer of the key at location MNKEYC+1 in
      !>          the root is moved to location MYKEYC+1 in the child pointer
      !>          array of the left child.
      !>
      !>        - The rightmost MNKEYC keys of the root are moved into the
      !>          new right child.
      !>
      !>        - The data values associated with the last MNKEYC keys of the
      !>          root are moved along with the keys.
      !>
      !>        - The left child pointers associated with the last MNKEYC keys
      !>          of the root are moved along with the keys.
      !>
      !>        - The right child pointer of the last in the root is moved to
      !>          location MYKEYC+1 in the child pointer array of the right
      !>          child.
      !>
      !>        - The left child pointer of the one key left in the root
      !>          points to the new left child.
      !>
      !>        - The right child pointer of the one key left in the root
      !>          points to the new right child.
      !>
      !>     As the above list shows, each of the new children of the root
      !>     contains the minimum allowed number of keys that a child node
      !>     may have.  Thus the size constraints on child nodes are met.
      !>     The root must be non-empty unless the tree is empty; this
      !>     condition is also met.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRIN.
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 26-OCT-1995 (NJB)
      module subroutine zzektr13 ( ctx, handle, tree )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
!
!     Local variables
!
      integer(ip) :: base
      integer(ip) :: c1page ( pgsizi )
      integer(ip) :: c2page ( pgsizi )
      integer(ip) :: child  ( 2 )
      integer(ip) :: delta
      integer(ip) :: i
      integer(ip) :: middle
      integer(ip) :: nrkeys
      integer(ip) :: root
      integer(ip) :: rpage  ( pgsizi )

      root    =  tree

      call zzekpgri ( ctx, handle, root, rpage )

      nrkeys  =  rpage ( trnkr )

!
!     The number of keys in the root must correspond exactly to an
!     overflow level of 1 key.
!
      if (  nrkeys  /=  ( mxkeyr + 1 )  ) then

         call ctx%setmsg ( 'Number of keys in root = #; should be #.' )
         call ctx%errint ( '#',  nrkeys                               )
         call ctx%errint ( '#',  mxkeyr+1                             )
         call ctx%sigerr ( 'SPICE(BUG)'                               )
         return

      end if

!
!     Allocate two new pages; these will become children of the root.
!     Each one will be assigned MNKEYC keys.
!
      do i = 1, 2
         call zzekpgal ( ctx, handle, int, child(i), base )
      end do

!
!     Set the key count in the first child.
!
      c1page = 0

      c1page ( trnkc )  =  mnkeyc

!
!     Copy in the keys, data pointers, and child pointers from the
!     first MNKEYC locations in the root.  Also take the left child
!     pointer of the middle key.
!
      c1page(trkeyc+1:trkeyc+1+mnkeyc-1) = rpage(trkeyr+1:trkeyr+1+mnkeyc-1)
      c1page(trdatc+1:trdatc+1+mnkeyc-1) = rpage(trdatr+1:trdatr+1+mnkeyc-1)
      c1page(trkidc+1:trkidc+1+mnkeyc) = rpage(trkidr+1:trkidr+1+mnkeyc)

!
!     Set up the key count in the second child.
!
      c2page = 0

      c2page ( trnkc  )  =  mnkeyc

!
!     Copy in the keys, data pointers, and child pointers from the
!     last MNKEYC locations in the root.  Also take the last right
!     child pointer.
!
      middle = mnkeyc + 1

      c2page(trkeyc+1:trkeyc+1+mnkeyc-1) = rpage(trkeyr+middle+1:trkeyr+middle+1+mnkeyc-1)
      c2page(trdatc+1:trdatc+1+mnkeyc-1) = rpage(trdatr+middle+1:trdatr+middle+1+mnkeyc-1)
      c2page(trkidc+1:trkidc+1+mnkeyc) = rpage(trkidr+middle+1:trkidr+middle+1+mnkeyc)

!
!     The keys in this second node must be adjusted to account for the
!     loss of the predecessors assigned to the subtree headed by the
!     left child, as well as of the middle key.
!
      delta  =  rpage ( trkeyr + middle )

      do i = 1, mnkeyc
         c2page( trkeyc + i )  =  c2page( trkeyc + i ) - delta
      end do

!
!     Now the root must be updated.  The root now contains just 1
!     key; that key should be shifted left to the first key location.
!     There are two child pointers; these point to the children just
!     created.  The depth of the tree has increased, as well as the
!     number of nodes in the tree.
!
      rpage ( trkeyr + 1 )  =  rpage ( trkeyr + middle )
      rpage ( trdatr + 1 )  =  rpage ( trdatr + middle )
      rpage ( trkidr + 1 )  =  child(1)
      rpage ( trkidr + 2 )  =  child(2)
      rpage ( trnkr      )  =  1
      rpage ( trdpth     )  =  rpage ( trdpth ) + 1
      rpage ( trnnod     )  =  rpage ( trnnod ) + 2

      rpage(trkeyr+2:trkeyr+2+mxkeyr-1) = 0
      rpage(trdatr+2:trdatr+2+mxkeyr-1) = 0
      rpage(trkidr+3:trkidr+3+mxkeyr-1) = 0

!
!     Write out our updates.
!
      call zzekpgwi ( ctx, handle,  root,      rpage  )
      call zzekpgwi ( ctx, handle,  child(1),  c1page )
      call zzekpgwi ( ctx, handle,  child(2),  c2page )

      return
      end subroutine zzektr13

      !> Execute a 3-1 merge:  move the contents of two children into
      !> the root node and delete the children.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>     3)  If there is not exactly 1 key in the root at the time this
      !>         routine is called, the error SPICE(BUG) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     Deletions from an EK tree start at a leaf node.  If the node
      !>     underflows, the EK system attempts to shuffle keys at the leaf
      !>     level to resolve the underflow.  That attempt failing, the system
      !>     delegates the problem upward to the next higher level.  Underflow
      !>     may occur there as well; if it does, the problem gets passed
      !>     upward again.  If the root has only two children and one of these
      !>     underflows, the system reduces the height of the tree by
      !>     executing what's called a `3-1' merge:  the root loses its two
      !>     children, and all of the keys in the children are moved into
      !>     the root.  The former grandchildren of the root become
      !>     children of the root.
      !>
      !>     A tree is eligible for a 3-1 merge only if the root has exactly
      !>     two children, and the sum of the key counts of the children
      !>     constitutes an underflow of 1 key:  that is, the sum is
      !>
      !>        2*MNKEYC - 1
      !>
      !>     After the 3-1 merge, the tree is balanced and all invariants
      !>     relating to key counts are restored.
      !>
      !>     The tree grows shorter by one level as a result of a 3-1 merge;
      !>     this is the only circumstance under which the tree grows shorter.
      !>
      !>     Below are the gory details concerning the actions of this routine.
      !>     All of the parameters referred to here (in capital letters) are
      !>     defined in the include file ektree.inc.
      !>
      !>     In a 3-1 merge:
      !>
      !>        - The keys of the left child are moved into the root.  These
      !>          become the leftmost MNKEYC or MNKEYC-1 keys of the root,
      !>          depending on whether the underflow occurred in the left
      !>          child.
      !>
      !>        - The data values associated with the keys of the left child
      !>          of the root are moved into the root along with the keys.
      !>
      !>        - The left child pointers associated with the keys of the left
      !>          child of the root are moved into the root along with the
      !>          keys.
      !>
      !>        - The last right child pointer in the left child of the root
      !>          the root is moved to location NLEFT+1 in the child pointer
      !>          array of the root, where NLEFT is the number of keys in
      !>          the former left child.  This pointer overwrites the root's
      !>          pointer to the left child.
      !>
      !>        - The keys of the right child are moved into the root.  These
      !>          become the rightmost MNKEYC or MNKEYC-1 keys of the root,
      !>          depending on whether the underflow occurred in the right
      !>          child.
      !>
      !>        - The data values associated with the keys of the right child
      !>          of the root are moved into the root along with the keys.
      !>
      !>        - The left child pointers associated with the keys of the right
      !>          child of the root are moved into the root along with the
      !>          keys.  The first of these pointers overwrites the root's
      !>          pointer to the right child.
      !>
      !>        - The last right child pointer in the right child of the root
      !>          the root is moved to location 2*MNKEYC+1 in the child pointer
      !>          array of the root.
      !>
      !>        - The former children of the root are deleted.
      !>
      !>     As the above list shows, the root contains the maximum allowed
      !>     number of keys after a 3-1 merge.  This is because
      !>
      !>        MXKEYR  =  MXKIDR - 1
      !>
      !>                =  2 * ( (2*MXKIDC - 2)/3 )
      !>
      !>                =  2 * ( (2*MXKIDC + 1)/3  - 1 )
      !>
      !>                =  2 * ( MNKIDC - 1)
      !>
      !>                =  2 * MNKEYC
      !>
      !>     Our assumptions were that there was one key in the root and
      !>     that the sum of the key counts of the two children of the root
      !>     was
      !>
      !>        ( 2 * MNKEYC ) - 1
      !>
      !>     Thus the size constraints on the root node are met.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRDL.
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 27-OCT-1995 (NJB)
      module subroutine zzektr31 ( ctx, handle, tree )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
!
!     Local variables
!
      integer(ip) :: c1page ( pgsizi )
      integer(ip) :: c2page ( pgsizi )
      integer(ip) :: child  ( 2 )
      integer(ip) :: delta
      integer(ip) :: i
      integer(ip) :: middle
      integer(ip) :: nlkeys
      integer(ip) :: nrkeys
      integer(ip) :: root
      integer(ip) :: rpage  ( pgsizi )
      integer(ip) :: sum

      root    =  tree

      call zzekpgri ( ctx, handle, root, rpage )

      nrkeys  =  rpage ( trnkr )

!
!     There must be exactly 1 key in the root.
!
      if ( nrkeys /= 1 ) then

         call ctx%setmsg ( 'Number of keys in root = #; should be 1.' )
         call ctx%errint ( '#',  nrkeys                               )
         call ctx%sigerr ( 'SPICE(BUG)'                               )
         return

      end if

!
!     Read in the child pages.  Get the key counts for these pages.
!
      child(1)  =  rpage ( trkidr + 1 )
      child(2)  =  rpage ( trkidr + 2 )

      call zzekpgri ( ctx, handle, child(1), c1page )
      call zzekpgri ( ctx, handle, child(2), c2page )

      nlkeys    =  c1page ( trnkc )
      nrkeys    =  c2page ( trnkc )
      sum       =  nlkeys + nrkeys

!
!     The sum of the number of keys in the two input nodes must
!     sum exactly to value representing an underflow level of 1 key.
!
      if (  sum  /=  ( 2*mnkeyc - 1 )  ) then

         call ctx%setmsg ( 'Number of keys in nodes LEFT = #; in RIGHT ' // &
                       '= #; counts summing to # were expected.'       )
         call ctx%errint ( '#',  nlkeys                                    )
         call ctx%errint ( '#',  nrkeys                                    )
         call ctx%errint ( '#',  2*mnkeyc-1                                )
         call ctx%sigerr ( 'SPICE(BUG)'                                    )
         return

      end if

!
!     Shift the key and data pointer in the root to right to allow
!     insertion of NLKEYS new entries on the left.  The child pointers
!     need not be shifted; they'll be overwritten later.
!
      rpage (trkeyr+nlkeys+1)  =  rpage (trkeyr+1)
      rpage (trdatr+nlkeys+1)  =  rpage (trdatr+1)

!
!     Copy in the keys, data pointers, and child pointers from the
!     left child into the root.  The number of predecessors of the
!     new keys is unchanged by this operation.
!
      rpage(trkeyr+1:trkeyr+1+nlkeys-1) = c1page(trkeyc+1:trkeyc+1+nlkeys-1)
      rpage(trdatr+1:trdatr+1+nlkeys-1) = c1page(trdatc+1:trdatc+1+nlkeys-1)
      rpage(trkidr+1:trkidr+1+nlkeys) = c1page(trkidc+1:trkidc+1+nlkeys)

!
!     Copy in the keys, data pointers, and child pointers from the
!     right child into the root.  The number of predecessors of the
!     new keys is increased by the value of the last key already
!     present.
!
      middle  =  nlkeys  +  1
      delta   =  rpage( trkeyr + middle )

      do i = 1, nrkeys
         rpage( trkeyr+middle+i )  =  c2page(trkeyc + i)  +  delta
      end do

      rpage(trdatr+middle+1:trdatr+middle+1+nrkeys-1) = c2page(trdatc+1:trdatc+1+nrkeys-1)
      rpage(trkidr+middle+1:trkidr+middle+1+nrkeys) = c2page(trkidc+1:trkidc+1+nrkeys)

!
!     Now the root must be updated.  The root now contains
!     the maximum allowed number of keys.  The depth of the tree
!     has decreased, as well as the number of nodes in the tree.
!
      rpage ( trnkr      )  =  mxkeyr
      rpage ( trdpth     )  =  rpage ( trdpth ) - 1
      rpage ( trnnod     )  =  rpage ( trnnod ) - 2

!
!     Write out the updated root.
!
      call zzekpgwi ( ctx, handle, root, rpage )

!
!     Free the pages occupied by the deleted children.
!
      do i = 1, 2
         call zzekpgfr ( ctx, handle, int, child(i)  )
      end do

      return
      end subroutine zzektr31

      !> Map a node in a tree to its DAS base integer address.
      !>
      !> The function returns the DAS integer base address of the
      !> specified node.
      !>
      !>## Detailed Input
      !>
      !>     NODE           is the number of a node in an EK tree.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the DAS integer base address of the
      !>     specified node.  This address is the predecessor of the first
      !>     DAS integer word occupied by the node.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  No error checking is done on the input node number.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine is a utility that allows callers to locate a node
      !>     of a tree in the DAS integer address space.  Most commonly, this
      !>     routine is used to locate the root of a tree.
      !>
      !>## Examples
      !>
      !>     See EKNSEG.
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
      !>-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
      module integer(ip) function zzektrbs ( ctx, node )
      use pepper_kinds, only: ip
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: node  !! Node number.
!
!     Just use the mapping supplied by the paging system.
!
      call zzekpgbs ( ctx, int, node, zzektrbs )

      return
      end function zzektrbs

      !> Free a tree:  deallocate all pages belonging to the tree.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the input tree is deeper than the maximum allowed depth
      !>         TRMXDP, the error SPICE(INVALIDFORMAT) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine cleans up the pages occupied by an EK tree; the pages
      !>     are deallocated by the EK paging system.  Freeing a tree allows
      !>     the pages previous occupied by the tree to be used for other
      !>     purposes.
      !>
      !>## Examples
      !>
      !>     1)  Return the pages occupied by the tree whose root node number
      !>         is TREE.  Assume HANDLE is a file handle of the EK to which
      !>         the tree belongs:
      !>
      !>            CALL EKTRFR ( HANDLE, TREE )
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 09-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN to insert DAS file name into
      !>        long error messages.
      !>
      !>-    SPICELIB Version 1.2.0, 18-JUN-1999 (WLT)
      !>
      !>        Removed a redundant call to CHKIN.
      !>
      !>-    SPICELIB Version 1.1.0, 14-OCT-1996 (NJB)
      !>
      !>        Bug fix:  the original version was untested and had numerous
      !>        problems.
      !>
      !>-    SPICELIB Version 1.0.0, 22-OCT-1995 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 14-OCT-1996 (NJB)
      !>
      !>        Bug fix:  the original version was untested and had numerous
      !>        problems.
      module subroutine zzektrfr ( ctx, handle, tree )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype

      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
!
!     Local parameters
!
      integer(ip), parameter :: paridx = 1

      integer(ip), parameter :: nkdidx = 2

      integer(ip), parameter :: begidx = 3

      integer(ip) :: depth
      integer(ip) :: first
      integer(ip) :: kidbas
      integer(ip) :: level
      integer(ip) :: nkeys
      integer(ip) :: nkids
      integer(ip) :: node
      integer(ip) :: page  ( pgsizi )
      integer(ip) :: remain
      integer(ip) :: stack ( 3, trmxdp )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Read in the root node.
!
      call zzekpgri ( ctx, handle, tree, page )

!
!     Check the depth of the tree.  If the tree is deeper than
!     we expected, we've a problem.
!
      depth  =  page ( trdpth )

      if ( depth > trmxdp ) then

         call ctx%setmsg ( 'Tree has depth #; max supported depth is #.' // &
                       'EK = #; TREE = #.'                           )
         call ctx%errint ( '#',  depth                                   )
         call ctx%errint ( '#',  trmxdp                                  )
         call ctx%errhan ( '#',  handle                                  )
         call ctx%errint ( '#',  tree                                    )
         call ctx%sigerr ( 'SPICE(INVALIDFORMAT)'                        )
         return

      end if

!
!     We traverse the tree in post-order fashion:  at each node,
!     we first delete all of the node's children in left-to-right
!     order, then we delete the node itself.  We use a stack to
!     keep track of the ancestors of the node we're currently
!     considering.
!
      level   =  1
      remain  =  page ( trnnod )
      node    =  tree

!
!     Initialize the child count and the location of the first
!     child in the current node.  The child count of the root is
!     one more than the number of keys in the root if the root has
!     children; otherwise, the child count is zero.
!
      nkeys  =  page ( trnkr )

      if ( depth == 1 ) then
         nkids = 0
      else
         nkids = nkeys + 1
      end if

      first  =  1

      do while ( remain > 0 )
!
!        At this point,
!
!           NODE is the current node to consider.
!           NKIDS is the number of children of NODE.
!           FIRST is the index of the first child in NODE.
!
         if ( nkids > 0 ) then
!
!           This node has children, so push the current node, the
!           number of children, and the location of the first child on
!           the stack.  Before incrementing the stack level, determine
!           the base address of the child pointers.
!
            if ( level == 1 ) then
               kidbas  =  trkidr
            else
               kidbas  =  trkidc
            end if

            stack ( paridx, level ) = node
            stack ( nkdidx, level ) = nkids
            stack ( begidx, level ) = first
            level                   = level + 1

!
!           Read in the first child node.
!
            node  =  page ( kidbas + first )

            call zzekpgri ( ctx, handle, node, page )

!
!           We've never visited this node before, so the node's
!           metadata is valid, and the first child pointer, if any,
!           is at location 1.
!
            nkeys  =  page ( trnkc )

            if ( level < depth ) then
               nkids  =  nkeys + 1
            else
               nkids  =  0
            end if

            first  =  1

         else
!
!           This node has no children.  We can free this page.
!
            call zzekpgfr ( ctx, handle, int, node )

            remain  =  remain - 1

!
!           Obtain the parent node by popping the stack.
!
            level   =  level - 1

            if ( level > 0 ) then

               node    =  stack ( paridx, level )
               first   =  stack ( begidx, level )
               nkids   =  stack ( nkdidx, level )

!
!              The parent has one less child, and the location of the
!              first child is the successor of the stored location.
!
               nkids  =  nkids - 1
               first  =  first + 1

!
!              The parent page has been overwritten; read it back in.
!
               call zzekpgri ( ctx, handle, node, page )

            end if

         end if
!
!        On this pass through the loop, we either visited a node
!        for the first time, or we deleted a node.  Therefore, we
!        made progress toward loop termination.
!
      end do

      return
      end subroutine zzektrfr

      !> Initialize an EK tree, returning the root of the tree.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>## Detailed Output
      !>
      !>     TREE           is the root node number of the tree created by
      !>                    this routine.  The root node number is used by the
      !>                    EK tree routines to identify the tree.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine is used to create a new, empty EK tree.  The
      !>     tree has a root node, but no keys are contained in the root.
      !>     The metadata area of the tree is initialized.
      !>
      !>## Examples
      !>
      !>     See EKBSEG.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 09-OCT-2021 (NJB)
      !>
      !>        Now PAGE is saved and initialized. This is not a bug fix;
      !>        it was done to pacify Valgrind.
      !>
      !>-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
      module subroutine zzektrit ( ctx, handle, tree )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(out) :: tree  !! Root of tree.
!
!     Local variables
!
      integer(ip) :: base
      integer(ip) :: p
      if ( .not. allocated ( ctx%zzektrit_state ) ) allocate ( ctx%zzektrit_state )
      associate ( page => ctx%zzektrit_state%page )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Start out by allocating a DAS integer page.  We'll write the root
!     node out to this page.
!
      call zzekpgal ( ctx, handle, int, p, base )

      page ( trtype )  =  trvers
      page ( trnnod )  =  1
      page ( trnkey )  =  0
      page ( trnkr  )  =  0
      page ( trdpth )  =  1

!
!     Set all keys to zero; set all child and data pointers to null.
!
      page(trkeyr+1:trkeyr+1+mxkeyr-1) = 0
      page(trdatr+1:trdatr+1+mxkeyr-1) = 0
      page(trkidr+1:trkidr+1+mxkidr-1) = 0

!
!     Write out the page.
!
      call zzekpgwi ( ctx, handle, p, page )

!
!     The identifier we return is just the page number of the tree's
!     root.
!
      tree  =  p

      return
      end associate
      end subroutine zzektrit

      !> Locate a specified key.  Return metadata describing the node
      !> containing the key.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     KEY            is an absolute key.  In EK trees, absolute keys are
      !>                    just ordinal positions relative to the leftmost
      !>                    element of the tree, with the leftmost element
      !>                    having position 1.  So setting KEY to 10, for
      !>                    example, indicates that the output VALUE is the
      !>                    10th item in the tree.
      !>
      !>                    KEY must be in the range 1 : NKEYS, where
      !>                    NKEYS is the number of keys in the tree.
      !>
      !>## Detailed Output
      !>
      !>     IDX            is the node-relative index of KEY:  this is the
      !>                    ordinal position of KEY relative to other keys
      !>                    in the same node.
      !>
      !>     NODE           is the number of the node containing KEY.
      !>
      !>     NOFFST         is the offset of NODE.  This is the count of the
      !>                    keys that precede every key in the subtree headed
      !>                    by NODE.  Adding NOFFST to any relative key stored
      !>                    in NODE will convert that key to an absolute key.
      !>
      !>     LEVEL          is the level of NODE in the tree.  The root is at
      !>                    level 1, children of the root are at level 2, and
      !>                    so on.
      !>
      !>     VALUE          is the integer value associated with the input key.
      !>                    Normally, this value is a data pointer.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the input key is out of range, the error
      !>         SPICE(INDEXOUTOFRANGE) is signaled.
      !>
      !>     4)  If the tree traversal fails to terminate at the leaf node
      !>         level, the error SPICE(BUG) is signaled.
      !>
      !>     5)  If the key is in range, but the key is not found, the error
      !>         SPICE(BUG) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine obtains the value associated with a key, and also
      !>     returns metadata describing the node containing the key and the
      !>     key's position in the node.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRUI.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN to insert DAS file name into
      !>        long error messages.
      !>
      !>        Added initializers for variables used to save old
      !>        values.
      !>
      !>-    Beta Version 1.0.0, 26-OCT-1995 (NJB)
      module subroutine zzektrlk ( ctx, handle, tree, key, idx, node, noffst, level, value )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlei
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: key  !! Key corresponding to value.
      integer(ip), intent(out) :: idx  !! Node-relative index of KEY.
      integer(ip), intent(out) :: node  !! Node containing key.
      integer(ip), intent(out) :: noffst  !! Offset of NODE.
      integer(ip), intent(out) :: level  !! Level of NODE.
      integer(ip), intent(out) :: value  !! Value associated with KEY.
      integer(ip), parameter :: acclen = 15
      integer(ip) :: depth
      integer(ip) :: plus
      logical :: found
      integer(ip) :: prvkey
      integer(ip) :: minus
      character(acclen) :: access
      logical :: samtre
      integer(ip) :: datbas
      integer(ip) :: child
      integer(ip) :: newkey
      integer(ip) :: totkey
      integer(ip) :: prev
      logical :: samkey
      if ( .not. allocated ( ctx%zzektrlk_state ) ) allocate ( ctx%zzektrlk_state )
      associate ( oldhan => ctx%zzektrlk_state%oldhan, &
                  oldidx => ctx%zzektrlk_state%oldidx, &
                  oldlvl => ctx%zzektrlk_state%oldlvl, &
                  oldmax => ctx%zzektrlk_state%oldmax, &
                  oldnod => ctx%zzektrlk_state%oldnod, &
                  oldnof => ctx%zzektrlk_state%oldnof, &
                  oldtre => ctx%zzektrlk_state%oldtre, &
                  oldkey => ctx%zzektrlk_state%oldkey, &
                  oldval => ctx%zzektrlk_state%oldval, &
                  page => ctx%zzektrlk_state%page, &
                  first => ctx%zzektrlk_state%first, &
                  leaf => ctx%zzektrlk_state%leaf, &
                  rdonly => ctx%zzektrlk_state%rdonly )
!
!     Use discovery check-in in this puppy.
!
!     Nothing found to begin with.
!
      found  =  .false.

      if ( first ) then
!
!        Find out the access method for the current file.
!
         call dasham ( ctx, handle, access )

         rdonly  =    access == 'READ'
         samkey  =    .false.
         samtre  =    .false.
         leaf    =    .false.
         first   =    .false.

      else
!
!        See whether we're looking at the same key, or at least
!        the same tree, as last time.  Note that for the tree to
!        be guaranteed to be the same, it must belong to a file open
!        for read access only.
!
         if ( handle /= oldhan ) then

            call dasham ( ctx, handle, access )

            rdonly  =    access == 'READ'
            samtre  =    .false.
            samkey  =    .false.

         else

            samtre  =  ( tree  ==  oldtre )  .and.   rdonly
            samkey  =  ( key   ==  oldkey )  .and.   samtre

         end if

      end if

!
!     If we're lucky enough to be getting a request for the previously
!     returned key, we're set.  If we've been asked for a key that is
!     very close to the previously requested key, we still may make
!     out pretty well.
!
      if ( samkey ) then
!
!        It's the same key as last time.
!
         idx     =  oldidx
         node    =  oldnod
         noffst  =  oldnof
         level   =  oldlvl
         value   =  oldval

         return

      else if ( samtre .and. leaf ) then
!
!        Compute the margins around the old key.  Keys that fall within
!        the interval defined by the old key and these margins are on
!        the same page as the old key.
!
         plus   =  oldmax - oldidx
         minus  =  oldidx - 1

         if (        (  key  <=  oldkey+plus   ) &
               .and. (  key  >=  oldkey-minus  )  ) then
!
!           The requested key lies on the same page as the old key.
!
            level   =  oldlvl

            if ( level == 1 ) then
               datbas  =  trdatr
            else
               datbas  =  trdatc
            end if

            idx     =  oldidx + ( key - oldkey )
            node    =  oldnod
            noffst  =  oldnof
            value   =  page ( datbas + idx )

            oldidx  =  idx
            oldkey  =  key
            oldval  =  value

            return

         end if

      end if

!
!     If we arrived here, we have some actual work to do.
!     Start out by looking at the root page.  Save the tree depth;
!     we'll use this for error checking.
!
      call zzekpgri ( ctx, handle, tree, page )

      depth  =  page ( trdpth )
      level  =  1

!
!     Find out how many keys are in the tree.  If KEY is outside
!     this range, we won't find it.
!
      totkey  =  page ( trnkey )

      if (  ( key < 1 )  .or.  ( key > totkey )  ) then

         call ctx%setmsg ( 'Key = #; valid range = 1:#. Tree = #, file = #')
         call ctx%errint ( '#',     key                                    )
         call ctx%errint ( '#',     totkey                                 )
         call ctx%errint ( '#',     tree                                   )
         call ctx%errhan ( '#',     handle                                 )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'                        )
         return

      end if

!
!     Find the last key at this level that is less than or equal to
!     the requested key.
!
      prev   =  lstlei (  key,  page(trnkr),  page(trkeyr+1)  )

      if ( prev > 0 ) then
         prvkey =  page(trkeyr+prev)
      else
         prvkey =  0
      end if

!
!     If we were lucky enough to get an exact match, set our outputs
!     and return.  The key offset in the root is zero.
!
      if ( prvkey == key ) then

         noffst  =  0
         idx     =  prev
         node    =  tree
         value   =  page ( trdatr + idx )

         oldhan  =  handle
         oldtre  =  tree
         oldkey  =  key
         oldnof  =  noffst
         oldnod  =  node
         oldidx  =  idx
         oldlvl  =  level
         oldval  =  value
         oldmax  =  page ( trnkr )

         leaf    =  level  ==  depth

!
!        The root has no parent or siblings, so these values
!        remain set to zero.  The same is true of the parent keys.
!
         return

      end if

!
!     Still here?  Traverse the pointer path until we find the key
!     or run out of progeny.
!
      child   =  page ( trkidr + prev + 1 )
      noffst  =  prvkey

      do while (  ( child > 0 ) .and. ( .not. found )  )
!
!        Look up the child node.
!
         call zzekpgri ( ctx, handle, child, page )

         level  =  level + 1

         if ( level > depth ) then

            call ctx%setmsg ( 'Runaway node pointer chain.  Key = #; '// &
                          'valid range = 1:#. Tree = #, file = #'      )
            call ctx%errint ( '#',     key                                 )
            call ctx%errint ( '#',     totkey                              )
            call ctx%errint ( '#',     tree                                )
            call ctx%errhan ( '#',     handle                              )
            call ctx%sigerr ( 'SPICE(BUG)'                                 )
            return

         end if

!
!        Find the last key at this level that is less than or equal to
!        the requested key.  Since the keys we're looking at now are
!        ordinal positions relative to the subtree whose root is the
!        current node, we must subtract from KEY the position of the
!        node preceding the first key of this subtree.
!
         newkey  =  key - noffst
         prev    =  lstlei (  newkey,  page(trnkc),  page(trkeyc+1)  )

         if ( prev > 0 ) then
            prvkey =  page(trkeyc+prev)
         else
            prvkey =  0
         end if

!
!        If we were lucky enough to get an exact match, set our outputs
!        and return.  The key offset for the current node is stored
!        in NOFFST.
!
         if ( prvkey == newkey ) then

            found   =  .true.
            idx     =  prev
            node    =  child
            value   =  page ( trdatc + idx )

            oldhan  =  handle
            oldtre  =  tree
            oldkey  =  key
            oldnof  =  noffst
            oldnod  =  node
            oldidx  =  idx
            oldlvl  =  level
            oldval  =  value
            oldmax  =  page ( trnkc )

            leaf    =  level  ==  depth

         else

            child   =  page ( trkidc + prev + 1 )
            noffst  =  prvkey + noffst

         end if

      end do

!
!     If we found the key, our outputs are already set.  If not, we've
!     got trouble.
!
      if ( .not. found ) then

         call ctx%setmsg ( 'Key #; valid range = 1:#. Tree = #, file = #.'// &
                       '  Key was not found.  This probably indicates'// &
                       ' a corrupted file or a bug in the EK code.'    )
         call ctx%errint ( '#',     key                                    )
         call ctx%errint ( '#',     totkey                                 )
         call ctx%errint ( '#',     tree                                   )
         call ctx%errhan ( '#',     handle                                 )
         call ctx%sigerr ( 'SPICE(BUG)'                                    )
         return

      end if

      return
      end associate
      end subroutine zzektrlk

      !> Given a key, return general information pertaining to the key's
      !> parent node.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for read or write
      !>                    access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     NODE           is the node number of interest.
      !>
      !>## Detailed Output
      !>
      !>     PARENT         is the number of the parent node of the node
      !>                    containing KEY.  If KEY is in the root, PARENT is
      !>                    set to zero.
      !>
      !>     PKEY           is a key in PARENT.  If PARENT is set to zero,
      !>                    PKEY is set to zero as well.  PKEY is used to
      !>                    traverse a chain of ancestors towards the to root.
      !>
      !>     POFFST         is the key offset of PARENT; this is the offset
      !>                    that must be added to the node-relative key
      !>                    values in PARENT to convert them to absolute keys.
      !>
      !>     LPIDX          is the index in PARENT of the key `to the left'
      !>                    of the node containing KEY.  This key is the
      !>                    immediate predecessor of the first key in the
      !>                    subtree headed by the node containing KEY.
      !>
      !>                    The key indices in PARENT start at 1.  If PARENT
      !>                    contains no keys that precede the node containing
      !>                    KEY, LPIDX is set to zero.
      !>
      !>     LPKEY          is the absolute key located in PARENT at index
      !>                    LPIDX.  If PARENT contains no keys that precede the
      !>                    node containing KEY, LPKEY is set to zero.
      !>
      !>     LSIB           is the number of the left sibling node of the node
      !>                    containing KEY.  If PARENT contains no keys that
      !>                    precede the node containing KEY, then the node
      !>                    containing KEY has no left sibling, and LSIB is
      !>                    set to zero.
      !>
      !>     RPIDX          is the index in PARENT of the key `to the right'
      !>                    of the node containing KEY.  This key is the
      !>                    immediate successor of the last key in the
      !>                    subtree headed by the node containing KEY.
      !>
      !>                    If PARENT contains no keys that succeed the node
      !>                    containing KEY, RPIDX is set to zero.
      !>
      !>     RPKEY          is the absolute key located in PARENT at index
      !>                    RPIDX.  If PARENT contains no keys that succeed the
      !>                    node containing KEY, RPKEY is set to zero.
      !>
      !>     RSIB           is the number of the right sibling node of the node
      !>                    containing KEY.  If PARENT contains no keys that
      !>                    succeed the node containing KEY, then the node
      !>                    containing KEY has no right sibling, and RSIB is
      !>                    set to zero.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>     3)  If the input key is out of range, the error
      !>         SPICE(INDEXOUTOFRANGE) is signaled.
      !>
      !>     4)  If the input key is not found in the tree, the error
      !>         SPICE(ITEMNOTFOUND) is signaled.  This error most likely
      !>         indicates the presence of a serious bug in the EK software,
      !>         or that the input EK file has been corrupted.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine supports tree operations that involve identifying
      !>     the parent node of a specified node.  In particular, this
      !>     routine supports updating ancestors of a node when an insertion
      !>     or deletion occurs.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRUD, ZZEKTRUI.
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
      !>-    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN to insert DAS file name into
      !>        long error messages.
      !>
      !>-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
      module subroutine zzektrpi ( ctx, handle, tree, key, parent, pkey, poffst, lpidx, lpkey, lsib, rpidx, rpkey, rsib )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlei
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: key  !! Key belonging to node of interest.
      integer(ip), intent(out) :: parent  !! Parent node of the node containing KEY.
      integer(ip), intent(out) :: pkey  !! A key in the parent node.
      integer(ip), intent(out) :: poffst  !! Key offset of the parent node.
      integer(ip), intent(out) :: lpidx  !! Node-relative index of the left parent key.
      integer(ip), intent(out) :: lpkey  !! Left parent key.
      integer(ip), intent(out) :: lsib  !! Node number of left sibling.
      integer(ip), intent(out) :: rpidx  !! Node-relative index of the right parent key.
      integer(ip), intent(out) :: rpkey  !! Right parent key.
      integer(ip), intent(out) :: rsib  !! Node number of right sibling.
!
!     Local variables
!
      integer(ip) :: child
      integer(ip) :: lkey
      integer(ip) :: maxkey
      integer(ip) :: newkey
      integer(ip) :: offset
      integer(ip) :: page   ( pgsizi )
      integer(ip) :: prev
      integer(ip) :: prvkey
      integer(ip) :: totkey

      logical :: found

!
!     Use discovery check-in in this puppy.
!
!     Nothing found to begin with.
!
      found  =  .false.

!
!     Get a local copy of the input key.  We may overwrite the input
!     key when we set PKEY.
!
      lkey   =   key

!
!     Start out by reading in the root page.  The node level starts
!     out at 1.
!
      call zzekpgri ( ctx, handle, tree, page )

      parent  =  0
      pkey    =  0
      poffst  =  0
      lpidx   =  0
      lpkey   =  0
      lsib    =  0
      rpidx   =  0
      rpkey   =  0
      rsib    =  0

!
!     Find out how many keys are in the tree.  If LKEY is outside
!     this range, we won't find it.
!
      totkey  =  page ( trnkey )

      if (  ( lkey < 1 )  .or.  ( lkey > totkey )  ) then

         call ctx%setmsg ( 'Key = #; valid range = 1:#. Tree = #, file = #')
         call ctx%errint ( '#',     lkey                                   )
         call ctx%errint ( '#',     totkey                                 )
         call ctx%errint ( '#',     tree                                   )
         call ctx%errhan ( '#',     handle                                 )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'                        )
         return

      end if

!
!     Find the last key at this level that is less than or equal to
!     the requested key.
!
      prev   =  lstlei (  lkey,  page(trnkr),  page(trkeyr+1)  )

      if ( prev > 0 ) then
         prvkey =  page(trkeyr+prev)
      else
         prvkey =  0
      end if

!
!     If we were lucky enough to get an exact match, we can quit now.
!     The root has no parent so the output values remain set to zero.
!
      if ( prvkey == lkey ) return

!
!     Still here?  Traverse the pointer path until we find the key
!     or run out of progeny.
!
      offset  =  prvkey
      parent  =  tree
      pkey    =  page ( trkeyr + 1 )
      maxkey  =  page ( trnkr      )

      if ( prev > 0 ) then
         lpidx  =  prev
         lpkey  =  page ( trkeyr + lpidx )
         lsib   =  page ( trkidr + lpidx )
      else
         lpidx  =  0
         lpkey  =  0
         lsib   =  0
      end if

      if ( prev < maxkey ) then
         rpidx  =  prev + 1
         rpkey  =  page ( trkeyr + rpidx     )
         rsib   =  page ( trkidr + rpidx + 1 )
      else
         rpidx  =  0
         rpkey  =  0
         rsib   =  0
      end if

      child  =  page ( trkidr + prev + 1 )
      found  =  .false.

      do while (  ( child > 0 ) .and. ( .not. found )  )
!
!        Read in the child page.
!
         call zzekpgri ( ctx, handle, child, page )

!
!        Find the last key at this level that is less than or equal to
!        the requested key.  Since the keys we're looking at now are
!        ordinal positions relative to the subtree whose root is the
!        current node, we must subtract from LKEY the position of the
!        node preceding the first key of this subtree.
!
         newkey  =  lkey - offset
         prev    =  lstlei (  newkey,  page(trnkc),  page(trkeyc+1)  )

         if ( prev > 0 ) then
            prvkey =  page(trkeyc+prev)
         else
            prvkey =  0
         end if

!
!        If we were lucky enough to get an exact match, we can quit.
!        The outputs are set.
!
         if ( prvkey == newkey ) then

            found  =  .true.

         else
!
!           Record information from the current node before we read the
!           next child page.
!
            parent  =  child
            poffst  =  offset
            pkey    =  page ( trkeyc + 1 )  +  offset
            maxkey  =  page ( trnkc      )

            if ( prev > 0 ) then
               lpidx  =  prev
               lpkey  =  page ( trkeyc + lpidx )
               lsib   =  page ( trkidc + lpidx )
            else
               lpidx  =  0
               lpkey  =  0
               lsib   =  0
            end if

            if ( prev < maxkey ) then
               rpidx  =  prev + 1
               rpkey  =  page ( trkeyc + rpidx     )
               rsib   =  page ( trkidc + rpidx + 1 )
            else
               rpidx  =  0
               rpkey  =  0
               rsib   =  0
            end if

!
!           Update the offset of the tree headed by CHILD, and set
!           the new child node.
!
            offset  =  prvkey + offset
            child   =  page ( trkidc + prev + 1 )

         end if

      end do

!
!     If we found the key, our outputs are already set.  If not, we've
!     got trouble.
!
      if ( .not. found ) then

         call ctx%setmsg ( 'Key #; valid range = 1:#. Tree = #, file = #.'// &
                       '  Key was not found.  This probably indicates'// &
                       ' a corrupted file or a bug in the EK code.'    )
         call ctx%errint ( '#',     lkey                                   )
         call ctx%errint ( '#',     totkey                                 )
         call ctx%errint ( '#',     tree                                   )
         call ctx%errhan ( '#',     handle                                 )
         call ctx%sigerr ( 'SPICE(ITEMNOTFOUND)'                           )
         return

      end if

      return
      end subroutine zzektrpi

      !> Rotate a specified number of keys from one node, through
      !> the parent, into a neighboring sibling node.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     LEFT,
      !>     RIGHT          are the node numbers of a pair of nodes to
      !>                    be balanced.  LEFT and RIGHT must be neighboring
      !>                    subnodes of a common parent.
      !>
      !>     PARENT         is the node number of the common parent node of
      !>                    nodes LEFT, RIGHT.
      !>
      !>     PKIDX          is the `parent key index', that is, the
      !>                    node-relative index of the key in the parent that
      !>                    sits between PARENT's child node pointers to
      !>                    nodes LEFT and RIGHT.  The key at location PKIDX
      !>                    is the immediate successor of the greatest key in
      !>                    the subnode headed by LEFT.  It is the immediate
      !>                    predecessor of the least key in the subnode headed
      !>                    by RIGHT.
      !>
      !>     NROT           is the number of keys to rotate.  Positive counts
      !>                    indicate that keys are to be rotated from node
      !>                    LEFT to node RIGHT; negative counts indicate
      !>                    rotation in the reverse direction.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If either LEFT or RIGHT are actually the root, the error
      !>         SPICE(BUG) is signaled.
      !>
      !>     4)  If LEFT and RIGHT are not neighboring sibling nodes, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>     5)  The rotation is not allowed to create an overflow of more
      !>         than one key in the destination node, not an underflow of
      !>         more than one key in the source node.  If either restriction
      !>         is violated, the error SPICE(BUG) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     Insertions into and deletions from EK trees can result in
      !>     overflows or underflows of keys in nodes affected by these
      !>     operations.  Many times key count invariants can be restored by
      !>     moving keys from one node into an adjacent sibling node.  This
      !>     maneuver is called `balancing' the nodes.  The process of moving
      !>     keys from one node, through the parent, into a neighboring
      !>     sibling node is called `rotating' the keys.
      !>
      !>     Key rotation affects the parent node of the neighboring children
      !>     because one key of the parent sits between the children.  This
      !>     `parent key' gets moved into one of the children as keys are
      !>     rotated.  If the rotation is to the right, the parent key is the
      !>     largest key of the rotated set; if the rotation is to the left,
      !>     the parent key is the least of the rotated set.
      !>
      !>     When keys are rotated, their data values move along with them.
      !>     In general, child pointers move along with keys, but there are
      !>     some tricky points:
      !>
      !>        - The left and right child pointers of the parent key don't
      !>          get updated; they continue to point to the two children
      !>          LEFT and RIGHT.
      !>
      !>        - On a right rotation, the right child pointer of the key that
      !>          gets moved into the parent key's original position becomes
      !>          the first left child pointer of the right sibling.  The left
      !>          child pointer of this key doesn't get moved at all.
      !>
      !>        - On a left rotation, the left child pointer of the key that
      !>          gets moved into the parent key's original position becomes
      !>          the last right child pointer of the left sibling.  The right
      !>          child pointer of this key becomes the left child pointer of
      !>          the first key of RIGHT.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRBN.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
      module subroutine zzektrrk ( ctx, handle, tree, left, right, parent, pkidx, nrot )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      use pepper_inc_ekpage
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: left  !! Left node of pair to participate in rotation.
      integer(ip), intent(in) :: right  !! Right node of pair to participate in rotation.
      integer(ip), intent(in) :: parent  !! Parent node of pair to participate in rotation.
      integer(ip), intent(in) :: pkidx  !! Parent key index.
      integer(ip), intent(in) :: nrot  !! Number of keys to rotate.
!
!     Local variables
!
      integer(ip) :: datbas
      integer(ip) :: dpar
      integer(ip) :: drotat
      integer(ip) :: dshift
      integer(ip) :: futrpk
      integer(ip) :: i
      integer(ip) :: keybas
      integer(ip) :: kidbas
      integer(ip) :: lnkeys
      integer(ip) :: lnsize
      integer(ip) :: lpage  ( pgsizi )
      integer(ip) :: lsib
      integer(ip) :: nvopar
      integer(ip) :: ppage  ( pgsizi )
      integer(ip) :: remain
      integer(ip) :: rnkeys
      integer(ip) :: root
      integer(ip) :: rpage  ( pgsizi )
      integer(ip) :: rsib
      integer(ip) :: schlep
      integer(ip) :: subsiz

      if ( nrot == 0 ) return

      root  =  tree

      if ( ( left == root ) .or. ( right == root )  ) then

         call ctx%setmsg ( 'Input node is root; only children are '   // &
                       'eligible for key rotation.'                )
         call ctx%sigerr ( 'SPICE(BUG)'                                )

      end if

!
!     Read in the input nodes.
!
      call zzekpgri ( ctx, handle, left,   lpage )
      call zzekpgri ( ctx, handle, right,  rpage )
      call zzekpgri ( ctx, handle, parent, ppage )

      if ( ctx%failed() ) return

!
!     Set the base index of the parent keys.  This value depends on
!     whether the parent is the root.  Do the same for the pointer
!     bases.
!
      if ( parent == tree ) then
         keybas = trkeyr
         datbas = trdatr
         kidbas = trkidr
      else
         keybas = trkeyc
         datbas = trdatc
         kidbas = trkidc
      end if

!
!     Verify that LEFT and RIGHT are siblings, and that PARENT is
!     their common parent.
!
      lsib  =  ppage ( kidbas+pkidx   )
      rsib  =  ppage ( kidbas+pkidx+1 )

      if (  ( lsib /= left ) .or. ( rsib /= right )  ) then

         call ctx%setmsg ( 'LEFT, RIGHT, PARENT, and PKIDX are '   // &
                       'inconsistent. LEFT = #; RIGHT = #; '   // &
                       'PARENT = #; PKIDX = #; LSIB derived '  // &
                       'from PARENT = #; RSIB = #.'             )
         call ctx%errint ( '#',   left                              )
         call ctx%errint ( '#',   right                             )
         call ctx%errint ( '#',   parent                            )
         call ctx%errint ( '#',   pkidx                             )
         call ctx%errint ( '#',   lsib                              )
         call ctx%errint ( '#',   rsib                              )
         call ctx%sigerr ( 'SPICE(BUG)'                             )
         return

      end if

!
!     Get the key counts for the left and right nodes.
!
      lnkeys  =  lpage ( trnkc )
      rnkeys  =  rpage ( trnkc )

!
!     The requested rotation will not be permitted to cause an
!     underflow of more than one key in the source node, nor an
!     overflow of more than one key in the destination node.
!
      if ( nrot > 0 ) then

         if (      ( ( lnkeys - nrot ) < ( mnkeyc - 1 ) ) &
             .or.  ( ( rnkeys + nrot ) > ( mxkeyc + 1 ) ) ) then

            call ctx%setmsg ( 'Node # and right sibling # contain # and ' // &
                          '# keys respectively; rotation of # keys '  // &
                          'to the right will violate the key count '  // &
                          'bounds of #:#.'                             )
            call ctx%errint ( '#',  left                                   )
            call ctx%errint ( '#',  right                                  )
            call ctx%errint ( '#',  lnkeys                                 )
            call ctx%errint ( '#',  rnkeys                                 )
            call ctx%errint ( '#',  nrot                                   )
            call ctx%errint ( '#',  mnkeyc-1                               )
            call ctx%errint ( '#',  mxkeyc+1                               )
            call ctx%sigerr ( 'SPICE(BUG)'                                 )
            return

         end if

      else if ( nrot < 0 ) then

         if (      ( ( lnkeys - nrot ) > ( mxkeyc + 1 ) ) &
             .or.  ( ( rnkeys + nrot ) < ( mnkeyc - 1 ) ) ) then

            call ctx%setmsg ( 'Node # and right sibling # contain # and ' // &
                          '# keys respectively; rotation of # keys '  // &
                          'to the left will violate the key count '   // &
                          'bounds of #:#.'                             )
            call ctx%errint ( '#',  left                                   )
            call ctx%errint ( '#',  right                                  )
            call ctx%errint ( '#',  lnkeys                                 )
            call ctx%errint ( '#',  rnkeys                                 )
            call ctx%errint ( '#',  -nrot                                  )
            call ctx%errint ( '#',  mnkeyc-1                               )
            call ctx%errint ( '#',  mxkeyc+1                               )
            call ctx%sigerr ( 'SPICE(BUG)'                                 )
            return

         end if

      end if

!
!     Compute the size of the tree headed by the left subnode.  We'll
!     need this later.  The size of this tree is one less than the
!     difference of the parent key and its predecessor, if any.
!
      if ( pkidx == 1 ) then
         lnsize  =  ppage ( keybas + 1 ) -  1
      else
         lnsize  =  ppage(keybas+pkidx)  -  ppage(keybas+pkidx-1)  -  1
      end if

!
!     Now, the actions we take depend on whether we must schlep keys
!     to the right or left.
!
      if ( nrot > 0 ) then
!
!        We'll rotate keys to the right.  There are a bunch of numbers
!        to compute first:
!
!           -- The number of keys remaining in the input node:  REMAIN
!
!           -- The size of the subtree headed by the
!              rotated keys:  SUBSIZ
!
!           -- The offset delta to be applied to the rotated
!              keys:  DROTAT
!
!           -- The offset delta to be applied to the keys shifted
!              right in the sibling:  DSHIFT
!
!           -- The new value of the old right parent key,
!              which gets rotated into the sibling:  NVOPAR
!
!           -- The offset delta to apply to the new right parent key,
!              DPAR.  Note that the successors of this key in the
!              parent node remain unchanged.
!
         schlep  =  nrot
         remain  =  lnkeys  -    schlep

!
!        The size of the rotated subtree is the original size of the
!        subtree headed by LEFT, minus the value of the key preceding
!        the rotated subtree.  That key, which resides at location
!        REMAIN + 1, is the future right parent key; this key is also
!        the successor of the subtree left behind.
!
         futrpk  =  lpage  ( trkeyc+remain+1 )
         subsiz  =  lnsize - futrpk

!
!        The rotated set of keys will no longer be preceded by the
!        set of keys of size NEWRPK that they originally followed.
!
         drotat  =  - futrpk

!
!        The shifted keys in the right sibling get SUBSIZ + 1 new
!        predecessors.
!
         dshift  =   subsiz + 1

!
!        The old right parent key will become the successor of the
!        shifted subtree.  Its value is just one greater than the
!        size of this subtree.
!
         nvopar  =   dshift

!
!        The new parent key has DSHIFT fewer predecessors after
!        the rotation.
!
         dpar    =   - dshift

!
!        It's time for some action.  First of all, shift the keys
!        in the sibling to the right.  Their data pointers and child
!        pointers move along with them.  Update all the keys by
!        applying the shift delta to them.
!
!        Move the rightmost elements of each data component first.
!        Adjust the keys at the same time.  Note that the regions
!        allocated to keys, data pointers, and child pointers occupy
!        non-overlapping addresses, so the order in which we shift
!        these data sets is not important.  Within each data set, we
!        must be careful not to trash occupied addresses.
!
         do i = rnkeys, 1, -1
            rpage (trkeyc + i + schlep)  =  rpage (trkeyc + i) + dshift
         end do

         do i = rnkeys, 1, -1
            rpage (trdatc + i + schlep)  =  rpage (trdatc + i)
         end do

         do i = rnkeys+1, 1, -1
            rpage (trkidc + i + schlep)  =  rpage (trkidc + i)
         end do

!
!        `Move' the old parent key to its target destination in the
!        sibling.  Actually, only the data pointer is copied; the key
!        is simply set to its new value.
!
         rpage ( trkeyc + schlep )  =  nvopar
         rpage ( trdatc + schlep )  =  ppage ( datbas + pkidx )

!
!        `Move' the future parent key to its target destination in the
!        parent.  The data pointer is copied; the key is adjusted by
!        the offset delta we've computed.
!
         ppage ( datbas + pkidx )  =  lpage ( trdatc + remain  + 1    )
         ppage ( keybas + pkidx )  =  ppage ( keybas + pkidx ) + dpar

!
!        Rotate the subtree following the future parent key to its
!        destination in the sibling.  Update the keys to account for
!        their new offset.
!
         do i = 1, schlep-1
            rpage(trkeyc+i) = lpage(trkeyc+remain+1+i) + drotat
         end do

         rpage(trdatc+1:trdatc+1+schlep-1-1) = lpage(trdatc+remain+2:trdatc+remain+2+schlep-1-1)
         rpage(trkidc+1:trkidc+1+schlep-1) = lpage(trkidc+remain+2:trkidc+remain+2+schlep-1)

!
!        Update the key counts in both the input node and sibling.
!
         lpage ( trnkc )  =  lpage ( trnkc )  -  schlep
         rpage ( trnkc )  =  rpage ( trnkc )  +  schlep

!
!        Update the pages in the kernel.
!
         call zzekpgwi ( ctx, handle, parent, ppage )
         call zzekpgwi ( ctx, handle, left,   lpage )
         call zzekpgwi ( ctx, handle, right,  rpage )

      else
!
!        Rotation to the left is almost, but not quite, a mirror image
!        of rotation to the right.
!
         schlep  =  - nrot
         remain  =    rnkeys  -  schlep

!
!        The size of the rotated subtree is one less than the value of
!        the future parent key.  This key resides at location
!        SCHLEP and is also the predecessor of the subtree
!        left behind.
!
         futrpk  =  rpage( trkeyc + schlep )
         subsiz  =  futrpk - 1

!
!        The rotated set of keys will be preceded by the keys already
!        present in LEFT, as well as the key moved in from the parent
!        node.
!
         drotat  =  lnsize + 1

!
!        The shifted keys in the right sibling lose SUBSIZ + 1
!        predecessors.
!
         dshift  =  - ( subsiz + 1 )

!
!        The old parent key will become the successor of the
!        keys already in LEFT; it will be the predecessor of the
!        rotated subtree.
!
         nvopar  =   drotat

!
!        The new parent key has (-DSHIFT) more predecessors after
!        the rotation.
!
         dpar    =  - dshift

!
!        It's time for some action.
!
!        `Move' the old parent key to its target destination in the
!        input node.  Actually, only the data pointer is copied; the key
!        is simply set to its new value.
!
         lpage ( trkeyc + lnkeys + 1 )  =  nvopar
         lpage ( trdatc + lnkeys + 1 )  =  ppage ( datbas + pkidx )

!
!        `Move' the future parent key to its target destination in the
!        parent.  The data pointer is copied; the key is adjusted by
!        the offset delta we've computed.
!
         ppage ( datbas + pkidx )  =  rpage ( trdatc + schlep )
         ppage ( keybas + pkidx )  =  ppage ( keybas + pkidx  ) + dpar

!
!        Rotate the subtree following the future parent key to its
!        destination in the sibling.  Update the keys to account for
!        their new offset.
!
         lpage(trkeyc+lnkeys+2:trkeyc+lnkeys+2+schlep-1-1) = rpage(trkeyc+1:trkeyc+1+schlep-1-1)
         lpage(trdatc+lnkeys+2:trdatc+lnkeys+2+schlep-1-1) = rpage(trdatc+1:trdatc+1+schlep-1-1)
         lpage(trkidc+lnkeys+2:trkidc+lnkeys+2+schlep-1) = rpage(trkidc+1:trkidc+1+schlep-1)

         do i = 1, schlep-1
            lpage(trkeyc+lnkeys+1+i) = lpage(trkeyc+lnkeys+1+i) + drotat
         end do

!
!        Shift the remaining elements of the sibling to the left.
!        Their data pointers and child pointers move along with them.
!        Update all the keys by applying the shift delta to them.
!
!        Move the leftmost elements of each data component first.
!        Adjust the keys at the same time.
!
         do i = 1, remain
            rpage (trkeyc + i)  =  rpage (trkeyc + i + schlep) + dshift
         end do

         do i = 1, remain
            rpage (trdatc + i)  =  rpage (trdatc + i + schlep)
         end do

         do i = 1, remain+1
            rpage (trkidc + i)  =  rpage (trkidc + i + schlep)
         end do

!
!        Update the key counts in both the input node and sibling.
!
         lpage ( trnkc )  =  lpage ( trnkc )  +  schlep
         rpage ( trnkc )  =  rpage ( trnkc )  -  schlep

!
!        Update the pages in the kernel.
!
         call zzekpgwi ( ctx, handle, parent, ppage )
         call zzekpgwi ( ctx, handle, left,   lpage )
         call zzekpgwi ( ctx, handle, right,  rpage )

      end if

      return
      end subroutine zzektrrk

      !> Weed out redundant, fully qualified row vectors from a join row
      !> set union.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     NJRS           is the number of join row sets in a join row set
      !>                    union to be weeded.
      !>
      !>     BASES          is an array of base addresses, in the scratch area,
      !>                    of a collection of join row sets from which
      !>                    redundant row vectors are to be weeded out.  A row
      !>                    vector is is redundant if and only if it is
      !>                    identical to another row vector, and the qualifying
      !>                    segment vectors of the two row vectors are
      !>                    identical as well.
      !>
      !>## Detailed Output
      !>
      !>     NJRS           is the number of join row sets after redundant
      !>                    rows have been removed.  If any join row sets
      !>                    become empty as a result of this weeding-out,
      !>                    the count of join row sets is reduced accordingly.
      !>
      !>     BASES          is the set of bases of join rows in the join row
      !>                    set union after weeding has been completed.
      !>                    Bases of empty join row sets are compressed out;
      !>                    the valid elements of the array are the first
      !>                    NJRS elements of BASES, where NJRS has been
      !>                    updated by this routine.
      !>
      !>     NROWS          is the total number of rows in the join row set
      !>                    union after the weeding process is finished.
      !>
      !>     See $Particulars for a more detailed description of the effect of
      !>     this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If JRSBAS is not the base address of a structurally valid
      !>         join row set union, the results of this routine will be
      !>         unpredictable.
      !>
      !>     2)  If NJRS is non-positive, or if NJRS exceeds the maximum
      !>         allowed number of constraint relations MAXCON, the error
      !>         SPICE(INVALIDCOUNT) will be signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects:  it modifies the join row
      !>     set designated by the input argument JRSBAS.  Every redundant
      !>     row vector is removed, and join row sets from which row vectors
      !>     are removed are compressed.  Empty join row sets are removed
      !>     from the union, as reflected by updates to NJRS and BASES.
      !>
      !>     The principal purpose of this routine is to support execution of
      !>     queries involving OR clauses; such queries may cause row vectors
      !>     satisfying both disjuncts to be included multiple times in the
      !>     set of matching row vectors.
      !>
      !>     The layout of a join row set in the EK scratch area is shown
      !>     in the join row set parameter include file.
      !>
      !>## Examples
      !>
      !>     See EKSRCH.
      !>
      !>## Restrictions
      !>
      !>     1) Loading or unloading EK files between name resolution of the
      !>        the input query and passing the query to this routine will
      !>        invalidate the checking done by this routine, and may cause
      !>        the routine to fail.
      !>
      !>     2) Assumes redundant row vectors never occur in any join row set.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>
      !>## Version
      !>
      !>-    Beta Version 1.1.1,  03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.1.0,  8-JAN-1996 (WLT)
      !>
      !>        Replaced a call to REPMI with ERRINT in the first
      !>        error check.
      !>
      !>-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
      module subroutine zzekweed ( ctx, njrs, bases, nrows )
      use pepper_kinds, only: ip
      use pepper_inc_ekjrs
      use pepper_inc_ekqlimit
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: njrs  !! Number of join row sets in union.
      integer(ip), intent(inout) :: bases ( * )  !! Scratch area base addresses of join row sets.
      integer(ip), intent(out) :: nrows  !! Total number of row vectors in join row set union.
      integer(ip) :: nr_1(1)  !! one-element copy of nr (array argument)
      integer(ip) :: nprdrv_1(1)  !! one-element copy of nprdrv (array argument)
      integer(ip) :: prwbas_1(1)  !! one-element copy of prwbas (array argument)
      integer(ip) :: nprdsv_1(1)  !! one-element copy of nprdsv (array argument)
      integer(ip) :: ncndrv_1(1)  !! one-element copy of ncndrv (array argument)
      integer(ip) :: crwbas_1(1)  !! one-element copy of crwbas (array argument)
      integer(ip) :: ncndsv_1(1)  !! one-element copy of ncndsv (array argument)
      integer(ip) :: ntab_1(1)  !! one-element copy of ntab (array argument)
!
!     Local variables
!
      integer(ip) :: i
      integer(ip) :: base
      integer(ip) :: cand
      integer(ip), allocatable :: candsv ( : )
      integer(ip) :: crv
      integer(ip) :: crwbas
      integer(ip), allocatable :: crwvec ( : )
      integer(ip) :: csgbas
      integer(ip) :: csv
      integer(ip) :: j
      integer(ip) :: loc
      integer(ip) :: ncndrv
      integer(ip) :: ncndsv
      integer(ip) :: ndel
      integer(ip) :: nprdrv
      integer(ip) :: nprdsv
      integer(ip) :: nr
      integer(ip) :: nrloc
      integer(ip) :: nsvloc
      integer(ip) :: ntab
      integer(ip) :: pred
      integer(ip), allocatable :: predsv ( : )
      integer(ip) :: prv
      integer(ip) :: prwbas
      integer(ip), allocatable :: prwvec ( : )
      integer(ip) :: psgbas
      integer(ip) :: psv
      integer(ip) :: rvsize
      integer(ip) :: svsize

      logical :: hit

!
!     PEPPER: no maximum number of join row sets (SPICELIB: MXJRS = 200,
!     a conjunction of the WHERE clause each).
!
      if ( njrs < 1 ) then

         call ctx%setmsg ( 'The number of join row sets in the union is #' )
         call ctx%errint ( '#', njrs                                       )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                           )
         return

      end if

!
!     Make sure that the addressing routines are properly initialized.
!
      call zzekvset ( ctx, njrs, bases )

!
!     Get the segment vector and row vector sizes.  The sizes that
!     apply to the first join row set will suffice throughout.
!
      loc     =  bases(1) + jtcidx

      call zzeksrd ( ctx, loc, loc, ntab_1 )
      ntab = ntab_1(1)
      allocate ( candsv(ntab), predsv(ntab), crwvec(ntab+1), prwvec(ntab+1) )

      svsize  =  ntab
      rvsize  =  ntab + 1

!
!     Mark redundant rows vectors for deletion.  One saving grace is
!     that redundant rows can never occur in the same join row set, as
!     long as that join row set represents a set of rows satisfying
!     a conjunction of constraints.
!
      do cand = 2, njrs
!
!        We'll compare row vectors in the CAND join row set to row
!        vectors in the preceding join row sets.  Only row vectors
!        corresponding to matching segment vectors need be compared.
!        Therefore, we'll loop over the segment vectors in the CAND
!        join row set, and for each such segment vector, loop over the
!        segment vectors in the preceding join row sets.  If a match
!        occurs, we'll compare row vectors corresponding to those
!        segment vectors.
!
!        NCNDSV will contain the number of segment vectors in the
!        `candidate' join row set.
!
         nsvloc  =  bases(cand)  +  jscidx

         call zzeksrd ( ctx, nsvloc, nsvloc, ncndsv_1 )
         ncndsv = ncndsv_1(1)

         do csv = 1, ncndsv
!
!           Look up the candidate segment vector.
!
            csgbas  =  bases(cand)  +  jsvbas  +  (csv-1)*svsize

            call zzeksrd  ( ctx, csgbas+1,  csgbas+svsize,  candsv  )

!
!           Get the row vector count and base address of the set of
!           row vectors for the candidate segment vector, in case
!           we need them.  (Referring to the diagram of the join
!           row set structure in the join row set parameter include
!           file may be helpful here.)
!
            base  =      bases(cand) &
                      +  jsvbas &
                      +  ncndsv  * svsize &
                      +  (csv-1) * 2

            call zzeksrd ( ctx, base+1, base+1, crwbas_1 )
            crwbas = crwbas_1(1)
            crwbas = crwbas + bases(cand)

            call zzeksrd ( ctx, base+2, base+2, ncndrv_1 )
            ncndrv = ncndrv_1(1)

!
!           For the current predecessor join row set, look up the
!           segment vectors in that join row set and compare them to the
!           candidate.
!
            do pred = 1, cand-1
!
!              Get the count of segment vectors in the current
!              predecessor join row set.
!
               nsvloc = bases(pred) + jscidx

               call zzeksrd ( ctx, nsvloc, nsvloc, nprdsv_1 )
               nprdsv = nprdsv_1(1)

               do psv = 1, nprdsv
!
!                 Look up the predecessor segment vector.
!
                  psgbas  =  bases(pred)  +  jsvbas  +  (psv-1)*svsize

                  call zzeksrd  ( ctx, csgbas+1,  csgbas+svsize,  predsv  )

!
!                 Compare the segment vectors and hope for the best.
!
                  if (  all(candsv(1:svsize) == predsv(1:svsize))  ) then
!
!                    Unfortunately, the two segment vectors match, so
!                    there's something to do.  We'll have to compare
!                    every row vector corresponding to the candidate
!                    segment vector with every row vector corresponding
!                    to the predecessor.
!
!                    Get the row vector count and base address of the
!                    set of row vectors for the current predecessor
!                    segment vector.  We already have on hand the
!                    corresponding quantities for the candidate
!                    segment vector.
!

                     base  =      bases(pred) &
                               +  jsvbas &
                               +  nprdsv  * svsize &
                               +  (psv-1) * 2

                     call zzeksrd ( ctx, base+1, base+1, prwbas_1 )
                     prwbas = prwbas_1(1)
                     prwbas = prwbas + bases(pred)

                     call zzeksrd ( ctx, base+2, base+2, nprdrv_1 )
                     nprdrv = nprdrv_1(1)

!
!                    Compare all row vectors.
!
                     do crv = 1, ncndrv

                        base  =  crwbas + (crv-1)*rvsize
                        call zzeksrd ( ctx, base+1, base+rvsize, crwvec )

                        prv   =  1
                        hit   =  .false.

                        do while (         ( prv <=  nprdrv ) &
                                    .and.  (     .not. hit    )   )

                           base  =  prwbas + (prv-1)*rvsize
                           call zzeksrd ( ctx, base+1, base+rvsize, prwvec )

                           if ( all(crwvec(1:rvsize) == prwvec(1:rvsize)) ) then
!
!                             The row vectors, together with their
!                             qualifying segment vectors, match.  The
!                             higher-indexed vector is considered
!                             redundant.  To mark this vector for
!                             deletion, we simply zero out the first
!                             element of the row vector.  This makes the
!                             row vector invalid, so it will not match
!                             any valid row vector we see later.
!
                              base  =  crwbas + (crv-1)*rvsize
                              call zzeksupd ( ctx, base+1, base+1, [ 0 ] )
                              hit   =  .true.
                           else
                              prv   =  prv + 1
                           end if

                        end do

                     end do

                  end if
!
!                 We've finished comparing row vectors for a pair of
!                 segment vectors, if it was necessary to do so.
!

               end do
!
!              We've compared all segment vectors in the current
!              predecessor join row set with the candidate segment
!              vector.
!

            end do
!
!           We've compared all segment vectors in all predecessor join
!           row sets to the current segment vector.
!

         end do
!
!        We've compared the candidate join row set to its predecessors.
!

      end do
!
!     We've compared all of the join row sets.
!
!     Now, clean up the join row set union by compressing out deleted
!     rows, segment vectors, and join row sets.
!
      j      =  1
      ndel   =  0

      do i = 1, njrs
!
!        Compress the current join row set.  If it ends up empty,
!        expel it from the union.
!
         call zzekjsqz ( ctx, bases(i) )

         nrloc = bases(i) + jrcidx
         call zzeksrd ( ctx, nrloc, nrloc, nr_1 )
         nr = nr_1(1)

         if ( nr == 0 ) then
!
!           This entire join row set can be deleted from the union.
!           Consider the next join row set.
!
            ndel     =  ndel + 1

         else

            bases(j) =  bases(i)
            j        =  j + 1

         end if

      end do

      njrs  = njrs - ndel

!
!     Count the rows remaining after our clean-up operation.
!
      nrows = 0

      do i = 1, njrs

         nrloc = bases(i) + jrcidx

         call zzeksrd ( ctx, nrloc, nrloc, nr_1 )
         nr = nr_1(1)

         nrows = nrows + nr

      end do

      return
      end subroutine zzekweed

      !> Open a new E-kernel file and prepare the file for writing.
      !>
      !>## Detailed Input
      !>
      !>     FNAME    is the name of a new E-kernel file to be created.
      !>
      !>     IFNAME   is the internal file name of a new E-kernel. The
      !>              internal file name may be up to 60 characters in
      !>              length.
      !>
      !>     NCOMCH   is the amount of space, measured in characters, to
      !>              be allocated in the comment area when the new EK
      !>              file is created. It is not necessary to allocate
      !>              space in advance in order to add comments, but
      !>              doing so may greatly increase the efficiency with
      !>              which comments may be added. Making room for
      !>              comments after data has already been added to the
      !>              file involves moving the data, and thus is slower.
      !>
      !>              NCOMCH must be greater than or equal to zero.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the EK handle of the file designated by FNAME.
      !>              This handle is used to identify the file to other
      !>              EK routines.
      !>
      !>## Parameters
      !>
      !>     FTSIZE   is the maximum number of DAS files that a user can
      !>              have open simultaneously. This includes any files used
      !>              by the DAS system.
      !>
      !>              See the include file das.inc for the actual value of
      !>              this parameter.
      !>
      !>## Exceptions
      !>
      !>     1)  If NCOMCH is less than zero, the error SPICE(INVALIDCOUNT)
      !>         is signaled. No file will be created.
      !>
      !>     2)  If IFNAME is invalid, an error is signaled by a routine in the
      !>         call tree of this routine.
      !>
      !>     3)  If the indicated file cannot be opened, an error is signaled
      !>         by a routine in the call tree of this routine. The new file
      !>         will be deleted.
      !>
      !>     4)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading ek.req for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects: it opens and prepares
      !>     an EK for addition of data.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) This example demonstrates how to open a new EK file, creating
      !>        the two segments described below, without any records.
      !>
      !>        The EK file will contain two segments, the first containing
      !>        the DATAORDERS table and the second containing the DATAITEMS
      !>        table.
      !>
      !>        The E-kernel DATAORDERS table called consists of the set of
      !>        columns listed below:
      !>
      !>           DATAORDERS
      !>
      !>              Column Name     Data Type
      !>              -----------     ---------
      !>              ORDER_ID        INTEGER
      !>              CUSTOMER_ID     INTEGER
      !>              LAST_NAME       CHARACTER*(*)
      !>              FIRST_NAME      CHARACTER*(*)
      !>              ORDER_DATE      TIME
      !>              COST            DOUBLE PRECISION
      !>
      !>        The columns of the DATAITEMS table are shown below:
      !>
      !>           DATAITEMS
      !>
      !>              Column Name     Data Type
      !>              -----------     ---------
      !>              ITEM_ID         INTEGER
      !>              ORDER_ID        INTEGER
      !>              ITEM_NAME       CHARACTER*(*)
      !>              DESCRIPTION     CHARACTER*(*)
      !>              PRICE           DOUBLE PRECISION
      !>
      !>        Note that it is not necessary to populate the first segment
      !>        with data before starting the second segment, or before
      !>        closing the EK.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EKOPN_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Include the EK Column Name Size (CNAMSZ)
      !>C
      !>      INCLUDE 'ekcnamsz.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         EKNAME
      !>      PARAMETER           ( EKNAME  = 'ekopn_ex1.bdb' )
      !>
      !>      INTEGER               DECLEN
      !>      PARAMETER           ( DECLEN = 200 )
      !>
      !>      INTEGER               NAMLEN
      !>      PARAMETER           ( NAMLEN = 40  )
      !>
      !>      INTEGER               NCOLS
      !>      PARAMETER           ( NCOLS  = 6   )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(DECLEN)    CDECLS ( NCOLS )
      !>      CHARACTER*(CNAMSZ)    CNAMES ( NCOLS )
      !>      CHARACTER*(NAMLEN)    IFNAME
      !>
      !>      INTEGER               HANDLE
      !>      INTEGER               NRESVC
      !>      INTEGER               SEGNO
      !>
      !>C
      !>C     Open a new EK file.  For simplicity, we will not
      !>C     reserve any space for the comment area, so the
      !>C     number of reserved comment characters is zero.
      !>C     The variable IFNAME is the internal file name.
      !>C
      !>      NRESVC  =  0
      !>      IFNAME  =  'Test EK/Created 01-JUN-2019'
      !>
      !>      CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE )
      !>
      !>C
      !>C     Set up the table and column names and declarations
      !>C     for the DATAORDERS segment.  We'll index all of
      !>C     the columns.  All columns are scalar, so we omit
      !>C     the size declaration.  Only the COST column may take
      !>C     null values.
      !>C
      !>      CNAMES(1) =  'ORDER_ID'
      !>      CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE'
      !>
      !>      CNAMES(2) =  'CUSTOMER_ID'
      !>      CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE'
      !>
      !>      CNAMES(3) =  'LAST_NAME'
      !>      CDECLS(3) =  'DATATYPE = CHARACTER*(*), ' //
      !>     .             'INDEXED  = TRUE'
      !>
      !>      CNAMES(4) =  'FIRST_NAME'
      !>      CDECLS(4) =  'DATATYPE = CHARACTER*(*), ' //
      !>     .             'INDEXED  = TRUE'
      !>
      !>      CNAMES(5) =  'ORDER_DATE'
      !>      CDECLS(5) =  'DATATYPE = TIME, INDEXED  = TRUE'
      !>
      !>      CNAMES(6) =  'COST'
      !>      CDECLS(6) =  'DATATYPE = DOUBLE PRECISION,' //
      !>     .             'INDEXED  = TRUE,'             //
      !>     .             'NULLS_OK = TRUE'
      !>
      !>C
      !>C     Start the first segment. Since we have no data for this
      !>C     segment, start the segment by just defining the new
      !>C     segment's schema.
      !>C
      !>      CALL EKBSEG ( HANDLE, 'DATAORDERS', 6,
      !>     .              CNAMES, CDECLS,       SEGNO )
      !>
      !>C
      !>C     At this point, the second segment could be
      !>C     created by an analogous process.  In fact, the
      !>C     second segment could be created at any time; it is
      !>C     not necessary to populate the first segment with
      !>C     data before starting the second segment.
      !>C
      !>C     Set up the table and column names and declarations
      !>C     for the DATAITEMS segment.  We'll index all of
      !>C     the columns.  All columns are scalar, so we omit
      !>C     the size declaration.
      !>C
      !>      CNAMES(1) =  'ITEM_ID'
      !>      CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE'
      !>
      !>      CNAMES(2) =  'ORDER_ID'
      !>      CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE'
      !>
      !>      CNAMES(3) =  'ITEM_NAME'
      !>      CDECLS(3) =  'DATATYPE = CHARACTER*(*),' //
      !>     .             'INDEXED  = TRUE'
      !>
      !>      CNAMES(4) =  'DESCRIPTION'
      !>      CDECLS(4) =  'DATATYPE = CHARACTER*(*),' //
      !>     .             'INDEXED  = TRUE'
      !>
      !>      CNAMES(5) =  'PRICE'
      !>      CDECLS(5) =  'DATATYPE = DOUBLE PRECISION,' //
      !>     .             'INDEXED  = TRUE'
      !>
      !>C
      !>C     Start the new segment. Since we have no data for this
      !>C     segment, start the segment by just defining the new
      !>C     segment's schema.
      !>C
      !>      CALL EKBSEG ( HANDLE, 'DATAITEMS', 5,
      !>     .              CNAMES, CDECLS,      SEGNO )
      !>
      !>C
      !>C     Close the file by a call to EKCLS.
      !>C
      !>      CALL EKCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program is executed, no output is presented on
      !>        screen. After run completion, a new EK file exists in the
      !>        output directory.
      !>
      !>## Restrictions
      !>
      !>     1)  No more than FTSIZE DAS files may be opened simultaneously.
      !>         See the include file das.inc for the value of FTSIZE.
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example, and updated $Restrictions and
      !>        $Parameters sections.
      !>
      !>-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
      module subroutine ekopn ( ctx, fname, ifname, ncomch, handle )
      use pepper_kinds, only: ip
      use pepper_inc_ektype
      use pepper_inc_ekfilpar
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: fname  !! Name of EK file.
      character(*), intent(in) :: ifname  !! Internal file name.
      integer(ip), intent(in) :: ncomch  !! The number of characters to reserve for comments.
      integer(ip), intent(out) :: handle  !! Handle attached to new EK file.

      integer(ip), parameter :: nwc = 1024

      integer(ip) :: base
      integer(ip) :: ncr
      integer(ip) :: p

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check the comment character count.
!
      if ( ncomch < 0 ) then

         call ctx%setmsg ( 'The number of reserved comment characters ' // &
                       'must be non-negative but was #.'            )
         call ctx%errint ( '#',  ncomch                                 )
         call ctx%sigerr ( 'SPICE(INVALIDCOUNT)'                        )
         return

      end if

!
!     A new DAS file is a must.  The file type is EK.
!     Reserve enough comment records to accommodate the requested
!     number of comment characters.
!
      ncr  =  ( nwc + ncomch - 1 ) / nwc

      call dasonw ( ctx, fname, 'EK', ifname, ncr, handle )

      if ( ctx%failed() ) return

!
!     Initialize the file for paged access.  The EK architecture
!     code is automatically set by the paging initialization routine.
!
      call zzekpgin ( ctx, handle )

      if ( ctx%failed() ) return

!
!     Allocate the first integer page for the file's metadata.  We
!     don't need to examine the page number; it's 1.
!
      call zzekpgan ( ctx, handle, int, p, base )

!
!     Initialize a new tree.  This tree will point to the file's
!     segments.
!
      call zzektrit ( ctx, handle, p )

!
!     Save the segment pointer's root page number.
!
      call dasudi ( ctx, handle, base+sgtidx, base+sgtidx, [ p ] )

!
!     That's it.  We're ready to add data to the file.
!
      return
      end subroutine ekopn

      !> Open a scratch (temporary) E-kernel file and prepare the file
      !> for writing.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     HANDLE   is the EK file handle of the file opened by this
      !>              routine. This handle is used to identify the file
      !>              to other EK routines.
      !>
      !>## Parameters
      !>
      !>     FTSIZE   is the maximum number of DAS files that a user can
      !>              have open simultaneously. This includes any files used
      !>              by the DAS system.
      !>
      !>              See the include file das.inc for the actual value of
      !>              this parameter.
      !>
      !>## Exceptions
      !>
      !>     1)  If the indicated file cannot be opened, an error is signaled
      !>         by a routine in the call tree of this routine. The new file
      !>         will be deleted.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>## Files
      !>
      !>     This routine creates a temporary EK file; the file is deleted
      !>     when the calling program terminates or when the file is closed
      !>     using the SPICELIB routine EKCLS.
      !>
      !>     See the EK Required Reading ek.req for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects: it opens and prepares
      !>     an EK for addition of data. "Scratch" files are automatically
      !>     deleted when the calling program terminates normally or when
      !>     closed using the SPICELIB routine EKCLS.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose we want to create an E-kernel which contains a table
      !>        of items that have been ordered but we do not want to keep
      !>        the file. The columns of this table are shown below:
      !>
      !>           DATAITEMS
      !>
      !>              Column Name     Data Type
      !>              -----------     ---------
      !>              ITEM_ID         INTEGER
      !>              ORDER_ID        INTEGER
      !>              ITEM_NAME       CHARACTER*(*)
      !>              DESCRIPTION     CHARACTER*(*)
      !>              PRICE           DOUBLE PRECISION
      !>
      !>        This examples demonstrates how to open a scratch EK file;
      !>        create the segment described above, how to insert a new record
      !>        into it, and how to summarize its contents.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EKOPS_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Include the EK Column Name Size (CNAMSZ); the maximum
      !>C     length of an input query (MAXQRY), the maximum number of
      !>C     columns per segment (MXCLSG); and the maximum length of
      !>C     a table name (TNAMSZ).
      !>C
      !>      INCLUDE 'ekcnamsz.inc'
      !>      INCLUDE 'ekglimit.inc'
      !>      INCLUDE 'ekqlimit.inc'
      !>      INCLUDE 'ektnamsz.inc'
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               EKNSEG
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         TABLE
      !>      PARAMETER           ( TABLE   = 'DATAITEMS'      )
      !>
      !>      INTEGER               DECLEN
      !>      PARAMETER           ( DECLEN = 200 )
      !>
      !>      INTEGER               DESCLN
      !>      PARAMETER           ( DESCLN = 80  )
      !>
      !>C
      !>C     One value per row/column element.
      !>C
      !>      INTEGER               MAXVAL
      !>      PARAMETER           ( MAXVAL = 1  )
      !>
      !>      INTEGER               NAMLEN
      !>      PARAMETER           ( NAMLEN = 40  )
      !>
      !>      INTEGER               COLSLN
      !>      PARAMETER           ( COLSLN = 5   )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(DECLEN)    CDECLS ( MXCLSG )
      !>      CHARACTER*(CNAMSZ)    CNAMES ( MXCLSG )
      !>      CHARACTER*(NAMLEN)    CVALS  ( MAXVAL )
      !>      CHARACTER*(DESCLN)    DESCRP
      !>      CHARACTER*(4)         DTYPES ( MXCLSG )
      !>      CHARACTER*(NAMLEN)    ITEMNM
      !>      CHARACTER*(TNAMSZ)    TABNAM
      !>
      !>      DOUBLE PRECISION      DVALS  ( MAXVAL )
      !>      DOUBLE PRECISION      PRICE
      !>
      !>      INTEGER               ESIZE
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               ITEMID
      !>      INTEGER               IVALS  ( MAXVAL )
      !>      INTEGER               NCOLS
      !>      INTEGER               NROWS
      !>      INTEGER               NSEG
      !>      INTEGER               NVALS
      !>      INTEGER               ORDID
      !>      INTEGER               RECNO
      !>      INTEGER               SEGNO
      !>      INTEGER               SIZES  ( MXCLSG )
      !>      INTEGER               STRLNS ( MXCLSG )
      !>
      !>      LOGICAL               INDEXD ( MXCLSG )
      !>      LOGICAL               ISNULL
      !>      LOGICAL               NULLOK ( MXCLSG )
      !>
      !>C
      !>C     Open a scratch EK file to use for temporary
      !>C     storage.
      !>C
      !>      CALL EKOPS ( HANDLE )
      !>
      !>C
      !>C     Set up the table and column names and declarations
      !>C     for the DATAITEMS segment. We'll index all of
      !>C     the columns. All columns are scalar, so we omit
      !>C     the size declaration.
      !>C
      !>      CNAMES(1) =  'ITEM_ID'
      !>      CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE'
      !>
      !>      CNAMES(2) =  'ORDER_ID'
      !>      CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE'
      !>
      !>      CNAMES(3) =  'ITEM_NAME'
      !>      CDECLS(3) =  'DATATYPE = CHARACTER*(*),' //
      !>     .             'INDEXED  = TRUE'
      !>
      !>      CNAMES(4) =  'DESCRIPTION'
      !>      CDECLS(4) =  'DATATYPE = CHARACTER*(*),' //
      !>     .             'INDEXED  = TRUE'
      !>
      !>      CNAMES(5) =  'PRICE'
      !>      CDECLS(5) =  'DATATYPE = DOUBLE PRECISION,' //
      !>     .             'INDEXED  = TRUE'
      !>
      !>C
      !>C     Start the segment. Since we have no data for this
      !>C     segment, start the segment by just defining the new
      !>C     segment's schema.
      !>C
      !>      CALL EKBSEG ( HANDLE, TABLE,  COLSLN,
      !>     .              CNAMES, CDECLS, SEGNO  )
      !>
      !>C
      !>C     Append a new, empty record to the DATAITEMS
      !>C     table. Recall that the DATAITEMS table
      !>C     is in the first segment.  The call will return
      !>C     the number of the new, empty record.
      !>C
      !>      SEGNO = 1
      !>      CALL EKAPPR ( HANDLE, SEGNO, RECNO )
      !>
      !>C
      !>C     At this point, the new record is empty. We fill in the
      !>C     data here. Data items are filled in one column at a
      !>C     time. The order in which the columns are filled in is
      !>C     not important.  We use the different add column entry
      !>C     routines to fill in column entries.  We'll assume
      !>C     that no entries are null. All entries are scalar,
      !>C     so the entry size is 1.
      !>C
      !>      ISNULL   =  .FALSE.
      !>      ESIZE    =  1
      !>
      !>C
      !>C     The following variables will contain the data for
      !>C     the new record.
      !>C
      !>      ORDID    =   10011
      !>      ITEMID   =   531
      !>      ITEMNM   =  'Sample item'
      !>      DESCRP   =  'This sample item is used only in tests.'
      !>      PRICE    =   1345.67D0
      !>
      !>C
      !>C     Note that the names of the routines called
      !>C     correspond to the data types of the columns.
      !>C
      !>      CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'ORDER_ID',
      !>     .              ESIZE,  ORDID,  ISNULL               )
      !>
      !>      CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'ITEM_ID',
      !>     .              ESIZE,  ITEMID, ISNULL               )
      !>
      !>      CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'ITEM_NAME',
      !>     .              ESIZE,  ITEMNM, ISNULL               )
      !>
      !>      CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'DESCRIPTION',
      !>     .              ESIZE,  DESCRP, ISNULL               )
      !>
      !>      CALL EKACED ( HANDLE, SEGNO,  RECNO, 'PRICE',
      !>     .              ESIZE,  PRICE,  ISNULL               )
      !>
      !>C
      !>C     At this point, we could perform read operations
      !>C     on the EK.
      !>C
      !>C     Return the number of segments in the EK. Dump the
      !>C     desired summary information for each one.
      !>C
      !>      NSEG = EKNSEG( HANDLE )
      !>      WRITE(*,'(A,I3)') 'Number of segments =', NSEG
      !>      WRITE(*,*)
      !>
      !>      DO SEGNO = 1, NSEG
      !>
      !>         CALL EKSSUM (  HANDLE,  SEGNO,   TABNAM,  NROWS,
      !>     .                  NCOLS,   CNAMES,  DTYPES,  SIZES,
      !>     .                  STRLNS,  INDEXD,  NULLOK         )
      !>
      !>         WRITE(*,'(2A)')   'Table containing segment: ', TABNAM
      !>         WRITE(*,'(A,I2)') 'Number of rows          : ', NROWS
      !>         WRITE(*,'(A,I2)') 'Number of columns       : ', NCOLS
      !>         WRITE(*,'(A)')    'Table data              : '
      !>
      !>         DO I = 1, NCOLS
      !>
      !>            WRITE(*,'(2A)') '  Column: ', CNAMES(I)
      !>            WRITE(*,'(2A)') '  Type  : ', DTYPES(I)
      !>
      !>            DO RECNO = 1, NROWS
      !>
      !>               IF ( DTYPES(I) .EQ. 'CHR' ) THEN
      !>
      !>                  CALL EKRCEC ( HANDLE,    SEGNO, RECNO,
      !>     .                          CNAMES(I), NVALS,
      !>     .                          CVALS,     ISNULL       )
      !>
      !>                  IF ( ISNULL ) THEN
      !>
      !>                     WRITE(*,'(A)') '  Data  : <null>'
      !>
      !>                  ELSE
      !>
      !>                     WRITE(*,'(2A)') '  Data  : ', CVALS
      !>
      !>                  END IF
      !>
      !>               ELSE IF ( DTYPES(I) .EQ. 'DP' ) THEN
      !>
      !>                  CALL EKRCED ( HANDLE,    SEGNO, RECNO,
      !>     .                          CNAMES(I), NVALS,
      !>     .                          DVALS,     ISNULL       )
      !>
      !>                  IF ( ISNULL ) THEN
      !>
      !>                     WRITE(*,'(A)') '  Data  : <null>'
      !>
      !>                  ELSE
      !>
      !>                     WRITE(*,'(A,F9.2)') '  Data  : ', DVALS
      !>
      !>                  END IF
      !>
      !>               ELSE IF ( DTYPES(I) .EQ. 'INT' ) THEN
      !>
      !>                  CALL EKRCEI ( HANDLE,    SEGNO, RECNO,
      !>     .                          CNAMES(I), NVALS,
      !>     .                          IVALS,     ISNULL       )
      !>
      !>                  IF ( ISNULL ) THEN
      !>
      !>                     WRITE(*,'(A)') '  Data  : <null>'
      !>
      !>                  ELSE
      !>
      !>                     WRITE(*,'(A,I6)') '  Data  : ', IVALS
      !>
      !>                  END IF
      !>
      !>               ENDIF
      !>
      !>C
      !>C              There is no time data. Otherwise, we would need
      !>C              to use an LSK and EKRCED to read it
      !>C              (internally, it is stored as double precision).
      !>C
      !>               WRITE(*,*)
      !>
      !>            END DO
      !>
      !>         END DO
      !>
      !>         WRITE(*,*) '----------------------------------------'
      !>
      !>      END DO
      !>
      !>C
      !>C     Close the file. This will delete the scratch file
      !>C     and all the data will be lost.
      !>C
      !>      CALL EKCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        Number of segments =  1
      !>
      !>        Table containing segment: DATAITEMS
      !>        Number of rows          :  1
      !>        Number of columns       :  5
      !>        Table data              :
      !>          Column: ITEM_ID
      !>          Type  : INT
      !>          Data  :    531
      !>
      !>          Column: ORDER_ID
      !>          Type  : INT
      !>          Data  :  10011
      !>
      !>          Column: ITEM_NAME
      !>          Type  : CHR
      !>          Data  : Sample item
      !>
      !>          Column: DESCRIPTION
      !>          Type  : CHR
      !>          Data  : This sample item is used only in tests.
      !>
      !>          Column: PRICE
      !>          Type  : DP
      !>          Data  :   1345.67
      !>
      !>         ----------------------------------------
      !>
      !>        Note that after run completion, there is no EK file in the
      !>        output directory as scratch files are deleted when they are
      !>        closed or when the calling program terminates.
      !>
      !>## Restrictions
      !>
      !>     1)  No more than FTSIZE DAS files may be opened simultaneously.
      !>         See the include file das.inc for the value of FTSIZE.
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
      !>-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard and improved
      !>        the API documentation. Added complete code example and updated
      !>        $Parameters section.
      !>
      !>-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
      module subroutine ekops ( ctx, handle )
      use pepper_kinds, only: ip
      use pepper_inc_ektype
      use pepper_inc_ekfilpar
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: handle  !! File handle attached to new EK file.

      integer(ip) :: base
      integer(ip) :: p

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      call dasops ( ctx, handle )

      if ( ctx%failed() ) return

!
!     Initialize the file for paged access.  The EK architecture
!     code is automatically set by the paging initialization routine.
!
      call zzekpgin ( ctx, handle )

      if ( ctx%failed() ) return

!
!     Allocate the first integer page for the file's metadata.  We
!     don't need to examine the page number; it's 1.
!
      call zzekpgan ( ctx, handle, int, p, base )

!
!     Initialize a new tree.  This tree will point to the file's
!     segments.
!
      call zzektrit ( ctx, handle, p )

!
!     Save the segment pointer's root page number.
!
      call dasudi ( ctx, handle, base+sgtidx, base+sgtidx, [ p ] )

!
!     That's it.  We're ready to add data to the file.
!
      return
      end subroutine ekops

      !> Create a new type 1 index for a specified EK column.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK that is open for write
      !>                    access.
      !>
      !>     COLDSC         is the column descriptor of the column for
      !>                    which the index is to be created.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects:  it creates a new, empty
      !>     type 1 index for a specified EK column.  Though this routine
      !>     does not require a segment to be specified, normally indexes
      !>     are created for columns belonging to specific segments.
      !>
      !>     Type 1 indexes are implemented as DAS B*-trees.  The data
      !>     pointers of an index tree contain record numbers.  Therefore, the
      !>     tree implements an abstract order vector.
      !>
      !>     In order to support the capability of creating an index for a
      !>     column that has already been populated with data, this routine
      !>     does not check that the specified column is empty.  The caller
      !>     must populate the index appropriately to reflect the order of
      !>     elements in the associated column.
      !>
      !>## Examples
      !>
      !>     See EKBSEG.
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
      !>-    Beta Version 1.0.0, 06-NOV-1995 (NJB)
      module subroutine zzekcix1 ( ctx, handle, coldsc )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(inout) :: coldsc ( cdscsz )  !! Column descriptor. (PEPPER: changed by the routine, an input in SPICELIB's table)
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Before trying to actually write anything, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     An empty type 1 segment is just an empty B*-tree.  The root
!     page number of the tree serves as the index pointer.
!
      coldsc(ixtidx)  =  1

      call zzektrit ( ctx, handle, coldsc(ixpidx) )

      return
      end subroutine zzekcix1

      !> Parse an EK query that has been scanned and tokenized.
      !> Represent the result as an encoded EK query.
      !>
      !>## Detailed Input
      !>
      !>     QUERY          is a string containing the original input query.
      !>                    QUERY is used only for creating error messages.
      !>
      !>     NTOKEN         is the number of tokens in the input query.
      !>
      !>     LXBEGS,
      !>     LXENDS         are lexeme begin and end pointers; the Ith
      !>                    lexeme in the query is
      !>
      !>                       QUERY ( LXBEGS(I) : LXENDS(I) )
      !>
      !>                    (Lexemes are strings that correspond to tokens
      !>                     in the language.)
      !>
      !>     TOKENS         is an array of token codes.  The Ith element of
      !>                    TOKENS represents the Ith token in the scanned
      !>                    query.
      !>
      !>     VALUES         is an array of values associated with tokens; the
      !>                    Ith element of VALUES corresponds to the Ith
      !>                    token.  Keywords, for example, are distinguished
      !>                    by codes in the VALUES array.  Literal numeric
      !>                    and string tokens use the VALUES array to point
      !>                    to elements of NUMVLS or CHBEGS and CHENDS,
      !>                    respectively.  Some tokens don't need to use
      !>                    VALUES, but to simplify indexing, each token gets
      !>                    an element of this array.
      !>
      !>     NUMVLS         is an array of double precision numbers used to
      !>                    store the values corresponding to literal numeric
      !>                    tokens.
      !>
      !>     CHRBUF         is a string used to store the values of literal
      !>                    string tokens.
      !>
      !>     CHBEGS,
      !>     CHENDS         are pairs of begin and end pointers into CHRBUF.
      !>                    These pointers delimit character values
      !>                    associated with literal string tokens.
      !>
      !>## Detailed Output
      !>
      !>     EQRYI,
      !>     EQRYC,
      !>     EQRYD          are the integer, character, and numeric portions
      !>                    of an encoded form of the input query.  The
      !>                    SELECT, FROM, WHERE, and ORDER BY clauses of the
      !>                    input query are all represented in this encoding.
      !>                    WHERE clause constraints have been normalized.
      !>
      !>                    Normalized queries have their constraints grouped
      !>                    into a disjunction of conjunctions of relational
      !>                    expressions, as symbolized below:
      !>
      !>                          ( <rel_exp_1_1> and <rel_exp_1_2> and ... )
      !>                       or ( <rel_exp_2_1> and <rel_exp_2_2> and ... )
      !>                                     .
      !>                                     .
      !>                                     .
      !>                       or ( <rel_exp_N_1> and <rel_exp_N_2> and ... )
      !>
      !>     ERROR,
      !>     PRSERR         are, respectively, a flag indicating whether the
      !>                    input query parsed correctly, and a message
      !>                    describing the parse error, if one occurred.  If
      !>                    no error occurred, ERROR is .FALSE. and PRSERR
      !>                    is blank.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  Most of the exceptions that can occur on a call to
      !>         ZZEKPARS are caused by errors in the input query.  ZZEKPARS
      !>         attempts to diagnose these via the output error flag and
      !>         error message, instead of signaling errors.  The
      !>         error messages that ZZEKPARS can return are listed below.
      !>         In the messages shown, the symbol # is used to designate
      !>         a marker for which a value can be substituted in an actual
      !>         message.
      !>
      !>            The BY keyword was not found following the
      !>            ORDER keyword.
      !>
      !>            Invalid keyword at location #.
      !>            Actual token was: #
      !>
      !>            Table or column name expected at location
      !>            #. Actual token was: #
      !>
      !>            Table name expected at location #.
      !>            Actual token was: #
      !>
      !>            Column name expected at location #.
      !>            Actual token was: #
      !>
      !>            Table alias, comma, or keyword expected at
      !>            location #. Actual token was: #
      !>
      !>            Comma or keyword expected at
      !>            location #. Actual token was: #
      !>
      !>            Comma expected at location #. Actual token was: #
      !>
      !>            PRSERR  =  More tokens were expected in query.
      !>
      !>            The keyword # was expected at location
      !>            #. Actual token was: #
      !>
      !>            Invalid token at location #. Token was: #
      !>
      !>            PRSERR  =  Number of tables in "FROM" clause exceeds
      !>            allowed maximum of #.
      !>
      !>            PRSERR  =  Number of order-by columns exceeds allowed
      !>            maximum of #.
      !>
      !>            PRSERR  =  Number of SELECT columns exceeds allowed
      !>            maximum of #.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine determines whether a query is syntactically correct;
      !>     it transforms correct queries into the EK system's encoded query
      !>     representation.
      !>
      !>     The encoded queries output by this routine are not ready for
      !>     execution; they still must undergo name resolution, time value
      !>     conversion, and semantic checking.  See EKFIND for an example of
      !>     the normal sequence of query processing.
      !>
      !>## Examples
      !>
      !>     See the header of EKFIND for examples of valid and invalid
      !>     queries.
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
      !>-    SPICELIB Version 4.1.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    SPICELIB Version 4.1.0, 15-OCT-1996 (NJB)
      !>
      !>        Bug fix: default order sense was not encoded when ORDER-BY
      !>        clause was not the last clause of the query.
      !>
      !>-    SPICELIB Version 4.0.0, 17-NOV-1995 (NJB)
      !>
      !>        Complete re-write for architecture 3.
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 4.1.0, 15-OCT-1996 (NJB)
      !>
      !>        Bug fix: default order sense was not encoded when ORDER-BY
      !>        clause was not the last clause of the query.  The old algorithm
      !>        assumed that no clauses followed the ORDER-BY clause, which
      !>        at one time was a limitation of the EK query language.
      module subroutine zzekpars ( ctx, query, ntoken, lxbegs, lxends, tokens, values, numvls, chrbuf, chbegs, chends, eqryi, eqryc, eqryd, error, prserr )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: repmc_inplace, repmi_inplace
      use pepper_inc_ektokn
      use pepper_inc_ekkeyw
      use pepper_inc_ekbool
      use pepper_inc_ektype
      use pepper_inc_ekopcd
      use pepper_inc_ekquery
      use pepper_inc_ekqlimit

      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: query  !! Query in string form.
      integer(ip), intent(in) :: ntoken  !! Number of tokens in query.
      integer(ip), intent(in) :: lxbegs ( * )  !! Lexeme begin and end positions in QUERY.
      integer(ip), intent(in) :: lxends ( * )  !! Lexeme begin and end positions in QUERY.
      integer(ip), intent(in) :: tokens ( * )  !! Token codes.
      integer(ip), intent(in) :: values ( * )  !! Values associated with tokens.
      real(wp), intent(in) :: numvls ( * )  !! Buffer containing numeric token values.
      character(*), intent(in) :: chrbuf  !! Buffer containing string token values.
      integer(ip), intent(in) :: chbegs ( * )  !! String token begin and end character positions.
      integer(ip), intent(in) :: chends ( * )  !! String token begin and end character positions.
      integer(ip), allocatable, intent(inout) :: eqryi ( : )  !! Parsed query and string and number value buffers. PEPPER: allocatable, lower bound LBCELL; grows as the query is encoded.
      character(*), intent(out) :: eqryc  !! Parsed query and string and number value buffers.
      real(wp), intent(out) :: eqryd  ( * )  !! Parsed query and string and number value buffers.
      logical, intent(out) :: error  !! Flag indicating whether query parsed correctly.
      character(*), intent(out) :: prserr  !! Parse error description.
      integer(ip) :: dsize

!
!     State parameters
!
      integer(ip), parameter :: from = 0

      integer(ip), parameter :: frmtab = from   + 1

      integer(ip), parameter :: frmcnt = frmtab + 1

      integer(ip), parameter :: frmals = frmcnt + 1

      integer(ip), parameter :: where = frmals + 1

      integer(ip), parameter :: order = where  + 1

      integer(ip), parameter :: ordrby = order  + 1

      integer(ip), parameter :: ordtab = ordrby + 1

      integer(ip), parameter :: ordnam = ordtab + 1

      integer(ip), parameter :: ordcol = ordnam + 1

      integer(ip), parameter :: ordsns = ordcol + 1

      integer(ip), parameter :: selkey = ordsns + 1

      integer(ip), parameter :: select = selkey + 1

      integer(ip), parameter :: seltab = select + 1

      integer(ip), parameter :: selnam = seltab + 1

      integer(ip), parameter :: selcol = selnam + 1

      integer(ip), parameter :: term = selcol + 1

!
!     Other local parameters
!

      integer(ip), parameter :: typlen = 32

      integer(ip), parameter :: keylen = 32

      character(typlen) :: errtyp
      character(keylen) :: expkey

      integer(ip) :: alsdsc ( eqvdsz )
      integer(ip) :: b
      integer(ip) :: coldsc ( eqvdsz )
      integer(ip) :: e
      integer(ip) :: i
      integer(ip) :: j
      integer(ip) :: l
      integer(ip) :: lxb
      integer(ip) :: lxe
      integer(ip) :: namdsc ( eqvdsz )
      integer(ip) :: ncnstr
      integer(ip) :: norder
      integer(ip) :: nsel
      integer(ip) :: ntabs
      integer(ip) :: state
      integer(ip) :: tabdsc ( eqvdsz )
      integer(ip) :: token
      integer(ip) :: toknum
      integer(ip) :: valdsc ( eqvdsz )

      logical :: fnd

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Initialize the encoded query each time, for safety.
!
!     PEPPER: the encoded query is initialized at the sizes the caller
!     allocated (SPICELIB: EQIMIN and MAXQNM); its integer part grows as
!     it is written.
!
      dsize = eqryi(eqnsiz)
      call zzekqini ( ctx, sizei ( ctx, eqryi ), dsize, eqryi, eqryc, eqryd )

      if ( ctx%failed() ) then
         error  =  .true.
         prserr =  'SPICE(BUG):  encoded query init failed.'
         return
      end if

!
!     The structure of a query is
!
!        <QUERY>                 =>    <SELECT clause> <FROM clause>
!                                      <WHERE clause> <ORDER BY clause>
!
!        <SELECT clause>         =>    SELECT <select list>
!
!        <select list>           =>    <column entry>
!                                    | <select list>, <column entry>
!
!        <column entry>          =>    <table name>.<column name>
!                                    | <column name>
!
!        <FROM clause>           =>    <table name list>
!
!        <table name list>       =>    <table entry>
!                                    | <table name list>, <table entry>
!
!        <table entry>           =>    <table name>
!                                    | <table name> <table alias>
!
!        <WHERE clause>          =>    WHERE <constraint expression>
!                                    | <NIL>
!
!        <ORDER BY clause>       =>    ORDER BY <order-by list>
!                                    | <NIL>
!
!        <order-by list>         =>    <order-by column entry>
!                                    | <order-by list>,
!                                      <order-by column entry>
!
!        <order-by column entry> =>    <column entry> <order>
!
!        <order>                 =>    ASC
!                                    | DESC
!                                    | <NIL>
!
!     We'll parse the clauses of the query in the following order:
!
!        FROM
!        WHERE     (if present)
!        ORDER BY  (if present)
!        SELECT
!
      call zzektloc ( ctx, tkkey,   kwfrom,  ntoken,  tokens,  values, &
                      toknum,  fnd                                )

      if ( .not. fnd ) then

         error   =  .true.
         errtyp  =  'FROM_NOT_FOUND'
         state   =   term

      else

         state   =  from
         ntabs   =  0
         nsel    =  0
         ncnstr  =  0
         norder  =  0
         error   =  .false.
         prserr  =  ' '
         errtyp  =  ' '

      end if

      do while ( state /= term )
!
!        Advance to the next token, if there is one.
!
         toknum  =  toknum + 1

         if ( toknum > ntoken ) then
!
!           We're out of tokens.  Set the token value to indicate
!           `end of query'.
!
            token  =  tkeoq

         else

            token  =  tokens(toknum)

         end if

!
!        Perform semantic actions based on the state and current token.
!
         if ( state == from ) then
!
!           We expect to see an identifier representing a table name.
!           No other tokens are allowed.
!
            if ( token == tkid ) then
!
!              We've found a table name (as far as we can tell at
!              this point).  Make sure we haven't exceeded the limit
!              for table names; if not, add the appropriate information
!              to the encoded query.
!
               ntabs  =   ntabs  +  1

               ! PEPPER: no maximum number of FROM tables (SPICELIB: MAXTAB = 10).
               i       =   values(toknum)
               b       =   chbegs(i)
               e       =   chends(i)
               l       =   e  -  b   +  1
               lxb     =   lxbegs(toknum)
               lxe     =   lxends(toknum)

               call zzekinqc ( ctx, chrbuf( b : e ), &
                               l, &
                               lxb, &
                               lxe, &
                               eqryi, &
                               eqryc, &
                               tabdsc          )

!
!                 Append the table descriptor to the integer part of the
!                 query.
!
               do j = 1, eqvdsz
                  call append_query ( ctx, tabdsc(j), eqryi )
               end do

!
!                 Add a place-holder value descriptor to reserve
!                 space for an alias descriptor for this table.  If an
!                 actual alias is supplied, we'll update this
!                 descriptor.
!
               alsdsc = 0
               alsdsc(eqdtyp)  =  chr

               do j = 1, eqvdsz
                  call append_query ( ctx, alsdsc(j), eqryi )
               end do

!
!                 Update the table count in the encoded query.
!
               call zzekweqi ( ctx, 'NUM_TABLES', ntabs, eqryi )

               state  =  frmtab

            else if ( token == tkeoq ) then

               error   =  .true.
               errtyp  =  'MORE_TOKENS_EXP'
               state   =  term

            else
!
!              We've got the wrong kind of token here.
!
               error   =  .true.
               errtyp  =  'TABLE_EXP'
               state   =  term

            end if
!
!           State is a member of {FRMTAB, TERM}.
!

         else if ( state == frmtab ) then
!
!           We should see a comma, an alias, one of the SELECT,
!           WHERE or ORDER keywords, or the end of the query.
!
            if ( token == tkeoq ) then
!
!              We're out of tokens.  It's time to parse the
!              WHERE clause.
!
               state   =  where

            else if ( token == tkcoma ) then
!
!              It's time to look for another table name.
!
               state  =  from

            else if ( token == tkid ) then
!
!              We've got an alias.  Add this string to the encoded
!              query.
!
               i   =   values(toknum)
               b   =   chbegs(i)
               e   =   chends(i)
               l   =   e  -  b   +  1
               lxb =   lxbegs(toknum)
               lxe =   lxends(toknum)

               call zzekinqc ( ctx, chrbuf( b : e ), &
                               l, &
                               lxb, &
                               lxe, &
                               eqryi, &
                               eqryc, &
                               alsdsc                        )

!
!              Update the place-holder alias descriptor in the integer
!              part of the query.
!
               eqryi(cardi( ctx,eqryi) - eqvdsz + 1:cardi( ctx,eqryi) - eqvdsz + 1+eqvdsz-1) = alsdsc

               state  =  frmals

            else if (  token ==  tkkey  )  then
!
!              The last table name in the FROM clause is followed by
!              a keyword.  SELECT, WHERE and ORDER are the only valid
!              possibilities.
!
               if (       ( values(toknum) /= kwwher ) &
                    .and. ( values(toknum) /= kwsel  ) &
                    .and. ( values(toknum) /= kwordr )  ) then
!
!                 We've got a keyword we don't want here.
!
                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               else
!
!                 Parse the WHERE clause.
!
                  state  =  where

               end if

            else
!
!              We've got the wrong kind of token altogether.
!
               error  =  .true.
               errtyp =  'ALIAS_EXP'
               state  =  term

            end if

!
!           STATE is a member of {FROM, FRMALS, WHERE, TERM}.
!

         else if ( state == frmals ) then
!
!           We should see a comma, the SELECT, WHERE or ORDER
!           keywords, or the end of the query.
!
            if ( token == tkeoq ) then
!
!              We're out of tokens.  It's time to parse the
!              WHERE clause.
!
               state   =  where

            else if ( token == tkcoma ) then
!
!              It's time to look for another table name.
!
               state  =  from

            else if (  token ==  tkkey  )  then
!
!              The last table name in the FROM clause is followed by
!              a keyword.  SELECT, WHERE and ORDER are the only valid
!              possibilities.
!
               if (       ( values(toknum) /= kwwher ) &
                    .and. ( values(toknum) /= kwsel  ) &
                    .and. ( values(toknum) /= kwordr )  ) then
!
!                 We've got a keyword we don't want here.
!
                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               else
!
!                 Parse the WHERE clause.
!
                  state  =  where

               end if

            else
!
!              We've got the wrong kind of token altogether.
!
               error  =  .true.
               errtyp =  'COMMA_OR_KEY_EXP'
               state  =  term

            end if
!
!           STATE is a member of {FROM, WHERE, TERM}.
!

         else if ( state == selkey ) then
!
!           It's time to parse the SELECT clause.  We'll need to
!           locate the SELECT keyword.
!
            call zzektloc ( ctx, tkkey,   kwsel,  ntoken,  tokens,  values, &
                            toknum,  fnd                               )

            if ( .not. fnd ) then

               error   =  .true.
               errtyp  =  'SELECT_NOT_FOUND'
               state   =   term

            else

               state   =  select

            end if

         else if ( state == select ) then
!
!           We must see either the * token, the ALL keyword,
!           or an identifier here.  The identifier may be a lone
!           column name, or it may be a column name qualified by a
!           table name or alias.
!
!           For the moment, we don't support the * or ALL options.
!
            if ( token == tkid ) then
!
!              We've found a name (as far as we can tell at this point).
!              Make sure we haven't exceeded the limit for SELECT
!              column names; if not, store the name string in the
!              encoded query, and save the descriptor until we've
!              figured out whether we're looking at a column name or
!              table name.
!
               nsel  =  nsel  +  1

               ! PEPPER: no maximum number of SELECT columns (SPICELIB: MAXSEL = 50).
               i       =   values(toknum)
               b       =   chbegs(i)
               e       =   chends(i)
               l       =   e  -  b   +  1
               lxb     =   lxbegs(toknum)
               lxe     =   lxends(toknum)

               call zzekinqc ( ctx, chrbuf( b : e ), &
                               l, &
                               lxb, &
                               lxe, &
                               eqryi, &
                               eqryc, &
                               namdsc          )

!
!                 Add a place-holder value descriptor to reserve
!                 space for a table descriptor for this name.  If it
!                 turns out that the current name is a table name, we'll
!                 update this descriptor.
!
               valdsc = 0
               valdsc(eqdtyp)  =  chr

               do j = 1, eqvdsz
                  call append_query ( ctx, valdsc(j), eqryi )
               end do

!
!                 Update the SELECT column count in the encoded query.
!
               call zzekweqi ( ctx, 'NUM_SELECT_COLS', nsel, eqryi )

               state  =  selnam

            else if ( token == tkeoq ) then

               error  =  .true.
               errtyp =  'MORE_TOKENS_EXP'
               state  =  term

            else
!
!              We've got the wrong kind of token here.
!
               error  =  .true.
               errtyp =  'TABLE_OR_COLUMN_EXP'
               state  =  term

            end if
!
!           State is a member of {SELNAM, TERM}.
!

         else if ( state == selnam ) then
!
!           We've seen a SELECT column name, or else the name
!           of a table qualifying a SELECT column name.
!
            if ( token == tkeoq ) then
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call append_query ( ctx, namdsc(j), eqryi )
               end do

               state   =   term

            else if ( token == tkcoma ) then
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.  Another name should follow.
!
               do j = 1, eqvdsz
                  call append_query ( ctx, namdsc(j), eqryi )
               end do

               state  =  select

            else if ( token == tkdot ) then
!
!              The name we picked up was a table name or alias.  A
!              column name should follow.
!
               state  =  seltab

            else if ( token == tkkey ) then
!
!              We have the last column name in the SELECT clause.
!
!              Append the saved name descriptor to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call append_query ( ctx, namdsc(j), eqryi )
               end do

!
!              The last column name in the SELECT clause is followed by
!              a keyword.  FROM, WHERE and ORDER are the only valid
!              possibilities.
!
               if (      ( values(toknum) == kwwher ) &
                    .or. ( values(toknum) == kwfrom ) &
                    .or. ( values(toknum) == kwordr )  ) then
!
!                 We're done with the SELECT clause.
!
                  state  =  term

               else
!
!                 We've got a keyword we don't want here.
!
                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               end if

            else
!
!              We've got the wrong kind of token here.
!
               error  =  .true.
               errtyp =  'BAD_TOKEN'
               state  =  term

            end if
!
!           STATE is a member of {SELECT, SELTAB, TERM}.
!

         else if ( state == seltab ) then
!
!           We've picked up a qualifying table name for a SELECT
!           column.  We must see a column name here.
!
            if ( token == tkid ) then
!
!              Update the place-holder table name descriptor in the
!              encoded query.
!
               eqryi(cardi( ctx,eqryi) - eqvdsz + 1:cardi( ctx,eqryi) - eqvdsz + 1+eqvdsz-1) = namdsc

!
!              Add the column name to the character part of the
!              encoded query.
!
               i   =   values(toknum)
               b   =   chbegs(i)
               e   =   chends(i)
               l   =   e  -  b   +  1
               lxb =   lxbegs(toknum)
               lxe =   lxends(toknum)

               call zzekinqc ( ctx, chrbuf( b : e ), &
                               l, &
                               lxb, &
                               lxe, &
                               eqryi, &
                               eqryc, &
                               coldsc           )

!
!              Add the descriptor for the column name to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call append_query ( ctx, coldsc(j), eqryi )
               end do

               state  =  selcol

            else if ( token == tkeoq ) then

               error  =  .true.
               errtyp =  'MORE_TOKENS_EXP'
               state  =  term

            else

               error  =  .true.
               errtyp =  'COLUMN_EXP'
               state  =  term

            end if
!
!           STATE is a member of {SELCOL, TERM}.
!

         else if ( state == selcol ) then
!
!           We've picked up a qualified column name.  At this point,
!           we should see a keyword, a comma, or the end of the
!           query.
!
            if ( token == tkkey ) then
!
!              The last column name in the SELECT clause is followed by
!              a keyword.  FROM, WHERE and ORDER are the only valid
!              possibilities.
!
               if (      ( values(toknum) == kwwher ) &
                    .or. ( values(toknum) == kwfrom ) &
                    .or. ( values(toknum) == kwordr )  ) then
!
!                 We're done with the SELECT clause.
!
                  state  =  term

               else
!
!                 We've got a keyword we don't want here.
!
                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               end if

            else if ( token == tkcoma ) then
!
!              We expect another SELECT column.
!
               state  =  select

            else if ( token == tkeoq ) then
!
!              We're done with the SELECT clause.
!
               state  =  term

            else

               error  =  .true.
               errtyp =  'COMMA_OR_KEY_EXP'
               state  =  term

            end if
!
!           STATE is a member of {SELECT, TERM}.
!

         else if ( state == where ) then
!
!           The WHERE clause is optional.  See whether we have one.  The
!           clause is started by a WHERE keyword.
!
            call zzektloc ( ctx, tkkey,   kwwher,  ntoken,  tokens,  values, &
                            toknum,  fnd                               )

            if ( fnd ) then
!
!              We're going to hand off the list of tokens that comprise
!              the WHERE clause of the query to a routine that will
!              parse the tokens and form a list of relational
!              constraints.  Once this is done, all we have to do here
!              is check the validity of the column names and the values
!              used in the constraints.
!
               call zzeknrml ( ctx, query, &
                               ntoken, &
                               lxbegs, &
                               lxends, &
                               tokens, &
                               values, &
                               numvls, &
                               chrbuf, &
                               chbegs, &
                               chends, &
                               eqryi, &
                               eqryc, &
                               eqryd, &
                               error, &
                               prserr )

               if ( error ) then

                  errtyp  =   'WHERE_ERROR'
                  state   =   term

               else
!
!                 Parse the ORDER BY clause, if one is present.
!
                  state   =   order

               end if

            else
!
!              Parse the ORDER BY clause, if one is present.
!
               state   =   order

            end if
!
!           STATE is a member of {ORDER, TERM}.
!

         else if ( state == order ) then
!
!           The ORDER BY clause is optional.  See whether we have one.
!           The clause is started by an ORDER keyword.
!
            call zzektloc ( ctx, tkkey,   kwordr,  ntoken,  tokens,  values, &
                            toknum,  fnd                               )

            if ( fnd ) then
!
!              The BY keyword should follow the ORDER keyword.
!
               if ( toknum < ntoken ) then

                  toknum  =  toknum + 1

                  if (       (  tokens(toknum) == tkkey ) &
                       .and. (  values(toknum) == kwby  )  ) then
!
!                    We're ready to parse the ORDER BY clause.
!
                     state  =  ordrby

                  else
!
!                    No BY keyword followed the ORDER keyword.
!
                     error  =  .true.
                     errtyp =  'BY_EXPECTED'
                     state  =  term

                  end if

               else
!
!                 We're out of tokens where we shouldn't be.
!
                  error  =  .true.
                  errtyp =  'BY_EXPECTED'
                  state  =  term

               end if

            else
!
!              We're ready to go on to the SELECT clause.
!
               state  =  selkey

            end if
!
!           STATE is a member of {ORDRBY, SELKEY, TERM}.
!

         else if ( state == ordrby ) then
!
!           We must see a name in the order column list here.
!           The name may be a lone column name, or it may be a column
!           name qualified by a table name or alias.
!
            if ( token == tkid ) then
!
!              We've found a name (as far as we can tell at this point).
!              Make sure we haven't exceeded the limit for order-by
!              column names; if not, store the name string in the
!              encoded query, and save the descriptor until we've
!              figured out whether we're looking at a column name or
!              table name.
!
               norder  =  norder  +  1

               ! PEPPER: no maximum number of ORDER BY columns (SPICELIB: MAXORD = 10).
               i       =   values(toknum)
               b       =   chbegs(i)
               e       =   chends(i)
               l       =   e  -  b   +  1
               lxb     =   lxbegs(toknum)
               lxe     =   lxends(toknum)

               call zzekinqc ( ctx, chrbuf( b : e ), &
                               l, &
                               lxb, &
                               lxe, &
                               eqryi, &
                               eqryc, &
                               namdsc          )

!
!                 Add a place-holder value descriptor to reserve
!                 space for a table descriptor for this name.  If it
!                 turns out that the current name is a table name, we'll
!                 update this descriptor.
!
               valdsc = 0
               valdsc(eqdtyp)  =  chr

               do j = 1, eqvdsz
                  call append_query ( ctx, valdsc(j), eqryi )
               end do

!
!                 Update the order-by column count in the encoded query.
!
               call zzekweqi ( ctx, 'NUM_ORDERBY_COLS', norder, eqryi )

               state  =  ordnam

            else if ( token == tkeoq ) then

               error  =  .true.
               errtyp =  'MORE_TOKENS_EXP'
               state  =  term

            else
!
!              We've got the wrong kind of token here.
!
               error  =  .true.
               errtyp =  'TABLE_OR_COLUMN_EXP'
               state  =  term

            end if
!
!           State is a member of {ORDNAM, TERM}.
!

         else if ( state == ordnam ) then
!
!           We've seen an order-by column name, or else the name
!           of a table qualifying an order-by column name.
!
            if ( token == tkeoq ) then
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call append_query ( ctx, namdsc(j), eqryi )
               end do

!
!              Since no ASCENDING or DESCENDING sense keyword was
!              supplied, append the default value ASCENDING to the
!              order-by column descriptor in the encoded query.
!
               call append_query ( ctx, eqasnd, eqryi )

!
!              We're done with the ORDER BY clause; go on to parse the
!              SELECT clause.
!
               state   =  selkey

            else if ( token == tkcoma ) then
!
!              The name we picked up was an unqualified column
!              name.  Append the saved name descriptor to the encoded
!              query.  Another name should follow.
!
               do j = 1, eqvdsz
                  call append_query ( ctx, namdsc(j), eqryi )
               end do

!
!              Since no ASCENDING or DESCENDING sense keyword was
!              supplied, append the default value ASCENDING to the
!              order-by column descriptor in the encoded query.
!
               call append_query ( ctx, eqasnd, eqryi )

               state  = ordrby

            else if ( token == tkdot ) then
!
!              The name we picked up was a table name or alias.  A
!              column name should follow.
!
               state = ordtab

            else if ( token == tkkey ) then
!
!              We have a column name, which may be followed by a
!              keyword indicating the sense of the ordering, or may
!              be followed by a keyword starting a new clause.
!
!              Append the saved name descriptor to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call append_query ( ctx, namdsc(j), eqryi )
               end do

!
!              Set the sense descriptor according to the keyword we've
!              picked up.  After this, we're ready to look for another
!              order-by column.
!
               if ( values(toknum) == kwasnd ) then

                  call append_query ( ctx, eqasnd, eqryi )

                  state = ordsns

               else if ( values(toknum) == kwdsnd ) then

                  call append_query ( ctx, eqdsnd, eqryi )

                  state = ordsns

               else if (      ( values(toknum) == kwwher ) &
                         .or. ( values(toknum) == kwfrom ) &
                         .or. ( values(toknum) == kwsel  )  ) then

!
!                 Since no ASCENDING or DESCENDING sense keyword was
!                 supplied, append the default value ASCENDING to the
!                 order-by column descriptor in the encoded query.
!
                  call append_query ( ctx, eqasnd, eqryi )

!
!                 We're done with the ORDER BY clause.  Go on to
!                 parse the SELECT clause.
!
                  state  =  selkey

               else
!
!                 We've got a keyword we don't want here.
!
                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               end if

            else
!
!              We've got the wrong kind of token here.
!
               error  =  .true.
               errtyp =  'BAD_TOKEN'
               state  =  term

            end if
!
!           STATE is a member of {ORDRBY, ORDTAB, ORDSNS, SELKEY, TERM}.
!

         else if ( state == ordtab ) then
!
!           We've picked up a qualifying table name for an order-by
!           column.  We must see a column name here.
!
            if ( token == tkid ) then
!
!              Update the place-holder table name descriptor in the
!              encoded query.
!
               eqryi(cardi( ctx,eqryi) - eqvdsz + 1:cardi( ctx,eqryi) - eqvdsz + 1+eqvdsz-1) = namdsc

!
!              Add the column name to the character part of the
!              encoded query.
!
               i   =   values(toknum)
               b   =   chbegs(i)
               e   =   chends(i)
               l   =   e  -  b   +  1
               lxb =   lxbegs(toknum)
               lxe =   lxends(toknum)

               call zzekinqc ( ctx, chrbuf( b : e ), &
                               l, &
                               lxb, &
                               lxe, &
                               eqryi, &
                               eqryc, &
                               coldsc           )

!
!              Add the descriptor for the column name to the encoded
!              query.
!
               do j = 1, eqvdsz
                  call append_query ( ctx, coldsc(j), eqryi )
               end do

               state  =  ordcol

            else if ( token == tkeoq ) then

               error  =  .true.
               errtyp =  'MORE_TOKENS_EXP'
               state  =  term

            else

               error  =  .true.
               errtyp =  'COLUMN_EXP'
               state  =  term

            end if
!
!           STATE is a member of {ORDCOL, TERM}.
!

         else if ( state == ordcol ) then
!
!           We've picked up a qualified column name.  At this point,
!           we should see a sense keyword, a comma, the end of the
!           query, or one of the FROM, SELECT, or WHERE keywords.
!
            if ( token == tkkey ) then

               if ( values(toknum) == kwasnd ) then
!
!                 The ASCENDING keyword has been supplied.  After this,
!                 look for another column.
!
                  call append_query ( ctx, eqasnd, eqryi )

                  state  =  ordsns

               else if ( values(toknum) == kwdsnd ) then
!
!                 The DESCENDING keyword has been supplied.  After this,
!                 look for another column.
!
                  call append_query ( ctx, eqdsnd, eqryi )

                  state  =  ordsns

               else if (      ( values(toknum) == kwwher ) &
                         .or. ( values(toknum) == kwfrom ) &
                         .or. ( values(toknum) == kwsel  )  ) then
!
!                 We're done with the ORDER BY clause.  Go on to
!                 parse the SELECT clause.
!
                  state  =  selkey

               else

                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               end if

            else if ( token == tkcoma ) then
!
!              The ASCENDING keyword is implied.
!
               call append_query ( ctx, eqasnd, eqryi )

               state  =  ordrby

            else if ( token == tkeoq ) then
!
!              The ASCENDING keyword is implied.
!
               call append_query ( ctx, eqasnd, eqryi )

!
!              We're done with the ORDER BY clause.  Parse the SELECT
!              clause.
!
               state  =  selkey

            else

               error  =  .true.
               errtyp =  'COMMA_OR_KEY_EXP'
               state  =  term

            end if
!
!           STATE is a member of {ORDRBY, ORDSNS, SELKEY, TERM}.
!

         else if ( state == ordsns ) then
!
!           We've picked up an order sense keyword.  At this point,
!           we should see comma or the end of the query, or one of the
!           FROM, SELECT, or WHERE keywords.
!
            if ( token == tkcoma ) then
!
!              We're ready to look for another column.
!
               state  =  ordrby

            else if ( token == tkeoq ) then
!
!              We're done with the ORDER BY clause.  Parse the SELECT
!              clause.
!
               state  =  selkey

            else if ( token == tkkey ) then

               if (      ( values(toknum) == kwwher ) &
                    .or. ( values(toknum) == kwfrom ) &
                    .or. ( values(toknum) == kwsel  )  ) then
!
!                 We're done with the ORDER BY clause.  Go on to
!                 parse the SELECT clause.
!
                  state  =  selkey

               else

                  error  =  .true.
                  errtyp =  'BAD_KEYWORD'
                  state  =  term

               end if

            else

               error  =  .true.
               errtyp =  'COMMA_EXP'
               state  =  term

            end if
!
!           STATE is a member of {ORDRBY, SELKEY, TERM}.
!

         else
!
!           Somehow, we've reached an invalid state.
!
            error  = .true.
            prserr = 'SPICE(BUG) -- Invalid state reached in EK parser.'
            state  =  term

         end if
!
!           STATE is a member of {ORDRBY, TERM}.
!

      end do
!
!     At this point, either an error has been detected, or the query
!     has been parsed, and the query is represented in encoded form
!     in the outputs EQRYI, EQRYC, and EQRYD.
!

!
!     We centralize construction of error messages in the following
!     section.
!
      if ( error ) then

         if ( errtyp == 'FROM_NOT_FOUND' ) then

            prserr = 'Every query must contain a FROM clause. ' // &
                     'The FROM keyword was not found.'

         else if ( errtyp == 'SELECT_NOT_FOUND' ) then

            prserr = 'Every query must contain a SELECT clause. ' // &
                     'The SELECT keyword was not found.'

         else if ( errtyp == 'BY_EXPECTED' ) then

            prserr = 'The BY keyword was not found following the ' // &
                     'ORDER keyword.'

         else if ( errtyp == 'BAD_KEYWORD' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Invalid keyword at location #. '  // &
                     'Actual token was: #'
            call repmi_inplace ( prserr, '#', lxb )
            call repmc_inplace ( prserr, '#', query(lxb:lxe) )

         else if ( errtyp == 'TABLE_OR_COLUMN_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Table or column name expected at location ' // &
                     '#. Actual token was: #'
            call repmi_inplace ( prserr, '#', lxb )
            call repmc_inplace ( prserr, '#', query(lxb:lxe) )

         else if ( errtyp == 'TABLE_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Table name expected at location #. '  // &
                     'Actual token was: #'
            call repmi_inplace ( prserr, '#', lxb )
            call repmc_inplace ( prserr, '#', query(lxb:lxe) )

         else if ( errtyp == 'COLUMN_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Column name expected at location #. '  // &
                     'Actual token was: #'
            call repmi_inplace ( prserr, '#', lxb )
            call repmc_inplace ( prserr, '#', query(lxb:lxe) )

         else if ( errtyp == 'ALIAS_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Table alias, comma, or keyword expected at ' // &
                     'location #. Actual token was: #'
            call repmi_inplace ( prserr, '#', lxb )
            call repmc_inplace ( prserr, '#', query(lxb:lxe) )

         else if ( errtyp == 'COMMA_OR_KEY_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Comma or keyword expected at ' // &
                     'location #. Actual token was: #'
            call repmi_inplace ( prserr, '#', lxb )
            call repmc_inplace ( prserr, '#', query(lxb:lxe) )

         else if ( errtyp == 'COMMA_EXP' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Comma expected at location #. Actual token was: #'
            call repmi_inplace ( prserr, '#', lxb )
            call repmc_inplace ( prserr, '#', query(lxb:lxe) )

         else if ( errtyp == 'MORE_TOKENS_EXP' ) then

            prserr  =  'More tokens were expected in query.'

         else if ( errtyp == 'KEYWORD_EXP' ) then

            prserr = 'The keyword # was expected at location ' // &
                     '#. Actual token was: #'
            call repmc_inplace ( prserr, '#', expkey )
            call repmi_inplace ( prserr, '#', lxb )
            call repmc_inplace ( prserr, '#', query(lxb:lxe) )

         else if ( errtyp == 'BAD_TOKEN' ) then

            lxb    =  lxbegs(toknum)
            lxe    =  lxends(toknum)
            prserr = 'Invalid token at location #. Token was: #'
            call repmi_inplace ( prserr, '#', lxb )
            call repmc_inplace ( prserr, '#', query(lxb:lxe) )

!
!        PEPPER: the TOO_MANY_* errors cannot occur any more (no maximum
!        numbers of FROM tables, SELECT and ORDER BY columns).
!
         else if ( errtyp == 'TOO_MANY_TABLES' ) then

            prserr  =  'Number of tables in "FROM" clause exceeds ' // &
                       'allowed maximum of #.'
            call repmi_inplace ( prserr, '#', maxtab )

         else if ( errtyp == 'TOO_MANY_ORD_COLS' ) then

            prserr  =  'Number of order-by columns exceeds allowed ' // &
                       'maximum of #.'
            call repmi_inplace ( prserr, '#', maxtab )

         else if ( errtyp == 'TOO_MANY_SEL_COLS' ) then

            prserr  =  'Number of SELECT columns exceeds allowed ' // &
                       'maximum of #.'
            call repmi_inplace ( prserr, '#', maxsel )

         else if ( errtyp /= 'WHERE_ERROR' ) then

            prserr  =  'SPICE(BUG)--Unrecognized error type.  Type ' // &
                       'was #.'
            call repmc_inplace ( prserr, '#', errtyp )

         end if

      else
!
!        Indicate that parsing is complete.
!
         call zzekweqi ( ctx, 'PARSED', itrue, eqryi )

      end if

      return
      end subroutine zzekpars

      !> Execute a 2-3 split:  split two sibling nodes into three nodes,
      !> each one approximately 2/3 full.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     LEFT           is the node number of the left node of a pair of
      !>                    siblings.  LEFT is either full or overflowing by
      !>                    one key.
      !>
      !>     RIGHT          is the node number of the right node of a pair of
      !>                    siblings.  The total number of keys in nodes
      !>                    LEFT and RIGHT amounts to an overflow of 1 key.
      !>
      !>     PARENT         is the node number of the common parent of LEFT
      !>                    and RIGHT.
      !>
      !>     PKIDX          is the node-relative index in PARENT of the key
      !>                    that sits between nodes LEFT and RIGHT.
      !>
      !>## Detailed Output
      !>
      !>     OVERFL         is a logical flag indicating whether the parent
      !>                    node overflowed as a result of the 2-3 split.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>     3)  If LEFT and RIGHT are not neighboring siblings, the error
      !>         SPICE(BUG) is signalled.
      !>
      !>     4)  If either LEFT or RIGHT are not children of PARENT, the error
      !>         SPICE(BUG) is signalled.
      !>
      !>     5)  If the sum of the number of keys in LEFT and RIGHT does not
      !>         correspond to an overflow of exactly 1 key, the error
      !>         SPICE(BUG) is signalled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     Insertions into an EK tree start at a leaf node.  If the node
      !>     overflows, the EK system attempts to shuffle keys at the leaf
      !>     level to resolve the overflow.  That attempt failing, the system
      !>     delegates the problem upward to the next higher level.
      !>
      !>     There are two ways to resolve overflow in a non-root node:
      !>     balance the overflowing node with one of its closest siblings
      !>     (see ZZEKTRBN), or if the closest siblings are full, execute a 2-3
      !>     split.
      !>
      !>     A 2-3 split involves creation of a new sibling node between two
      !>     siblings, one of which is full and one of which contains one
      !>     excess key, and redistributing the keys between the three nodes
      !>     and their common parent so that each of the three siblings is
      !>     approximately two-thirds full.  The parent gains a key in the
      !>     process.
      !>
      !>     After the 2-3 split, the tree is balanced and the siblings
      !>     satisfy the key count invariants.  However, the parent of the
      !>     siblings may overflow by one key.
      !>
      !>     Below are the gory details concerning the actions of this routine.
      !>     All of the parameters referred to here (in capital letters) are
      !>     defined in the include file ektree.inc.
      !>
      !>     In a 2-3 split:
      !>
      !>        - The leftmost (2*MXKEYC)/3 keys of the left child remain in
      !>          that child.
      !>
      !>        - The rest of the keys in the left child node are rotated
      !>          through the parent into the middle child.  The last of these
      !>          rotated keys remains in the parent.  The others become the
      !>          leftmost keys of the middle child.
      !>
      !>        - The data values associated with the rotated keys of the
      !>          left child are moved along with the keys.
      !>
      !>        - All but the leftmost of the left child pointers associated
      !>          with the rotated keys of the left child are moved along with
      !>          the keys.  The leftmost of these pointers remains in the left
      !>          child node.
      !>
      !>        - The right child pointers associated with the rotated keys
      !>          of the left child node move along with the keys, except for
      !>          the right child pointer of the leftmost key of the rotated
      !>          set.  This leftmost key ends up in the parent, but its right
      !>          child pointer becomes the leftmost left child pointer of the
      !>          center sibling.
      !>
      !>        - The key from the left child node that is rotated into the
      !>          parent loses both of its original child pointers; these
      !>          are replaced by pointers to the left and center siblings.
      !>
      !>        - The parent key that originally sat between the left and
      !>          right siblings is moved down into the center sibling, along
      !>          with its data value.  It becomes the immediate successor of
      !>          the set of nodes rotated into the center from the left child.
      !>
      !>        - The actions taken to rotate keys from the right child are
      !>          basically symmetric with those that apply to the left child,
      !>          except that the number of keys left in the right node is
      !>          (2*MXKEYC+2)/3, and these keys are shifted to the left side
      !>          of the right node.  The rightmost key of the rotated set
      !>          contributed by the right child is placed in the parent as
      !>          the successor of the key moved into the parent from the left
      !>          child.  The rest of the rotated set become successors of
      !>          the key moved into the middle child from the parent.
      !>
      !>        - The middle child ends up with (2*MXKEYC+1)/3 keys.  This
      !>          may be deduced from the facts that the original two children
      !>          had between them an overflow of one key, the parent gained
      !>          a key, and the expression
      !>
      !>             2*MXKEYC     2*MXKEYC+1     2*MXKEYC+2
      !>             --------  +  ----------  +  ----------
      !>                 3             3             3
      !>
      !>          where integer division is performed, yields one less than
      !>          the same expression when real division is performed (since
      !>          exactly one of the numerators is a multiple of 3).  So the
      !>          above expression evaluates to
      !>
      !>             2*MXKEYC
      !>
      !>          which is exactly one less than the number of keys in the
      !>          original two siblings.
      !>
      !>          Since
      !>
      !>             MNKEYC  =  MNKIDC - 1
      !>                     =  (  ( 2*MXKIDC + 1 ) / 3   )  -  1
      !>                     =  ( 2*MXKIDC - 2 ) / 3
      !>                     =  ( 2*MXKEYC ) / 3
      !>
      !>          we see that the smallest of the new child nodes has at
      !>          least the minimum allowed number of keys.  The constraint
      !>          on the maximum is met as well, since the maximum is
      !>          approximately 3/2 times the minimum, and the minimum is
      !>          approximately 40.
      !>
      !>     As the above description shows, the parent gains a key as a
      !>     result of a 2-3 split.  This may cause the parent to overflow;
      !>     if it does, the overflow at the parent's level must be resolved.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRIN.
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
      !>-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
      module subroutine zzektr23 ( ctx, handle, tree, left, right, parent, pkidx, overfl )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: left  !! Left sibling node.
      integer(ip), intent(in) :: right  !! Right sibling node.
      integer(ip), intent(in) :: parent  !! Common parent node.
      integer(ip), intent(in) :: pkidx  !! Node-relative index of parent key.
      logical, intent(out) :: overfl  !! Overflow flag.
      integer(ip) :: nnode_1(1)  !! one-element copy of nnode (array argument)
!
!     Local variables
!
      integer(ip) :: base
      integer(ip) :: c1page ( pgsizi )
      integer(ip) :: c2page ( pgsizi )
      integer(ip) :: c3page ( pgsizi )
      integer(ip) :: datbas
      integer(ip) :: i
      integer(ip) :: keybas
      integer(ip) :: kidbas
      integer(ip) :: ldelta
      integer(ip) :: lnmove
      integer(ip) :: lshift
      integer(ip) :: lsib
      integer(ip) :: lsize
      integer(ip) :: ltrsiz
      integer(ip) :: msize
      integer(ip) :: new
      integer(ip) :: nlkeys
      integer(ip) :: nnode
      integer(ip) :: npkeys
      integer(ip) :: nrkeys
      integer(ip) :: ppage ( pgsizi )
      integer(ip) :: rbase
      integer(ip) :: rdelta
      integer(ip) :: rnmove
      integer(ip) :: root
      integer(ip) :: rshift
      integer(ip) :: rsib
      integer(ip) :: rsize
      integer(ip) :: sum

!
!     The plan is to take two sibling nodes, one of which is full and
!     one of which is overflowing by 1 key, and to split off about
!     one third of the keys from each one into a new node.  The new
!     node will be a child of the common parent of the input nodes and
!     will be inserted between them.
!
!     After the split, the sum of the numbers of keys in the three
!     children will be exactly 2*MXKEYC.  The numbers of keys in the
!     left, middle, and right nodes will be, respectively:
!
      lsize  =   (2*mxkeyc    )/3
      msize  =   (2*mxkeyc + 1)/3
      rsize  =   (2*mxkeyc + 2)/3

!
!     Note that exactly one of the numerators above is a multiple of 3,
!     so the sum of the above numbers is 1 less than if real division
!     were performed.  Therefore, the sum of the numbers of keys in the
!     child nodes is 2*MXKEYC.  The parent will contain one more node
!     than it did before the split:  the key originally between LEFT and
!     RIGHT will be moved down into the middle child, and the
!     smallest key moved from LEFT and the largest key moved from RIGHT
!     will go into PARENT.
!
      call zzekpgri ( ctx, handle, left,   c1page )
      call zzekpgri ( ctx, handle, right,  c2page )
      call zzekpgri ( ctx, handle, parent, ppage  )

!
!     The actual addresses in the parent node depend on whether the
!     parent is the root.  Compute the necessary bases to avoid a lot
!     of cases.
!
      root  =  tree

      if ( parent == root ) then
         keybas  =  trkeyr
         datbas  =  trdatr
         kidbas  =  trkidr
      else
         keybas  =  trkeyc
         datbas  =  trdatc
         kidbas  =  trkidc
      end if

!
!     Verify that LEFT and RIGHT are siblings, and that PARENT is
!     their common parent.
!
      lsib  =  ppage ( kidbas+pkidx   )
      rsib  =  ppage ( kidbas+pkidx+1 )

      if (  ( lsib /= left ) .or. ( rsib /= right )  ) then

         call ctx%setmsg ( 'LEFT, RIGHT, PARENT, and PKIDX are '   // &
                       'inconsistent. LEFT = #; RIGHT = #; '   // &
                       'PARENT = #; PKIDX = #; LSIB derived '  // &
                       'from PARENT = #; RSIB = #.'             )
         call ctx%errint ( '#',   left                              )
         call ctx%errint ( '#',   right                             )
         call ctx%errint ( '#',   parent                            )
         call ctx%errint ( '#',   pkidx                             )
         call ctx%errint ( '#',   lsib                              )
         call ctx%errint ( '#',   rsib                              )
         call ctx%sigerr ( 'SPICE(BUG)'                             )
         return

      end if

      nlkeys  =  c1page ( trnkc )
      nrkeys  =  c2page ( trnkc )
      sum     =  nlkeys + nrkeys

!
!     The sum of the number of keys in the two input nodes must
!     sum exactly to the value representing an overflow level of 1 key.
!
      if (  sum  /=  ( 2*mxkeyc + 1 )  ) then

         call ctx%setmsg ( 'Number of keys in LEFT = #; number of keys '// &
                       'in right = #; but sum should be #.'          )
         call ctx%errint ( '#',  left                                    )
         call ctx%errint ( '#',  right                                   )
         call ctx%errint ( '#',  2*mxkeyc+1                              )
         call ctx%sigerr ( 'SPICE(BUG)'                                  )
         return

      end if

!
!     Allocate a new page.  This page will become the right sibling
!     of LEFT and the left sibling of RIGHT.
!
      call zzekpgal ( ctx, handle, int,   new, base )
      c3page = 0

!
!     It's time to set up the keys in the middle child.  First, we'll
!     take the last LSHIFT keys from the left node, where
!
      lshift  =  nlkeys - ( lsize + 1 )

!
!     When these keys are moved, they lose LDELTA predecessors, where
!     LDELTA is the size of the key set preceding and including the key
!     at location LSIZE + 1.  The size of this subtree is just the
!     key value at location LSIZE+1.
!
      ldelta  =  c1page ( trkeyc + lsize + 1 )

      do i = 1, lshift
         c3page(trkeyc+i) =  c1page(trkeyc+lsize+1+i) -  ldelta
      end do

      c3page(trdatc+1:trdatc+1+lshift-1) = c1page(trdatc+lsize+2:trdatc+lsize+2+lshift-1)
      c3page(trkidc+1:trkidc+1+lshift) = c1page(trkidc+lsize+2:trkidc+lsize+2+lshift)

!
!     Compute the size of the tree headed by the left subnode.  We'll
!     need this shortly.  The size of this tree is one less than the
!     difference of the parent key and its predecessor, if any.
!
      if ( pkidx == 1 ) then
         ltrsiz  =  ppage ( keybas + 1 ) -  1
      else
         ltrsiz  =  ppage(keybas+pkidx)  -  ppage(keybas+pkidx-1)  -  1
      end if

!
!     The next item to add to the middle child is the middle key
!     from the parent.  The data pointer is copied; the key value is
!     simply set.  The value of the key is one more than the size of
!     the entire key set (including descendants) we moved into the
!     middle from the left.  LNMOVE is the size of this key set.
!
!     No child pointer is copied.
!
      lnmove                  = ltrsiz - ldelta
      c3page(trkeyc+lshift+1) = lnmove + 1

      c3page(trdatc+lshift+1) =  ppage ( datbas + pkidx )

!
!     Now we copy keys from the right child into the middle.  We'll
!     take the first RSHIFT keys from the right node, where
!
      rshift  =  nrkeys - ( rsize + 1 )

!
!     When these keys are moved, they gain RDELTA predecessors, where
!     RDELTA is the size of the key set already in the middle node.
!
      rdelta  =  lnmove + 1

      do i = 1, rshift
         c3page(trkeyc+lshift+1+i) = c2page(trkeyc+i) + rdelta
      end do

      c3page(trdatc+lshift+2:trdatc+lshift+2+rshift-1) = c2page(trdatc+1:trdatc+1+rshift-1)
      c3page(trkidc+lshift+2:trkidc+lshift+2+rshift) = c2page(trkidc+1:trkidc+1+rshift)

!
!     Save the size of the entire key set moved into the middle from
!     the right.
!
      rnmove  =  c2page(trkeyc+rshift+1) - 1

!
!     Set the key count in the new child.
!
      c3page ( trnkc  )  =  msize

!
!     The middle child is complete.
!
!     The next step is to set up the parent node.  The original parent
!     key at index PKIDX is replaced by the key from the left child
!     at location LSIZE + 1.  The following parent keys are shifted
!     right by one location, making room for a second key following
!     the one at PKIDX.  This newly freed slot is filled in with the
!     key at location RSHIFT+1 in the right child.
!
!     The keys in the parent to the right of position PKIDX+1 gain no
!     predecessors as the result of these re-arrangements.
!
!     Get the number of keys in the parent.
!
      if ( parent == root ) then
         npkeys = ppage ( trnkr )
      else
         npkeys = ppage ( trnkc )
      end if

!
!     Make room for the new key.  Shift elements starting from the
!     right.
!
      do i = npkeys,  pkidx+1,  -1
         ppage(keybas+i+1)  =  ppage(keybas+i)
      end do

      do i = npkeys,  pkidx+1,  -1
         ppage(datbas+i+1)  =  ppage(datbas+i)
      end do

      do i = npkeys+1,  pkidx+1,  -1
         ppage(kidbas+i+1)  =  ppage(kidbas+i)
      end do

!
!     Copy in the data pointer from the left child.  Note that
!     no child pointer comes along.
!
      ppage ( datbas+pkidx )  =  c1page ( trdatc + lsize + 1 )

!
!     Set the key value at PKIDX.  The value exceeds that of the
!     preceding key, if any, by one more than the size of the subtree
!     headed by the left child.  That size is one less than
!     LDELTA, since LDELTA includes the key at location LSIZE+1.
!
      if ( pkidx == 1 ) then
         ppage ( keybas+pkidx ) =  ldelta
      else
         ppage ( keybas+pkidx ) =  ppage ( keybas+pkidx-1 ) + ldelta
      end if

!
!     Copy in the data pointer from the right child.  Again, note that
!     no child pointer comes along.
!
      ppage ( datbas+pkidx+1 ) = c2page ( trdatc + rshift + 1 )

!
!     Set the key value at PKIDX+1.  The value exceeds that of the
!     preceding key by one more than the size of the subtree headed by
!     the middle child.
!
      ppage ( keybas+pkidx+1 ) =    ppage (keybas+pkidx) &
                                  + lnmove &
                                  + rnmove &
                                  + 2

!
!     The child pointer at PKIDX+1 does get set:  it points to the
!     middle child.
!
      ppage ( kidbas+pkidx+1 ) = new

!
!     Remarkably, the only required change to the parent's metadata is
!     updating the key count.  At this point, we can set the overflow
!     flag, depending on the status of the parent.
!
      if ( parent == root ) then

         ppage ( trnkr )  =  ppage ( trnkr ) + 1
         overfl           =  ppage ( trnkr )   ==   mxkeyr + 1

      else

         ppage ( trnkc )  =  ppage ( trnkc ) + 1
         overfl           =  ppage ( trnkc )   ==   mxkeyc + 1

      end if

!
!     Update the metadata in the first child.  This node has lost
!     just enough keys to give it size LSIZE.
!
      c1page ( trnkc  )  =  lsize

!
!     For safety, clean out the vacated key and pointer locations.
!     Clear the overflow addresses as well.
!
      c1page(trkeyc + lsize + 1:trkeyc + lsize + 1+mxkeyc+1-lsize-1) = 0
      c1page(trdatc + lsize + 1:trdatc + lsize + 1+mxkeyc+1-lsize-1) = 0
      c1page(trkidc + lsize + 2:trkidc + lsize + 2+mxkidc+1-(lsize+1)-1) = 0

!
!     The first child is set.
!
!     To adjust the second child, we must shift the keys and pointers
!     left to fill in the vacated space.  The keys in this second child
!     must be adjusted to account for the loss of the predecessors
!     moved to the middle child and the parent.
!
!     Shift elements starting from the left.
!
      do i = 1,  rsize
         c2page(trkeyc+i)  =  c2page(trkeyc+rshift+1+i) - ( rnmove + 1 )
      end do

      do i = 1,  rsize
         c2page(trdatc+i)  =  c2page(trdatc+rshift+1+i)
      end do

      do i = 1,  rsize+1
         c2page(trkidc+i)  =  c2page(trkidc+rshift+1+i)
      end do

!
!     Update the key count in the second child.  This node has lost
!     just enough keys to give it size RSIZE.
!
      c2page ( trnkc  )  =  rsize

!
!     For safety, clean out the vacated key and pointer locations.
!     Clear the overflow addresses as well.
!
      c2page(trkeyc + rsize + 1:trkeyc + rsize + 1+mxkeyc+1-rsize-1) = 0
      c2page(trdatc + rsize + 1:trdatc + rsize + 1+mxkeyc+1-rsize-1) = 0
      c2page(trkidc + rsize + 2:trkidc + rsize + 2+mxkidc+1-(rsize+1)-1) = 0

!
!     The second child is set.
!
!     The last change we must make is to update the node count in
!     the root.
!
      if ( parent == root ) then

         ppage ( trnnod ) =  ppage ( trnnod ) + 1

      else
!
!        We won't read in the whole root page; we'll just get the
!        base address of the root and update the affected location.
!
         rbase  =  zzektrbs ( ctx, root )

         call dasrdi ( ctx, handle, rbase+trnnod, rbase+trnnod, nnode_1 )
         nnode = nnode_1(1)
         call dasudi ( ctx, handle, rbase+trnnod, rbase+trnnod, [ nnode+1 ] )

      end if

!
!     Write out our updates.
!
      call zzekpgwi ( ctx, handle,  parent,  ppage  )
      call zzekpgwi ( ctx, handle,  left,    c1page )
      call zzekpgwi ( ctx, handle,  right,   c2page )
      call zzekpgwi ( ctx, handle,  new,     c3page )

      return
      end subroutine zzektr23

      !> Execute a 3-2 merge:  merge three neighboring sibling nodes, two
      !> of which contain the minimum number of keys and one of which
      !> has an underflow of one key, into two nodes, each one
      !> approximately full.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     LEFT           is the node number of the left node of a trio of
      !>                    siblings.  LEFT either contains the minimum
      !>                    allowed number of keys  or is underflowing by
      !>                    one key.
      !>
      !>     MIDDLE         is the node number of the middle node of a trio of
      !>                    siblings.  MIDDLE either contains the minimum
      !>                    allowed number of keys  or is underflowing by
      !>                    one key.
      !>
      !>     RIGHT          is the node number of the right node of a trio of
      !>                    siblings.  The total number of keys in nodes
      !>                    LEFT, MIDDLE and RIGHT amounts to an underflow of 1
      !>                    key.
      !>
      !>     PARENT         is the node number of the common parent of LEFT
      !>                    and RIGHT.
      !>
      !>     LPKIDX         is the node-relative index within PARENT of the
      !>                    left parent key of MIDDLE.  This key is the
      !>                    immediate predecessor of the first key in the
      !>                    subtree headed by MIDDLE.
      !>
      !>## Detailed Output
      !>
      !>     UNDRFL         is a logical flag indicating whether the parent
      !>                    node underflowed as a result of the 3-2 merge.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>     3)  If LEFT and RIGHT are not neighboring siblings, the error
      !>         SPICE(BUG) is signaled.
      !>
      !>     4)  If either LEFT or RIGHT are not children of PARENT, the error
      !>         SPICE(BUG) is signaled.
      !>
      !>     5)  If the sum of the number of keys in LEFT and RIGHT does not
      !>         correspond to an underflow of exactly 1 key, the error
      !>         SPICE(BUG) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     Deletions from an EK tree start at a leaf node.  If the node
      !>     underflows, the EK system attempts to shuffle keys at the leaf
      !>     level to resolve the underflow.  That attempt failing, the system
      !>     delegates the problem upward to the next higher level.
      !>
      !>     There are two ways to resolve underflow in a non-root node:
      !>     balance the underflowing node with one of its closest siblings
      !>     (see ZZEKTRBN), or if the closest siblings contain the minimum
      !>     number of keys, execute a 3-2 merge.
      !>
      !>     A 3-2 merge involves deletion of the middle node of a trio of
      !>     neighboring siblings, two of which contain the minimum
      !>     number of keys and one of which has an underflow of one key,
      !>     and redistributing the keys between the two remaining nodes
      !>     and their common parent so that each of the two remaining siblings
      !>     contains the maximum number of keys.  The parent loses a key in
      !>     the process.
      !>
      !>     After the 3-2 merge, the tree is balanced and the siblings
      !>     satisfy the key count invariants.  However, the parent of the
      !>     siblings may underflow by one key.
      !>
      !>     Below are the gory details concerning the actions of this routine.
      !>     All of the parameters referred to here (in capital letters) are
      !>     defined in the include file ektree.inc.
      !>
      !>     In a 3-2 merge:
      !>
      !>        - The left parent key of the middle child is rotated down
      !>          into the left child and is appended on the right to the key
      !>          set of that child.  The left parent key's data pointer moves
      !>          along with the key.  The child pointers of this parent key
      !>          do not move along with the key; these pointers point to the
      !>          left and middle child nodes.
      !>
      !>        - The keys of the middle child are divided into three sets:
      !>          a set to be rotated left through the parent node into the
      !>          left child, a singleton set consisting of a key to be moved
      !>          up into the parent, and a set of keys to be rotated right
      !>          through the parent into the right child.  The sizes of the
      !>          leftmost and rightmost of these sets differ by at most 1.
      !>
      !>        - The number of keys that are rotated left is picked so that
      !>          after the rotation, the size of the left node will be
      !>          (3*MNKEYC)/2.  The data pointers of these keys move along
      !>          with the keys.  All of the left and right child pointers
      !>          of these keys move along with the keys into the left child.
      !>
      !>        - The singleton key in the child moves up into the parent
      !>          node.  Its data pointer moves with it.  After the move into
      !>          the parent node, the left child pointer of this key points
      !>          to the left child; the right child pointer points to the
      !>          right child.
      !>
      !>        - The right parent key of the middle child is rotated right
      !>          into the right child.  The data pointer of this key moves
      !>          with the key.  The child pointers of this parent key
      !>          do not move along with the key; these pointers point to the
      !>          middle and right child nodes.
      !>
      !>        - The remaining keys in the middle child are rotated right
      !>          into the right child; these become the leftmost keys of
      !>          that child.  The data pointers of these keys move along
      !>          with them.  The child pointers of these keys also move
      !>          along with them.
      !>
      !>        - The right child ends up with
      !>
      !>             (3*MNKEYC) - (3*MNKEYC)/2
      !>
      !>          keys.  This may be deduced from the facts that the original
      !>          three children had between them an underflow of one key, the
      !>          parent lost a key, and the left child has (3*MNKEYC)/2 keys.
      !>
      !>          Since
      !>
      !>             MNKEYC    =  MNKIDC - 1
      !>                       =  (  ( 2*MXKIDC + 1 ) / 3   )  -  1
      !>                       =  ( 2*MXKIDC - 2 ) / 3
      !>                       =  ( 2*MXKEYC ) / 3
      !>
      !>          we have
      !>
      !>             3*MNKEYC  <  2*MXKEYC
      !>                       -
      !>
      !>          If 3*MNKEYC is odd, we have strict inequality and also
      !>
      !>             3*MNKEYC  =  2 * ( (3*MNKEYC)/2 )  +  1
      !>
      !>          so
      !>
      !>             3*MNKEYC  + 1  =  2 * ( (3*MNKEYC)/2 )  +  2
      !>
      !>                            =  2 * (  (3*MNKEYC)/2  +  1 )
      !>
      !>                            <  2 * MXKEYC
      !>                            -
      !>
      !>          So in this case, the larger of the child nodes, which has
      !>          size
      !>
      !>             (3*MNKEYC)/2  +  1
      !>
      !>          has a key count no greater than MXKEYC.
      !>
      !>          If 3*MNKEYC is even, then the left and right child are the
      !>          same size, and the inequality
      !>
      !>             3*MNKEYC  <  2*MXKEYC
      !>                       -
      !>
      !>          implies directly that both nodes have size no greater than
      !>          MXKEYC.
      !>
      !>          Since both child nodes have size approximately 3/2 * MNKEYC,
      !>          and since MNKEYC is approximately 40, the minimum size
      !>          constraints on the child nodes are easily met.
      !>
      !>     As the above description shows, the parent loses a key as a
      !>     result of a 3-2 merge.  This may cause the parent to underflow;
      !>     if it does, the underflow at the parent's level must be resolved.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRDL.
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
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Fixed typo in comments.
      !>
      !>-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
      module subroutine zzektr32 ( ctx, handle, tree, left, middle, right, parent, lpkidx, undrfl )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      use pepper_inc_ekpage
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: left  !! Left sibling node.
      integer(ip), intent(in) :: middle  !! Middle sibling node.
      integer(ip), intent(in) :: right  !! Right sibling node.
      integer(ip), intent(in) :: parent  !! Common parent node.
      integer(ip), intent(in) :: lpkidx  !! Node-relative index of left parent key of MIDDLE.
      logical, intent(out) :: undrfl  !! Underflow flag.
      integer(ip) :: nnode_1(1)  !! one-element copy of nnode (array argument)
!
!     Local parameters
!
      integer(ip), parameter :: lsize = (3*mnkeyc)/2

      integer(ip), parameter :: rsize = 3*mnkeyc - lsize

      integer(ip) :: c1page ( pgsizi )
      integer(ip) :: c2page ( pgsizi )
      integer(ip) :: c3page ( pgsizi )
      integer(ip) :: datbas
      integer(ip) :: i
      integer(ip) :: keybas
      integer(ip) :: kidbas
      integer(ip) :: leftsz
      integer(ip) :: lmidsz
      integer(ip) :: lpkey
      integer(ip) :: lsib
      integer(ip) :: midsiz
      integer(ip) :: msib
      integer(ip) :: n
      integer(ip) :: nlkeys
      integer(ip) :: nmkeys
      integer(ip) :: nnode
      integer(ip) :: npkeys
      integer(ip) :: nrkeys
      integer(ip) :: ppage ( pgsizi )
      integer(ip) :: psize
      integer(ip) :: rbase
      integer(ip) :: rmidsz
      integer(ip) :: root
      integer(ip) :: rpkey
      integer(ip) :: rshift
      integer(ip) :: rsib
      integer(ip) :: sizbas
      integer(ip) :: sum

!
!     The plan is to take three sibling nodes, two of which contain
!     the minimum number of keys and one of which is underflowing by one
!     key, and to merge these nodes into two nodes.  This process
!     reduces the number of nodes in the parent by one and may cause the
!     parent to underflow.
!
!     After the merge, the sum of the numbers of keys in the two
!     children will be exactly (3*MNKEYC).  The numbers of keys in the
!     left and right nodes will be, respectively:
!
!        LSIZE  =   INT (  (3*MNKEYC)/2  )
!        RSIZE  =   (3*MNKEYC)  - LSIZE
!
!     We need to be sure that LSIZE and RSIZE are in the range
!
!        MNKEYC : MXKEYC
!
!     The definition of LSIZE implies that
!
!        LSIZE  =  MNKEYC + INT ( MNKEYC/2 )
!
!     so
!
!        MNKEYC + INT ( MNKEYC/2 )  <  LSIZE <  (3/2)*MNKEYC
!                                   -        -
!
!     and since
!
!        MNKEYC  =  MNKIDC - 1
!                =  INT ( ( 2*MXKIDC + 1 ) / 3 ) - 1
!                =  INT ( ( 2*MXKEYC + 3 ) / 3 ) - 1
!                =  INT ( ( 2*MXKEYC     ) / 3 )
!
!     we have
!
!        (3/2) * MNKEYC  =  (3/2) * INT ( (2*MXKEYC) / 3 )  <  MXKEYC
!                                                           -
!
!     Thus  LSIZE is guaranteed to be in range.
!
!     When MNKEYC is even, RSIZE is equal to LSIZE and thus is
!     within bounds.  When MNKEYC is odd, RSIZE exceeds LSIZE by 1, so
!
!        MNKEYC  <  RSIZE
!
!     It remains to be shown that
!
!        RSIZE   <  MXKEYC
!                -
!
!     when MNKEYC is odd.  When this is the case, the quantity
!
!        (3/2) * MNKEYC
!
!     is not an integer and therefore is strictly less than MXKEYC.
!     This quantity is also greater than LSIZE, so we conclude that
!
!        LSIZE  <  MXKEYC - 1
!               -
!
!     Since RSIZE exceeds LSIZE by 1, we have
!
!        RSIZE  <  MXKEYC
!               -
!
!     as we claimed.
!
!     All right, read in the child and parent pages.
!
      call zzekpgri ( ctx, handle, left,   c1page )
      call zzekpgri ( ctx, handle, middle, c2page )
      call zzekpgri ( ctx, handle, right,  c3page )
      call zzekpgri ( ctx, handle, parent, ppage  )

!
!     The actual addresses in the parent node depend on whether the
!     parent is the root.  Compute the necessary bases to avoid a lot
!     of cases.
!
      root  =  tree

      if ( parent == root ) then
         keybas  =  trkeyr
         datbas  =  trdatr
         kidbas  =  trkidr
         sizbas  =  trnkr
      else
         keybas  =  trkeyc
         datbas  =  trdatc
         kidbas  =  trkidc
         sizbas  =  trnkc
      end if

!
!     Check the left parent key of the middle child.
!
      psize =  ppage ( sizbas )

      if (  ( lpkidx < 1 ) .or. ( lpkidx > psize-1 )  )  then

         call ctx%setmsg ( 'Left parent key of MIDDLE is out of '   // &
                       'range.  Value is #; valid range is 1:#'  )
         call ctx%errint ( '#',   lpkidx                             )
         call ctx%errint ( '#',   psize-1                            )
         call ctx%sigerr ( 'SPICE(BUG)'                              )
         return

      end if

!
!     Retain the left and right parent key values of the middle child.
!
      lpkey  =  ppage( keybas + lpkidx    )
      rpkey  =  ppage( keybas + lpkidx + 1)

!
!     Verify that LEFT, MIDDLE, and RIGHT are siblings, and that PARENT
!     is their common parent.
!
      lsib  =  ppage ( kidbas+lpkidx   )
      msib  =  ppage ( kidbas+lpkidx+1 )
      rsib  =  ppage ( kidbas+lpkidx+2 )

      if (      ( lsib /= left   ) &
           .or. ( msib /= middle ) &
           .or. ( rsib /= right  )  ) then

         call ctx%setmsg ( 'LEFT, RIGHT, MIDDLE, PARENT, and PKIDX are '  // &
                       'inconsistent. LEFT = #; MIDDLE = #; '         // &
                       'RIGHT = #; PARENT = #; LPKIDX = #; LSIB '     // &
                       'derived from PARENT = #; MSIB = #; RSIB = #.'  )
         call ctx%errint ( '#',   left                                     )
         call ctx%errint ( '#',   middle                                   )
         call ctx%errint ( '#',   right                                    )
         call ctx%errint ( '#',   parent                                   )
         call ctx%errint ( '#',   lpkidx                                   )
         call ctx%errint ( '#',   lsib                                     )
         call ctx%errint ( '#',   msib                                     )
         call ctx%errint ( '#',   rsib                                     )
         call ctx%sigerr ( 'SPICE(BUG)'                                    )
         return

      end if

!
!     Get the number of keys in the parent.
!
      if ( parent == root ) then
         npkeys = ppage ( trnkr )
      else
         npkeys = ppage ( trnkc )
      end if

!
!     Get the number of keys in each child.
!
      nlkeys  =  c1page ( trnkc )
      nmkeys  =  c2page ( trnkc )
      nrkeys  =  c3page ( trnkc )

      sum     =  nlkeys + nmkeys + nrkeys

!
!     The sum of the number of keys in the three input nodes must
!     sum exactly to value representing an underflow level of 1 key.
!
      if (  sum  /=  ( 3*mnkeyc - 1 )  ) then

         call ctx%setmsg ( 'Number of keys in nodes LEFT = #; in MIDDLE ' // &
                       '= #; in RIGHT = #; counts summing to # were ' // &
                       'expected.'                                     )
         call ctx%errint ( '#',  nlkeys                                    )
         call ctx%errint ( '#',  nmkeys                                    )
         call ctx%errint ( '#',  nrkeys                                    )
         call ctx%errint ( '#',  3*mnkeyc-1                                )
         call ctx%sigerr ( 'SPICE(BUG)'                                    )
         return

      end if

!
!     We're set to carry out the merge.  Here's an overview of what
!     gets moved where.
!
!        The left parent key of the middle node moves into the left
!        node, at the end of the node.
!
!        The first N-1 keys and N child pointers of the middle node get
!        moved into the left node, where
!
!           N  =  LSIZE - ( 1 + NLKEYS ) + 1
!
!        The Nth key of the middle node moves into the parent,
!        replacing the left parent key of the middle node.
!
!        The right parent key of the middle node moves into the right
!        node, at the beginning of the node.
!
!        The keys from position N+1 onward in the middle node, as
!        well as all of the remaining child pointers, move into the
!        right node, at the beginning.
!
!        The right parent key's location is filled in by shifting
!        the keys, data pointers, and child pointers in the parent
!        to the left by one position.  The child pointer removed by this
!        operation is the pointer to the middle child.
!
!        The middle child node disappears.
!
!     Before re-arranging things, we'll need to have on hand the key
!     counts for various sets of keys.  We'll use the variable LEFTSZ
!     for the number of keys in the subtree headed by LEFT.  We'll
!     use the variable LMIDSZ to refer to the `subtree' headed by
!     the set of keys in the middle node that will be shifted into
!     the left child.  The variable RMSIZE will represent the size of
!     the key set moved from the middle child into the right child.
!     MIDSIZ will be the key count for the subtree headed by the middle
!     child.
!
!     Consistent with usage above, the variable N will represent
!     the index of the key in the middle node that will rapturously
!     ascend into the parent.
!

      if ( lpkidx == 1 ) then
         leftsz  =  lpkey - 1
      else
         leftsz  =  lpkey - ppage(keybas+lpkidx-1) - 1
      end if

      n       =  lsize  -  ( 1 + nlkeys ) + 1

      lmidsz  =  c2page(trkeyc+n)  - 1
      midsiz  =  rpkey  -  lpkey   - 1
      rmidsz  =  midsiz -  lmidsz  - 1

!
!     Move the left parent key into the left child.  The key itself
!     doesn't really move; its value is simply re-assigned.  The
!     data pointer is copied, however.  The child pointer at location
!     LSIZE+1 is unaffected by this move.
!
      c1page ( trkeyc + nlkeys + 1 )  =  leftsz + 1
      c1page ( trdatc + nlkeys + 1 )  =  ppage ( datbas + lpkidx )

!
!     Move the first N-1 keys and data pointers, and the first N
!     child pointers, from the middle child into the left
!     child.  The moved keys will gain LEFTSZ + 1 predecessors.
!
      do i = 1, n-1
         c1page(trkeyc+nlkeys+1+i) =  c2page(trkeyc+i) +  leftsz + 1
      end do

      c1page(trdatc+nlkeys+2:trdatc+nlkeys+2+n-1-1) = c2page(trdatc+1:trdatc+1+n-1-1)
      c1page(trkidc+nlkeys+2:trkidc+nlkeys+2+n-1) = c2page(trkidc+1:trkidc+1+n-1)

!
!     Set the key count in the left child.
!
      c1page ( trnkc )  =  lsize

!
!     The left child is complete.  Now it's time to set up the right
!     child.  First off, we'll shift the node's contents to the right
!     by the number of new keys we're going to insert.  Shift the
!     rightmost elements first.  The shifted keys will gain RMIDSZ+1
!     predecessors, so  we adjust the keys as we shift them.
!
      rshift  =  nmkeys - n + 1

      do i = nrkeys, 1, -1
         c3page(trkeyc+rshift+i) =  c3page(trkeyc+i) + rmidsz + 1
      end do

      do i = nrkeys, 1, -1
         c3page(trdatc+rshift+i) =  c3page(trdatc+i)
      end do

      do i = nrkeys+1, 1, -1
         c3page(trkidc+rshift+i) =  c3page(trkidc+i)
      end do

!
!     The key at location RSHIFT receives the former right parent key
!     of the middle child.   The key value is simply assigned; the
!     data pointer is copied.  The child pointer at location RSHIFT
!     will be set later.
!
      c3page ( trkeyc + rshift )  =  rmidsz + 1
      c3page ( trdatc + rshift )  =  ppage ( datbas+lpkidx+1 )

!
!     The first RSHIFT-1 locations in the right child are filled in
!     with data from the middle child.  The moved keys lose LMIDSZ+1
!     predecessors.
!
      do i = 1, rshift-1
         c3page(trkeyc+i) = c2page(trkeyc+n+i) - lmidsz - 1
      end do

      c3page(trdatc+1:trdatc+1+rshift-1-1) = c2page(trdatc+n+1:trdatc+n+1+rshift-1-1)
      c3page(trkidc+1:trkidc+1+rshift-1) = c2page(trkidc+n+1:trkidc+n+1+rshift-1)

!
!     Update the key count in the right child.
!
      c3page ( trnkc )  =  rsize

!
!     The right child is complete.  It's time to update the parent.
!
!     The key at location N in the middle child replaces the left parent
!     key.  The key value is actually re-assigned; the data pointer does
!     move.  The left parent key increases by the number of keys moved
!     into the subtree headed by the left child.
!
      ppage ( keybas + lpkidx )  =  lpkey + lmidsz + 1
      ppage ( datbas + lpkidx )  =  c2page( trdatc + n )

!
!     The parent keys, data pointers, and child pointers at locations
!     LPKIDX+2 onward get shifted left by one position.  The keys lose
!     no  predecessors as the result of these re-arrangements.
!
      do i = lpkidx+1, npkeys-1
         ppage(keybas+i)  =  ppage(keybas+i+1)
      end do

      do i = lpkidx+1, npkeys-1
         ppage(datbas+i)  =  ppage(datbas+i+1)
      end do

      do i = lpkidx+1, npkeys
         ppage(kidbas+i)  =  ppage(kidbas+i+1)
      end do

!
!     Zero out the freed locations.
!
      ppage ( keybas+npkeys   ) = 0
      ppage ( datbas+npkeys   ) = 0
      ppage ( kidbas+npkeys+1 ) = 0

!
!     The only required change to the parent's metadata is
!     updating the key count.  At this point, we can set the
!     underflow flag, depending on the status of the parent.
!
      if ( parent == root ) then

         ppage ( trnkr )  =  ppage ( trnkr ) - 1
         undrfl           =  ppage ( trnkr )   ==   0

      else

         ppage ( trnkc )  =  ppage ( trnkc ) - 1
         undrfl           =  ppage ( trnkc )   ==   mnkeyc - 1

      end if

!
!     The last change we must make is to update the node count in
!     the root.
!
      if ( parent == root ) then

         ppage ( trnnod ) =  ppage ( trnnod ) - 1

      else
!
!        We won't read in the whole root page; we'll just get the
!        base address of the root and update the affected location.
!
         rbase  =  zzektrbs ( ctx, root )

         call dasrdi ( ctx, handle, rbase+trnnod, rbase+trnnod, nnode_1 )
         nnode = nnode_1(1)
         call dasudi ( ctx, handle, rbase+trnnod, rbase+trnnod, [ nnode-1 ] )

      end if

!
!     Write out our updates.
!
      call zzekpgwi ( ctx, handle,  parent,  ppage  )
      call zzekpgwi ( ctx, handle,  left,    c1page )
      call zzekpgwi ( ctx, handle,  right,   c3page )

!
!     Free the page used by the middle child.
!
      call zzekpgfr ( ctx, handle, int, middle )

      return
      end subroutine zzektr32

      !> Return the data pointer from a specified location in an EK tree.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     KEY            is an absolute key indicating the location of
      !>                    the desired pointer.  In EK trees, absolute keys
      !>                    are just ordinal positions relative to the leftmost
      !>                    element of the tree, with the leftmost element
      !>                    having position 1.  So setting KEY to 10, for
      !>                    example, indicates that the output PTR is the 10th
      !>                    item in the tree.
      !>
      !>                    KEY must be in the range 1 : NKEYS, where
      !>                    NKEYS is the number of keys in the tree.
      !>
      !>## Detailed Output
      !>
      !>     PTR            is the integer value associated with the input key.
      !>                    Normally, this value is a data pointer.  However,
      !>                    the EK tree system makes no use of this fact, so
      !>                    PTR need not actually be a pointer.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the input key is out of range, the error is diagnosed by
      !>         routines called by this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine obtains the value associated with a key, without
      !>     returning the metadata supplied by the lower-level lookup routine
      !>     ZZEKTRLK.
      !>
      !>## Examples
      !>
      !>     See EKDELR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 26-OCT-1995 (NJB)
      module subroutine zzektrdp ( ctx, handle, tree, key, ptr )
      use pepper_kinds, only: ip
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: key  !! Key corresponding to pointer.
      integer(ip), intent(out) :: ptr  !! Data pointer.
!
!     Local variables
!
      integer(ip) :: idx
      integer(ip) :: node
      integer(ip) :: noffst
      integer(ip) :: level

!
!     Use discovery check-in in this puppy.
!
      call zzektrlk ( ctx, handle, tree,   key,   idx, &
                      node,   noffst, level, ptr  )

      return
      end subroutine zzektrdp

      !> Return the number of keys in a node.
      !>
      !> The function returns the total number of keys in the specified
      !> node of an EK tree.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for read or write
      !>                    access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     NODE           is the node number of interest.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the total number of keys in the specified
      !>     node of the EK tree designated by TREE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine should be used to determine the current size of a
      !>     node in an EK tree; the tree's metadata should not be accessed
      !>     directly to extract this information.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRIN.
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
      !>-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
      module integer(ip) function zzektrnk ( ctx, handle, tree, node )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: node  !! Node of interest.
      integer(ip) :: value_1(1)  !! one-element copy of the result (array argument)

      integer(ip) :: addrss

!
!     Go straight to the address at which the key count for the
!     node is stored.
!
      if ( tree == node ) then
         addrss  =  zzektrbs ( ctx, node )  +  trnkr
      else
         addrss  =  zzektrbs ( ctx, node )  +  trnkc
      end if

      call dasrdi ( ctx, handle, addrss, addrss, value_1 )
      zzektrnk = value_1(1)

      return
      end function zzektrnk

      !> Identify the immediate siblings of a node:  return a key in each
      !> sibling and the siblings' node numbers.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for read or write
      !>                    access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     KEY            is a key belonging to a node whose sibling nodes
      !>                    are sought.  KEY is expected to be an absolute,
      !>                    not node-relative, key.
      !>
      !>## Detailed Output
      !>
      !>     LSIB           is the number of the left sibling node of the node
      !>                    containing KEY.  If the node containing KEY has no
      !>                    left sibling, LSIB is set to zero.
      !>
      !>     LKEY           is an absolute key in node LSIB.
      !>
      !>     RSIB           is the number of the right sibling node of the node
      !>                    containing KEY.  If the node containing KEY has no
      !>                    right sibling, RSIB is set to zero.
      !>
      !>     RKEY           is an absolute key in node RSIB.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an error occurs while looking up the parent of the node
      !>         containing KEY, the error will be diagnosed by routines
      !>         called by this routine.  It is not an error for the node
      !>         containing KEY to have no parent, as long as KEY belongs to
      !>         the root.
      !>
      !>     3)  If an I/O error occurs while reading the indicated file, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine is a utility intended for use by other routines in
      !>     the EKTRxx set.
      !>
      !>     The output keys LKEY and RKEY may be used to find the siblings
      !>     of the sibling nodes LSIB and RSIB.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRDL.
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
      !>-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
      module subroutine zzektrsb ( ctx, handle, tree, key, lsib, lkey, rsib, rkey )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: key  !! Key of interest.
      integer(ip), intent(out) :: lsib  !! Left sibling node.
      integer(ip), intent(out) :: lkey  !! Key in left sibling.
      integer(ip), intent(out) :: rsib  !! Right sibling node.
      integer(ip), intent(out) :: rkey  !! Key in right sibling.
      integer(ip) :: rkey_1(1)  !! one-element copy of rkey (array argument)
      integer(ip) :: lkey_1(1)  !! one-element copy of lkey (array argument)
      integer(ip) :: loffst_1(1)  !! one-element copy of loffst (array argument)
!
!     Local variables
!
      integer(ip) :: addrss
      integer(ip) :: base
      integer(ip) :: keybas
      integer(ip) :: kidbas
      integer(ip) :: loffst
      integer(ip) :: llpidx
      integer(ip) :: lpidx
      integer(ip) :: lpkey
      integer(ip) :: nkbas
      integer(ip) :: parent
      integer(ip) :: pkey
      integer(ip) :: poffst
      integer(ip) :: roffst
      integer(ip) :: rpidx
      integer(ip) :: rpkey

!
!     Start out by looking up the parent node.  We get LSIB
!     and RSIB for free.
!
      call zzektrpi ( ctx, handle, tree,   key,  parent, pkey,  poffst, &
                      lpidx,  lpkey,  lsib, rpidx,  rpkey, rsib   )

      if ( ctx%failed() ) return

!
!     Set the base addresses for the child pointers and keys,
!     based on whether the parent is the root.
!
      if ( parent == tree ) then
         keybas = trkeyr
         kidbas = trkidr
         nkbas  = trnkr
      else
         keybas = trkeyc
         kidbas = trkidc
         nkbas  = trnkc
      end if

!
!     We need to find absolute keys in each sibling that exists.
!     To do this, we need the node offset of each sibling node.
!     That offset is the value of the parent key preceding each node,
!     plus the parent's offset.
!
      if ( lpidx > 1 ) then
!
!        The left parent key has a predecessor.  This predecessor is
!        the immediate predecessor of the left sibling node.
!
         llpidx  =  lpidx  -  1
         base    =  zzektrbs ( ctx, parent )
         addrss  =  base   +  keybas  +  llpidx

         call dasrdi ( ctx, handle, addrss, addrss, loffst_1 )
         loffst = loffst_1(1)

         loffst  =  loffst + poffst

!
!        Get the first key from the left sibling.  Convert the key
!        to an absolute key.
!
         base    =  zzektrbs ( ctx, lsib )
         addrss  =  base + trkeyc + 1

         call dasrdi ( ctx, handle, addrss, addrss, lkey_1 )
         lkey = lkey_1(1)

         lkey    =  lkey + loffst

      else if ( lpidx == 1 ) then
!
!        The left parent key is the first key.  The left sibling has
!        no predecessor.
!
!        Get the first key from the left sibling.  Convert the key
!        to an absolute key.
!
         base    =  zzektrbs ( ctx, lsib )
         addrss  =  base + trkeyc + 1

         call dasrdi ( ctx, handle, addrss, addrss, lkey_1 )
         lkey = lkey_1(1)

         lkey    =  lkey + poffst

      else
!
!        There's no left sibling.  Set the left sibling's key to a
!        value that won't be mistaken for a valid one.
!
         lkey    =  0

      end if

!
!     LKEY is set.  It's time to produce an absolute key for the
!     right sibling.
!
      if ( rpidx > 0 ) then
!
!        The right parent key exists.  This key is the
!        immediate predecessor of the right sibling node.
!
         roffst  =  rpkey  +  poffst

!
!        Get the first key from the right sibling.  Convert the key
!        to an absolute key.
!
         base    =  zzektrbs ( ctx, rsib )
         addrss  =  base + trkeyc + 1

         call dasrdi ( ctx, handle, addrss, addrss, rkey_1 )
         rkey = rkey_1(1)

         rkey    =  rkey + roffst

      else
!
!        There's no right sibling.  Set the right sibling's key to a
!        value that won't be mistaken for a valid one.
!
         rkey    =  0

      end if
!
!     All outputs are set.
!

      return
      end subroutine zzektrsb

      !> Return the number of keys in a tree.
      !>
      !> The function returns the total number of keys in the specified
      !> EK tree.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for read or write
      !>                    access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the total number of keys in the specified
      !>     EK tree.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading the indicated file, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine should be used to determine the current size of an
      !>     EK tree; the tree's metadata should not be accessed directly to
      !>     extract this information.
      !>
      !>## Examples
      !>
      !>     See EKNSEG.
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
      !>-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
      module integer(ip) function zzektrsz ( ctx, handle, tree )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip) :: value_1(1)  !! one-element copy of the result (array argument)

      integer(ip) :: addrss

!
!     Go straight to the address at which the key count is stored.
!
      addrss  =  zzektrbs ( ctx, tree )  +  trnkey

      call dasrdi ( ctx, handle, addrss, addrss, value_1 )
      zzektrsz = value_1(1)

      return
      end function zzektrsz

      !> Delete a value from a tree at a specified location without
      !> balancing the tree.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     KEY            is an absolute key indicating the deletion
      !>                    location.  In EK trees, absolute keys are just
      !>                    ordinal positions relative to the leftmost element
      !>                    of the tree, with the leftmost element having
      !>                    position 1.  So setting KEY to 10, for example,
      !>                    indicates that the input VALUE is the 10th item in
      !>                    the tree.
      !>
      !>                    KEY must be in the range 1 : NKEYS, where
      !>                    NKEYS is the number of keys in the tree prior to
      !>                    the deletion.
      !>
      !>## Detailed Output
      !>
      !>     TRGKEY         is an absolute key identifying the node from which
      !>                    the deletion occurred.  This node may be different
      !>                    from the node that contained KEY before the
      !>                    deletion; see $Particulars for details.
      !>
      !>     UNDRFL         is a logical flag indicating whether the node
      !>                    at which VALUE was inserted underflowed as a
      !>                    result.  Child nodes must contain at least
      !>                    MNKEYC keys; this bound is declared in ektree.inc.
      !>                    The root node is permitted to become empty.
      !>
      !>                    When an underflow condition exists, the tree
      !>                    violates an invariant.  The underflow must be
      !>                    resolved before any other insertions or deletions
      !>                    are performed.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the input key is out of range, the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine deletes a value from an EK tree at the ordinal
      !>     position indicated by KEY.  The deletion is always done from a
      !>     leaf node.  If KEY is not in a leaf node, the value corresponding
      !>     to KEY is swapped with that of an immediate neighbor, and the
      !>     neighbor is deleted. This is possible because every key is either
      !>     in a leaf or has the property that its predecessor and successor
      !>     are both located in leaf nodes.
      !>
      !>     After the deletion, the successor of location from which the
      !>     deletion actually was done is shifted to the next-lower-indexed
      !>     position.  The routine updates all affected key counts and key
      !>     values, both in the target node and all ancestors of the target.
      !>     Here the target node is the leaf from which the deletion was
      !>     actually done.
      !>
      !>     The caller must balance the tree when underflow occurs.
      !>
      !>     Deletion is not quite the opposite of insertion.  Note that the
      !>     output TRGKEY has no analog in the unbalanced insertion routine
      !>     ZZEKTRUI.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRDL.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 09-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN to insert DAS file name into
      !>        long error messages.
      !>
      !>-    Beta Version 1.1.0, 18-JUN-1999 (WLT)
      !>
      !>        Fixed calls to CHKIN and CHKOUT so that the same name
      !>        is used throughout the routine.
      !>
      !>-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
      module subroutine zzektrud ( ctx, handle, tree, key, trgkey, undrfl )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: key  !! Key to delete.
      integer(ip), intent(out) :: trgkey  !! Key identifying node from which deletion occurred.
      logical, intent(out) :: undrfl  !! Underflow flag.
!
!     Local variables
!
      integer(ip) :: datptr
      integer(ip) :: depth
      integer(ip) :: i
      integer(ip) :: keyidx
      integer(ip) :: leaf
      integer(ip) :: level
      integer(ip) :: loffst
      integer(ip) :: lpage  ( pgsizi )
      integer(ip) :: lpidx
      integer(ip) :: lpidx2
      integer(ip) :: lpkey
      integer(ip) :: lpkey2
      integer(ip) :: lsib
      integer(ip) :: lsib2
      integer(ip) :: nkeys
      integer(ip) :: nlkeys
      integer(ip) :: nnode
      integer(ip) :: parent
      integer(ip) :: paren2
      integer(ip) :: pkey
      integer(ip) :: pkey2
      integer(ip) :: poffs2
      integer(ip) :: poffst
      integer(ip) :: prev
      integer(ip) :: root
      integer(ip) :: rpage  ( pgsizi )
      integer(ip) :: rpidx
      integer(ip) :: rpidx2
      integer(ip) :: rpkey
      integer(ip) :: rpkey2
      integer(ip) :: rsib
      integer(ip) :: rsib2
      integer(ip) :: target
      integer(ip) :: tnkeys
      integer(ip) :: toffst
      integer(ip) :: totkey
      integer(ip) :: tpage  ( pgsizi )

!
!     Set the variable ROOT, so we'll have something mnemonic to go
!     by when referring to the root node.
!
      root  =  tree

!
!     We always need to update the root page, so read it now.
!
      call zzekpgri ( ctx, handle, root, rpage )

!
!     The allowed range of keys is 1 to TOTKEY, where TOTKEY is the
!     total number of keys already present.
!
      totkey  =  rpage ( trnkey )

      if (  ( key < 1 ) .or. ( key > totkey)  ) then

         call ctx%setmsg ( 'Key = #. Valid range is 1:#.  File = #.' )
         call ctx%errint ( '#',     key                              )
         call ctx%errint ( '#',     totkey                           )
         call ctx%errhan ( '#',     handle                           )
         return

      end if

!
!     Get the number of nodes in the tree.  Also save the tree's depth.
!
      nnode  =  rpage ( trnnod )
      depth  =  rpage ( trdpth )

!
!     Find the point at which the deletion is to occur.  When the
!     tree contains only one node, no search is necessary.
!
      if ( nnode == 1 ) then
!
!        This is the simplest case; all we need do is delete the
!        key from the root node.
!
!        Set:
!
!           - The number of keys in the tree
!           - The number of keys in the root
!           - The last key
!           - The data pointer for the last key
!           - The child pointer following the last key
!
!        In the root node, relative keys coincide with absolute keys,
!        so the key value need not be adjusted.
!
         nkeys             =  totkey
         rpage ( trnkey )  =  nkeys  -  1
         rpage ( trnkr  )  =  nkeys  -  1

!
!        Shift the keys, data pointer, and child pointers to the left
!        of the deleted key.  Update the shifted keys.
!
         do i = key, nkeys-1
            rpage ( trkeyr+i )  =  rpage ( trkeyr+i+1 ) - 1
            rpage ( trdatr+i )  =  rpage ( trdatr+i+1 )
         end do

         do i = key, nkeys
            rpage ( trkidr+i )  =  rpage ( trkidr+i+1 )
         end do

!
!        Zero out the freed entries.
!
         rpage ( trkeyr + nkeys   )  =  0
         rpage ( trdatr + nkeys   )  =  0
         rpage ( trkidr + nkeys+1 )  =  0

!
!        Update the key count.
!
         nkeys   =  nkeys - 1

!
!        Underflow never occurs in the root; the tree simply becomes
!        empty if no keys are left.
!
         undrfl  =  .false.

!
!        The first key in the root will serve as the target key,
!        as long as the root isn't empty.
!
         if ( nkeys > 0 ) then
            trgkey  =  rpage(trkeyr+1)
         else
            trgkey  =  0
         end if

!
!        Write the page back out, and we're all set.
!
         call zzekpgwi ( ctx, handle, root, rpage )

      else if ( key == totkey ) then
!
!        The deleted key is the last key in the tree.  This case
!        is simple, because no remaining keys change as a result of
!        this deletion.
!
         call zzektrlk ( ctx, handle,  tree,    key,    keyidx, &
                         target,  toffst,  level,  datptr  )

         if ( ctx%failed() ) return

         call zzekpgri ( ctx, handle, target, tpage )

         nkeys   =  tpage ( trnkc )

!
!        Zero out the freed entries.
!
         tpage ( trkeyc + nkeys   )  =  0
         tpage ( trdatc + nkeys   )  =  0
         tpage ( trkidc + nkeys+1 )  =  0

!
!        Update the key count for this node:
!
         tpage ( trnkc  )  =  tpage ( trnkc ) - 1

!
!        Since the key we deleted has no successors, there's no need
!        to adjust any other keys.  We must decrement the total
!        node count in the root, however.
!
         rpage ( trnkey ) =  totkey - 1

!
!        Underflow occurs when the node started out at the minimum
!        key count.
!
         undrfl  =  nkeys == mnkeyc

!
!        The first key in the target page is the target key.  Return
!        an absolute key.
!
         trgkey  =  tpage(trkeyc+1)  +  toffst

!
!        Write the affected pages back out.
!
         call zzekpgwi ( ctx, handle, root,   rpage )
         call zzekpgwi ( ctx, handle, target, tpage )

      else
!
!        Locate the item we wish to delete.
!
         call zzektrlk ( ctx, handle,  tree,    key,    keyidx, &
                         target,  toffst,  level,  datptr )

         if ( level == depth ) then
!
!           The node containing KEY is a leaf node, which is what we
!           want.  Deletions always take place at leaf nodes.
!
!           Since we'll have to update the ancestors of TARGET,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the deletion
!           we're going to do will temporarily screw up our
!           addressing method, we want to do this look-up while
!           we're sure it will work.
!
            call zzektrpi ( ctx, handle, tree,  key,  parent, pkey,  poffst, &
                            lpidx,  lpkey, lsib, rpidx,  rpkey, rsib   )

            if ( ctx%failed() ) return

!
!           Read the target page.  Get the key count for this node.
!
            call zzekpgri ( ctx, handle, target, tpage )

            tnkeys = tpage ( trnkc )

!
!           Each node is allowed to underflow by 1 element.  If there
!           is already a deficit, OK, that's it.
!
            if ( tnkeys < mnkeyc ) then

               call ctx%setmsg ( 'Node = #. Tree = #. File = #. Key ' // &
                             'count = #; max allowed, including ' // &
                             'overflow, is #.'                     )
               call ctx%errint ( '#',   target                         )
               call ctx%errint ( '#',   tree                           )
               call ctx%errhan ( '#',   handle                         )
               call ctx%errint ( '#',   tnkeys                         )
               call ctx%errint ( '#',   mxkeyc + 1                     )
               call ctx%sigerr ( 'SPICE(BUG)'                          )
               return

            end if

!
!           Shift the keys, data pointers, and child pointers starting
!           at KEY to the left by 1 position.  Careful, move the
!           leftmost elements first.  Update the shifted key values
!           while we're at it.
!
            do i = keyidx, tnkeys-1
               tpage (trkeyc+i)  =  tpage (trkeyc+i+1) - 1
            end do

            do i = keyidx, tnkeys-1
               tpage (trdatc+i)  =  tpage (trdatc+i+1)
            end do

            do i = keyidx, tnkeys
               tpage (trkidc+i)  =  tpage (trkidc+i+1)
            end do

!
!           Update the key count for the target node.
!
            tpage ( trnkc ) =  tnkeys - 1

!
!           Underflow occurs when the node started out at the minimum
!           count.
!
            undrfl  =  tnkeys == mnkeyc

!
!           The first key in the target page is the target key.
!
            trgkey  =  tpage(trkeyc+1)  +  toffst

!
!           Write the target page back out.
!
            call zzekpgwi ( ctx, handle, target, tpage )

         else
!
!           The node containing KEY is not a leaf node.  Therefore,
!           KEY > 1 and KEY has a predecessor.  This predecessor
!           is guaranteed to reside in a leaf node.  This is simply
!           a property of B*-trees, of which EK trees are a subclass.
!           Find this predecessor.
!
            call zzektrlk ( ctx, handle,  tree,    key-1,  prev, &
                            leaf,    loffst,  level,  datptr  )

            if ( ctx%failed() ) return

!
!           Since we'll have to update the ancestors of LEAF,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the deletion
!           we're going to do will temporarily screw up our
!           addressing method, we want to do this look-up while
!           we're sure it will work.
!
            call zzektrpi ( ctx, handle, tree,  key-1, parent, pkey,  poffst, &
                            lpidx,  lpkey, lsib,  rpidx,  rpkey, rsib  )

            if ( ctx%failed() ) return

!
!           Since deletions are allowed only in leaf nodes, we'll
!           perform a little sleight-of-code:  We'll move the key's
!           predecessor into the key's location, then remove the
!           predecessor from its leaf node.  The order of the keys
!           is not disturbed by this re-arrangement.
!
!           Moving the key's predecessor into the key's location is
!           accomplished simply by transferring the data pointer.
!
            call zzekpgri ( ctx, handle, leaf, lpage )

            if ( target == root ) then
!
!              The root page has already been read into RPAGE.
!
               rpage (trdatr+keyidx)  =  lpage (trdatc+prev)

            else

               call zzekpgri ( ctx, handle, target, tpage )

               tpage (trdatc+keyidx)  =  lpage (trdatc+prev)

            end if

!
!           The keys and data pointers in the leaf must be shifted
!           left to account for the deletion.  We'll zero out the
!           freed elements.  All child pointers are NIL and hence need
!           not be shifted.
!
            nlkeys = lpage ( trnkc )

            do i = prev, nlkeys-1
               lpage (trkeyc+i)  =  lpage (trkeyc+i+1) - 1
               lpage (trdatc+i)  =  lpage (trdatc+i+1)
            end do

!
!           Update the key count for the leaf node.
!
            lpage ( trnkc ) =  nlkeys - 1

!
!           Underflow occurs when the leaf node started out at the
!           minimum count.
!
            undrfl  =  nlkeys == mnkeyc

!
!           The first key in the leaf page is the target key.
!
            trgkey  =  lpage(trkeyc+1)  +  loffst

!
!           Write the leaf, and if necessary, the target page back out.
!
            call zzekpgwi ( ctx, handle, leaf, lpage )

            if ( target /= root ) then
               call zzekpgwi ( ctx, handle, target, tpage )
            end if

!
!           The next step will be to update the ancestors of LEAF.
!           For the purposes of this operation, LEAF is the target
!           node.
!
            target  =  leaf

         end if

!
!        We must update the affected keys in every ancestor of TARGET.
!        We've already looked up information for the parent of
!        TARGET.  See the note at the prior call to ZZEKTRPI.
!
         do while ( parent /= root )
!
!           Before going to work on the parent, get *its* parent's info.
!           This is the last chance to do so.
!
            call zzektrpi ( ctx, handle, tree,   pkey,   paren2, &
                            pkey2,  poffs2, lpidx2, lpkey2, &
                            lsib2,  rpidx2, rpkey2, rsib2  )

!
!           Read the parent node.  All keys from the right parent key
!           onward get decremented.  Remember that there may be no
!           right parent key.
!
            call zzekpgri ( ctx, handle, parent, tpage )

            tnkeys  =  tpage ( trnkc  )

            if ( rpidx > 0 ) then

               do i = rpidx, tnkeys
                  tpage(trkeyc + i)  =  tpage(trkeyc + i) - 1
               end do

!
!              Write the updated page back out.
!
               call zzekpgwi ( ctx, handle, parent, tpage )

            end if

            parent = paren2
            pkey   = pkey2
            rpidx  = rpidx2

         end do

!
!        Update the keys in the root.  Recall that the root page has
!        already been read into RPAGE.
!
         tnkeys  =  rpage ( trnkr )

         if ( rpidx > 0 ) then

            do i = rpidx, tnkeys
               rpage(trkeyr + i)  =  rpage(trkeyr + i) - 1
            end do

         end if

!
!        Update the total key count for the tree.
!
         rpage ( trnkey )  =  totkey - 1

!
!        Write the updated root page back out.
!
         call zzekpgwi ( ctx, handle, root, rpage )

      end if

      return
      end subroutine zzektrud

      !> Insert a value into a tree at a specified location without
      !> balancing the tree.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     KEY            is an absolute key indicating the insertion
      !>                    location.  In EK trees, absolute keys are just
      !>                    ordinal positions relative to the leftmost element
      !>                    of the tree, with the leftmost element having
      !>                    position 1.  So setting KEY to 10, for example,
      !>                    indicates that the input VALUE is the 10th item in
      !>                    the tree.
      !>
      !>                    KEY must be in the range 1 : (NKEYS+1), where
      !>                    NKEYS is the number of keys in the tree prior to
      !>                    the insertion.
      !>
      !>     VALUE          is an integer value to be inserted into the
      !>                    specified tree at the ordinal position KEY.
      !>
      !>## Detailed Output
      !>
      !>     OVERFL         is a logical flag indicating whether the node
      !>                    at which VALUE was inserted overflowed as a result.
      !>                    Nodes contain extra space to temporarily
      !>                    accommodate an overflow of one value.
      !>
      !>                    When an overflow condition exists, the tree
      !>                    violates an invariant.  The overflow must be
      !>                    resolved before any other insertions or deletions
      !>                    are performed.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the input key is out of range, the error
      !>         SPICE(INVALIDINDEX) is signaled.
      !>
      !>     4)  If the attempted insertion causes overflow in the target node
      !>         by more than 1 key, the error SPICE(NODETOOFULL) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine inserts a new value into an EK tree at the ordinal
      !>     position indicated by KEY.  The insertion is always done in a
      !>     leaf node.  This is possible because every key is either in a
      !>     leaf or has the property that its predecessor and successor are
      !>     both located in leaf nodes.
      !>
      !>     If the inserted value is not appended to the tree, the value
      !>     previously at location KEY is shifted to the next-higher-indexed
      !>     position.  The routine updates all affected key counts and key
      !>     values, both in the target node and all ancestors of the target.
      !>
      !>     The caller must balance the tree when overflow occurs.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRIN.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 09-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN to insert DAS file name into
      !>        long error messages.
      !>
      !>-    Beta Version 1.1.0, 18-JUN-1999 (WLT)
      !>
      !>        Removed redundant calls to CHKIN.
      !>
      !>-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
      module subroutine zzektrui ( ctx, handle, tree, key, value, overfl )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: key  !! Key to insert.
      integer(ip), intent(in) :: value  !! Value to insert.
      logical, intent(out) :: overfl  !! Overflow flag.
!
!     Local variables
!
      integer(ip) :: datloc
      integer(ip) :: datptr
      integer(ip) :: depth
      integer(ip) :: i
      integer(ip) :: idx
      integer(ip) :: keyloc
      integer(ip) :: kidloc
      integer(ip) :: level
      integer(ip) :: lpidx
      integer(ip) :: lpidx2
      integer(ip) :: lpkey
      integer(ip) :: lpkey2
      integer(ip) :: lsib
      integer(ip) :: lsib2
      integer(ip) :: next
      integer(ip) :: nkeys
      integer(ip) :: nnode
      integer(ip) :: parent
      integer(ip) :: paren2
      integer(ip) :: pkey
      integer(ip) :: pkey2
      integer(ip) :: poffs2
      integer(ip) :: poffst
      integer(ip) :: prev
      integer(ip) :: root
      integer(ip) :: rpage  ( pgsizi )
      integer(ip) :: rpidx
      integer(ip) :: rpidx2
      integer(ip) :: rpkey
      integer(ip) :: rpkey2
      integer(ip) :: rsib
      integer(ip) :: rsib2
      integer(ip) :: target
      integer(ip) :: tnkeys
      integer(ip) :: toffst
      integer(ip) :: totkey
      integer(ip) :: tpage  ( pgsizi )

!
!     Set the variable ROOT, so we'll have something mnemonic to go
!     by when referring to the root node.
!
      root  =  tree

!
!     We always need to update the root page, so read it now.
!
      call zzekpgri ( ctx, handle, root, rpage )

!
!     The allowed range of keys is 1 to (TOTKEY+1), where TOTKEY is the
!     total number of keys already present.
!
      totkey  =  rpage ( trnkey )

      if (  ( key < 1 ) .or. ( key > totkey+1 )  ) then

         call ctx%setmsg ( 'Key = #. Valid range is 1:#.  File = #.' )
         call ctx%errint ( '#',     key                              )
         call ctx%errint ( '#',     totkey+1                         )
         call ctx%errhan ( '#',     handle                           )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                     )
         return

      end if

!
!     Get the number of nodes in the tree.  Also save the tree's depth.
!
      nnode  =  rpage ( trnnod )
      depth  =  rpage ( trdpth )

!
!     Find the point at which the insertion is to occur.  When the
!     tree contains only one node, no search is necessary.
!
      if ( nnode == 1 ) then
!
!        This is the simplest case; all we need do is set up the
!        key in the root node.
!
!        Set:
!
!           - The number of keys in the tree
!           - The number of keys in the root
!           - The last key
!           - The data value for the last key
!           - The child pointer following the last key
!
!        In the root node, relative keys coincide with absolute keys,
!        so the key value need not be adjusted.
!
         nkeys             =  totkey
         rpage ( trnkey )  =  nkeys  +  1
         rpage ( trnkr  )  =  nkeys  +  1

!
!        Shift the keys, data value, and child pointers to the right
!        of the new key.  Update the shifted keys.
!
         do i = nkeys, key, - 1
            rpage ( trkeyr+i+1 ) = rpage ( trkeyr + i ) + 1
            rpage ( trdatr+i+1 ) = rpage ( trdatr + i )
         end do

         do i = nkeys+1, key, - 1
            rpage ( trkidr+i+1 ) = rpage ( trkidr + i )
         end do

         rpage ( trkeyr + key )  =  key
         rpage ( trdatr + key )  =  value
         rpage ( trkidr + key )  =  0

!
!        Update the key count.
!
         nkeys   =  nkeys + 1

!
!        The node into which the key was inserted was the root.
!
         target  =  root

!
!        Overflow occurs when the root started out full.
!
         overfl  =  nkeys == mxkeyr+1

!
!        Write the page back out, and we're all set.
!
         call zzekpgwi ( ctx, handle, root, rpage )

      else if ( key == totkey+1 ) then
!
!        The new key will be the last key in the tree.  This case
!        is simple:  the key goes in the last node of the tree.
!        Since every child node contains more than one key, we can
!        find the node by looking up the last key already present.
!
         call zzektrlk ( ctx, handle,  tree,    key-1,  idx, &
                         target,  toffst,  level,  datptr  )

         if ( ctx%failed() ) return

         call zzekpgri ( ctx, handle, target, tpage )

         nkeys   =  tpage ( trnkc )

         keyloc  =  trkeyc  +  nkeys  +  1
         datloc  =  trdatc  +  nkeys  +  1
         kidloc  =  trkidc  +  nkeys  +  1

!
!        The last node in the tree is always at the lowest level,
!        so the relative value of the new key can be computed from
!        that of its predecessor.
!
         tpage ( keyloc )  =  tpage ( keyloc-1 )  +  1
         tpage ( datloc )  =  value
         tpage ( kidloc )  =  0

!
!        Update the key count for this node:
!
         tpage ( trnkc  )  =  tpage ( trnkc ) + 1

!
!        Since the key we inserted has no successors, there's no need
!        to adjust any other keys.  We must increment the total
!        node count in the root, however.
!
         rpage ( trnkey ) =  totkey + 1

!
!        Overflow occurs when the node started out full.
!
         overfl  =  nkeys == mxkeyc

!
!        Write the affected pages back out.
!
         call zzekpgwi ( ctx, handle, root,   rpage )
         call zzekpgwi ( ctx, handle, target, tpage )

      else
!
!        The item we wish to insert will displace the item whose
!        ordinal position is KEY.  Locate this target item.
!
         call zzektrlk ( ctx, handle,  tree,    key,    next, &
                         target,  toffst,  level,  datptr  )

         if ( level == depth ) then
!
!           The node containing KEY is a leaf node, which is what we
!           want.  Insertions always take place at leaf nodes.
!
!           Since we'll have to update the ancestors of TARGET,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the insertion
!           we're going to do will temporarily screw up our
!           addressing method, we want to do this look-up while
!           we're sure it will work.
!
            call zzektrpi ( ctx, handle, tree,  key,  parent, pkey,  poffst, &
                            lpidx,  lpkey, lsib, rpidx,  rpkey, rsib   )

            if ( ctx%failed() ) return

!
!           Read the target page.  Get the key count for this node.
!
            call zzekpgri ( ctx, handle, target, tpage )

            tnkeys = tpage ( trnkc )

!
!           Each node is allowed to overflow by 1 element.  If there's
!           no more room, OK, that's it.
!
            if ( tnkeys > mxkeyc ) then

               call ctx%setmsg ( 'Node = #. Tree = #. File = #. Key ' // &
                             'count = #; max allowed, including ' // &
                             'overflow, is #.'                     )
               call ctx%errint ( '#',   target                         )
               call ctx%errint ( '#',   tree                           )
               call ctx%errhan ( '#',   handle                         )
               call ctx%errint ( '#',   tnkeys                         )
               call ctx%errint ( '#',   mxkeyc + 1                     )
               call ctx%sigerr ( 'SPICE(NODETOOFULL)'                  )
               return

            end if

!
!           Shift the keys, data values, and child pointers starting
!           at NEXT over to the right by 1 position.  Careful, move the
!           rightmost elements first.  Update the shifted key values
!           while we're at it.
!
            do i = tnkeys, next, -1
               tpage (trkeyc + i + 1)  =  tpage (trkeyc + i)  +  1
            end do

            do i = tnkeys, next, -1
               tpage (trdatc + i + 1)  =  tpage (trdatc + i)
            end do

            do i = tnkeys+1, next, -1
               tpage (trkidc + i + 1)  =  tpage (trkidc + i)
            end do

!
!           The new key simply takes the value of the old one.  The
!           corresponding data value must be set, however.
!
            tpage ( trdatc + next ) = value

         else
!
!           The node containing KEY is not a leaf node.  Therefore,
!           KEY > 1 and KEY has a predecessor.  This predecessor
!           is guaranteed to reside in a leaf node.  This is simply
!           a property of B*-trees, of which EK trees are a subclass.
!
            call zzektrlk ( ctx, handle,  tree,    key-1,  prev, &
                            target,  toffst,  level,  datptr  )

            if ( ctx%failed() ) return

!
!           Since we'll have to update the ancestors of TARGET,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the insertion
!           we're going to do will temporarily screw up our
!           addressing method, we want to do this look-up while
!           we're sure it will work.
!
            call zzektrpi ( ctx, handle, tree,  key-1, parent, pkey,  poffst, &
                            lpidx,  lpkey, lsib,  rpidx,  rpkey, rsib  )

            if ( ctx%failed() ) return

!
!           The predecessor of KEY will be the last key present in the
!           node TARGET.  Make sure there's room in the node.
!
            call zzekpgri ( ctx, handle, target, tpage )

            tnkeys = tpage ( trnkc )

            if ( tnkeys > mxkeyc + 1 ) then

               call ctx%setmsg ( 'Node = #. Tree = #. File = #. Key ' // &
                             'count = #; max allowed, including ' // &
                             'overflow, is #.'                     )
               call ctx%errint ( '#',   target                         )
               call ctx%errint ( '#',   tree                           )
               call ctx%errhan ( '#',   handle                         )
               call ctx%errint ( '#',   tnkeys                         )
               call ctx%errint ( '#',   mxkeyc + 1                     )
               call ctx%sigerr ( 'SPICE(NODETOOFULL)'                  )
               return

            end if

!
!           Set the new key and the corresponding data and child
!           pointers.
!
            tpage ( trkeyc + prev + 1 )   =   prev + 1
            tpage ( trdatc + prev + 1   ) =   value
            tpage ( trkidc + prev + 2   ) =   0

         end if

!
!        Update the key count for the target node.
!
         tpage ( trnkc ) =  tnkeys + 1

!
!        Overflow occurs when the node started out full.
!
         overfl  =  tnkeys == mxkeyc

!
!        Write the target page back out.
!
         call zzekpgwi ( ctx, handle, target, tpage )

!
!        We must update the affected keys in every ancestor of TARGET.
!        We've already looked up information for the parent of
!        TARGET.  See the note at the prior call to ZZEKTRPI.
!
         do while ( parent /= root )
!
!           Before going to work on the parent, get *its* parent's info.
!           This is the last chance to do so.
!
            call zzektrpi ( ctx, handle, tree,   pkey,   paren2, &
                            pkey2,  poffs2, lpidx2, lpkey2, &
                            lsib2,  rpidx2, rpkey2, rsib2  )

!
!           Read the parent node.  All keys from the right parent key
!           onward get incremented.  Remember that there may be no
!           right parent key.
!
            call zzekpgri ( ctx, handle, parent, tpage )

            tnkeys  =  tpage ( trnkc  )

            if ( rpidx > 0 ) then

               do i = rpidx, tnkeys
                  tpage(trkeyc + i)  =  tpage(trkeyc + i) + 1
               end do

!
!              Write the updated page back out.
!
               call zzekpgwi ( ctx, handle, parent, tpage )

            end if

            parent = paren2
            pkey   = pkey2
            rpidx  = rpidx2

         end do

!
!        Update the keys in the root.  Recall that the root page has
!        already been read into RPAGE.
!
         tnkeys  =  rpage ( trnkr )

         if ( rpidx > 0 ) then

            do i = rpidx, tnkeys
               rpage(trkeyr + i)  =  rpage(trkeyr + i) + 1
            end do

         end if

!
!        Update the total key count for the tree.
!
         rpage ( trnkey )  =  totkey + 1

!
!        Write the updated root page back out.
!
         call zzekpgwi ( ctx, handle, root, rpage )

      end if

      return
      end subroutine zzektrui

      !> Return the number of segments in a specified EK.
      !>
      !> The function returns the number of segments in the specified
      !> E-kernel.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of an EK file opened for read access.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of segments in the EK identified
      !>     by HANDLE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, an error is signaled by a routine in the
      !>         call tree of this routine. EKNSEG will return the value zero.
      !>
      !>     2)  If an I/O error occurs while trying to read the EK, the error
      !>         is signaled by a routine in the call tree of this routine.
      !>         EKNSEG will return the value zero.
      !>
      !>## Files
      !>
      !>     See the description of HANDLE in $Detailed_Input.
      !>
      !>## Particulars
      !>
      !>     This routine is used to support the function of summarizing an
      !>     EK file. Given the number of segments in the file, a program
      !>     can use EKSSUM in a loop to summarize each of them.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Find the number of segments on an EK.
      !>
      !>        Use the EK kernel below as test input file for loading the
      !>        experiment database. This kernel contains the Deep
      !>        Impact spacecraft sequence data based on the integrated
      !>        Predicted Events File covering the whole primary mission.
      !>
      !>           dif_seq_050112_050729.bes
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EKNSEG_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      INTEGER               EKNSEG
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      INTEGER               HANDLE
      !>      INTEGER               NSEG
      !>
      !>C
      !>C     Open the EK file, returning the file handle
      !>C     associated with the open file to the variable named
      !>C     HANDLE.
      !>C
      !>      CALL EKOPR ( 'dif_seq_050112_050729.bes', HANDLE )
      !>
      !>C
      !>C     Return the number of segments in the EK.
      !>C
      !>      NSEG = EKNSEG( HANDLE )
      !>      WRITE(*,*) 'Number of segments = ', NSEG
      !>
      !>C
      !>C     Close the file.
      !>C
      !>      CALL EKCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Number of segments =            2
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
      !>-    SPICELIB Version 1.1.0, 25-MAY-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete code example.
      !>
      !>-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
      module integer(ip) function eknseg ( ctx, handle )
      use pepper_kinds, only: ip
      use pepper_inc_ekfilpar
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! EK file handle.
      integer(ip) :: tree_1(1)  !! one-element copy of tree (array argument)

      integer(ip) :: base
      integer(ip) :: tree

!
!     Set a default value for EKNSEG.
!
      eknseg  =  0

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Make sure this is a paged DAS EK.
!
      call zzekpgch ( ctx, handle, 'READ' )

      if ( ctx%failed() ) return

!
!     Obtain the base address of the first integer page.
!
      base = zzektrbs ( ctx, 1 )

!
!     Look up the head node of the segment tree.
!
      call dasrdi ( ctx, handle, base+sgtidx, base+sgtidx, tree_1 )
      tree = tree_1(1)

!
!     Get the entry count for the segment tree.
!
      eknseg  =  zzektrsz ( ctx, handle, tree )

      return
      end function eknseg

      !> Look up a specified record pointer from an EK index.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is an EK file handle.  The file may be open for
      !>                    reading or writing.
      !>
      !>     COLDSC         is the column descriptor of the column to which
      !>                    the index of interest belongs.
      !>
      !>     KEY            is the key of the record pointer of interest.  This
      !>                    key is the ordinal position of the record pointer
      !>                    in the index.
      !>
      !>## Detailed Output
      !>
      !>     RECPTR         is the record pointer corresponding to the input
      !>                    key.  This pointer gives the base address of
      !>                    the record pointer structure for the record having
      !>                    ordinal position KEY within the specified column,
      !>                    where the order is defined by the column's index.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If KEY is out of range, the error will be diagnosed by
      !>         routines called by this routine.
      !>
      !>     3)  If an I/O error occurs while reading or the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine finds the record pointer for a record having a
      !>     specified ordinal position in a column, where the order is
      !>     defined by the column's index.
      !>
      !>## Examples
      !>
      !>     See EKSRCH.
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
      !>-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
      module subroutine zzekixlk ( ctx, handle, coldsc, key, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip), intent(in) :: key  !! Key.
      integer(ip), intent(out) :: recptr  !! Record pointer.
      integer(ip) :: recptr_1(1)  !! one-element copy of recptr (array argument)
!
!     Local variables
!
      integer(ip) :: addrss
      integer(ip) :: base
      integer(ip) :: itype
      integer(ip) :: q
      integer(ip) :: r
      integer(ip) :: tree

      itype  =  coldsc ( ixtidx )

      if ( itype == 1 ) then
!
!        For type 1 indexes, the index pointer is the root node of
!        a B*-tree.  Just use the tree look up routine.
!
         tree  = coldsc ( ixpidx )

         call zzektrdp ( ctx, handle, tree, key, recptr )

      else if ( itype == 2 ) then
!
!        For type 2 indexes, the index pointer is the base address
!        of the index.  We must compute the offset from this base to
!        the index element having ordinal position KEY.
!
         base    =    coldsc(ixpidx)
         q       =    (key-1)  / ipsize
         r       =    key  - q * ipsize
         addrss  =    base + q * pgsizi + r

         call dasrdi ( ctx, handle, addrss, addrss, recptr_1 )
         recptr = recptr_1(1)

      else
!
!        Sorry, no other types of indexes are supported.
!
         call ctx%setmsg ( 'The index type # is not supported.' )
         call ctx%errint ( '#',   itype                         )
         call ctx%sigerr ( 'SPICE(INVALIDTYPE)'                 )
         return

      end if

      end subroutine zzekixlk

      !> Look up the record pointer of an EK record having a specified
      !> ordinal position in a specified EK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is an EK file handle.  The file may be open for
      !>                    reading or writing.
      !>
      !>     SEGDSC         is the descriptor of the segment to which the
      !>                    record of interest belongs.
      !>
      !>     N              is the ordinal position of the record in the
      !>                    segment.
      !>
      !>## Detailed Output
      !>
      !>     RECPTR         is the record pointer corresponding to the input
      !>                    key.  This pointer identifies the record of
      !>                    interest.  The interpretation of RECPTR depends
      !>                    on the type of segment to which the record belongs.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If N is out of range, the error will be diagnosed by
      !>         routines called by this routine.
      !>
      !>     3)  If an I/O error occurs while reading or the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine finds the record pointer for a record having a
      !>     specified ordinal position in a segment.
      !>
      !>## Examples
      !>
      !>     See EKSRCH.
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
      !>-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
      module subroutine zzekrplk ( ctx, handle, segdsc, n, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      use pepper_inc_ekpage
      use pepper_inc_eksegdsc
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip), intent(in) :: n  !! Ordinal position of record.
      integer(ip), intent(out) :: recptr  !! Record pointer.
!
!     Local variables
!
      integer(ip) :: stype
      integer(ip) :: tree

      stype  =  segdsc ( ektidx )

      if ( stype == 1 ) then
!
!        For type 1 segments, the record pointer is obtained from
!        the record tree.
!
         tree  = segdsc ( rtidx )

         call zzektrdp ( ctx, handle, tree, n, recptr )

      else if ( stype == 2 ) then
!
!        For type 2 segments, the record pointer *is* the ordinal
!        position of the record.
!
         recptr = n

      else
!
!        Sorry, no other types of segments are supported.
!
         call ctx%setmsg ( 'The segment type # is not supported.' )
         call ctx%errint ( '#',   stype                           )
         call ctx%sigerr ( 'SPICE(INVALIDTYPE)'                   )
         return

      end if

      end subroutine zzekrplk

      !> One-shot tree load:  insert an entire array into an empty
      !> tree.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>                    The tree must be empty.
      !>
      !>     SIZE           is the size of the tree to create:  SIZE is the
      !>                    number of values that will be inserted into the
      !>                    tree.
      !>
      !>     VALUES         is an array of integer values to be inserted into
      !>                    the tree.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the input tree is not empty, the error SPICE(NONEMPTYTREE)
      !>         is signaled.
      !>
      !>     4)  If the depth of the tree needed to hold the number of values
      !>         indicated by SIZE exceeds the maximum depth limit, the error
      !>         SPICE(COUNTTOOLARGE) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine creates an EK tree and loads the tree with the
      !>     integer values supplied in the array VALUES.  The ordinal
      !>     positions of the values in the tree correspond to the positions
      !>     of the values in the input array:  for example, the 10th element
      !>     of the array is pointed to by the key 10.
      !>
      !>     This routine loads a tree much faster than can be done by
      !>     sequentially loading the set of values by successive calls to
      !>     ZZEKTRIN.  On the other hand, the caller must declare an array
      !>     large enough to hold all of the values to be loaded.  Note that
      !>     a partially full tree cannot be extended using this routine.
      !>
      !>## Examples
      !>
      !>     See EKFFLD.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 05-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN
      !>
      !>        Cleaned up use of unnecessary variables and unneeded
      !>        declarations.
      !>
      !>-    Beta Version 1.1.0, 18-JUN-1999 (WLT)
      !>
      !>        Removed redundant calls to CHKIN
      !>
      !>-    Beta Version 1.0.0, 22-OCT-1995 (NJB)
      module subroutine zzektr1s ( ctx, handle, tree, size, values )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      use pepper_inc_ektype
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: size  !! Size of tree.
      integer(ip), intent(in) :: values  ( * )  !! Values to insert.

      integer(ip) :: base
      integer(ip) :: basidx
      integer(ip) :: bigsiz
      integer(ip) :: child
      integer(ip) :: d
      integer(ip) :: div
      integer(ip) :: i
      integer(ip) :: key
      integer(ip) :: kidbas
      integer(ip) :: level
      integer(ip) :: maxsiz
      integer(ip) :: n
      integer(ip) :: nbig
      integer(ip) :: next
      integer(ip) :: nkeys
      integer(ip) :: nkids
      integer(ip) :: nnodes
      integer(ip) :: node
      integer(ip) :: npred
      integer(ip) :: nsmall
      integer(ip) :: page  ( pgsizi )
      integer(ip) :: q
      integer(ip) :: reqsiz
      integer(ip) :: s
      integer(ip) :: stlsiz ( trmxdp )
      integer(ip) :: stnbig ( trmxdp )
      integer(ip) :: stnext ( trmxdp )
      integer(ip) :: stnkey ( trmxdp )
      integer(ip) :: stnode ( trmxdp )
      integer(ip) :: stnbas ( trmxdp )
      integer(ip) :: subd
      integer(ip) :: subsiz
      integer(ip) :: totnod
      integer(ip) :: tsize

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Make sure the input tree is empty.
!
      tsize  =  zzektrsz ( ctx, handle, tree )

      if ( tsize > 0 ) then

         call ctx%setmsg ( 'Tree has size #; should be empty.' // &
                       'EK = #; TREE = #.'                 )
         call ctx%errint ( '#',  tsize                         )
         call ctx%errhan ( '#',  handle                        )
         call ctx%errint ( '#',  tree                          )
         call ctx%sigerr ( 'SPICE(NONEMPTYTREE)'               )
         return

      end if

!
!     Compute the tree depth required.  The largest tree of a given
!     depth D contains the root node plus S(D) child nodes, where
!
!        S(1)  =  1
!
!     and if D is at least 2,
!                                    D - 2
!                                    ____
!                                    \              i
!        S(D)  =  MAX_SIZE      *    /      MAX_SIZE
!                         Root       ----           Child
!                                    i = 0
!
!                                    D - 2
!                                    ____
!                                    \            i
!              =  MXKIDR        *    /      MXKIDC
!                                    ----
!                                    i = 0
!
!                                         D-1
!                                   MXKIDC   -  1
!              =  MXKIDR        *   -------------
!                                    MXKIDC  - 1
!
!     If all of these nodes are full, the number of keys that
!     can be held in this tree is
!
!        MXKEYR  +  S(D) * MXKEYC
!
!     We want the minimum value of D such that this expression
!     is greater than or equal to SIZE.
!

      tsize  =  mxkeyr
      d      =  1
      s      =  1

      do while ( tsize < size )

         d  =  d  +  1

         if ( d == 2 ) then

            s  =  mxkeyr

         else
!
!           For computational purposes, the relationship
!
!              S(D+1)  =   MXKIDR  +  MXKIDC * S(D)
!
!           is handy.
!
            s  =  mxkidr  +  mxkidc * s

         end if

         tsize   =   mxkeyr  +  s * mxkeyc

      end do

!
!     If the tree must be deeper than we expected, we've a problem.
!
      if ( d > trmxdp ) then

         call ctx%setmsg ( 'Tree has depth #; max supported depth is #.' // &
                       'EK = #; TREE = #.'                           )
         call ctx%errint ( '#',  d                                       )
         call ctx%errint ( '#',  trmxdp                                  )
         call ctx%errhan ( '#',  handle                                  )
         call ctx%errint ( '#',  tree                                    )
         call ctx%sigerr ( 'SPICE(COUNTTOOLARGE)'                        )
         return

      end if

!
!     The basic error checks are done.  At this point, we can build the
!     tree.
!
!     The approach is to fill in the tree in a top-down fashion.
!     We decide how big each subtree of the root will be; this
!     information allows us to decide which keys actually belong
!     in the root.  Having filled in the root, we repeat the process
!     for each subtree of the root in left-to-right order.
!
!     We use a stack to keep track of the ancestors of the
!     node we're currently considering.  The table below shows the
!     items we save on the stack and the stack variables associated
!     with those items:
!
!        Item                                 Stack Variable
!        ----                                 ---------------
!        Node number                          STNODE
!
!        Number of keys in node               STNKEY
!
!        Larger subtree size                  STLSIZ
!
!        Number of large subtrees             STNBIG
!
!        Index of next subtree to visit       STNEXT
!
!        Base index of node                   STNBAS
!
      node              =  tree
      subsiz            =  size
      next              =  1
      level             =  1
      basidx            =  0

      do while ( level > 0 )
!
!        At this point, LEVEL, NEXT, NODE, SUBSIZ and BASIDX are set.
!

         if ( next == 1 ) then
!
!           This node has not been visited yet.  We'll fill in this
!           node before proceeding to fill in its descendants.  The
!           first step is to compute the number and sizes of the
!           subtrees of this node.
!
!           Decide the large subtree size and the number of subtrees of
!           this node.  The depth SUBD of the subtrees of this node is
!           D - LEVEL.  Each subtree has size bounded by the sizes of
!           the subtree of depth SUBD in which all nodes contain MNKEYC
!           keys and the by the subtree of depth SUBD in which all nodes
!           contain MXKEYC keys.  If this node is not the root and is
!           not a leaf node, the number of subtrees must be between
!           MNKIDC and MXKIDC.
!
            if ( level == 1 ) then
!
!              We're working on the root.  The number of subtrees is
!              anywhere between 0 and MXKIDR, inclusive.  We'll create
!              a tree with the minimum number of subtrees of the root.
!
               if ( d > 1 ) then
!
!                 We'll find the number of subtrees of maximum size
!                 that we would need to hold the non-root keys of the
!                 tree.  We'll then determine the actual required sizes
!                 of these subtrees.
!
                  subd    =  d - 1

                  nnodes  =  0

                  do i = 1, subd
                     nnodes =  mxkidc * nnodes  +  1
                  end do

                  maxsiz  =  nnodes * mxkeyc

!
!                 If we had NKIDS subtrees of size MAXSIZ, NKIDS
!                 would be the smallest integer such that
!
!                    ( NKIDS - 1 )  +  NKIDS * MAXSIZ  >  SUBSIZ
!                                                      -
!
!                 or equivalently,
!
!                     NKIDS * ( MAXSIZ + 1 )  >  SUBSIZ + 1
!                                             -
!
!                 We'll compute this value of NKIDS.
!
                  q       =  subsiz + 1
                  div     =  maxsiz + 1
                  nkids  =  ( q + div - 1 )  / div

!
!                 The minimum number of keys we must store in child
!                 nodes is the number of keys in the tree, minus those
!                 that can be accommodated in the root:
!
                  n       =   subsiz - ( nkids - 1 )

!
!                 Now we can figure out how large the subtrees would
!                 have to be in order to hold N keys, if all subtrees
!                 had the same size.
!
                  bigsiz  =  ( n + nkids - 1 ) / nkids

!
!                 We may have more capacity than we need if all subtrees
!                 have size BIGSIZ.  So, we'll allow some subtrees to
!                 have size BIGSIZ-1.  Not all subtrees can have the
!                 smaller size (otherwise BIGSIZ would have been
!                 smaller).  The first NBIG subtrees will have the
!                 larger size.
!
                  nsmall  =  ( nkids * bigsiz ) - n
                  nbig    =    nkids - nsmall

                  nkeys   =  nkids  -  1

               else
!
!                 All keys are in the root.
!
                  nkeys   =  size
                  nkids   =  0

               end if

!
!              Read in the root page.
!
               call zzekpgri ( ctx, handle, tree, page )

!
!              We have enough information to fill in the root node.
!              We'll allocate nodes for the immediate children.
!              There is one key `between' each child pointer.
!
               do i = 1, nkeys
!
!                 The Ith key may be found by considering the number
!                 of keys in the subtree between the Ith key and its
!                 predecessor in the root.
!
                  if ( i == 1 ) then
                     npred = 0
                  else
                     npred = page( trkeyr+i-1 )
                  end if

                  if ( d > 1 ) then
!
!                    The tree contains subtrees.
!
                     if ( i <= nbig ) then
                        key   =  npred  + bigsiz + 1
                     else
                        key   =  npred  + bigsiz
                     end if

                  else

                     key  =  i

                  end if

                  page ( trkeyr + i ) =  key
                  page ( trdatr + i ) =  values ( key )

               end do

               totnod  =  1

               do i = 1, nkids
!
!                 Allocate a node for the Ith child.  Store pointers
!                 to these nodes.
!
                  call zzekpgal ( ctx, handle, int, child, base )
                  page ( trkidr + i )  =  child
                  totnod               =  totnod + 1

               end do

!
!              Fill in the root's metadata.  There is one item that
!              we'll have to fill in when we're done:  the number of
!              nodes in the tree.  We know the rest of the information
!              now.
!
               page ( trnkey )  =  size
               page ( trdpth )  =  d
               page ( trkeyr )  =  nkeys
               page ( trnnod )  =  0

!
!              Write out the root.
!
               call zzekpgwi ( ctx, handle, tree, page )

            else if ( level < d ) then
!
!              The current node is a non-leaf child node.
!
               page = 0
!
!              The tree headed by this node has depth D-LEVEL+1 and
!              must hold SUBSIZ keys.  We must figure out the size
!              and number of subtrees of the current node.  Unlike in
!              the case of the root, we must have between MNKIDC
!              and MXKIDC subtrees of this node.  We start out by
!              computing the required subtree size if there were
!              exactly MNKIDC subtrees.  In this case, the total
!              number of keys in the subtrees would be
!
!                 SUBSIZ  -  MNKEYC
!
               n       =  subsiz - mnkeyc
               reqsiz  =  ( n + mnkeyc - 1 )  /  mnkeyc

!
!              Compute the maximum allowable number of keys in
!              a subtree.
!
               subd    =  d - level

               nnodes  =  0

               do i = 1, subd
                  nnodes =  mxkidc * nnodes  +  1
               end do

               maxsiz  =  nnodes * mxkeyc

!
!              If the number REQSIZ we came up with is a valid size,
!              we'll be able to get the correct number of children
!              by using subtrees of size REQSIZ and REQSIZ-1.  Note
!              that it's impossible for REQSIZ to be too small,
!              since the smallest possible number of subtrees is
!              MNKIDC.
!
               if ( reqsiz <= maxsiz ) then
!
!                 Decide how many large and small subtrees we need.
!
                  nkids   =  mnkidc
                  bigsiz  =  reqsiz
                  nsmall  =  bigsiz * nkids  -  n
                  nbig    =  nkids  -  nsmall

               else
!
!                 See how many subtrees of size MAXSIZ it would take
!                 to hold the requisite number of keys.  We know the
!                 number is more than MNKIDC.  If we have NKIDS
!                 subtrees of size MAXSIZ, the total number of
!                 keys in the subtree headed by NODE is
!
!                   ( NKIDS - 1 )  +  ( NKIDS * MAXSIZ )
!
!                 or
!
!                     NKIDS * ( MAXSIZ + 1 )   -   1
!
!                 We must find the smallest value of NKIDS such
!                 that the above quantity is greater than or equal
!                 to SUBSIZ.
!
                  q      =  subsiz + 1
                  div    =  maxsiz + 1
                  nkids  =  ( q + div - 1 )  / div

!
!                 We know that NKIDS subtrees of size MAXSIZ, plus
!                 NKIDS-1 keys in NODE, can hold at least SUBSIZ
!                 keys.  We now want to find the smallest subtree
!                 size such that NKIDS subtrees of that size,
!                 together with the NKIDS-1 keys in NODE, contain
!                 at least SUBSIZ keys.  The size we seek will
!                 become BIGSIZ, the larger of the two subtree
!                 sizes we'll use.  So BIGSIZ is the smallest
!                 integer such that
!
!                    ( NKIDS - 1 ) + ( NKIDS * BIGSIZ )  >  SUBSIZ
!                                                        -
!
!                 or equivalently
!
!                    BIGSIZ * NKIDS  >  SUBSIZ - NKIDS + 1
!                                    -
!
                  q       =  subsiz - nkids + 1
                  div     =  nkids
                  bigsiz  =  ( q + div - 1 )  / div

                  nsmall  =  bigsiz * nkids  -  q
                  nbig    =  nkids  -  nsmall

               end if

!
!              Fill in the keys for the current node.
!
               nkeys   =  nkids  -  1

               do i = 1, nkeys
!
!                 The Ith key may be found by considering the number
!                 of keys in the subtree between the Ith key and its
!                 predecessor in the current node.
!
                  if ( i == 1 ) then
                     npred = basidx
                  else
                     npred = basidx + page( trkeyc+i-1 )
                  end if

                  if ( i <= nbig ) then
                     key   =  npred  + bigsiz + 1
                  else
                     key   =  npred  + bigsiz
                  end if

                  page ( trkeyc + i ) =  key - basidx
                  page ( trdatc + i ) =  values ( key )

               end do

               do i = 1, nkids
!
!                 Allocate a node for the Ith child.  Store pointers
!                 to these nodes.
!
                  call zzekpgal ( ctx, handle, int, child, base )

                  page ( trkidc + i )  =  child
                  totnod               =  totnod + 1

               end do

!
!              We can now fill in the metadata for the current node.
!
               page ( trnkc )  =  nkeys

               call zzekpgwi ( ctx, handle, node, page )

            end if

!
!           Unless the current node is a leaf node, prepare to visit
!           the first child of the current node.
!
            if ( level < d ) then
!
!              Push our current state.
!
               stnode ( level )  = node
               stnkey ( level )  = nkeys
               stlsiz ( level )  = bigsiz
               stnbig ( level )  = nbig
               stnext ( level )  = 2
               stnbas ( level )  = basidx

!
!              NEXT is already set to 1.  BASIDX is set, since the
!              base index of the first child is that of the parent.
!
               if ( level == 1 ) then
                  kidbas  =  trkidr
               else
                  kidbas  =  trkidc
               end if

               level   =  level + 1
               node    =  page ( kidbas+1 )
               subsiz  =  bigsiz

            else if ( level > 1 ) then
!
!              The current node is a child leaf node.  There are no
!              calculations to do; we simply assign keys and pointers,
!              write out metadata, and pop our state.
!
               nkeys  =  subsiz

               do i = 1, nkeys

                  key                 =  basidx + i
                  page ( trkeyc + i ) =  i
                  page ( trdatc + i ) =  values ( key )

               end do

!
!              We can now fill in the metadata for the current node.
!
               page ( trnkc )  =  nkeys

               call zzekpgwi ( ctx, handle, node, page )

!
!              A leaf node is a subtree unto itself, and we're
!              done with this subtree.  Pop our state.
!
               level  =  level - 1

               if ( level >= 1 ) then

                  node    =  stnode ( level )
                  nkeys   =  stnkey ( level )
                  bigsiz  =  stlsiz ( level )
                  nbig    =  stnbig ( level )
                  next    =  stnext ( level )
                  basidx  =  stnbas ( level )
                  nkids   =  nkeys + 1

!
!                 Read in the current node.
!
                  call zzekpgri ( ctx, handle, node, page )

               end if

            else
!
!              The only node is the root.  Pop out.
!
               level  =  0

            end if
!
!           We've decided which node to go to next at this point.
!           At this point, LEVEL, NEXT, NODE, SUBSIZ and BASIDX are set.
!

         else
!
!           The current node has been visited already.  Visit the
!           next child, if there is one.
!
            if ( next <= nkids ) then
!
!              Prepare to visit the next child of the current node.
!
               stnext ( level )   =   next  +  1

               if ( level == 1 ) then
                  kidbas  =  trkidr
               else
                  kidbas  =  trkidc
               end if

               node    =  page ( kidbas + next )

               if ( next <= nbig ) then
                  subsiz  =  stlsiz(level)
               else
                  subsiz  =  stlsiz(level) - 1
               end if

               if ( next <= nbig+1 ) then

                  basidx  =      stnbas(level) &
                             + ( next-1 ) * stlsiz(level) &
                             + ( next-1 )

               else

                  basidx  =      stnbas(level) &
                             +   nbig           *   stlsiz(level) &
                             + ( next-nbig-1 )  * ( stlsiz(level) - 1 ) &
                             + ( next-1 )
               end if

               level  =  level + 1
               next   =  1

!
!              LEVEL, NEXT, NODE, SUBSIZ, and BASIDX are set.
!

            else
!
!              We're done with the current subtree.  Pop the stack.
!
               level   =  level - 1

               if ( level >= 1 ) then

                  node    =  stnode ( level )
                  nkeys   =  stnkey ( level )
                  bigsiz  =  stlsiz ( level )
                  nbig    =  stnbig ( level )
                  next    =  stnext ( level )
                  basidx  =  stnbas ( level )
                  nkids   =  nkeys + 1
!
!                 Read in the current node.
!
                  call zzekpgri ( ctx, handle, node, page )

               end if

            end if

         end if

!
!        On this pass through the loop, we either---
!
!           - Visited a node for the first time and filled in the
!             node.
!
!           - Advanced to a new node that has not yet been visited.
!
!           - Exited from a completed subtree.
!
!        Each of these actions can be performed a finite number of
!        times.  Therefore, we made progress toward loop termination.
!
      end do

!
!     The last chore is setting the total number of nodes in the root.
!
      base  = zzektrbs ( ctx, tree )

      call dasudi ( ctx, handle, base+trnnod, base+trnnod, [ totnod ] )

      return
      end subroutine zzektr1s

      !> Solve overflow in a node by balancing the node
      !> with one of its sibling nodes.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     LEFT,
      !>     RIGHT          are the node numbers of a pair of nodes to
      !>                    be balanced.  LEFT and RIGHT must be neighboring
      !>                    subnodes of a common parent.
      !>
      !>     PARENT         is the node number of the common parent node of
      !>                    nodes LEFT, RIGHT.
      !>
      !>     PKIDX          is the `parent key index', that is, the
      !>                    node-relative index of the key in the parent that
      !>                    sits between PARENT's child node pointers to
      !>                    nodes LEFT and RIGHT.  The key at location PKIDX
      !>                    is the immediate successor of the greatest key in
      !>                    the subnode headed by LEFT.  It is the immediate
      !>                    predecessor of the least key in the subnode headed
      !>                    by RIGHT.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If either LEFT or RIGHT are actually the root, the error
      !>         SPICE(BUG) is signaled.
      !>
      !>     4)  If LEFT and RIGHT are not neighboring sibling nodes, the
      !>         error will be diagnosed by routines called by this routine.
      !>
      !>     5)  The sum of the key counts in LEFT and RIGHT must be between
      !>         2*MNKEYC and 2*MXKEYC; otherwise the key count invariants
      !>         cannot be satisfied by balancing.  If the sum fails to meet
      !>         this condition, the error SPICE(BUG) is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     Insertions into and deletions from EK trees can result in
      !>     overflows or underflows of keys in nodes affected by these
      !>     operations.  Many times key count invariants can be restored by
      !>     moving keys from one node into an adjacent sibling node.  This
      !>     maneuver is called `balancing' the nodes.  After balancing, the
      !>     key counts of the affected nodes differ by at most 1.
      !>
      !>     The balancing process also affects the parent node of the
      !>     neighboring children because one key of the parent sits between
      !>     the children.  This `parent key' gets moved into one of the
      !>     children as keys are shifted.  If the shift is to the right, the
      !>     parent key is the largest key of the shifted set; if the shift
      !>     is to the left, the parent key is the least of the shifted set.
      !>
      !>     When keys are shifted, their data values move along with them.
      !>     In general, child pointers move along with keys, but there are
      !>     some tricky points:
      !>
      !>        - The left and right child pointers of the parent key don't
      !>          get updated; they continue to point to the two children
      !>          LEFT and RIGHT.
      !>
      !>        - On a right shift, the right child pointer of the key that
      !>          gets moved into the parent key's original position becomes
      !>          the first left child pointer of the right sibling.  The left
      !>          child pointer of this key doesn't get moved at all.
      !>
      !>        - On a left shift, the left child pointer of the key that
      !>          gets moved into the parent key's original position becomes
      !>          the last right child pointer of the left sibling.  The right
      !>          child pointer of this key becomes the left child pointer of
      !>          the first key of RIGHT.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRIN.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 27-OCT-1995 (NJB)
      module subroutine zzektrbn ( ctx, handle, tree, left, right, parent, pkidx )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: left  !! Left node of pair to be balanced.
      integer(ip), intent(in) :: right  !! Right node of pair to be balanced.
      integer(ip), intent(in) :: parent  !! Parent node of pair to be balanced.
      integer(ip), intent(in) :: pkidx  !! Parent key index.
!
!     Local variables
!
      integer(ip) :: lnkeys
      integer(ip) :: rnkeys
      integer(ip) :: root
      integer(ip) :: schlep
      integer(ip) :: sum

      root  =  tree

      if ( ( left == root ) .or. ( right == root )  ) then

         call ctx%setmsg ( 'Input node is root; only children can be ' // &
                       'balanced.'                                 )
         call ctx%sigerr ( 'SPICE(BUG)'                                )

      end if

!
!     Get the key counts for the left and right nodes.
!
      lnkeys  =  zzektrnk ( ctx, handle, tree, left  )
      rnkeys  =  zzektrnk ( ctx, handle, tree, right )

!
!     Balancing the nodes should give each of them a key count in
!     the range of
!
!        MNKEYC : MXKEYC
!
!     If that's not possible, we have a serious problem.
!

      sum  =  lnkeys + rnkeys

      if (      (  sum >  2*mxkeyc  ) &
           .or. (  sum <  2*mnkeyc  )   )  then

         call ctx%setmsg ( 'Node # and right sibling # contain # and # '  // &
                       'keys respectively; count sum should be in '   // &
                       'range #:#.'                                    )
         call ctx%errint ( '#',  left                                      )
         call ctx%errint ( '#',  right                                     )
         call ctx%errint ( '#',  lnkeys                                    )
         call ctx%errint ( '#',  rnkeys                                    )
         call ctx%errint ( '#',  2*mnkeyc                                  )
         call ctx%errint ( '#',  2*mxkeyc                                  )
         call ctx%sigerr ( 'SPICE(BUG)'                                    )
         return

      end if

!
!     Now, the actions we take depend on whether we must schlep keys
!     to the right or left.
!
      if ( lnkeys > rnkeys ) then

         schlep  =        lnkeys  -  ( (sum+1) / 2 )

      else if ( lnkeys < rnkeys ) then

         schlep  =  - (   rnkeys  -  ( (sum+1) / 2 )   )

      else

         schlep  =  0

      end if

!
!     Rotate the requested number of keys.
!
      call zzektrrk ( ctx, handle, tree, left, right, parent, pkidx, schlep )

      return
      end subroutine zzektrbn

      !> Get a key from a node by index:  return the key having a specified
      !> index in a specified node.  The node of interest is identified
      !> by a key in the node.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     NODKEY         is an absolute key belonging to the node
      !>                    containing the key of interest.
      !>
      !>                    NODKEY must be in the range 1 : NKEYS, where
      !>                    NKEYS is the number of keys in the tree.
      !>
      !>     N              is the node-relative index of the key of interest.
      !>                    Indices of keys start at 1.
      !>
      !>## Detailed Output
      !>
      !>     KEY            is the absolute key located at index N within the
      !>                    node containing NODKEY.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the input key is less than 1 or greater than the number
      !>         of keys in the specified tree, the error SPICE(INVALIDINDEX)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine allows lookup of keys by index.  It is frequently
      !>     used by other EK private routines to find the first key of a node.
      !>
      !>## Examples
      !>
      !>     See ZZEKTRIN.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
      module subroutine zzektrki ( ctx, handle, tree, nodkey, n, key )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: nodkey  !! Key identifying node containing key of interest.
      integer(ip), intent(in) :: n  !! Index of key of interest.
      integer(ip), intent(out) :: key  !! Key located at index N.
      integer(ip) :: key_1(1)  !! one-element copy of key (array argument)
!
!     Local variables
!
      integer(ip) :: addrss
      integer(ip) :: base
      integer(ip) :: idx
      integer(ip) :: level
      integer(ip) :: node
      integer(ip) :: noffst
      integer(ip) :: ptr
      integer(ip) :: size

      call zzektrlk ( ctx,  handle,  tree,    nodkey,  idx, &
                       node,    noffst,  level,   ptr   )

      size  =  zzektrnk ( ctx, handle, tree,  node )

!
!     Reject bad indices.
!
      if (  ( n < 0 ) .or. ( n > size )  )  then

         call ctx%setmsg ( 'Key index = #; valid range in node # is ' // &
                       '1:#'                                       )
         call ctx%errint ( '#',  n                                     )
         call ctx%errint ( '#',  node                                  )
         call ctx%errint ( '#',  size                                  )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                       )
         return

      end if

      base  =  zzektrbs ( ctx, node )

      if ( level == 1 ) then
         addrss = base + trkeyr + n
      else
         addrss = base + trkeyc + n
      end if

      call dasrdi ( ctx, handle, addrss, addrss, key_1 )
      key = key_1(1)

!
!     Map the key from relative to absolute.
!
      key  =  key + noffst

      end subroutine zzektrki

      !> Search an EK tree linearly to find a specified data value.  The
      !> function returns the index at which the value is found.
      !>
      !> The function returns the lowest index at which the input value
      !> is found, or zero if the value is not found.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for read or write
      !>                    access.
      !>
      !>     TREE           is the root node of the tree to search.
      !>
      !>     IVAL           is the value to search for.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the lowest index at which the input value
      !>     is found, or zero if the value is not found.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If TREE is invalid, strange errors may result.
      !>
      !>     3)  If an I/O error occurs while reading the indicated file,
      !>         the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This searches a tree for a specified value.  It is an approximate
      !>     inverse of ZZEKTRLK.  However, ZZEKTRLK operates in logarithmic
      !>     time (as a function of the tree's size), while this function
      !>     plods along in linear time.
      !>
      !>## Examples
      !>
      !>     See ZZEKRP2N.
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
      !>-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
      module integer(ip) function zzektrls ( ctx, handle, tree, ival )
      use pepper_kinds, only: ip
      use pepper_inc_ektree
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Tree root.
      integer(ip), intent(in) :: ival  !! Value to search for.
!
!     Local variables
!
      integer(ip) :: i
      integer(ip) :: n
      integer(ip) :: value

      zzektrls  =  0
      n         =  zzektrsz ( ctx, handle, tree )

      do i = 1, n

         call zzektrdp ( ctx, handle, tree, i, value )

         if ( ival == value ) then
            zzektrls = i
            return
         end if

      end do

      return
      end function zzektrls

      !> Return the integer metadata location of a specified segment.  The
      !> number and DAS integer base address of the first integer
      !> page of the metadata are returned.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is an EK file handle.  The EK may be open for read
      !>                    or write access.
      !>
      !>     SEGNO          is the number of the segment whose integer metadata
      !>                    location is sought.
      !>
      !>## Detailed Output
      !>
      !>    PAGE            is the number of the first page containing integer
      !>                    metadata for the specified segment.  The segment
      !>                    descriptor starts at the first address of this
      !>                    page.
      !>
      !>    BASE            is the DAS integer base address of the page
      !>                    whose number is given by PAGE.  BASE is the
      !>                    predecessor of the first DAS integer word
      !>                    belonging to this page.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If SEGNO is out of range, the error SPICE(INVALIDINDEX)
      !>         will be signaled.
      !>
      !>     3)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects:  it deletes a record
      !>     from an EK segment.  Deleting a record implies:
      !>
      !>        1) All column entries in the record are deleted.
      !>
      !>        2) Link counts are decremented for data pages containing
      !>           column entries in the record to be deleted.  Pages whose
      !>           link counts drop to zero are freed.
      !>
      !>        3) All column indexes are updated for the parent segment.
      !>
      !>        4) The link count is decremented for the page containing the
      !>           record pointer structure of the record to be deleted.  If
      !>           the link count drops to zero, the page is freed.
      !>
      !>        5) The pointer to the deleted record is deleted from the
      !>           record tree for the parent segment.
      !>
      !>        6) The segment's metadata is updated to reflect the new
      !>           record count.
      !>
      !>## Examples
      !>
      !>     See EKINSR.
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
      !>-    SPICELIB Version 1.2.0, 02-OCT-2021 (JDR)
      !>
      !>        Fixed typo in the INVALIDINDEX short error message.
      !>
      !>-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
      !>
      !>        Added the required discovery CHKIN.
      !>
      !>-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
      module subroutine zzekmloc ( ctx, handle, segno, page, base )
      use pepper_kinds, only: ip
      use pepper_inc_ekfilpar
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segno  !! Segment number.
      integer(ip), intent(out) :: page  !! Integer metadata start page number.
      integer(ip), intent(out) :: base  !! Page base.
      integer(ip) :: tree_1(1)  !! one-element copy of tree (array argument)
!
!     Local variables
!
      integer(ip) :: nseg
      integer(ip) :: tbase
      integer(ip) :: tree

!
!     Validate the segment number to start out.
!
!     Get the segment count; validate SEGNO.
!
      nseg  =  eknseg ( ctx, handle )

!
!     Check out SEGNO.
!
      if (  ( segno < 1 ) .or. ( segno > nseg )  )  then

         call ctx%setmsg ( 'Segment number = #; valid range is 1:#.' )
         call ctx%errint ( '#',  segno                               )
         call ctx%errint ( '#',  nseg                                )
         call ctx%sigerr ( 'SPICE(INVALIDINDEX)'                     )
         return

      end if

!
!     Find the segment in the segment tree.
!     Obtain the base address of the first integer page.
!
      tbase = zzektrbs ( ctx, 1 )

!
!     Look up the head node of the segment tree.
!
      call dasrdi ( ctx, handle, tbase+sgtidx, tbase+sgtidx, tree_1 )
      tree = tree_1(1)

!
!     Get the segment pointer for the segment having index SEGNO.
!     This pointer is actually the page number we're looking for.
!
      call zzektrdp ( ctx, handle, tree, segno, page )

!
!     Return the base address of the metadata page as well.
!
      base  =  zzektrbs ( ctx, page )

      end subroutine zzekmloc

      !> Delete a value from an EK tree at a specified location.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     KEY            is an absolute key indicating the deletion
      !>                    location.  In EK trees, absolute keys are just
      !>                    ordinal positions relative to the leftmost element
      !>                    of the tree, with the leftmost element having
      !>                    position 1.  So setting KEY to 10, for example,
      !>                    indicates that the input VALUE is the 10th item in
      !>                    the tree.
      !>
      !>                    KEY must be in the range 1 : NKEYS, where
      !>                    NKEYS is the number of keys in the tree prior to
      !>                    the deletion.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the input key is out of range, the error is diagnosed by
      !>         routines called by this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine deletes a value from an EK tree at a specified
      !>     location.  The successor of the value and all higher-indexed
      !>     values have their indexes decremented.  Since keys are stored in
      !>     subtree-relative form, the only keys actually modified by the
      !>     deletion itself are higher-indexed keys in the node from which
      !>     the deletion is done, and higher-indexed keys in the chain of
      !>     ancestors of this node.
      !>
      !>     The deletion is always done from a leaf node.  If KEY is not in a
      !>     leaf node, the value corresponding to KEY is swapped with that of
      !>     an immediate neighbor, and the neighbor is deleted.  This is
      !>     possible because every key is either in a leaf or has the property
      !>     that its predecessor and successor are both located in leaf nodes.
      !>
      !>     The deletion is not the end of the story, however:  it's possible
      !>     that the node from which the deletion is done (the `target node')
      !>     will underflow.  If underflow occurs, this routine will restore
      !>     the tree to its normal form as follows:
      !>
      !>        1)  If a neighbor of the target node contains at least one more
      !>            key than the minimum allowed number, data will be `rotated'
      !>            from the neighbor node, through the target's parent,
      !>            and into the target.  The deletion is complete at this
      !>            point.
      !>
      !>        2)  If the target node has only one neighbor, but that neighbor
      !>            is neighbor to a sibling that can contribute a key, data
      !>            will be rotated from the second sibling, through the
      !>            siblings' parent, into the first sibling, and then from
      !>            the first sibling through the target's parent, and into
      !>            the target.  The deletion is complete at this point.
      !>
      !>        3)  If the target is not a child of the root, and if
      !>            the target has two neighbors, but neither neighbor has a
      !>            key to spare, then the target node and its neighbors will
      !>            be merged into two nodes:   this is called a `3-2 merge'.
      !>            The parent node is modified appropriately so that all
      !>            values are in the proper order and all subtree-relative
      !>            keys are correct.  This `3-2 merge' decreases the number
      !>            of values in the parent by one.  If the decrease does not
      !>            cause an underflow in the parent, the deletion is complete.
      !>
      !>            If the target has only one neighbor, and both the neighbor
      !>            and the neighbor's other neighbor (which always exists)
      !>            contain the minimum number of keys, these three nodes are
      !>            combined into two via a 3-2 merge.
      !>
      !>        4)  If the parent underflows as a result of a 3-2 merge, the
      !>            solution process is repeated at the parent's level.  The
      !>            process iterates until the underflow is resolved or a
      !>            child of the root underflows.
      !>
      !>        5)  If a child of the root underflows, the problem is solved
      !>            by balancing keys with a neighbor if possible.  Balancing
      !>            cannot be done only if the root has only two children, and
      !>            these contain the minimum number of keys.  In this case,
      !>            the contents of the two children of the root are moved
      !>            into the root and the children are eliminated.  The
      !>            children of the child nodes become children of the root.
      !>            This is the only case in which the tree grows shorter.
      !>
      !>            The process of collapsing two child nodes into the root is
      !>            called a `3-1 merge'.  After a 3-1 merge is performed, the
      !>            number of values in each node is within bounds.
      !>
      !>     An EK tree is always balanced after a deletion:  all leaf nodes
      !>     are at the same level.
      !>
      !>## Examples
      !>
      !>     See EKDELR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
      module subroutine zzektrdl ( ctx, handle, tree, key )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: key  !! Key at which to delete value.




!
!     Local parameters
!
      integer(ip), parameter :: merg32 = balnce + 1

      integer(ip), parameter :: merg31 = merg32 + 1

      integer(ip), parameter :: llchck = merg31 + 1

      integer(ip), parameter :: rrchck = llchck + 1

      integer(ip) :: idx
      integer(ip) :: left
      integer(ip) :: level
      integer(ip) :: lkey
      integer(ip) :: llkey
      integer(ip) :: llsib
      integer(ip) :: lnkey
      integer(ip) :: lnode
      integer(ip) :: lpidx
      integer(ip) :: lpkey
      integer(ip) :: lrkey
      integer(ip) :: lrsib
      integer(ip) :: lsib
      integer(ip) :: mnode
      integer(ip) :: nkeys
      integer(ip) :: node
      integer(ip) :: noffst
      integer(ip) :: parent
      integer(ip) :: pkey
      integer(ip) :: poffst
      integer(ip) :: ptr
      integer(ip) :: right
      integer(ip) :: rkey
      integer(ip) :: rlkey
      integer(ip) :: rlsib
      integer(ip) :: rnode
      integer(ip) :: root
      integer(ip) :: rpidx
      integer(ip) :: rpkey
      integer(ip) :: rrkey
      integer(ip) :: rrsib
      integer(ip) :: rsib
      integer(ip) :: state
      integer(ip) :: trgkey
      integer(ip) :: trust

      logical :: undrfl

!
!     Set the variable ROOT, so we'll have something mnemonic to go
!     by when referring to the root node.
!
      root  =  tree

!
!     Work with a local copy of the input key.
!
      lkey  =  key

!
!     The first step is to delete the key from the tree without
!     balancing.  This step may cause a node to underflow.  We'll
!     handle the underflow later.
!
      call zzektrud ( ctx, handle, tree, lkey, trgkey, undrfl )

      if ( ctx%failed() ) return

!
!     If the deletion didn't result in an underflow, we're done.
!
      if ( .not. undrfl ) return

!
!     Handle node underflows, as required.  We describe our approach
!     below.  If any step fails, we try the next step.  We proceed
!     until we succeed in resolving the underflow.
!
!        1) If an immediate sibling can contribute a key, balance NODE
!           with that sibling.
!
!        2) If both left and right siblings exist, but neither can
!           contribute a key, execute a 3-2 merge.
!
!        3) If the left sibling has its own left sibling, and if that
!           second left sibling can contribute a key, rotate a key
!           from that sibling into NODE's left sibling.  Then execute
!           (1).
!
!        4) If the left sibling has its own left sibling, and if that
!           second left sibling cannot contribute a key, execute a 3-2
!           merge using NODE as the rightmost child.
!
!        5) Same as (3), except on the right side.
!
!        6) Same as (4), except on the right side.
!
!        7) Arrival at this step implies that NODE is a child of the
!           root and has one sibling.  Execute a 3-1 merge.
!

      state = lcheck

      do while ( state /= term )

         if ( state == lcheck ) then
!
!           Look up the node containing the target key TRGKEY.  This
!           is where the underflow occurred; note that this node may
!           be different from the one that contained LKEY.
!
            call zzektrlk ( ctx,  handle,  tree,    trgkey,  idx, &
                             node,    noffst,  level,   ptr  )

!
!           Look up the siblings of NODE.  If either sibling exists
!           and has a surplus of keys, we can remove the underflow
!           by balancing.
!
            call zzektrsb ( ctx,  handle,  tree,  trgkey, &
                             lsib,    lkey,  rsib,    rkey  )

            if ( lsib > 0 ) then

               nkeys = zzektrnk ( ctx, handle, tree, lsib )

               if ( nkeys > mnkeyc ) then
!
!                 The left sibling can contribute a key.
!
                  lnkey  =  lkey
                  lnode  =  lsib
                  rnode  =  node

                  state  =  balnce

               else if ( rsib > 0 ) then
!
!                 The left sibling cannot help with balancing, but
!                 the right sibling may be able to.
!
                  state  =  rcheck

               else
!
!                 The right sibling does not exist; the only chance
!                 of balancing will come from the left sibling of
!                 LSIB, if such a sibling exists.
!
                  state  =  llchck

               end if

            else
!
!              There is no left sibling, so there must be a right
!              sibling.  Examine it.
!
               state = rcheck

            end if

         else if ( state == rcheck ) then
!
!           See whether there's a node surplus in the right sibling
!           The left sibling has already been checked and found wanting,
!           or wasn't found at all.
!
            nkeys = zzektrnk ( ctx, handle, tree, rsib )

            if ( nkeys > mnkeyc ) then
!
!              The right sibling can contribute a key.
!
               lnkey  =  trgkey
               lnode  =  node
               rnode  =  rsib

               state  =  balnce

            else if ( lsib > 0 ) then
!
!              NODE has siblings on both sides, and each one contains
!              the minimum number of keys.  Execute a 3-2 merge.
!
               lnkey   =  lkey
               lnode   =  lsib
               mnode   =  node
               rnode   =  rsib

               state   =  merg32

            else
!
!              Look for the right sibling of the right sibling.
!
               state  =  rrchck

            end if

         else if ( state == llchck ) then
!
!           See whether the left sibling has its own left sibling.
!
            call zzektrsb ( ctx,  handle,  tree,   lkey, &
                             llsib,   llkey,  lrsib,  lrkey  )

            if ( llsib > 0 ) then

               nkeys = zzektrnk ( ctx, handle, tree, llsib )

               if ( nkeys > mnkeyc ) then
!
!                 The left**2 sibling can contribute a key.  Rotate
!                 this key into the left sibling.  We'll need the
!                 parent and index of left parent key of LSIB in order
!                 to do this rotation.
!
                  call zzektrpi ( ctx, handle,  tree,    lkey,   parent, &
                                  pkey,    poffst,  lpidx,  lpkey, &
                                  llsib,   rpidx,   rpkey,  lrsib  )

                  call zzektrrk ( ctx, handle,  tree,    llsib,  lsib, &
                                  parent,  lpidx,   1              )

!
!                 Now LSIB has a one-key surplus, so we can balance
!                 LSIB and NODE.
!
                  lnkey  =  lkey
                  lnode  =  lsib
                  rnode  =  node

                  state  =  balnce

               else
!
!                 The left**2 sibling contains the minimum allowed
!                 number of keys.  Execute a 3-2 merge, with NODE
!                 as the right node.
!
                  lnkey   =  llkey
                  lnode   =  llsib
                  mnode   =  lsib
                  rnode   =  node

                  state   =  merg32

               end if

            else
!
!              LSIB and NODE are the only children of their parent.
!              The parent must be the root.  Also, LSIB and NODE
!              together contain the one less than twice the minimum
!              allowed number of keys.  Execute a 3-1 merge.
!
               lnode  =  lsib
               rnode  =  node

               state  =  merg31

            end if

         else if ( state == rrchck ) then
!
!           See whether the right sibling has its own right sibling.
!
            call zzektrsb ( ctx,  handle,  tree,   rkey, &
                             rlsib,   rlkey,  rrsib,  rrkey  )

            if ( rrsib > 0 ) then

               nkeys = zzektrnk ( ctx, handle, tree, rrsib )

               if ( nkeys > mnkeyc ) then
!
!                 The right**2 sibling can contribute a key.  Rotate
!                 this key into the right sibling.  We'll need the
!                 parent and index of the right parent key of RSIB in
!                 order to do this rotation.
!
                  call zzektrpi ( ctx, handle,  tree,    rkey,   parent, &
                                  pkey,    poffst,  lpidx,  lpkey, &
                                  rlsib,   rpidx,   rpkey,  rrsib  )

                  call zzektrrk ( ctx, handle,  tree,    rsib,   rrsib, &
                                  parent,  rpidx,   -1             )

!
!                 Now RSIB has a one-key surplus, so we can balance
!                 RSIB and NODE.
!
                  lnkey  =  trgkey
                  lnode  =  node
                  rnode  =  rsib

                  state  =  balnce

               else
!
!                 The right**2 sibling contains the minimum allowed
!                 number of keys.  Execute a 3-2 merge, with NODE
!                 as the left node.
!
                  lnkey   =  trgkey
                  lnode   =  node
                  mnode   =  rsib
                  rnode   =  rrsib

                  state   =  merg32

               end if

            else
!
!              RSIB and NODE are the only children of their parent.
!              The parent must be the root.  Also, RSIB and NODE
!              together contain one less than twice the minimum allowed
!              number of keys.  Execute a 3-1 merge.
!
               lnode  =  node
               rnode  =  rsib

               state  =  merg31

            end if

         else if ( state == balnce ) then
!
!           LNODE has a right sibling, and between the two nodes,
!           there are enough keys to accommodate the underflow.  After
!           balancing these nodes, we're done.
!
            call zzektrpi ( ctx, handle,  tree,    lnkey,   parent, &
                            pkey,    poffst,  lpidx,   lpkey, &
                            rlsib,   rpidx,   rpkey,   rrsib  )

!
!           The common parent of the nodes is PARENT.  The right parent
!           key of the left node is at location RPIDX.  We're ready to
!           balance the nodes.
!
            call zzektrbn ( ctx, handle, tree, lnode, rnode, parent, rpidx )

            state  =  term

         else if ( state == merg32 ) then
!
!           LNODE, MNODE, and RNODE are siblings, and between the three
!           nodes, there's an underflow of one key.  Merge these three
!           nodes into two.  This merging process removes a key from the
!           parent; the parent may underflow as a result.
!
!           After executing the 3-2 merge, to ensure that we reference
!           the parent correctly, we'll obtain a fresh key from the
!           parent.
!
!           To start with, we'll get a trusted key from the
!           leftmost node LNODE.  The first key of LNODE won't be
!           touched by the merge.
!
            call zzektrki ( ctx, handle,  tree,    lnkey,   1,   trust )

            call zzektrpi ( ctx, handle,  tree,    lnkey,   parent, &
                            pkey,    poffst,  lpidx,   lpkey, &
                            rlsib,   rpidx,   rpkey,   rrsib  )

!
!           The right parent key of the left node is the left parent
!           key of the middle node.  The index of this key is required
!           by ZZEKTR32.
!
            call zzektr32 ( ctx, handle,  tree,    lnode,  mnode, &
                            rnode,   parent,  rpidx,  undrfl  )

            if ( undrfl ) then
!
!              We'll need to handle underflow in the parent.
!              The parent should be correctly identified by the
!              parent of TRUST.
!
!              Note that a 3-2 merge can't create an underflow in
!              the parent if the parent is the root:  the parent
!              contains at least one key after this merge.
!
               call zzektrpi ( ctx, handle, tree,   trust,  parent, &
                               pkey,   poffst, lpidx,  lpkey, &
                               left,   rpidx,  rpkey,  right  )

               trgkey =  pkey
               state  =  lcheck

            else
               state  =  term
            end if

         else if ( state == merg31 ) then
!
!           We've got an underflow in the two children of the root.
!           Move all of the keys from these children into the root.
!           The root contains the maximum allowed number of keys
!           after this merge.
!
            call zzektr31 ( ctx, handle, tree )
            state  =  term

         end if

      end do

      return
      end subroutine zzektrdl

      !> Insert a value into an EK tree at a specified location.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     KEY            is an absolute key indicating the insertion
      !>                    location.  In EK trees, absolute keys are just
      !>                    ordinal positions relative to the leftmost element
      !>                    of the tree, with the leftmost element having
      !>                    position 1.  So setting KEY to 10, for example,
      !>                    indicates that the input VALUE is the 10th item in
      !>                    the tree.
      !>
      !>                    KEY must be in the range 1 : (NKEYS+1), where
      !>                    NKEYS is the number of keys in the tree prior to
      !>                    the insertion.
      !>
      !>     VALUE          is an integer value to be inserted into the
      !>                    specified tree at the ordinal position KEY.
      !>
      !>## Detailed Output
      !>
      !>     None.  See $Particulars for a description of the effect of this
      !>     routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     3)  If the input key is out of range, the error is diagnosed by
      !>         routines called by this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine inserts a value into an EK tree at a specified
      !>     location.  If the location is occupied, the value previously at
      !>     that location and all higher-indexed values have their indexes
      !>     incremented.  Since keys are stored in subtree-relative form,
      !>     the only keys actually modified by the insertion itself are
      !>     higher-indexed keys in the node into which the insertion is done,
      !>     and higher-indexed keys in the chain of ancestors of this node.
      !>
      !>     The insertion is not the end of the story, however:  it's possible
      !>     that the node at which the insertion is done (the `target node')
      !>     will overflow.  If overflow occurs, this routine will restore the
      !>     tree to its normal form as follows:
      !>
      !>        1)  If a neighbor of the target node has room, data will be
      !>           `rotated' from the target node, through the target's parent,
      !>            and into the neighbor.  The insertion is complete at this
      !>            point.
      !>
      !>        2)  If no neighbor has room, then the target node and a
      !>            neighbor are split and recombined into three nodes:  this
      !>            is called a `2-3 split'.  The parent node is modified
      !>            appropriately so that all values are in the proper order
      !>            and all subtree-relative keys are correct.  This 2-3 split
      !>            increases the number of values in the parent by one.  If
      !>            the increase does not cause an overflow in the parent, the
      !>            insertion is complete.
      !>
      !>        3)  If the parent overflows as a result of a 2-3 split, the
      !>            solution process is repeated at the parent's level.  The
      !>            process iterates until the overflow is resolved or the
      !>            root overflows.
      !>
      !>        4)  If the root overflows, the root is split into two children
      !>            and a new root node; the new root contains a single value.
      !>            The children of the old root become children of the two
      !>            new child nodes of the new root.  This is the only
      !>            case in which the tree grows taller.
      !>
      !>            The process of splitting the root is called a `1-3 split'.
      !>            After a 1-3 split is performed, the number of values in
      !>            each node is within bounds.
      !>
      !>     An EK tree is always balanced after an insertion:  all leaf nodes
      !>     are at the same level.
      !>
      !>## Examples
      !>
      !>     See EKINSR.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    Beta Version 1.0.0, 01-NOV-1995 (NJB)
      module subroutine zzektrin ( ctx, handle, tree, key, value )
      use pepper_kinds, only: ip
      use pepper_inc_ekpage
      use pepper_inc_ektree
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: key  !! Key at which to insert value.
      integer(ip), intent(in) :: value  !! Value to insert.




!
!     Local parameters
!
      integer(ip), parameter :: splt23 = balnce + 1

      integer(ip), parameter :: splt13 = splt23 + 1

      integer(ip) :: idx
      integer(ip) :: left
      integer(ip) :: level
      integer(ip) :: lkey
      integer(ip) :: lnode
      integer(ip) :: lpidx
      integer(ip) :: lpkey
      integer(ip) :: lval
      integer(ip) :: nkeys
      integer(ip) :: node
      integer(ip) :: noffst
      integer(ip) :: nsize
      integer(ip) :: parent
      integer(ip) :: pkey
      integer(ip) :: pkidx
      integer(ip) :: poffst
      integer(ip) :: right
      integer(ip) :: rnode
      integer(ip) :: root
      integer(ip) :: rpidx
      integer(ip) :: rpkey
      integer(ip) :: state
      integer(ip) :: trust

      logical :: overfl

!
!     Set the variable ROOT, so we'll have something mnemonic to go
!     by when referring to the root node.
!
      root  =  tree

!
!     Work with local copies of the input key and value.
!
      lkey  =  key
      lval  =  value

!
!     The first step is to insert the key into the tree without
!     balancing.  This step may cause a node to overflow.  We'll
!     handle the overflow later.  In general, the probability of
!     overflow is low:  each overflow creates at least one new node,
!     but the ratio of nodes to keys is very small.
!
      call zzektrui ( ctx, handle, tree, lkey, lval, overfl )

      if ( ctx%failed() ) return

!
!     If the insertion didn't result in an overflow, we're done.
!
      if ( .not. overfl ) return

!
!     Handle node overflows, as required.
!
      state = lcheck

      do while ( state /= term )

         if ( state == lcheck ) then
!
!           Look up the node containing LKEY.
!
            call zzektrlk ( ctx,  handle,  tree,    lkey,   idx, &
                             node,    noffst,  level,  lval  )

            if ( node == root ) then

               state  =  splt13

            else
!
!              See if there's room in the left sibling.  Of course,
!              there must be a left sibling in order for there to be
!              room.
!
               call zzektrpi ( ctx, handle, tree,   lkey,   parent, &
                               pkey,   poffst, lpidx,  lpkey, &
                               left,   rpidx,  rpkey,  right  )

               if ( left > 0 ) then

                  nkeys = zzektrnk ( ctx, handle, tree, left )

                  if ( nkeys < mxkeyc ) then
                     lnode  =  left
                     rnode  =  node
                     pkidx  =  lpidx
                     state  =  balnce
                  else
                     state  =  rcheck
                  end if

               else
                  state = rcheck
               end if

            end if

         else if ( state == rcheck ) then
!
!           See whether there's room in the right sibling, if there
!           is a right sibling.  The left sibling has already been
!           checked and found wanting.
!
            if ( right > 0 ) then

               nkeys = zzektrnk ( ctx, handle, tree, right )

               if ( nkeys < mxkeyc ) then

                  lnode  =  node
                  rnode  =  right
                  pkidx  =  rpidx

                  state  =  balnce

               else

                  lnode  =  node
                  rnode  =  right
                  pkidx  =  rpidx
                  state  =  splt23

               end if

            else
!
!              The left sibling is full, but at least it's there.
!
               lnode  =  left
               rnode  =  node
               pkidx  =  lpidx
               state  =  splt23

            end if

         else if ( state == balnce ) then
!
!           LNODE has a right sibling, and between the two nodes,
!           there's enough room to accommodate the overflow.  After
!           balancing these nodes, we're done.
!
            call zzektrbn ( ctx, handle, tree, lnode, rnode, parent, pkidx )

            state  =  term

         else if ( state == splt23 ) then
!
!           LNODE has a right sibling, and between the two nodes,
!           there's an overflow of one key.  Split these two nodes
!           into three.  This splitting process adds a key to the
!           parent; the parent may overflow as a result.
!
!           After executing the 2-3 split, to ensure that we reference
!           the parent correctly, we'll obtain a fresh key from the
!           parent.  The old key PKEY may not be in the parent any more;
!           this key may have been rotated into the middle node created
!           by the 2-3 split.
!
!           To start with, we'll get a trusted key from the
!           original node NODE.  If NODE got mapped to LNODE,
!           then the first key in NODE will be unchanged by
!           the 2-3 split.  If NODE got mapped to RNODE, then
!           the last key in NODE will be unchanged.
!
            if ( node == lnode ) then
!
!              Save the first key from NODE.
!
               call zzektrki ( ctx, handle, tree, lkey, 1, trust )

            else
!
!              Save the last key from NODE.
!
               nsize  =  zzektrnk ( ctx, handle, tree, node )

               call zzektrki ( ctx, handle, tree, lkey, nsize, trust )

            end if

            call zzektr23 ( ctx, handle, tree,  lnode, rnode, &
                            parent, pkidx, overfl       )

            if ( overfl ) then

               if ( parent == root ) then

                  state   =  splt13

               else
!
!                 We'll need to handle overflow in the parent.
!                 The parent should be correctly identified by the
!                 parent of TRUST.
!
                  call zzektrpi ( ctx, handle, tree,   trust,  parent, &
                                  pkey,   poffst, lpidx,  lpkey, &
                                  left,   rpidx,  rpkey,  right  )

                  lkey    =  pkey
                  state   =  lcheck

               end if

            else
               state  =  term
            end if

         else if ( state == splt13 ) then
!
!           We've got an overflow in the root.  Split the root,
!           creating two new children.  The root contains a single
!           key after this split.
!
            call zzektr13 ( ctx, handle, tree )
            state  =  term

         end if

      end do

      return
      end subroutine zzektrin

      !> Delete a specified data page for a specified EK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     SEGDSC         is the descriptor of the segment from which to
      !>                    delete a data page.
      !>
      !>     TYPE           is the data type of the page.
      !>
      !>     P              is number of the page to delete.  This
      !>                    number is recognized by the EK paged access
      !>                    routines.
      !>
      !>## Detailed Output
      !>
      !>     SEGDSC         is the descriptor of the segment from which the
      !>                    specified page was deleted.  If P is the current
      !>                    data page of TYPE, the descriptor element
      !>                    specifying the last word in use of this data type
      !>                    will be updated on exit from this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.  The file may be corrupted.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects:  it deletes an EK data
      !>     page from a specified segment.  The segment's metadata is updated
      !>     to reflect deletion of the page.  If the deleted page is the last
      !>     one of its type in use in the specified segment, the last word in
      !>     use of that type is set to the maximum value.  This prevents
      !>     further attempts to write to the page.
      !>
      !>     The changes made by this routine to the target EK file become
      !>     permanent when the file is closed.  Failure to close the file
      !>     properly will leave it in an indeterminate state.
      !>
      !>## Examples
      !>
      !>     See EKDELR.
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
      !>-    Beta Version 2.0.0, 02-APR-1996 (NJB)
      !>
      !>        Updated to make SEGDSC an in-out argument.  The last word
      !>        in use of the data type of P is set to the maximum value
      !>        on output.  Also, an error in the deletion of the page
      !>        from the parent data page tree was corrected.
      !>
      !>-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
      module subroutine zzekdps ( ctx, handle, segdsc, type, p )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(inout) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip), intent(in) :: type  !! Data type of page.
      integer(ip), intent(in) :: p  !! Page number.
!
!     Local variables
!
      integer(ip) :: mbase
      integer(ip) :: loc
      integer(ip) :: tree

!
!     Ashes to ashes, dust to dust.  This page goes to the free list.
!
      call zzekpgfr ( ctx, handle, type, p )

      if ( ctx%failed() ) return

!
!     Update the segment's metadata.  For type 1 segments,
!     the new page into the page tree of the appropriate data type.
!
!     If this page is the last one in use in the segment, set the last
!     word in use of the appropriate type to the maximum number.  This
!     prevents further writing to the page we're deleting.
!
      mbase  =  segdsc ( imdidx )

      if ( type == chr ) then

         tree  =  segdsc ( cptidx )

         if ( segdsc(lcpidx) == p ) then
            call dasudi ( ctx, handle, mbase+lcwidx, mbase+lcwidx, [ cpsize ] )
         end if

         if ( p == segdsc(lcpidx) ) then
            segdsc(lcwidx) = cpsize
         end if

      else if ( type == dp ) then

         tree  =  segdsc ( dptidx )

         if ( segdsc(ldpidx) == p ) then
            call dasudi ( ctx, handle, mbase+ldwidx, mbase+ldwidx, [ dpsize ] )
         end if

         if ( p == segdsc(ldpidx) ) then
            segdsc(ldwidx) = dpsize
         end if

      else if ( type == int ) then
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, one of the allocation routines would have
!        complained.
!
         tree  =  segdsc ( iptidx )

         if ( segdsc(lipidx) == p ) then
            call dasudi ( ctx, handle, mbase+liwidx, mbase+liwidx, [ ipsize ] )
         end if

         if ( p == segdsc(lipidx) ) then
            segdsc(liwidx) = ipsize
         end if

      end if

!
!     Remove the page's number from the data page tree of the
!     appropriate type.  This removal requires finding the key that
!     points to the page to be removed.
!
      loc  =  zzektrls ( ctx, handle, tree, p )

      call zzektrdl ( ctx,  handle, tree, loc )

      return
      end subroutine zzekdps

      !> Complete a fast load operation on a new type 1 E-kernel segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         the handle of an EK file that is open for writing.
      !>                    A `begin segment for fast load' operation must
      !>                    have already been performed for the designated
      !>                    segment.
      !>
      !>     SEGNO          is the number of the type 1 segment to complete.
      !>
      !>     RCPTRS         is an array of record pointers for the input
      !>                    segment.  This array is obtained as an output
      !>                    from EKIFLD, the routine called to initiate a
      !>                    fast load.
      !>
      !>## Detailed Output
      !>
      !>     WORK           is the input work space array, after use.  WORK
      !>                    will generally be modified by this routine.
      !>
      !>     See the $Particulars section for a description of the
      !>     effects of this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an attempt is made to finish a segment other than the
      !>         one last initialized by EKIFLD, the error SPICE(WRONGSEGMENT)
      !>         is signaled.
      !>
      !>     3)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine completes a type 1 EK segment after the data has been
      !>     written via the fast column loader routines.
      !>
      !>## Examples
      !>
      !>     See EKFFLD.
      !>
      !>## Restrictions
      !>
      !>     1)  Only one segment can be created at a time using the fast
      !>         load routines.
      !>
      !>     2)  No other EK operation may interrupt a fast load.  For
      !>         example, it is not valid to issue a query while a fast load
      !>         is in progress.
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
      !>-    SPICELIB Version 1.1.0, 11-OCT-2021 (NJB)
      !>
      !>        All local arrays are now completely initialized.
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.0.0, 08-NOV-1995 (NJB)
      module subroutine zzekff01 ( ctx, handle, segno, rcptrs )
      use pepper_kinds, only: ip
      use pepper_inc_ekdatpag
      use pepper_inc_ekglimit
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segno  !! Segment number.
      integer(ip), intent(inout) :: rcptrs ( * )  !! Record pointers.
      integer(ip) :: colidx_1(1)  !! one-element copy of colidx (array argument)
      integer(ip) :: stkseg_1(1)  !! one-element copy of stkseg (array argument)
      integer(ip) :: stkhan_1(1)  !! one-element copy of stkhan (array argument)
!
!     Local parameters
!
      integer(ip), parameter :: bufsiz = 100

      integer(ip) :: addrss
      integer(ip) :: adrbuf ( bufsiz )
      integer(ip) :: base
      integer(ip) :: col
      integer(ip) :: colidx
      integer(ip) :: colord ( mxclsg )
      integer(ip) :: i
      integer(ip) :: ipage  ( pgsizi )
      integer(ip) :: j
      integer(ip) :: loc
      integer(ip) :: mbase
      integer(ip) :: ncols
      integer(ip) :: npage
      integer(ip) :: nr
      integer(ip) :: nrows
      integer(ip) :: nrp
      integer(ip) :: p
      integer(ip) :: pagloc
      integer(ip) :: pbase
      integer(ip) :: recno
      integer(ip) :: remain
      integer(ip) :: row
      integer(ip) :: rpsize
      integer(ip) :: segdsc ( sdscsz )
      integer(ip) :: stkbas
      integer(ip) :: stkhan
      integer(ip) :: stkseg
      integer(ip) :: tree

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Initialize all arrays.
!
      adrbuf = 0
      colord = 0
      ipage = 0
      segdsc = 0

!
!     Dig the handle and segment number out of the EK stack.  If the
!     stacked values don't match the inputs HANDLE and SEGNO, we've
!     got trouble.
!
      call zzeksrd ( ctx, 1, 1, stkhan_1 )
      stkhan = stkhan_1(1)
      call zzeksrd ( ctx, 2, 2, stkseg_1 )
      stkseg = stkseg_1(1)

      if (  ( stkhan /= handle ) .or. ( stkseg /= segno )  ) then

         call ctx%setmsg ( 'Attempt to finish fast load of wrong segment.'// &
                       '  Input segment number is #; stacked segment '// &
                       'number is #.  Input handle is #; stacked '    // &
                       'handle is #.'                                 )
         call ctx%errint ( '#',  segno                                    )
         call ctx%errint ( '#',  stkseg                                   )
         call ctx%errint ( '#',  handle                                   )
         call ctx%errint ( '#',  stkhan                                   )
         call ctx%sigerr ( 'SPICE(WRONGSEGMENT)'                          )
         return

      end if

!
!     Look up the segment descriptor for the indicated segment.  Find
!     out how many rows and columns the segment contains.
!
      call zzekmloc ( ctx,  handle,  segno,    p,             mbase   )
      call dasrdi   ( ctx,  handle,  mbase+1,  mbase+sdscsz,  segdsc  )

      nrows  =  segdsc ( nridx )
      ncols  =  segdsc ( ncidx )

!
!     Determine the order in which the columns were added.  The order
!     may differ from that in which the columns were declared.  The
!     ordinal position of each column is stored on the stack right
!     before its address data.  COLORD will map ordinal positions given
!     by a column declaration to ordinal positions on the stack.
!
      do i = 1, ncols

         loc  =  (i-1)*(nrows+1)  +  3

         call zzeksrd ( ctx, loc, loc, colidx_1 )
         colidx = colidx_1(1)

         colord ( colidx ) = i

      end do

!
!     We'll need to create a record pointer structure for each row
!     in the segment.  We compute the number of record pointers that
!     can fit on one page.  We also compute the number of pages we'll
!     need to hold the pointers.
!
      rpsize  =    dptbas + ncols
      nrp     =    ipsize / rpsize
      npage   =  ( nrows + nrp - 1 ) / nrp

!
!     We'll write out record pointers a pageful at a time.  Each
!     record pointer is initialized to indicate that the record is
!     old, and that there is no corresponding modified record.
!
      remain  =  nrows
      recno   =  0

      do i = 1, npage
!
!        Get the base address of the current page.  The address
!        can be derived from the address of the first record pointer
!        structure on the page.
!
         addrss  =  rcptrs(recno+1) + 1

         call zzekpgpg ( ctx, int, addrss, p, pbase )

         ipage(1:ipsize) = 0

!
!        NR is the number of record pointers we'll write to this page.
!
         nr  =  min ( nrp, remain )

         do j = 1, nr
!
!           Initialize the modified record pointer and status for
!           each record pointer on the page.
!
            base                     =  (j-1) * rpsize
            ipage ( base + staidx )  =  old
            ipage ( base + rcpidx )  =  uninit

         end do

!
!        For each column, take NR addresses off the stack and
!        write them into the page.
!
         do col = 1, ncols
!
!           The stack starts out with the target file handle and
!           segment number.  Next comes the data for each column.
!           Each column is identified by its ordinal position.  The
!           addresses for the data of each column follow.  The addresses
!           for each column are stored contiguously.
!
            j       =  colord( col )
            stkbas  =  (j-1)*(nrows+1)  +  3
            loc     =  stkbas           +  recno

            call zzeksrd ( ctx, loc+1, loc+nr, adrbuf )

            do row = 1, nr

               base           =  (row-1) * rpsize
               pagloc         =  base + dptbas + col
               ipage(pagloc)  =  adrbuf(row)

            end do

         end do

!
!        Write out the initialized pointer page.
!
         call zzekpgwi ( ctx, handle, p, ipage )

         recno   =  recno   +  nr
         remain  =  remain  -  nr

      end do

!
!     Create the record pointer tree for this segment.
!
      call zzektrit ( ctx,  handle,  tree                   )
      call zzektr1s ( ctx,  handle,  tree,  nrows,  rcptrs  )

!
!     Update the record tree pointer and row count in the segment
!     descriptor.  Set the records of the last DAS words in use
!     to their maximum values, to ensure allocation of new pages
!     if further writes are done.
!
      call zzekmloc ( ctx,  handle,  segno,        p,            base   )
      call dasudi ( ctx, handle, base+rtidx, base+rtidx, [ tree ] )
      call dasudi ( ctx, handle, base+nridx, base+nridx, [ nrows ] )
      call dasudi ( ctx, handle, base+lcwidx, base+lcwidx, [ cpsize ] )
      call dasudi ( ctx, handle, base+ldwidx, base+ldwidx, [ dpsize ] )
      call dasudi ( ctx, handle, base+liwidx, base+liwidx, [ ipsize ] )

      return
      end subroutine zzekff01

      !> Look up the descriptor of a specified EK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for read or write
      !>                    access.
      !>
      !>     SEGNO          is the number of a segment whose descriptor is
      !>                    desired.  A segment number is simply the ordinal
      !>                    position of the segment in its parent EK.
      !>
      !>## Detailed Output
      !>
      !>     SEGDSC         is the descriptor of the specified segment.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If SEGNO is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     3)  If an I/O error occurs while reading the indicated file,
      !>         the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine centralizes the coded needed to look up the
      !>     descriptor of a specified segment.  This is a frequently
      !>     performed function.
      !>
      !>## Examples
      !>
      !>     See EKACEC.
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
      !>-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
      module subroutine zzeksdsc ( ctx, handle, segno, segdsc )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segno  !! Segment number.
      integer(ip), intent(out) :: segdsc ( sdscsz )  !! Segment descriptor.
!
!     Local variables
!
      integer(ip) :: mp
      integer(ip) :: mbase

      call zzekmloc ( ctx,  handle,  segno,    mp,            mbase   )
      call dasrdi   ( ctx,  handle,  mbase+1,  mbase+sdscsz,  segdsc  )

      return
      end subroutine zzeksdsc

      !> Return general segment information for a specified segment in a
      !> specified EK.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is an EK file handle.  The file may be open for
      !>                    reading or writing.
      !>
      !>     SEGNO          is the number of the segment whose summary is
      !>                    desired.  Segments are numbered from 1 to NSEG,
      !>                    where NSEG is the count of segments in the file.
      !>
      !>## Detailed Output
      !>
      !>     TABNAM         is the name of the table to which the segment
      !>                    belongs.
      !>
      !>     SEGDSC         is an EK segment descriptor.  The contents of this
      !>                    integer array are described in the include file
      !>
      !>                       eksegdsc.inc.
      !>
      !>                    Two commonly used elements of the
      !>                    descriptor are the number of rows in the table
      !>                    and the number of columns in the table.  The
      !>                    indices of these items are given by the parameters
      !>                    NRIDX and NCIDX, respectively.
      !>
      !>     CNAMES         is a list of names of data columns in the segment.
      !>
      !>     CDSCRS         is a list of descriptors of columns in the segment.
      !>                    Elements (1:CDSCSZ,I) of this integer array
      !>                    comprise the descriptor of the Ith column in the
      !>                    segment.  The contents of a column descriptor are
      !>                    listed below.  The parameters shown in the first
      !>                    subscript of CDSCRS are declared in the include
      !>                    file
      !>
      !>                       ekcoldsc.inc.
      !>
      !>                    We recommend using these parameters in any calling
      !>                    routine.
      !>
      !>                       CDSCRS(CLSIDX,I):    Column class
      !>                       CDSCRS(TYPIDX,I):    Data type
      !>                       CDSCRS(LENIDX,I):    String length
      !>                       CDSCRS(SIZIDX,I):    Element size
      !>                       CDSCRS(NAMIDX,I):    Column name base address
      !>                       CDSCRS(IXTIDX,I):    Column index's type code
      !>                       CDSCRS(IXPIDX,I):    Column index's pointer
      !>                       CDSCRS(NULIDX,I):    Null flag
      !>                       CDSCRS(ORDIDX,I):    Column's ordinal position
      !>                                            in parent table
      !>                       CDSCRS(METIDX,I):    Column's integer metadata
      !>                                            pointer
      !>                       CDSCRS(11,I):        Reserved.
      !>
      !>                    Notes:
      !>
      !>                       1) Element 3 applies only to character columns.
      !>
      !>                          Element 3 takes the boolean value IFALSE
      !>                          if the column contains variable-length
      !>                          strings.
      !>
      !>                          The boolean parameter IFALSE is represented
      !>                          by the integer -1.
      !>
      !>                       2) Element 4 takes the boolean value IFALSE
      !>                          if the column contains variable-size
      !>                          arrays.
      !>
      !>                       3) Element 6 takes the value IFALSE if the
      !>                          column is not indexed.
      !>
      !>                       4) Element 8 takes the value IFALSE if null
      !>                          values are not allowed in the column.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The output arguments will not be
      !>         modified.
      !>
      !>     2)  If SEGNO is not the index of an existing segment in the
      !>         specified file, the error SPICE(INDEXOUTOFRANGE) will be
      !>         signaled.  The output arguments will not be modified.
      !>
      !>     3)  If an I/O error occurs while attempting to obtain summary
      !>         information for the specified segment, the error will be
      !>         diagnosed by routines called by this routine.  The output
      !>         arguments may be modified in this case.
      !>
      !>## Files
      !>
      !>     See the description of HANDLE in $Detailed_Input.
      !>
      !>## Particulars
      !>
      !>     This routine supports the function of summarizing a binary
      !>     EK file, allowing NAIF Toolkit users to determine whether it
      !>     contains data of interest.  The routine also also provides
      !>     address information necessary to retrieve information from the
      !>     segment.
      !>
      !>## Examples
      !>
      !>     1)  Dump the table and column names of the segments in an EK.
      !>
      !>            C
      !>            C     Open the EK for read access and get the number of
      !>            C     segments it
      !>            C     contains.
      !>            C
      !>                  CALL EKOPR ( EKNAME, HANDLE )
      !>
      !>                  NSEG = EKNSEG ( HANDLE )
      !>
      !>            C
      !>            C     Loop through the segments, dumping the desired
      !>            C     summary information for each one.
      !>            C
      !>                  WRITE (*,*) ' '
      !>                  WRITE (*,*) ' '
      !>                  WRITE (*,*) 'Segment summary for file ', EKNAME
      !>                  WRITE (*,*) ' '
      !>                  WRITE (*,*) ' '
      !>
      !>                  DO I = 1, NSEG
      !>
      !>                     CALL ZZEKSINF ( HANDLE,  I,      TABNAM,
      !>                 .                   SEGDSC,  CNAMES, CDSCRS  )
      !>
      !>                     WRITE (*,*)
      !>                 .   '========================================'      //
      !>                 .   '========================================'
      !>
      !>                     WRITE (*,*) 'Table containing segment: ', TABNAM
      !>
      !>                     WRITE (*,*) ' '
      !>                     WRITE (*,*) 'Column names: '
      !>                     WRITE (*,*) ' '
      !>
      !>                     DO J = 1, SEGDSC(NCIDX)
      !>
      !>                        WRITE (*,*) '   '//CNAMES(J)
      !>
      !>                     END DO
      !>
      !>                     WRITE (*,*)
      !>                 .   '========================================'      //
      !>                 .   '========================================'
      !>
      !>                  END DO
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
      !>-    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    Beta Version 1.1.0, 03-JUL-1996 (NJB)
      !>
      !>        Bug fix:  table and column names are now padded with trailing
      !>        blanks on output if necessary.
      !>
      !>-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 1.1.0, 03-JUL-1996 (NJB)
      !>
      !>        Bug fix:  table and column names are now padded with trailing
      !>        blanks on output if necessary.  Previously, if the caller
      !>        declared these variables with string lengths longer than
      !>        TNAMSZ and CNAMSZ respectively, the trailing characters
      !>        at positions past those designated by these parameters were
      !>        left unassigned on output.
      module subroutine zzeksinf ( ctx, handle, segno, tabnam, segdsc, cnames, cdscrs )
      use pepper_kinds, only: ip
      use pepper_inc_ekcnamsz
      use pepper_inc_ekcoldsc
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of EK.
      integer(ip), intent(in) :: segno  !! Number of segment to be summarized.
      character(*), intent(out) :: tabnam  !! Name of table containing segment.
      integer(ip), intent(out) :: segdsc ( sdscsz    )  !! Segment descriptor.
      character(*), intent(out) :: cnames (         * )  !! Names of columns in segment.
      integer(ip), intent(out) :: cdscrs ( cdscsz, * )  !! Descriptors of columns in segment.
!
!     Local variables
!
      integer(ip) :: base
      integer(ip) :: i
      integer(ip) :: ncols
      integer(ip) :: nseg
      integer(ip) :: p

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Verify that the target file is a paged DAS EK open for read
!     access, or we can't summarize the file.
!
      call zzekpgch ( ctx, handle, 'READ' )

      if ( ctx%failed() ) return

!
!     Find out how many segments are in the file, so we can check
!     the index for validity.
!
      nseg = eknseg ( ctx, handle )

      if (  ( segno < 1 ) .or. ( segno > nseg )  ) then

         call ctx%setmsg ( 'Segment index was #; valid range is 1:#' )
         call ctx%errint ( '#',  segno                               )
         call ctx%errint ( '#',  nseg                                )
         call ctx%sigerr ( 'SPICE(INDEXOUTOFRANGE)'                  )
         return

      end if

!
!     We're ready to proceed.  The first step is to find the
!     segment's metadata location and look up the segment descriptor.
!
      call zzekmloc ( ctx, handle, segno,  p,           base   )
      call dasrdi   ( ctx, handle, base+1, base+sdscsz, segdsc )

!
!     Get the table name.  The table's base address is in the segment
!     descriptor.
!
      call dasrdc ( ctx, handle, &
                    segdsc(tnmidx) + 1, &
                    segdsc(tnmidx) + tnamsz, &
                    1, &
                    tnamsz, &
                    tabnam                   )

      if (  len( tabnam )  >  tnamsz  ) then
         tabnam (tnamsz+1:) = ' '
      end if

!
!     Read the column descriptors.  The first one starts at DAS
!     integer address
!
!        BASE + CDOFF + 1.
!
      ncols   =  segdsc(ncidx)

      call dasrdi ( ctx, handle, &
                    base + cdoff + 1, &
                    base + cdoff + ncols*cdscsz, &
                    cdscrs                        )

!
!     Now read the column names into the names array.
!
      call dasrdc ( ctx, handle, &
                    segdsc(nmlidx) + 1, &
                    segdsc(nmlidx) + ncols*cnamsz, &
                    1, &
                    cnamsz, &
                    cnames  )

      if (   len( cnames(1) )  >  cnamsz  ) then

         do i = 1, ncols
            cnames(i)(cnamsz+1:) = ' '
         end do

      end if

!
!     All output arguments are set, or else FAILED() is .TRUE.
!

      return
      end subroutine zzeksinf

      !> Append an item to a tree.  The key indicating the location of
      !> the new item is returned.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     TREE           is the root node number of the tree of interest.
      !>
      !>     VALUE          is an integer value to be appended to the
      !>                    specified tree.
      !>
      !>## Detailed Output
      !>
      !>     KEY            is an absolute key indicating the insertion
      !>                    location.  In EK trees, absolute keys are just
      !>                    ordinal positions relative to the leftmost element
      !>                    of the tree, with the leftmost element having
      !>                    position 1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine appends a new value to an EK tree; this action is
      !>     equivalent to inserting the value at position (NKEYS+1), where
      !>     NKEYS is the number of keys in the tree prior to the insertion.
      !>
      !>     The tree is balanced and satisfies all invariants at the
      !>     completion of the appending.
      !>
      !>## Examples
      !>
      !>     See EKBSEG.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Literature References
      !>
      !>     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
      !>         3/Sorting and Searching" 1973, pp 471-479.
      !>
      !>         EK trees are closely related to the B* trees described by
      !>         Knuth.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.2, 22-SEP-2004 (EDW)
      !>
      !>        Edited 1.0.1 Version entry to not include
      !>        the token used to mark the $Procedure section.
      !>
      !>-    Beta Version 1.0.1, 14-OCT-1996 (NJB)
      !>
      !>        $Procedure line was corrected.
      !>
      !>-    Beta Version 1.0.0, 22-OCT-1995 (NJB)
      module subroutine zzektrap ( ctx, handle, tree, value, key )
      use pepper_kinds, only: ip
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: tree  !! Root of tree.
      integer(ip), intent(in) :: value  !! Value to append.
      integer(ip), intent(out) :: key  !! Key pointing to new value.
      key  =  zzektrsz ( ctx, handle, tree )   +   1

      call zzektrin ( ctx, handle, tree, key, value )

      end subroutine zzektrap

      !> Complete a fast write operation on a new E-kernel segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is the handle of an EK file that is open for writing.
      !>              A "begin segment for fast write" operation must
      !>              have already been performed for the designated
      !>              segment.
      !>
      !>     SEGNO    is the number of the segment to complete.
      !>
      !>     RCPTRS   is an array of record pointers for the input
      !>              segment. This array is obtained as an output
      !>              from EKIFLD, the routine called to initiate a
      !>              fast write.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>     See the $Particulars section for a description of the
      !>     effects of this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, an error is signaled by a routine in the
      !>         call tree of this routine.
      !>
      !>     2)  If an attempt is made to finish a segment other than the one
      !>         last initialized by EKIFLD, an error is signaled by a routine
      !>         in the call tree of this routine.
      !>
      !>     3)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error is signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading ek.req for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine completes an EK segment after the data has been
      !>     written via the fast column writer routines. The segment must
      !>     have been created by a call to EKIFLD. The fast column
      !>     writer routines are:
      !>
      !>        EKACLC {EK, add column, character}
      !>        EKACLD {EK, add column, double precision}
      !>        EKACLI {EK, add column, integer}
      !>
      !>     The segment is not guaranteed to be readable until all columns
      !>     have been added. After the columns have been added, the segment
      !>     may be extended by inserting more records and filling in those
      !>     records using the EKACEx routines.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Suppose we want to create an Sequence Component E-kernel
      !>        named 'ekffld_ex1.bes' which contains records of orders for
      !>        data products. The E-kernel has a table called DATAORDERS that
      !>        consists of the set of columns listed below:
      !>
      !>           DATAORDERS
      !>
      !>              Column Name     Data Type
      !>              -----------     ---------
      !>              ORDER_ID        INTEGER
      !>              CUSTOMER_ID     INTEGER
      !>              LAST_NAME       CHARACTER*(*)
      !>              FIRST_NAME      CHARACTER*(*)
      !>              ORDER_DATE      TIME
      !>              COST            DOUBLE PRECISION
      !>
      !>        The order database also has a table of items that have been
      !>        ordered. The columns of this table are shown below:
      !>
      !>           DATAITEMS
      !>
      !>              Column Name     Data Type
      !>              -----------     ---------
      !>              ITEM_ID         INTEGER
      !>              ORDER_ID        INTEGER
      !>              ITEM_NAME       CHARACTER*(*)
      !>              DESCRIPTION     CHARACTER*(*)
      !>              PRICE           DOUBLE PRECISION
      !>
      !>        The file "ekffld_ex1.bdb" will contain two segments, the first
      !>        containing the DATAORDERS table and the second containing the
      !>        DATAITEMS table.
      !>
      !>        This example demonstrates how to open a new EK file and create
      !>        the first of the segments described above.
      !>
      !>        Use the LSK kernel below to load the leap seconds and time
      !>        constants required for the conversions.
      !>
      !>           naif0012.tls
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM EKFFLD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Include the EK Column Name Size (CNAMSZ)
      !>C
      !>      INCLUDE 'ekcnamsz.inc'
      !>
      !>C
      !>C     Local parameters
      !>C
      !>      CHARACTER*(*)         LSK
      !>      PARAMETER           ( LSK    = 'naif0012.tls' )
      !>
      !>      CHARACTER*(*)         TABLE
      !>      PARAMETER           ( TABLE  = 'DATAORDERS'   )
      !>
      !>      INTEGER               DECLEN
      !>      PARAMETER           ( DECLEN = 200 )
      !>
      !>      INTEGER               FNMLEN
      !>      PARAMETER           ( FNMLEN = 50  )
      !>
      !>      INTEGER               LNMLEN
      !>      PARAMETER           ( LNMLEN = 50  )
      !>
      !>      INTEGER               NAMLEN
      !>      PARAMETER           ( NAMLEN = 40  )
      !>
      !>      INTEGER               NCOLS
      !>      PARAMETER           ( NCOLS  = 6   )
      !>
      !>      INTEGER               NROWS
      !>      PARAMETER           ( NROWS  = 9   )
      !>
      !>      INTEGER               UTCLEN
      !>      PARAMETER           ( UTCLEN = 30  )
      !>
      !>C
      !>C     Local variables
      !>C
      !>      CHARACTER*(DECLEN)    CDECLS ( NCOLS )
      !>      CHARACTER*(32)        CNAMES ( NCOLS )
      !>      CHARACTER*(FNMLEN)    FNAMES ( NROWS )
      !>      CHARACTER*(LNMLEN)    LNAMES ( NROWS )
      !>      CHARACTER*(NAMLEN)    IFNAME
      !>      CHARACTER*(UTCLEN)    ODATE
      !>
      !>      DOUBLE PRECISION      COSTS  ( NROWS )
      !>      DOUBLE PRECISION      ETS    ( NROWS )
      !>
      !>      INTEGER               CSTIDS ( NROWS )
      !>      INTEGER               HANDLE
      !>      INTEGER               I
      !>      INTEGER               NRESVC
      !>      INTEGER               ORDIDS ( NROWS )
      !>      INTEGER               RCPTRS ( NROWS )
      !>      INTEGER               SEGNO
      !>      INTEGER               SIZES  ( NROWS )
      !>      INTEGER               WKINDX ( NROWS )
      !>
      !>      LOGICAL               NLFLGS ( NROWS )
      !>
      !>C
      !>C     Load a leapseconds kernel for UTC/ET conversion.
      !>C
      !>      CALL FURNSH ( 'naif0012.tls' )
      !>
      !>C
      !>C     Open a new EK file.  For simplicity, we will not
      !>C     reserve any space for the comment area, so the
      !>C     number of reserved comment characters is zero.
      !>C     The variable IFNAME is the internal file name.
      !>C
      !>      NRESVC  =  0
      !>      IFNAME  =  'Test EK/Created 20-SEP-1995'
      !>
      !>      CALL EKOPN ( 'ekffld_ex1.bes', IFNAME, NRESVC, HANDLE )
      !>
      !>C
      !>C     Set up the table and column names and declarations
      !>C     for the DATAORDERS segment.  We'll index all of
      !>C     the columns.  All columns are scalar, so we omit
      !>C     the size declaration.  Only the COST column may take
      !>C     null values.
      !>C
      !>      CNAMES(1) =  'ORDER_ID'
      !>      CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE'
      !>
      !>      CNAMES(2) =  'CUSTOMER_ID'
      !>      CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE'
      !>
      !>      CNAMES(3) =  'LAST_NAME'
      !>      CDECLS(3) =  'DATATYPE = CHARACTER*(*),' //
      !>     .             'INDEXED  = TRUE'
      !>
      !>      CNAMES(4) =  'FIRST_NAME'
      !>      CDECLS(4) =  'DATATYPE = CHARACTER*(*),' //
      !>     .             'INDEXED  = TRUE'
      !>
      !>      CNAMES(5) =  'ORDER_DATE'
      !>      CDECLS(5) =  'DATATYPE = TIME, INDEXED  = TRUE'
      !>
      !>      CNAMES(6) =  'COST'
      !>      CDECLS(6) =  'DATATYPE = DOUBLE PRECISION,' //
      !>     .             'INDEXED  = TRUE,'             //
      !>     .             'NULLS_OK = TRUE'
      !>
      !>C
      !>C     Start the segment.  We presume the number of  rows
      !>C     of data is known in advance.
      !>C
      !>      CALL EKIFLD ( HANDLE,  TABLE,  NCOLS, NROWS,
      !>     .              CNAMES,  CDECLS, SEGNO, RCPTRS )
      !>
      !>C
      !>C     At this point, arrays containing data for the
      !>C     segment's columns may be filled in.  The names
      !>C     of the data arrays are shown below.
      !>C
      !>C        Column           Data array
      !>C
      !>C        'ORDER_ID'       ORDIDS
      !>C        'CUSTOMER_ID'    CSTIDS
      !>C        'LAST_NAME'      LNAMES
      !>C        'FIRST_NAME'     FNAMES
      !>C        'ORDER_DATE'     ETS
      !>C        'COST'           COSTS
      !>C
      !>      DO I = 1, NROWS
      !>
      !>         ORDIDS(I) = I
      !>         CSTIDS(I) = I * 100
      !>         COSTS(I)  = I * 100.D0
      !>
      !>         CALL REPMI ( 'Order # Customer first name',
      !>     .                '#', I, FNAMES(I)             )
      !>         CALL REPMI ( 'Order # Customer last name',
      !>     .                '#', I, LNAMES(I)             )
      !>         CALL REPMI ( '1998 Mar #', '#', I, ODATE   )
      !>
      !>         CALL UTC2ET ( ODATE,  ETS(I) )
      !>
      !>         NLFLGS(I) = .FALSE.
      !>
      !>      END DO
      !>
      !>      NLFLGS(2) = .TRUE.
      !>
      !>C
      !>C     The SIZES array shown below is ignored for scalar
      !>C     and fixed-size array columns, so we need not
      !>C     initialize it.  For variable-size arrays, the
      !>C     Ith element of the SIZES array must contain the size
      !>C     of the Ith column entry in the column being written.
      !>C     Normally, the SIZES array would be reset for each
      !>C     variable-size column.
      !>C
      !>C     The NLFLGS array indicates which entries are null.
      !>C     It is ignored for columns that don't allow null
      !>C     values.  In this case, only the COST column allows
      !>C     nulls.
      !>C
      !>C     Add the columns of data to the segment.  All of the
      !>C     data for each column is written in one shot.
      !>C
      !>      CALL EKACLI ( HANDLE, SEGNO,  'ORDER_ID',
      !>     .              ORDIDS, SIZES,  NLFLGS,  RCPTRS, WKINDX )
      !>
      !>      CALL EKACLI ( HANDLE, SEGNO,  'CUSTOMER_ID',
      !>     .              CSTIDS, SIZES,  NLFLGS,  RCPTRS, WKINDX )
      !>
      !>      CALL EKACLC ( HANDLE, SEGNO,  'LAST_NAME',
      !>     .              LNAMES, SIZES,  NLFLGS,  RCPTRS, WKINDX )
      !>
      !>      CALL EKACLC ( HANDLE, SEGNO,  'FIRST_NAME',
      !>     .              FNAMES, SIZES,  NLFLGS,  RCPTRS, WKINDX )
      !>
      !>      CALL EKACLD ( HANDLE, SEGNO,  'ORDER_DATE',
      !>     .              ETS,    SIZES,  NLFLGS,  RCPTRS, WKINDX )
      !>
      !>      CALL EKACLD ( HANDLE, SEGNO,  'COST',
      !>     .              COSTS,  SIZES,  NLFLGS,  RCPTRS, WKINDX )
      !>
      !>C
      !>C     Complete the segment.  The RCPTRS array is that
      !>C     returned by EKIFLD.
      !>C
      !>      CALL EKFFLD ( HANDLE, SEGNO, RCPTRS )
      !>
      !>C
      !>C     At this point, the second segment could be
      !>C     created by an analogous process.  In fact, the
      !>C     second segment could be created at any time; it is
      !>C     not necessary to populate the first segment with
      !>C     data before starting the second segment.
      !>C
      !>C     The file must be closed by a call to EKCLS.
      !>C
      !>      CALL EKCLS ( HANDLE )
      !>
      !>      END
      !>```
      !>
      !>        When this program is executed, no output is presented on
      !>        screen. After run completion, a new EK file exists in the
      !>        output directory.
      !>
      !>## Restrictions
      !>
      !>     1)  Only one segment can be created at a time using the fast
      !>         write routines.
      !>
      !>     2)  No other EK operation may interrupt a fast write. For
      !>         example, it is not valid to issue a query while a fast write
      !>         is in progress.
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
      !>-    SPICELIB Version 1.2.0, 24-NOV-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. and
      !>        created complete code example from existing fragment.
      !>
      !>-    SPICELIB Version 1.1.2, 09-JAN-2002 (NJB)
      !>
      !>        Documentation change: instances of the phrase "fast load"
      !>        were replaced with "fast write."
      !>
      !>-    SPICELIB Version 1.1.1, 18-JUN-1999 (WLT)
      !>
      !>        Corrected CHKOUT value to be same as CHKIN.
      !>
      !>-    SPICELIB Version 1.0.1, 31-MAR-1998 (NJB)
      !>
      !>        Made miscellaneous header corrections.
      !>
      !>-    SPICELIB Version 1.0.0, 08-NOV-1995 (NJB)
      module subroutine ekffld ( ctx, handle, segno, rcptrs )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segno  !! Segment number.
      integer(ip), intent(inout) :: rcptrs ( * )  !! Record pointers. (PEPPER: changed by the routine, an input in SPICELIB's table)

      integer(ip) :: segdsc ( sdscsz )
      integer(ip) :: stype

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Read in the segment descriptor, and get the segment's type.
!
      call zzeksdsc ( ctx,  handle,  segno,  segdsc )

      stype  =  segdsc(ektidx)

!
!     Complete the fast write preparations appropriate to the segment's
!     type.
!
      if ( stype == 1 ) then

         call zzekff01 ( ctx, handle, segno, rcptrs )

      else if ( stype == 2 ) then
!
!        Currently, no actions are taken to complete a type 2 segment.
!

      else

         call ctx%setmsg ( 'Segment type # is not currently supported.' )
         call ctx%errint ( '#', stype                                   )
         call ctx%sigerr ( 'SPICE(BUG)'                                 )
         return

      end if

      return
      end subroutine ekffld

      !> Return summary information for a specified segment in a
      !> specified EK.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE   is an EK file handle specifying the EK containing
      !>              the segment to be summarized.
      !>
      !>     SEGNO    is the number of the segment whose summary is
      !>              desired. Segments are numbered from 1 to NSEG,
      !>              where NSEG is the count of segments in the file.
      !>
      !>## Detailed Output
      !>
      !>     TABNAM   is the name of the table to which the segment
      !>              belongs.
      !>
      !>     NROWS    is the number of rows in the segment.
      !>
      !>     NCOLS    is the number of columns in the segment. The
      !>              maximum number of columns in a segment is given
      !>              by the parameter MXCLSG, which is defined in the
      !>              include file
      !>
      !>                 ekglimit.inc.
      !>
      !>              Currently, this limit is set at 100 columns.
      !>
      !>     CNAMES   is an array of names of columns in the segment.
      !>
      !>     DTYPES   is an array of data types of columns in the
      !>              segment. Each data type is indicated by a short
      !>              character string. The strings and their meanings
      !>              are:
      !>
      !>                 'CHR'       Character type.
      !>                 'DP'        Double precision type.
      !>                 'INT'       Integer type.
      !>                 'TIME'      Time type.
      !>
      !>              The Ith element of DTYPES corresponds to the
      !>              column whose name is the Ith element of CNAMES.
      !>
      !>     SIZES    is an array of declared sizes of column entries.
      !>              The Ith element of SIZES is the declared size of
      !>              the column whose name is the Ith element of CNAMES.
      !>              Scalar-valued columns have size 1; fixed-size,
      !>              array-valued columns have size greater than 1.
      !>              Array valued columns of variable size have a size
      !>              value of -1.
      !>
      !>     STRLNS   is an array of declared string lengths of
      !>              character column entries. These lengths are
      !>              defined only for columns of character type.
      !>              The Ith element of SIZES is the declared size of
      !>              the column whose name is the Ith element of CNAMES,
      !>              if that column has character type; otherwise, the
      !>              Ith element of STRLNS is undefined. For
      !>              character columns having variable string length,
      !>              the returned value of STRLNS is -1.
      !>
      !>     INDEXD   is an array of logical flags indicating whether the
      !>              corresponding columns are indexed. The Ith element
      !>              of INDEXD applies to the column whose name is the
      !>              Ith element of CNAMES.
      !>
      !>     NULLOK   is an array of logical flags indicating whether the
      !>              corresponding columns allow null values. The Ith
      !>              element of NULLOK applies to the column whose name
      !>              is the Ith element of CNAMES.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, an error is signaled by a routine in the
      !>         call tree of this routine. The output arguments will not be
      !>         modified.
      !>
      !>     2)  If SEGNO is not the index of an existing segment in the
      !>         specified file, the error SPICE(INDEXOUTOFRANGE) is signaled.
      !>         The output arguments will not be modified.
      !>
      !>     3)  If an I/O error occurs while attempting to obtain summary
      !>         information for the specified segment, the error is signaled
      !>         by a routine in the call tree of this routine. The output
      !>         arguments may be modified in this case.
      !>
      !>## Files
      !>
      !>     See the description of HANDLE in $Detailed_Input.
      !>
      !>## Particulars
      !>
      !>     This routine supports the function of summarizing a binary
      !>     EK file, allowing NAIF Toolkit users to determine whether it
      !>     contains data of interest. The routine also also provides
      !>     address information necessary to retrieve information from the
      !>     segment.
      !>
      !>## Examples
      !>
      !>     1)  Dump the table and column names of the segments in an EK.
      !>
      !>            C
      !>            C     Open the EK for read access and get the number of
      !>            C     segments it contains.
      !>            C
      !>                  CALL EKOPR ( EKNAME, HANDLE )
      !>
      !>                  NSEG = EKNSEG ( HANDLE )
      !>
      !>            C
      !>            C     Loop through the segments, dumping the desired
      !>            C     summary information for each one.
      !>            C
      !>                  WRITE (*,*) ' '
      !>                  WRITE (*,*) ' '
      !>                  WRITE (*,*) 'Segment summary for file ', EKNAME
      !>                  WRITE (*,*) ' '
      !>                  WRITE (*,*) ' '
      !>
      !>                  DO I = 1, NSEG
      !>
      !>                     CALL EKSSUM (  HANDLE,  SEGNO,   TABNAM,  NROWS,
      !>                 .                  NCOLS,   CNAMES,  DTYPES,  SIZES,
      !>                 .                  STRLNS,  INDEXD,  NULLOK         )
      !>
      !>                     WRITE (*,*)
      !>                 .   '========================================'      //
      !>                 .   '========================================'
      !>
      !>                     WRITE (*,*) 'Table containing segment: ', TABNAM
      !>
      !>                     WRITE (*,*) ' '
      !>                     WRITE (*,*) 'Number of rows:     ', NROWS
      !>                     WRITE (*,*) 'Number of columns:  ', NCOLS
      !>                     WRITE (*,*) ' '
      !>                     WRITE (*,*) 'Column names and attributes: '
      !>                     WRITE (*,*) ' '
      !>
      !>                     DO J = 1, NCOLS
      !>
      !>                        WRITE (*,*) 'Column:   '//CNAMES(J)
      !>                        WRITE (*,*) ' '
      !>                        WRITE (*,*) 'Data type: ', DTYPES(J)
      !>                        WRITE (*,*) 'Dimension: ', SIZES(J)
      !>
      !>                        IF ( DTYPES(J) .EQ. 'CHR' ) THEN
      !>                           WRITE (*,*) 'String length: ', STRLNS(J)
      !>                        END IF
      !>
      !>                        IF ( INDEXD(J) ) THEN
      !>                           WRITE (*,*) 'Indexed'
      !>                        END IF
      !>
      !>                        IF ( NULLOK(J) ) THEN
      !>                           WRITE (*,*) 'Nulls allowed'
      !>                        ELSE
      !>                           WRITE (*,*) 'Nulls not allowed'
      !>                        END IF
      !>
      !>                        WRITE (*,*) ' '
      !>                     END DO
      !>
      !>                     WRITE (*,*)
      !>                 .   '========================================'      //
      !>                 .   '========================================'
      !>
      !>                  END DO
      !>
      !>                  END
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
      !>-    SPICELIB Version 1.2.0, 02-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed previous
      !>        version number.
      !>
      !>-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
      !>
      !>        Bug fix: correct parameter is now used to set dimension
      !>        of local variable SEGDSC.
      !>
      !>-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB)
      !>
      !>        Bug fix: correct parameter SDSCSZ is now used to set dimension
      !>        of local variable SEGDSC. Previously, the parameter
      !>        CDSCSZ had been used.
      module subroutine ekssum ( ctx, handle, segno, tabnam, nrows, ncols, cnames, dtypes, sizes, strlns, indexd, nullok )
      use pepper_kinds, only: ip
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekglimit
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! Handle of EK.
      integer(ip), intent(in) :: segno  !! Number of segment to be summarized.
      character(*), intent(out) :: tabnam  !! Name of table containing segment.
      integer(ip), intent(out) :: nrows  !! Number of rows in segment.
      integer(ip), intent(out) :: ncols  !! Number of columns in segment.
      character(*), intent(out) :: cnames ( * )  !! Names of columns in segment.
      character(*), intent(out) :: dtypes ( * )  !! Data types of columns in segment.
      integer(ip), intent(out) :: sizes  ( * )  !! Entry sizes of columns in segment.
      integer(ip), intent(out) :: strlns ( * )  !! String lengths of columns in segment.
      logical, intent(out) :: indexd ( * )  !! Flags indicating whether columns are indexed.
      logical, intent(out) :: nullok ( * )  !! Flags indicating whether columns allow nulls.

      integer(ip), parameter :: ntypes = 4

      integer(ip), parameter :: short = 4

      character(short), parameter :: typstr ( ntypes ) = &
         [ character(short) :: 'CHR', 'DP', 'INT', 'TIME' ]

      integer(ip) :: cdscrs ( cdscsz, mxclsg )
      integer(ip) :: i
      integer(ip) :: segdsc ( sdscsz )

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Get the info from a knowledgeable source.
!
      call zzeksinf ( ctx, handle, segno, tabnam, segdsc, cnames, cdscrs )

      if ( ctx%failed() ) return

      nrows  =  segdsc ( nridx )
      ncols  =  segdsc ( ncidx )

      do i = 1, ncols

         dtypes(i)  =  typstr (  cdscrs(typidx,i)  )

         sizes (i)  =  cdscrs(sizidx,i)

         if ( cdscrs(typidx,i) == chr ) then
            strlns(i)  =  cdscrs(lenidx,i)
         else
            strlns(i)  =  0
         end if

         indexd(i)  =  cdscrs(ixtidx,i) /= ifalse
         nullok(i)  =  cdscrs(nflidx,i) /= ifalse

      end do

      return
      end subroutine ekssum

      !> Allocate a series of contiguous data pages for a specified EK
      !> segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     SEGDSC         is the descriptor of the segment for which to
      !>                    allocate a series of data pages.
      !>
      !>     TYPE           is the data type of the desired pages.
      !>
      !>     N              is the number of pages desired.  All pages
      !>                    allocated are new.   A new page is one that has not
      !>                    been allocated before.
      !>
      !>## Detailed Output
      !>
      !>     P              is the number of the first page of the allocated
      !>                    series.  The rest of the pages have numbers
      !>
      !>                       P+1, P+2, ... , P+N-1
      !>
      !>                    These numbers are recognized by the EK paged access
      !>                    routines.
      !>
      !>     BASE           is the DAS base address of the first allocated
      !>                    page.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.  The file may be corrupted.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects: it allocates a series of
      !>     new, contiguous EK data pages for a specified segment. The
      !>     segment's metadata are updated to reflect acquisition of the
      !>     pages.
      !>
      !>     This routine, not ZZEKAPS, should be used when contiguous pages
      !>     are required.
      !>
      !>     Each allocated page is initialized as follows:
      !>
      !>        - The page's link count is zeroed out.
      !>
      !>        - The page's forward pointer is zeroed out.
      !>
      !>     After all pages are allocated, the metadata for the segment are
      !>     adjusted to reflect ownership of the allocated pages.
      !>
      !>     The changes made by this routine to the target EK file become
      !>     permanent when the file is closed.  Failure to close the file
      !>     properly will leave it in an indeterminate state.
      !>
      !>## Examples
      !>
      !>     See ZZEKWPAI.
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
      !>-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
      module subroutine zzekacps ( ctx, handle, segdsc, type, n, p, base )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip), intent(in) :: type  !! Data type of page.
      integer(ip), intent(in) :: n  !! Number of pages to allocate.
      integer(ip), intent(out) :: p  !! Page number.
      integer(ip), intent(out) :: base  !! DAS base address of page.
!
!     Local variables
!
      integer(ip) :: b
      integer(ip) :: i
      integer(ip) :: idx
      integer(ip) :: p2
      integer(ip) :: tree

!
!     Allocate the pages.
!
      call zzekpgan ( ctx, handle, type, p, base )

      do i = 2, n
         call zzekpgan ( ctx, handle, type, p2, b )
      end do

      if ( ctx%failed() ) return

!
!     Initialize the pages.
!
      do i = 1, n
!
!        Zero out the page's link count and forward pointer.
!
         call zzekslnk ( ctx, handle, type, p+i-1, 0 )
         call zzeksfwd ( ctx, handle, type, p+i-1, 0 )

      end do

!
!     Update the segment's metadata.  Insert the number of each new
!     page into the page tree of the appropriate data type.
!
      if ( type == chr ) then

         tree  =  segdsc ( cptidx )

      else if ( type == dp ) then

         tree  =  segdsc ( dptidx )

      else
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, one of the allocation routines would have
!        complained.
!
         tree  =  segdsc ( iptidx )

      end if

      do i = 1, n
         call zzektrap ( ctx, handle, tree, p+i-1, idx )
      end do

      return
      end subroutine zzekacps

      !> Allocate a data page for a specified EK segment.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     SEGDSC         is the descriptor of the segment for which to
      !>                    allocate a data page.
      !>
      !>     TYPE           is the data type of the desired page.
      !>
      !>     NEW            is a logical flag indicating whether a new page
      !>                    is desired.  A new page is one that has not been
      !>                    allocated before.  If NEW is .FALSE., a page
      !>                    on the free list may be returned.
      !>
      !>## Detailed Output
      !>
      !>     P              is the page number of the allocated page.  This
      !>                    number is recognized by the EK paged access
      !>                    routines.
      !>
      !>     BASE           is the DAS base address of the allocated page.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.  The file may be corrupted.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects:  it allocates an EK data
      !>     page for a specified segment.  The segment's metadata is updated
      !>     to reflect acquisition of the page.
      !>
      !>     The allocated page is initialized as follows:
      !>
      !>        - The page's link count is zeroed out.
      !>
      !>        - The page's forward pointer is zeroed out.
      !>
      !>        - The metadata for the segment is adjusted to reflect ownership
      !>          of the allocated page.
      !>
      !>     The changes made by this routine to the target EK file become
      !>     permanent when the file is closed.  Failure to close the file
      !>     properly will leave it in an indeterminate state.
      !>
      !>## Examples
      !>
      !>     See EKIFLD.
      !>
      !>## Restrictions
      !>
      !>     1)  This routine cannot be used to allocate series of contiguous
      !>         pages!  Use ZZEKACPS if contiguous pages are required.
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
      !>-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
      !>
      !>        Corrected typo in comments.
      !>
      !>-    Beta Version 1.0.0, 08-NOV-1995 (NJB)
      module subroutine zzekaps ( ctx, handle, segdsc, type, new, p, base )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segdsc ( sdscsz )  !! Segment descriptor.
      integer(ip), intent(in) :: type  !! Data type of page.
      logical, intent(in) :: new  !! Flag indicating whether page is new.
      integer(ip), intent(out) :: p  !! Page number.
      integer(ip), intent(out) :: base  !! DAS base address of page.
!
!     Local variables
!
      integer(ip) :: idx
      integer(ip) :: tree

      if ( new ) then
!
!        We must allocate a new page.
!
         call zzekpgan ( ctx, handle, type, p, base )

      else
!
!        We can allocate a page from the free list if one is available.
!        Otherwise take a new page.
!
         call zzekpgal ( ctx, handle, type, p, base )

      end if

      if ( ctx%failed() ) return

!
!     Zero out the page's link count and forward pointer.
!
      call zzekslnk ( ctx, handle, type, p, 0 )
      call zzeksfwd ( ctx, handle, type, p, 0 )

!
!     Update the segment's metadata.  For type 1 segments,
!     the new page into the page tree of the appropriate data type.
!
      if ( type == chr ) then

         tree  =  segdsc ( cptidx )

      else if ( type == dp ) then

         tree  =  segdsc ( dptidx )

      else if ( type == int ) then
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, one of the allocation routines would have
!        complained.
!
         tree  =  segdsc ( iptidx )

      end if

      call zzektrap ( ctx, handle, tree, p, idx )

      return
      end subroutine zzekaps

      !> Start a new type 1 segment in an E-kernel.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         the handle of an EK file that is open for writing.
      !>
      !>     TABNAM         is the name of the EK table to which the current
      !>                    segment belongs.  All segments in the EK file
      !>                    designated by HANDLE must have identical column
      !>                    attributes. TABNAM must not exceed 32 characters
      !>                    in length.  Case is not significant.  Table names
      !>                    must start with a letter and contain only
      !>                    characters from the set {A-Z,a-z,0-9,$,_}.
      !>
      !>     NCOLS          is the number of columns in a new segment.
      !>
      !>     CNAMES,
      !>     CDSCRS         are, respectively, and array of column names and
      !>                    their corresponding descriptors:  the Ith element
      !>                    of CNAMES and the Ith descriptor apply to
      !>                    the Ith column in the segment.
      !>
      !>## Detailed Output
      !>
      !>     SEGNO          is the number of the segment created by this
      !>                    routine.  Segment numbers are used as unique
      !>                    identifiers by other EK access routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects:  it prepares an EK for
      !>     the addition of a new type 1 segment.  Type 1 segments are
      !>     `ordinary' in the sense that they support record insertion,
      !>     column entry update, and record deletion operations.
      !>
      !>     Type 1 segments may contains columns of class 1 through 6.
      !>
      !>     By way of contrast, type 2 segments have fixed record counts.
      !>
      !>## Examples
      !>
      !>     See EKBSEG.
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
      !>-    Beta Version 1.0.0, 17-NOV-1995 (NJB)
      module subroutine zzekbs01 ( ctx, handle, tabnam, ncols, cnames, cdscrs, segno )
      use pepper_kinds, only: ip
      use pepper_strings, only: ucase, ucase_inplace
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_ekdatpag
      use pepper_inc_ekfilpar
      use pepper_inc_ekglimit
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      character(*), intent(in) :: tabnam  !! Table name.
      integer(ip), intent(in) :: ncols  !! Number of columns in the segment.
      character(*), intent(in) :: cnames ( * )  !! Names of columns.
      integer(ip), intent(inout) :: cdscrs ( cdscsz, * )  !! Descriptors of columns. (PEPPER: changed by the routine, an input in SPICELIB's table)
      integer(ip), intent(out) :: segno  !! Segment number.
      integer(ip) :: sgtree_1(1)  !! one-element copy of sgtree (array argument)



      character(pgsizc) :: cpage
      character(cnamsz) :: tmpcnm
      character(tnamsz) :: tmptnm

      integer(ip) :: base
      integer(ip) :: cbase
      integer(ip) :: cp1
      integer(ip) :: cp
      integer(ip) :: cpagno
      integer(ip) :: cpt
      integer(ip) :: dpt
      integer(ip) :: dscbas
      integer(ip) :: i
      integer(ip) :: ipage  ( pgsizi )
      integer(ip) :: ipagno
      integer(ip) :: ipt
      integer(ip) :: metasz
      integer(ip) :: nambas
      integer(ip) :: ncpage
      integer(ip) :: nipage
      integer(ip) :: p
      integer(ip) :: p1
      integer(ip) :: p1base
      integer(ip) :: rec
      integer(ip) :: room
      integer(ip) :: sgtree

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Before trying to actually write anything, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     The metadata layout has the following form:
!
!        +------------------------------------------+
!        |                                          |
!        |            segment descriptor            |
!        |                                          |
!        +------------------------------------------+
!        |            column descriptor 1           |
!        +------------------------------------------+
!        |            column descriptor 2           |
!        +------------------------------------------+
!                              .
!                              .
!                              .
!        +------------------------------------------+
!        |           column descriptor m            |
!        +------------------------------------------+
!
!     The column descriptors may span multiple pages, but they
!     always occupy contiguous DAS integer addresses.
!
!     In addition, the metadata area includes a character page
!     that contains the segment's table name and the table's
!     column names.
!
!     Calculate the number of contiguous integer pages we'll need.
!     This value is a function of the number of columns.
!
      metasz  =    sdscsz  +  ncols * cdscsz

      nipage  =  ( metasz  +  pgsizi  -  1 ) / pgsizi
!
!     Allocate NIPAGE new integer pages.  Insisting on new pages
!     enforces contiguity.  Also allocate one character page, which
!     need not be new.
!
      call zzekpgan ( ctx, handle, int, p1, p1base )

      do i = 2, nipage
         call zzekpgan ( ctx, handle, int, p, base )
      end do

!
!     Calculate the number of contiguous character pages we'll need.
!
      ncpage  =  ( tnamsz  +  ncols * cnamsz  +  pgsizc - 1 ) / pgsizc

      call zzekpgan ( ctx, handle, chr, cp1, cbase )

      do i = 2, ncpage
         call zzekpgan ( ctx, handle, chr, p, base )
      end do

!
!     Initialize the record tree.
!
      call zzektrit ( ctx, handle, rec )

!
!     On the third day of Christmas, we initialized three data page
!     trees:  one for each data type.
!
      call zzektrit ( ctx, handle, cpt  )
      call zzektrit ( ctx, handle, dpt )
      call zzektrit ( ctx, handle, ipt )

!
!     Prepare the contents of the first integer page:  initialize
!     everything other than the column descriptors.
!
!     The last data word in use for each data type is initialized
!     to indicate that no room is left in the current page.  This
!     forces allocation of a new page when data must be added.  The
!     `last word' counts of each type for both the data and modified
!     record trees are initialized in this fashion.
!
      ipage = 0
!
!     The value at index EKTIDX is the segment type.
!
      ipage ( ektidx )  =  1
      ipage ( snoidx )  =  eknseg ( ctx, handle )  +  1
      ipage ( imdidx )  =  p1base
      ipage ( tnmidx )  =  cbase
      ipage ( ncidx  )  =  ncols
      ipage ( nridx  )  =  0
      ipage ( rtidx  )  =  rec
      ipage ( cptidx )  =  cpt
      ipage ( dptidx )  =  dpt
      ipage ( iptidx )  =  ipt
      ipage ( mflidx )  =  itrue
      ipage ( iflidx )  =  ifalse
      ipage ( shdidx )  =  ifalse
      ipage ( cfhidx )  =  0
      ipage ( csnidx )  =  0
      ipage ( lcpidx )  =  0
      ipage ( ldpidx )  =  0
      ipage ( lipidx )  =  0
      ipage ( lcwidx )  =  cpsize
      ipage ( ldwidx )  =  dpsize
      ipage ( liwidx )  =  ipsize
      ipage ( nmlidx )  =  cbase + tnamsz

!
!     Initialize the character metadata page:  fill in the table name.
!     The table name gets converted to upper case and is left justified.
!
      cpage  =  ' '

      tmptnm = adjustl(tabnam)
      call ucase_inplace ( tmptnm )

      cpage ( :tnamsz )  =  tmptnm

!
!     Now for the column-specific tasks.  We write out a descriptor for
!     each column.  At the same time, we write out the column's name.
!
      ipagno = 1
      cpagno = 1
      p      = p1
      cp     = cp1

      do i = 1, ncols
!
!        Insert the column's ordinal position in the segment into
!        the column's descriptor.
!
         cdscrs( ordidx, i )  =  i

!
!        Write the Ith column name into the character metdata page.  (We
!        know the name is non-blank.)  Blank-pad the name on the right,
!        up to a length of CNAMSZ characters, if necessary.  Convert the
!        name to upper case as well.
!
         call ucase  ( cnames(i), tmpcnm )

         nambas  =  tnamsz  +  (i-1)*cnamsz  -  (cpagno-1)*pgsizc
         room    =  pgsizc  -  nambas

         if ( cnamsz <= room ) then

            cpage ( nambas+1 : nambas+cnamsz ) =  tmpcnm

!
!           Fill the column name's base address into the descriptor.
!
            cdscrs( namidx, i ) =  cbase + (cpagno-1)*pgsizc + nambas

         else
!
!           Some or all of the column name will overflow onto the next
!           page.
!
            if ( room > 0 ) then

               cpage ( nambas+1 : nambas+room ) = tmpcnm(:room)

               cdscrs( namidx, i ) =  cbase + (cpagno-1)*pgsizc + nambas

            else

               cdscrs( namidx, i ) =  cbase +  cpagno*pgsizc

            end if

!
!           Write out the page we just filled up.
!
            call zzekpgwc ( ctx, handle, cp, cpage )

!
!           The next character page will hold the overflow.  The next
!           page is the successor of page CP, since we allocated
!           consecutive character pages.
!
            cp       =  cp     + 1
            cpagno   =  cpagno + 1
            cpage    =  tmpcnm(room+1:)

         end if

!
!        If the column is indexed, create a new index for this column.
!        Currently, data type 1 indexes are in vogue.  Set the
!        descriptor to indicate the data type and to point to the index.
!
         if ( cdscrs(ixtidx,i) /= ifalse ) then
!
!           ZZEKCIX1 will update the descriptor to indicate the type and
!           location of the new index.
!
            call zzekcix1 ( ctx, handle, cdscrs(1,i) )

         end if

!
!        Add the column descriptor to the metadata page, if the
!        descriptor will fit.  We may need to allocate another page
!        to hold the descriptor.
!
         dscbas  =  sdscsz  +  (i-1)*cdscsz  -  (ipagno-1)*pgsizi
         room    =  pgsizi  -  dscbas

         if ( cdscsz <= room ) then
!
!           The whole descriptor fits in the current page.
!
            ipage(dscbas+1:dscbas+1+cdscsz-1) = cdscrs(:,i)

         else
!
!           Some or all of the descriptor will overflow onto the next
!           page.
!
            if ( room > 0 ) then
               ipage(dscbas+1:dscbas+1+room-1) = cdscrs(1:room,i)
            end if

!
!           Write out the page we just filled up.
!
            call zzekpgwi ( ctx, handle, p, ipage )

!
!           The next integer page will hold the overflow.  The next page
!           is the successor of page P, since we allocated consecutive
!           pages.
!
            p       =  p + 1
            ipagno  =  ipagno + 1

            ipage = 0
            ipage(1:cdscsz-room) = cdscrs(room+1:room+1+cdscsz-room-1,i)

         end if

!
!        If we encountered a DAS error, leave now.
!
         if ( ctx%failed() ) return

      end do

!
!     Write out the last integer metadata page, and write out the
!     character metadata page.
!
      call zzekpgwi ( ctx, handle, p,  ipage )
      call zzekpgwc ( ctx, handle, cp, cpage )

!
!     At this point, the segment's metadata is filled in.  We must
!     update the file's segment list information to account for this
!     segment.  All we need do is add a new entry to the file's
!     segment pointer tree.  First, look up the tree.
!
      call zzekpgbs ( ctx, int, 1, base )

      call dasrdi ( ctx, handle, base+sgtidx, base+sgtidx, sgtree_1 )
      sgtree = sgtree_1(1)

!
!     Append the head node of this segment at the end of the segment
!     tree.  The tree will point to the first integer metadata page of
!     the new segment.
!
      call zzektrap ( ctx, handle, sgtree, p1, segno )

      return
      end subroutine zzekbs01

      !> Start a new type 2 segment in an E-kernel.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         the handle of an EK file that is open for writing.
      !>
      !>     TABNAM         is the name of the EK table to which the current
      !>                    segment belongs.  All segments in the EK file
      !>                    designated by HANDLE must have identical column
      !>                    attributes. TABNAM must not exceed 32 characters
      !>                    in length.  Case is not significant.  Table names
      !>                    must start with a letter and contain only
      !>                    characters from the set {A-Z,a-z,0-9,$,_}.
      !>
      !>     NCOLS          is the number of columns in a new segment.
      !>
      !>     CNAMES,
      !>     CDSCRS         are, respectively, and array of column names and
      !>                    their corresponding descriptors:  the Ith element
      !>                    of CNAMES and the Ith descriptor apply to
      !>                    the Ith column in the segment.
      !>
      !>## Detailed Output
      !>
      !>     CDSCRS         are the input column descriptors, with their name
      !>                    base and ordinal position elements filled in.
      !>
      !>     SEGNO          is the number of the segment created by this
      !>                    routine.  Segment numbers are used as unique
      !>                    identifiers by other EK access routines.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects:  it prepares an EK for
      !>     the addition of a new type 2 segment.  Type 2 segments have
      !>     fixed record counts:  they do not support record insertion,
      !>     or deletion operations.  They do not support arbitrary column
      !>     entry update operations either, since some updates change the
      !>     size of the affected entries.
      !>
      !>     Type 2 segments may contains columns of class 7 through 9.
      !>
      !>     By way of contrast, type 1 segments support variable record
      !>     counts.
      !>
      !>## Examples
      !>
      !>     See EKBSEG.
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
      !>-    Beta Version 1.0.0, 17-NOV-1995 (NJB)
      module subroutine zzekbs02 ( ctx, handle, tabnam, ncols, cnames, cdscrs, segno )
      use pepper_kinds, only: ip
      use pepper_strings, only: ucase, ucase_inplace
      use pepper_inc_ekbool
      use pepper_inc_ekcoldsc
      use pepper_inc_ekcnamsz
      use pepper_inc_ekdatpag
      use pepper_inc_ekfilpar
      use pepper_inc_ekglimit
      use pepper_inc_ekpage
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      character(*), intent(in) :: tabnam  !! Table name.
      integer(ip), intent(in) :: ncols  !! Number of columns in the segment.
      character(*), intent(in) :: cnames ( * )  !! Names of columns.
      integer(ip), intent(inout) :: cdscrs ( cdscsz, * )  !! Descriptors of columns.
      integer(ip), intent(out) :: segno  !! Segment number.
      integer(ip) :: sgtree_1(1)  !! one-element copy of sgtree (array argument)



      character(pgsizc) :: cpage
      character(cnamsz) :: tmpcnm
      character(tnamsz) :: tmptnm

      integer(ip) :: base
      integer(ip) :: cbase
      integer(ip) :: cp1
      integer(ip) :: cp
      integer(ip) :: cpagno
      integer(ip) :: cpt
      integer(ip) :: dpt
      integer(ip) :: dscbas
      integer(ip) :: i
      integer(ip) :: ipage  ( pgsizi )
      integer(ip) :: ipagno
      integer(ip) :: ipt
      integer(ip) :: metasz
      integer(ip) :: nambas
      integer(ip) :: ncpage
      integer(ip) :: nipage
      integer(ip) :: p
      integer(ip) :: p1
      integer(ip) :: p1base
      integer(ip) :: room
      integer(ip) :: sgtree

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Before trying to actually write anything, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     The metadata layout has the following form:
!
!        +------------------------------------------+
!        |                                          |
!        |            segment descriptor            |
!        |                                          |
!        +------------------------------------------+
!        |            column descriptor 1           |
!        +------------------------------------------+
!        |            column descriptor 2           |
!        +------------------------------------------+
!                              .
!                              .
!                              .
!        +------------------------------------------+
!        |           column descriptor m            |
!        +------------------------------------------+
!
!     The column descriptors may span multiple pages, but they
!     always occupy contiguous DAS integer addresses.
!
!     In addition, the metadata area includes a character page
!     that contains the segment's table name and the table's
!     column names.
!
!     Calculate the number of contiguous integer pages we'll need.
!     This value is a function of the number of columns.
!
      metasz  =    sdscsz  +  ncols * cdscsz

      nipage  =  ( metasz  +  pgsizi  -  1 ) / pgsizi
!
!     Allocate NIPAGE new integer pages.  Insisting on new pages
!     enforces contiguity.  Also allocate one character page, which
!     need not be new.
!
      call zzekpgan ( ctx, handle, int, p1, p1base )

      do i = 2, nipage
         call zzekpgan ( ctx, handle, int, p, base )
      end do

!
!     Calculate the number of contiguous character pages we'll need.
!
      ncpage  =  ( tnamsz  +  ncols * cnamsz  +  pgsizc - 1 ) / pgsizc

      call zzekpgan ( ctx, handle, chr, cp1, cbase )

      do i = 2, ncpage
         call zzekpgan ( ctx, handle, chr, p, base )
      end do

!
!     On the third day of Christmas, we initialized three data page
!     trees:  one for each data type.
!
      call zzektrit ( ctx, handle, cpt  )
      call zzektrit ( ctx, handle, dpt )
      call zzektrit ( ctx, handle, ipt )

!
!     Prepare the contents of the first integer page:  initialize
!     everything other than the column descriptors.
!
!     The last data word in use for each data type is initialized
!     to indicate that no room is left in the current page.  This
!     forces allocation of a new page when data must be added.  The
!     `last word' counts of each type for both the data and modified
!     record trees are initialized in this fashion.
!
      ipage = 0
!
!     The value at index EKTIDX is the segment type.
!
      ipage ( ektidx )  =  2
      ipage ( snoidx )  =  eknseg ( ctx, handle )  +  1
      ipage ( imdidx )  =  p1base
      ipage ( tnmidx )  =  cbase
      ipage ( ncidx  )  =  ncols
      ipage ( nridx  )  =  0
      ipage ( rtidx  )  =  0
      ipage ( cptidx )  =  cpt
      ipage ( dptidx )  =  dpt
      ipage ( iptidx )  =  ipt
      ipage ( mflidx )  =  itrue
      ipage ( iflidx )  =  ifalse
      ipage ( shdidx )  =  ifalse
      ipage ( cfhidx )  =  0
      ipage ( csnidx )  =  0
      ipage ( lcpidx )  =  0
      ipage ( ldpidx )  =  0
      ipage ( lipidx )  =  0
      ipage ( lcwidx )  =  cpsize
      ipage ( ldwidx )  =  dpsize
      ipage ( liwidx )  =  ipsize
      ipage ( nmlidx )  =  cbase + tnamsz

!
!     Initialize the character metadata page:  fill in the table name.
!     The table name gets converted to upper case and is left justified.
!
      cpage  =  ' '

      tmptnm = adjustl(tabnam)
      call ucase_inplace ( tmptnm )

      cpage ( :tnamsz )  =  tmptnm

!
!     Now for the column-specific tasks.  We write out a descriptor for
!     each column.  At the same time, we write out the column's name.
!
      ipagno = 1
      cpagno = 1
      p      = p1
      cp     = cp1

      do i = 1, ncols
!
!        Insert the column's ordinal position in the segment into
!        the column's descriptor.
!
         cdscrs( ordidx, i )  =  i

!
!        Write the Ith column name into the character metdata page.  (We
!        know the name is non-blank.)  Blank-pad the name on the right,
!        up to a length of CNAMSZ characters, if necessary.  Convert the
!        name to upper case as well.
!
         call ucase  ( cnames(i), tmpcnm )

         nambas  =  tnamsz  +  (i-1)*cnamsz  -  (cpagno-1)*pgsizc
         room    =  pgsizc  -  nambas

         if ( cnamsz <= room ) then

            cpage ( nambas+1 : nambas+cnamsz ) =  tmpcnm

!
!           Fill the column name's base address into the descriptor.
!
            cdscrs( namidx, i ) =  cbase + (cpagno-1)*pgsizc + nambas

         else
!
!           Some or all of the column name will overflow onto the next
!           page.
!
            if ( room > 0 ) then

               cpage ( nambas+1 : nambas+room ) = tmpcnm(:room)

               cdscrs( namidx, i ) =  cbase + (cpagno-1)*pgsizc + nambas

            else

               cdscrs( namidx, i ) =  cbase +  cpagno*pgsizc

            end if

!
!           Write out the page we just filled up.
!
            call zzekpgwc ( ctx, handle, cp, cpage )

!
!           The next character page will hold the overflow.  The next
!           page is the successor of page CP, since we allocated
!           consecutive character pages.
!
            cp       =  cp     + 1
            cpagno   =  cpagno + 1
            cpage    =  tmpcnm(room+1:)

         end if

!
!        Add the column descriptor to the metadata page, if the
!        descriptor will fit.  We may need to allocate another page
!        to hold the descriptor.
!
         dscbas  =  sdscsz  +  (i-1)*cdscsz  -  (ipagno-1)*pgsizi
         room    =  pgsizi  -  dscbas

         if ( cdscsz <= room ) then
!
!           The whole descriptor fits in the current page.
!
            ipage(dscbas+1:dscbas+1+cdscsz-1) = cdscrs(:,i)

         else
!
!           Some or all of the descriptor will overflow onto the next
!           page.
!
            if ( room > 0 ) then
               ipage(dscbas+1:dscbas+1+room-1) = cdscrs(1:room,i)
            end if

!
!           Write out the page we just filled up.
!
            call zzekpgwi ( ctx, handle, p, ipage )

!
!           The next integer page will hold the overflow.  The next page
!           is the successor of page P, since we allocated consecutive
!           pages.
!
            p       =  p + 1
            ipagno  =  ipagno + 1

            ipage = 0
            ipage(1:cdscsz-room) = cdscrs(room+1:room+1+cdscsz-room-1,i)

         end if

!
!        If we encountered a DAS error, leave now.
!
         if ( ctx%failed() ) return

      end do

!
!     Write out the last integer metadata page, and write out the
!     character metadata page.
!
      call zzekpgwi ( ctx, handle, p,  ipage )
      call zzekpgwc ( ctx, handle, cp, cpage )

!
!     At this point, the segment's metadata is filled in.  We must
!     update the file's segment list information to account for this
!     segment.  All we need do is add a new entry to the file's
!     segment pointer tree.  First, look up the tree.
!
      call zzekpgbs ( ctx, int, 1, base )

      call dasrdi ( ctx, handle, base+sgtidx, base+sgtidx, sgtree_1 )
      sgtree = sgtree_1(1)

!
!     Append the head node of this segment at the end of the segment
!     tree.  The tree will point to the first integer metadata page of
!     the new segment.
!
      call zzektrap ( ctx, handle, sgtree, p1, segno )

      return
      end subroutine zzekbs02

      !> Delete a specified class 4 column entry from an EK record.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for write access.
      !>
      !>     SEGDSC         is the descriptor of the segment from which to
      !>                    delete the specified column entry.
      !>
      !>     COLDSC         is the descriptor of the column from which to
      !>                    delete the specified column entry.
      !>
      !>     RECPTR         is a pointer to the record containing the column
      !>                    entry to delete.
      !>
      !>## Detailed Output
      !>
      !>     None.  See the $Particulars section for a description of the
      !>     effect of this routine.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.  The file will not be modified.
      !>
      !>     2)  If an I/O error occurs while reading or writing the indicated
      !>         file, the error will be diagnosed by routines called by this
      !>         routine.  The file may be corrupted.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This routine operates by side effects:  it deletes a column entry
      !>     from an EK segment.  The status of the record containing the entry
      !>     is set to `updated'.  The deleted entry is marked as
      !>     `uninitialized'.
      !>
      !>     The link counts for the pages containing the deleted column entry
      !>     are decremented.  If the count for a page becomes zero, that page
      !>     is freed.  If the entry to be deleted is already uninitialized
      !>     upon entry to this routine, no link counts are modified.  The
      !>     record containing the entry is still marked `updated' in this
      !>     case.
      !>
      !>     The changes made by this routine to the target EK file become
      !>     permanent when the file is closed.  Failure to close the file
      !>     properly will leave it in an indeterminate state.
      !>
      !>## Examples
      !>
      !>     See EKDELR.
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
      !>-    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN to insert DAS file name into
      !>        long error message.
      !>
      !>        Deleted unneeded declarations and code.
      !>
      !>-    Beta Version 1.0.0, 28-SEP-1995 (NJB)
      module subroutine zzekde04 ( ctx, handle, segdsc, coldsc, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_ekcoldsc
      use pepper_inc_ekdatpag
      use pepper_inc_ekrecptr
      use pepper_inc_eksegdsc
      use pepper_inc_ektype
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(inout) :: segdsc ( sdscsz )  !! Segment descriptor. (PEPPER: changed by the routine, an input in SPICELIB's table)
      integer(ip), intent(in) :: coldsc ( cdscsz )  !! Column descriptor.
      integer(ip), intent(in) :: recptr  !! Record pointer.
      integer(ip) :: nelts_1(1)  !! one-element copy of nelts (array argument)
      integer(ip) :: datptr_1(1)  !! one-element copy of datptr (array argument)
!
!     Local variables
!
      integer(ip) :: base
      integer(ip) :: datptr
      integer(ip) :: nelts
      integer(ip) :: next
      integer(ip) :: nlinks
      integer(ip) :: nseen
      integer(ip) :: p
      integer(ip) :: ptrloc
      integer(ip) :: recno

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Before trying to actually modify the file, do every error
!     check we can.
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( ctx, handle, 'WRITE' )

      if ( ctx%failed() ) return

!
!     Compute the data pointer location.  If the data pointer is
!     already set to `uninitialized', there's nothing to do.  If
!     the element is null, just set it to `uninitialized'.  The
!     presence of actual data obligates us to clean up, however.
!
      ptrloc  =  recptr + dptbas + coldsc(ordidx)

      call dasrdi ( ctx, handle, ptrloc, ptrloc, datptr_1 )
      datptr = datptr_1(1)

      if ( datptr > 0 ) then
!
!        Get the element count for the entry.
!
         call dasrdi ( ctx, handle, datptr, datptr, nelts_1 )
         nelts = nelts_1(1)

!
!        Set the data pointer to indicate the item is uninitialized.
!
         call dasudi ( ctx, handle, ptrloc, ptrloc, [ uninit ] )

!
!        Find the number of the page containing the column entry.
!
         call zzekpgpg ( ctx, int, datptr, p, base )

!
!        Look up the forward pointer.  This pointer will be valid
!        if the column entry is continued on another page.
!
         call zzekgfwd ( ctx, handle, int, p, next )

!
!        Get the link count for the current page.  If we have more
!        than one link to the page, decrement the link count.  If
!        we're down to one link, this deletion will finish off the
!        page:  we'll deallocate it.
!
         call zzekglnk ( ctx, handle, int, p, nlinks )

         if ( nlinks > 1 ) then

            call zzekslnk ( ctx, handle, int, p, nlinks-1 )

         else
!
!           If we removed the last item from the page, we can delete
!           the page.  ZZEKDPS adjusts the segment's metadata
!           to reflect the deallocation.
!
            call zzekdps  ( ctx, handle, segdsc, int, p )

         end if

         nseen = min (  nelts,  (base+ipsize-datptr)  )

         do while (  ( nseen < nelts ) .and. ( .not. ctx%failed() )  )
!
!           The column entry is continued on the page indicated by
!           NEXT.
!
!           Get the link count for the current page.  If we have more
!           than one link to the page, decrement the link count.  If
!           we're down to one link, this deletion will finish off the
!           page:  we'll deallocate it.
!
            p  =  next

            call zzekgfwd ( ctx, handle, int, p, next   )
            call zzekglnk ( ctx, handle, int, p, nlinks )

            if ( nlinks > 1 ) then

               call zzekslnk ( ctx, handle, int, p, nlinks-1 )

            else
!
!              If we removed the last item from the page, we can delete
!              the page.  ZZEKDPS adjusts the segment's metadata
!              to reflect the deallocation.
!
               call zzekdps  ( ctx, handle, segdsc, int, p )

            end if

            nseen  =  min (  nelts,  ( ipsize + nseen )  )

         end do

      else if ( datptr == null ) then
!
!        Mark the entry as `uninitialized'.
!
         call dasudi ( ctx, handle, ptrloc, ptrloc, [ uninit ] )

      else if ( datptr /= uninit ) then
!
!        UNINIT was the last valid possibility.  The data pointer is
!        corrupted.
!
         call ctx%setmsg ( 'Data pointer is corrupted. SEGNO = #; '  // &
                       'COLIDX =  #; RECNO = #; EK = #'          )
         call ctx%errint ( '#',  segdsc(snoidx)                      )
         call ctx%errint ( '#',  coldsc(ordidx)                      )
         call ctx%errint ( '#',  recno                               )
         call ctx%errhan ( '#',  handle                              )
         call ctx%sigerr ( 'SPICE(BUG)'                              )
         return

      end if

!
!     Set the record's status to indicate that this record is updated.
!
      call dasudi ( ctx, handle, recptr+staidx, recptr+staidx, [ update ] )

      return
      end subroutine zzekde04

      !> Find the EK record number corresponding to a specified record
      !> pointer.  Beware, for type 1 segments, this is done by linear
      !> searching.
      !>
      !> The function returns the number of the record having the
      !> specified record pointer.
      !>
      !>## Detailed Input
      !>
      !>     HANDLE         is a file handle of an EK open for read or write
      !>                    access.
      !>
      !>     SEGNO          is the number of the segment containing the
      !>                    record of interest.
      !>
      !>     RECPTR         is a record pointer.  The number of the record
      !>                    having this pointer is sought.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the number of the record having the
      !>     specified record pointer.  The record should always be found.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If HANDLE is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     2)  If SEGNO is invalid, the error will be diagnosed by routines
      !>         called by this routine.
      !>
      !>     3)  If an I/O error occurs while reading the indicated file,
      !>         the error will be diagnosed by routines called by this
      !>         routine.
      !>
      !>     4)  This routine should never be passed an input record pointer
      !>         that is not known to be valid.  If this error is trapped,
      !>         it is evidence of a bug.
      !>
      !>## Files
      !>
      !>     See the EK Required Reading for a discussion of the EK file
      !>     format.
      !>
      !>## Particulars
      !>
      !>     This searches an EK record tree for the record number
      !>     corresponding to a specified record pointer.  Caution:  this
      !>     routine plods along in linear time.  It is intended primarily
      !>     for use in error handling.
      !>
      !>## Examples
      !>
      !>     See EKDELR.
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
      !>-    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB)
      !>
      !>        Now uses ERRHAN to insert DAS file name into
      !>        long error messages.
      !>
      !>-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
      module integer(ip) function zzekrp2n ( ctx, handle, segno, recptr )
      use pepper_kinds, only: ip
      use pepper_inc_eksegdsc
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! File handle.
      integer(ip), intent(in) :: segno  !! Segment number.
      integer(ip), intent(in) :: recptr  !! Record pointer.
!
!     Local variables
!
      integer(ip) :: segdsc ( sdscsz )
      integer(ip) :: stype

      zzekrp2n  =  0

      call zzeksdsc ( ctx, handle, segno, segdsc )

      if ( ctx%failed() ) return

      stype = segdsc ( ektidx )

      if ( stype == 1 ) then

         zzekrp2n  =  zzektrls ( ctx, handle, segdsc(rtidx), recptr )

         if ( zzekrp2n == 0 ) then

            call ctx%setmsg ( 'Record having pointer # not found in ' // &
                          'segment # of file #'                   )
            call ctx%errint ( '#',  recptr                            )
            call ctx%errint ( '#',  segno                             )
            call ctx%errhan ( '#',  handle                            )
            call ctx%sigerr ( 'SPICE(BUG)'                            )

         end if

      else if ( stype == 2 ) then

         zzekrp2n = recptr

      else

         call ctx%setmsg ( 'Segment type # is not supported.  SEGNO = #.'// &
                       ' File = #.'                                  )
         call ctx%errint ( '#',  stype                                   )
         call ctx%errint ( '#',  segno                                   )
         call ctx%errhan ( '#',  handle                                  )
         call ctx%sigerr ( 'SPICE(BUG)'                                  )

      end if

      return
      end function zzekrp2n

   !> PEPPER: [[appndi]] for the integer part of an encoded query, which grows
   !> (SPICELIB: a cell of EQIMIN elements, sized for the largest query).
   subroutine append_query ( ctx, item, eqryi )
      use pepper_containers, only: grow_cell
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: item  !! the item to append
      integer(ip), allocatable, intent(inout) :: eqryi ( : )  !! the integer part of the encoded query, eqryi(lbcell:)
      if ( cardi ( ctx, eqryi ) >= sizei ( ctx, eqryi ) ) then
         call grow_cell ( eqryi, cardi ( ctx, eqryi ) + 1 )
      end if
      call appndi ( ctx, item, eqryi )
   end subroutine append_query

end submodule pepper_ek_storage
