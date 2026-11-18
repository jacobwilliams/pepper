!> Body names and NAIF ID codes (built-in, and defined in the kernel pool or by boddef), and body
!> constants from the pool (bodvrd, bodvcd); hash sets of names and integers.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
!>
!>## ZZHSC (Private---Add-only Character Hash)
!>
!> PEPPER: SPICELIB's umbrella routine ZZHSC is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzhsc. Its documentation follows.
!>
!>### Abstract
!>
!>     SPICE Private routine intended solely for the support of SPICE
!>     routines. Users should not call this routine directly due
!>     to the volatile nature of this routine.
!>
!>     Manipulate add-only character hash.
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
!>     LBPOOL      is the lower bound of the collision list array.
!>
!>### Exceptions
!>
!>     1) If ZZHSC is called directly, the error SPICE(BOGUSENTRY) is
!>        signaled.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     ZZHSC should never be called directly, but should instead be
!>     accessed only through its entry points.
!>
!>     The purpose of this routine is to manipulate add-only character
!>     hashes used for buffering purposes by various SPICE subsystems.
!>     This umbrella has the following entry points:
!>
!>           ZZHSCINI       Initialize a hash.
!>
!>           ZZHSCADD       Add an item to a hash.
!>
!>           ZZHSCCHK       Check if a item is in a hash.
!>
!>           ZZHSCAVL       Get available room in hash.
!>
!>           ZZHSCINF       Get h/k information about hash.
!>
!>     An add-only hash consists of the head node pointer list (HEDLST),
!>     hash node collision list (COLLST), and the item list (ITEMS).
!>
!>     The function ZZHASH2 computes the index of an element in the head
!>     node pointer list (HEDLST).
!>
!>     The HEDLST element at this index contains either 0, indicating
!>     that there is not yet data for this hash value in the hash, or
!>     the index of the head node and its item in the hash collision
!>     (COLLST) and item (ITEMS) lists.
!>
!>     The COLLST element at the head node index contain either 0,
!>     indicating that there is only one item for this hash value in the
!>     hash, or the index of the next node and its item in the hash
!>     collision (COLLST) and item (ITEMS) lists.
!>
!>     The COLLST element at the next node index contain either 0,
!>     indicating that this is the last item for this hash value in the
!>     hash, or the index of the next node and its item in the hash
!>     collision (COLLST) and item (ITEMS) lists.
!>
!>     And so on.
!>
!>     Pictorially the hash looks like this:
!>
!>                     List of head   List of hash      List of
!>                        nodes        collisions        items
!>
!>                       HEDLST          COLLST          ITEMS
!>
!>                                     +---------+
!>                                     |         |
!>                                     +---------+
!>                                         ...
!>                                     +---------+
!>                                     | 1st Free|
!>                                     +---------+
!>                                     |  Size   |
!>                     +---------+     +---------+     +---------+
!>                     |         |     |         |     |         |
!>                     +---------+ !=0 +---------+     +---------+
!>                  .->|Head Node| -.  |         |     |         |
!>                  |  +---------+  |  +---------+     +---------+
!>       ZZHASH2(ITEM) |         |  |  |         |     |         |
!>                     +---------+  |  +---------+     +---------+
!>                     |         |  `->|Head of  |     |Item     |
!>                     |         |     |collision| !=0 |corresp. |
!>                     |         |     |list for | -.  |to head  |
!>                     |         |     | ITEM    |  |  |of list  |
!>                     +---------+     +---------+  |  +---------+
!>                     |         |     |         |  |  |         |
!>                     +---------+     +---------+  |  +---------+
!>                     |         |     |         |<-'  |         |
!>                     |         |     |Next Node| !=0 |Next Item|
!>                     |         |     |         |--.  |         |
!>                     +---------+     +---------+  |  +---------+
!>                     |         |     |         |  |  |         |
!>                     +---------+     +---------+  |  +---------+
!>                     |         |     |         |  |  |         |
!>                     +---------+     +---------+  |  +---------+
!>                     |         |     |Next Node|<-'  |Next Item|
!>                     +---------+     +---------+ etc +---------+
!>                        ...              ...            ...
!>                     +---------+     +---------+     +---------+
!>                     |         |     |         |     |         |
!>                     +---------+     +---------+     +---------+
!>
!>### Examples
!>
!>     An add-only hash used together with flat data arrays provides a
!>     simple, fast-access buffering mechanism. The subsystems that need
!>     such buffering would normally initialize the hash, add items to
!>     the hash and buffer data associated with items in flat data
!>     arrays, and find items in the hash and, if found, use buffered
!>     data associated with the items rather than computing/getting
!>     their data again.
!>
!>     An add-only hash is normally used in one of the following ways --
!>     set up to never be filled up, stop buffering when filled up, and
!>     trash when filled up and start buffering again. Three examples
!>     below illustrate each of these ways.
!>
!>     Example 1: Set up to never be filled up
!>     ---------------------------------------
!>
!>     C
!>     C     Parameters: pool lower boundary, hash size and item
!>     C     size.
!>     C
!>           INTEGER               LBPOOL
!>           PARAMETER           ( LBPOOL = -5 )
!>
!>           INTEGER               HSHSIZ
!>           PARAMETER           ( HSHSIS = 5003 )
!>
!>           INTEGER               NAMSIZ
!>           PARAMETER           ( NAMSIZ = 32 )
!>
!>     C
!>     C     Hash arrays and data buffer.
!>     C
!>           INTEGER               HEDLST  (          HSHSIZ )
!>           INTEGER               COLLST  ( LBPOOL : HSHSIZ )
!>           CHARACTER*(NAMSIZ)    ITEMS   (          HSHSIZ )
!>           INTEGER               DATBUF  (          HSHSIZ )
!>
!>     C
!>     C     Miscellaneous variables.
!>     C
!>           CHARACTER*(NAMSIZ)    ITEM
!>
!>           INTEGER               ITEMAT
!>           INTEGER               MYDATA
!>
!>           LOGICAL               FIRST
!>           INTEGER               AVAIL
!>           LOGICAL               NEW
!>
!>     C
!>     C     Data items.
!>     C
!>           DATA FIRST             / .TRUE.  /
!>
!>     C
!>     C     Initialize hash.
!>     C
!>           IF ( FIRST ) THEN
!>              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
!>              FIRST = .FALSE.
!>           END IF
!>           ...
!>     C
!>     C     Check for presence of or add an item to the hash. If the
!>     C     item is in the hash, NEW will be .FALSE. If the item is
!>     C     not in the hash, it will be added and NEW will be .TRUE.
!>     C     This call can fail only if the hash fills up which
!>     C     should not happen because it was declared to never fill
!>     C     up.
!>     C
!>           CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM,
!>          .                                ITEMAT, NEW )
!>           IF ( FAILED() ) THEN
!>              RETURN
!>           END IF
!>
!>           IF ( .NOT. NEW ) THEN
!>     C
!>     C        Simply use buffered value.
!>     C
!>              MYDATA = DATBUF( ITEMAT )
!>
!>           ELSE
!>     C
!>     C        Get value.
!>     C
!>              ...
!>     C
!>     C        Buffer value.
!>     C
!>              DATBUF( ITEMAT ) = MYDATA
!>
!>           END IF
!>
!>     Example 2: Stop buffering when filled up
!>     ----------------------------------------
!>
!>     C
!>     C     Use the same declarations as the first example.
!>     C
!>           ...
!>     C
!>     C     Initialize hash.
!>     C
!>           IF ( FIRST ) THEN
!>              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
!>              FIRST = .FALSE.
!>           END IF
!>           ...
!>     C
!>     C     Check if this item is in the hash.
!>     C
!>           CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT )
!>
!>           IF ( ITEMAT .NE. 0 ) THEN
!>     C
!>     C        Simply use buffered value.
!>     C
!>              MYDATA = DATBUF( ITEMAT )
!>
!>           ELSE
!>     C
!>     C        Get value.
!>     C
!>              ...
!>     C
!>     C        Buffer value, but only if the hash is not full.
!>     C
!>              CALL ZZHSCAVL( COLLST, AVAIL )
!>              IF ( AVAIL .GT. 0 ) THEN
!>                 CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM,
!>          .                                  ITEMAT, NEW )
!>                 DATBUF( ITEMAT ) = MYDATA
!>              END IF
!>
!>           END IF
!>
!>     Example 3: Trash when filled up and start buffering again
!>     ---------------------------------------------------------
!>
!>     C
!>     C     Use the same declarations as the first example.
!>     C
!>           ...
!>     C
!>     C     Initialize hash.
!>     C
!>           IF ( FIRST ) THEN
!>              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
!>              FIRST = .FALSE.
!>           END IF
!>           ...
!>     C
!>     C     Check if this item is in the hash.
!>     C
!>           CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT )
!>
!>           IF ( ITEMAT .NE. 0 ) THEN
!>     C
!>     C        Simply use buffered value.
!>     C
!>              MYDATA = DATBUF( ITEMAT )
!>
!>           ELSE
!>     C
!>     C        Get value.
!>     C
!>              ...
!>     C
!>     C        Buffer value, if the hash is full trash re-initialize
!>     C        it first.
!>     C
!>              CALL ZZHSCAVL( COLLST, AVAIL )
!>              IF ( AVAIL .LE. 0 ) THEN
!>                 CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST )
!>              END IF
!>              CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM,
!>          .                                  ITEMAT, NEW )
!>              DATBUF( ITEMAT ) = MYDATA
!>
!>           END IF
!>
!>### Restrictions
!>
!>     For sake of speed all entry points do no or minimal error
!>     checking. It is the responsibility of the caller to ensure that
!>     all inputs are properly initialized.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     J. Diaz del Rio    (ODC Space)
!>     B.V. Semenov       (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 1.1.0, 12-OCT-2020 (JDR) (BVS)
!>
!>        Changed entry point ZZHSCINI to not check RETURN()
!>        on entry. That entry point now initializes the hash
!>        as long as the size parameter is greater than zero.
!>
!>        Fixed short error message in umbrella routine.
!>
!>-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
!>
!>### Implementation notes
!>
!>     Hash control area items.
!>
!>## ZZHSI (Private---Add-only Integer Hash)
!>
!> PEPPER: SPICELIB's umbrella routine ZZHSI is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzhsi. Its documentation follows.
!>
!>### Abstract
!>
!>     SPICE Private routine intended solely for the support of SPICE
!>     routines. Users should not call this routine directly due
!>     to the volatile nature of this routine.
!>
!>     Manipulate add-only integer hash.
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
!>     LBPOOL      is the lower bound of the collision list array.
!>
!>### Exceptions
!>
!>     1) If ZZHSI is called directly, the error SPICE(BOGUSENTRY) is
!>        signaled.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     ZZHSI should never be called directly, but should instead be
!>     accessed only through its entry points.
!>
!>     The purpose of this routine is to manipulate add-only integer
!>     hashes used for buffering purposes by various SPICE subsystems.
!>     This umbrella has the following entry points:
!>
!>           ZZHSIINI       Initialize a hash.
!>
!>           ZZHSIADD       Add an item to a hash.
!>
!>           ZZHSICHK       Check if a item is in a hash.
!>
!>           ZZHSIAVL       Get available room in hash.
!>
!>           ZZHSIINF       Get h/k information about hash.
!>
!>     An add-only hash consists of the head node pointer list (HEDLST),
!>     hash node collision list (COLLST), and the item list (ITEMS).
!>
!>     The function ZZHASH2 computes the index of an element in the head
!>     node pointer list (HEDLST).
!>
!>     The HEDLST element at this index contains either 0, indicating
!>     that there is not yet data for this hash value in the hash, or
!>     the index of the head node and its item in the hash collision
!>     (COLLST) and item (ITEMS) lists.
!>
!>     The COLLST element at the head node index contain either 0,
!>     indicating that there is only one item for this hash value in the
!>     hash, or the index of the next node and its item in the hash
!>     collision (COLLST) and item (ITEMS) lists.
!>
!>     The COLLST element at the next node index contain either 0,
!>     indicating that this is the last item for this hash value in the
!>     hash, or the index of the next node and its item in the hash
!>     collision (COLLST) and item (ITEMS) lists.
!>
!>     And so on.
!>
!>     Pictorially the hash looks like this:
!>
!>                     List of head   List of hash      List of
!>                        nodes        collisions        items
!>
!>                       HEDLST          COLLST          ITEMS
!>
!>                                     +---------+
!>                                     |         |
!>                                     +---------+
!>                                         ...
!>                                     +---------+
!>                                     | 1st Free|
!>                                     +---------+
!>                                     |  Size   |
!>                     +---------+     +---------+     +---------+
!>                     |         |     |         |     |         |
!>                     +---------+ !=0 +---------+     +---------+
!>                  .->|Head Node| -.  |         |     |         |
!>                  |  +---------+  |  +---------+     +---------+
!>       ZZHASHI(ITEM) |         |  |  |         |     |         |
!>                     +---------+  |  +---------+     +---------+
!>                     |         |  `->|Head of  |     |Item     |
!>                     |         |     |collision| !=0 |corresp. |
!>                     |         |     |list for | -.  |to head  |
!>                     |         |     | ITEM    |  |  |of list  |
!>                     +---------+     +---------+  |  +---------+
!>                     |         |     |         |  |  |         |
!>                     +---------+     +---------+  |  +---------+
!>                     |         |     |         |<-'  |         |
!>                     |         |     |Next Node| !=0 |Next Item|
!>                     |         |     |         |--.  |         |
!>                     +---------+     +---------+  |  +---------+
!>                     |         |     |         |  |  |         |
!>                     +---------+     +---------+  |  +---------+
!>                     |         |     |         |  |  |         |
!>                     +---------+     +---------+  |  +---------+
!>                     |         |     |Next Node|<-'  |Next Item|
!>                     +---------+     +---------+ etc +---------+
!>                        ...              ...            ...
!>                     +---------+     +---------+     +---------+
!>                     |         |     |         |     |         |
!>                     +---------+     +---------+     +---------+
!>
!>### Examples
!>
!>     An add-only hash used together with flat data arrays provides a
!>     simple, fast-access buffering mechanism. The subsystems that need
!>     such buffering would normally initialize the hash, add items to
!>     the hash and buffer data associated with items in flat data
!>     arrays, and find items in the hash and, if found, use buffered
!>     data associated with the items rather than computing/getting
!>     their data again.
!>
!>     An add-only hash is normally used in one of the following ways --
!>     set up to never be filled up, stop buffering when filled up, and
!>     trash when filled up and start buffering again. Three examples
!>     below illustrate each of these ways.
!>
!>     Example 1: Set up to never be filled up
!>     ---------------------------------------
!>
!>     C
!>     C     Parameters: list lower boundary, hash size and item
!>     C     size.
!>     C
!>           INTEGER               LBPOOL
!>           PARAMETER           ( LBPOOL = -5 )
!>
!>           INTEGER               HSHSIZ
!>           PARAMETER           ( HSHSIS = 5003 )
!>
!>           INTEGER               NAMSIZ
!>           PARAMETER           ( NAMSIZ = 32 )
!>
!>     C
!>     C     Hash arrays and data buffer.
!>     C
!>           INTEGER               HEDLST  (          HSHSIZ )
!>           INTEGER               COLLST  ( LBPOOL : HSHSIZ )
!>           INTEGER               ITEMS   (          HSHSIZ )
!>           INTEGER               DATBUF  (          HSHSIZ )
!>
!>     C
!>     C     Miscellaneous variables.
!>     C
!>           INTEGER               ITEM
!>
!>           INTEGER               ITEMAT
!>           INTEGER               MYDATA
!>
!>           LOGICAL               FIRST
!>           INTEGER               AVAIL
!>           LOGICAL               NEW
!>
!>     C
!>     C     Data items.
!>     C
!>           DATA FIRST             / .TRUE.  /
!>
!>     C
!>     C     Initialize hash.
!>     C
!>           IF ( FIRST ) THEN
!>              CALL ZZHSIINI ( HSHSIZ, HEDLST, COLLST )
!>              FIRST = .FALSE.
!>           END IF
!>           ...
!>     C
!>     C     Check for presence of or add an item to the hash. If the
!>     C     item is in the hash, NEW will be .FALSE. If the item is
!>     C     not in the hash, it will be added and NEW will be .TRUE.
!>     C     This call can fail only if the hash fills up which
!>     C     should not happen because it was declared to never fill
!>     C     up.
!>     C
!>           CALL ZZHSIADD ( HEDLST, COLLST, ITEMS, ITEM,
!>          .                                ITEMAT, NEW )
!>           IF ( FAILED() ) THEN
!>              RETURN
!>           END IF
!>
!>           IF ( .NOT. NEW ) THEN
!>     C
!>     C        Simply use buffered value.
!>     C
!>              MYDATA = DATBUF( ITEMAT )
!>
!>           ELSE
!>     C
!>     C        Get value.
!>     C
!>              ...
!>     C
!>     C        Buffer value.
!>     C
!>              DATBUF( ITEMAT ) = MYDATA
!>
!>           END IF
!>
!>     Example 2: Stop buffering when filled up
!>     ----------------------------------------
!>
!>     C
!>     C     Use the same declarations as the first example.
!>     C
!>           ...
!>     C
!>     C     Initialize hash.
!>     C
!>           IF ( FIRST ) THEN
!>              CALL ZZHSIINI ( HSHSIZ, HEDLST, COLLST )
!>              FIRST = .FALSE.
!>           END IF
!>           ...
!>     C
!>     C     Check if this item is in the hash.
!>     C
!>           CALL ZZHSICHK ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT )
!>
!>           IF ( ITEMAT .NE. 0 ) THEN
!>     C
!>     C        Simply use buffered value.
!>     C
!>              MYDATA = DATBUF( ITEMAT )
!>
!>           ELSE
!>     C
!>     C        Get value.
!>     C
!>              ...
!>     C
!>     C        Buffer value, but only if the hash is not full.
!>     C
!>              CALL ZZHSIAVL( COLLST, AVAIL )
!>              IF ( AVAIL .GT. 0 ) THEN
!>                 CALL ZZHSIADD ( HEDLST, COLLST, ITEMS, ITEM,
!>          .                                  ITEMAT, NEW )
!>                 DATBUF( ITEMAT ) = MYDATA
!>              END IF
!>
!>           END IF
!>
!>     Example 3: Trash when filled up and start buffering again
!>     ---------------------------------------------------------
!>
!>     C
!>     C     Use the same declarations as the first example.
!>     C
!>           ...
!>     C
!>     C     Initialize hash.
!>     C
!>           IF ( FIRST ) THEN
!>              CALL ZZHSIINI ( HSHSIZ, HEDLST, COLLST )
!>              FIRST = .FALSE.
!>           END IF
!>           ...
!>     C
!>     C     Check if this item is in the hash.
!>     C
!>           CALL ZZHSICHK ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT )
!>
!>           IF ( ITEMAT .NE. 0 ) THEN
!>     C
!>     C        Simply use buffered value.
!>     C
!>              MYDATA = DATBUF( ITEMAT )
!>
!>           ELSE
!>     C
!>     C        Get value.
!>     C
!>              ...
!>     C
!>     C        Buffer value, if the hash is full trash re-initialize
!>     C        it first.
!>     C
!>              CALL ZZHSIAVL( COLLST, AVAIL )
!>              IF ( AVAIL .LE. 0 ) THEN
!>                 CALL ZZHSIINI ( HSHSIZ, HEDLST, COLLST )
!>              END IF
!>              CALL ZZHSIADD ( HEDLST, COLLST, ITEMS, ITEM,
!>          .                                  ITEMAT, NEW )
!>              DATBUF( ITEMAT ) = MYDATA
!>
!>           END IF
!>
!>### Restrictions
!>
!>     For sake of speed all entry points do no or minimal error
!>     checking. It is the responsibility of the caller to ensure that
!>     all inputs are properly initialized.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     J. Diaz del Rio    (ODC Space)
!>     B.V. Semenov       (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 1.1.0, 14-OCT-2021 (JDR) (BVS) (NJB)
!>
!>        Changed entry point ZZHSIINI to not check RETURN()
!>        on entry. That entry point now initializes the hash
!>        as long as the size parameter is greater than zero.
!>
!>        Fixed short error message in umbrella routine.
!>
!>        Bug fix: fixed routine name in CHKIN/CHKOUT calls
!>        in the ZZHSICHK entry (ZZHSIADD -> ZZHSICHK).
!>
!>-    SPICELIB Version 1.0.0, 01-AUG-2013 (BVS)
!>
!>### Implementation notes
!>
!>     Hash control area items.
!>
!>## ZZBODBLT (Private --- Retrieve Built-In Body-Code Maps)
!>
!> PEPPER: SPICELIB's umbrella routine ZZBODBLT is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzbodblt. Its documentation follows.
!>
!>### Abstract
!>
!>     SPICE Private routine intended solely for the support of SPICE
!>     routines.  Users should not call this routine directly due
!>     to the volatile nature of this routine.
!>
!>     This is the umbrella routine that contains entry points to
!>     access the built-in body name-code mappings.
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
!>     See the include file 'zzbodtrn.inc' for the list of parameters
!>     this routine utilizes.
!>
!>### Exceptions
!>
!>     1) The error SPICE(BOGUSENTRY) is signaled if ZZBODBLT is
!>        called directly.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     ZZBODBLT should never be called directly, instead access
!>     the entry points:
!>
!>        ZZBODGET      Fetch the built-in body name/code list.
!>
!>        ZZBODLST      Output the name-ID mapping list.
!>
!>### Examples
!>
!>     See ZZBODTRN and its entry points for details.
!>
!>### Restrictions
!>
!>     1) No duplicate entries should appear in the built-in
!>        BLTNAM list.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     F.S. Turner     (JPL)
!>     B.V. Semenov    (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 2.3.2, 01-OCT-2021 (NJB)
!>
!>        Corrected typos in comments.
!>
!>-    SPICELIB Version 2.3.1, 27-FEB-2007 (EDW)
!>
!>        Completed the ZZBODLST declarations section.
!>
!>-    SPICELIB Version 2.3.0, 17-MAR-2003 (EDW)
!>
!>        Added a call to ZZIDMAP to retrieve the default
!>        mapping list. "zzbodtrn.inc" no longer
!>        contains the default mapping list.
!>
!>-    SPICELIB Version 2.2.0  21-FEB-2003 (BVS)
!>
!>        Changed MER-A and MER-B to MER-1 and MER-2.
!>
!>-    SPICELIB Version 2.1.0  04-DEC-2002 (EDW)
!>
!>       Added new assignments to the default collection:
!>
!>       -226     ROSETTA
!>        517     CALLIRRHOE
!>        518     THEMISTO
!>        519     MAGACLITE
!>        520     TAYGETE
!>        521     CHALDENE
!>        522     HARPALYKE
!>        523     KALYKE
!>        524     IOCASTE
!>        525     ERINOME
!>        526     ISONOE
!>        527     PRAXIDIKE
!>
!>-    SPICELIB Version 2.0.0, 23-AUG-2002 (FST)
!>
!>        Initial release.  This begins at Version 2.0.0 because
!>        the entry point ZZBODLST was cut out of ZZBODTRN and
!>        placed here at Version 1.0.0.
!>
!>### Revisions
!>
!>-    SPICELIB Version 2.0.0, 23-AUG-2002 (FST)
!>
!>        The entries following this one were copied from
!>        the version section of ZZBODTRN.  SPICELIB has
!>        been changed to ZZBODTRN for convenience in noting
!>        version information relevant for that module.
!>
!>        This was done to carry the history of body name-code
!>        additions with this new umbrella.
!>
!>        Added to the collection:
!>        -236   MESSENGER
!>
!>-    ZZBODTRN Version 3.2.0, 14-AUG-2002 (EDW)
!>
!>        Added the ZZBODKIK entry point.
!>
!>        Moved the NAIF_BODY_NAME/CODE to subroutine
!>        ZZBODKER. No change in logic.
!>
!>        Added logic to enforce the precedence masking;
!>        logic removes duplicate assignments of ZZBODDEF.
!>        Removed the NAMENOTUNIQUE error block.
!>
!>-    ZZBODTRN Version 3.1.5, 27-NOV-2001 (EDW)
!>
!>        Added to the collection:
!>        -200   CONTOUR
!>        -146   LUNAR-A
!>        -135   DRTS-W
!>
!>        Added the subroutine ZZBODLST as an entry point.
!>        The routine outputs the current name-ID mapping
!>        list to some output device.
!>
!>-    ZZBODTRN Version 3.1.0, 17-OCT-2001 (EDW)
!>
!>        To improve clarity, the BEGXX block initialization now
!>        exists in the include file zzbodtrn.inc.
!>
!>        Removed the comments concerning the 851, 852, ... temporary
!>        codes.
!>
!>        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME
!>        as a DATA statement.
!>
!>        Edited headers to match information in naif_ids required
!>        reading.
!>
!>        Edited headers, removed typos and bad grammar, clarified
!>        descriptions.
!>
!>        Added to the collection
!>        -41    MARS EXPRESS, MEX
!>        -44    BEAGLE 2, BEAGLE2
!>        -70    DEEP IMPACT IMPACTOR SPACECRAFT
!>        -94    MO, MARS OBSERVER
!>        -140   DEEP IMPACT FLYBY SPACECRAFT
!>        -172   SLCOMB, STARLIGHT COMBINER
!>        -205   SLCOLL, STARLIGHT COLLECTOR
!>        -253   MER-A
!>        -254   MER-B
!>
!>        Corrected typo, vehicle -188 should properly be MUSES-C,
!>        previous versions listed the name as MUSES-B.
!>
!>        Removed from collection
!>        -84    MARS SURVEYOR 01 LANDER
!>        -154   EOS-PM1
!>        -200   PLUTO EXPRESS 1, PEX1
!>        -202   PLUTO EXPRESS 2, PEX2
!>
!>-    ZZBODTRN Version 3.0.0, 29-MAR-2000 (WLT)
!>
!>        The ID codes for Cluster 1, 2, 3 and 4 were added.  The
!>        ID coded for Pluto Express were removed.  The ID codes
!>        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation
!>        and Contour were added.
!>
!>-    ZZBODTRN Version 2.0.0, 26-JAN-1998 (EDW)
!>
!>        The Galileo probe ID -228 replaces the incorrect ID -344.
!>        DSS stations 5 through 65 added to the collection.
!>
!>        Added to the collection
!>        -107   TROPICAL RAINFALL MEASURING MISSION, TRMM
!>        -154,  EOS-PM1
!>        -142   EOS-AM1
!>        -151   AXAF
!>        -1     GEOTAIL
!>        -13    POLAR
!>        -21    SOHO
!>        -8     WIND
!>        -25    LUNAR PROSPECTOR, LPM
!>        -116   MARS POLAR LANDER, MPL
!>        -127   MARS CLIMATE ORBITER, MCO
!>        -188   MUSES-C
!>        -97    TOPEX/POSEIDON
!>        -6     PIONEER-6, P6
!>        -7     PIONEER-7, P7
!>        -20    PIONEER-8, P8
!>        -23    PIONEER-10, P10
!>        -24    PIONEER-11, P11
!>        -178   NOZOMI, PLANET-B
!>        -79    SPACE INFRARED TELESCOPE FACILITY, SIRTF
!>        -29    STARDUST, SDU
!>        -47    GENESIS
!>        -48    HUBBLE SPACE TELESCOPE, HST
!>        -200   PLUTO EXPRESS 1, PEX1
!>        -202   PLUTO EXPRESS 2, PEX2
!>        -164   YOHKOH, SOLAR-A
!>        -165   MAP
!>        -166   IMAGE
!>        -53    MARS SURVEYOR 01 ORBITER
!>         618   PAN
!>         716   CALIBAN
!>         717   SYCORAX
!>        -30    DS-1 (low priority)
!>        -58    HALCA
!>        -150   HUYGEN PROBE, CASP
!>        -55    ULS
!>
!>        Modified ZZBODC2N and ZZBODN2C so the user may load an
!>        external IDs kernel to override or supplement the standard
!>        collection.  The kernel must be loaded prior a call to
!>        ZZBODC2N or ZZBODN2C.
!>
!>-    ZZBODTRN Version 1.1.0, 22-MAY-1996 (WLT)
!>
!>        Added the id-code for Comet Hyakutake, Comet Hale-Bopp,
!>        Mars 96, Cassini Simulation, MGS Simulation.
!>
!>-    ZZBODTRN Version 1.0.0, 25-SEP-1995 (BVS)
!>
!>        Renamed umbrella subroutine and entry points to
!>        correspond private routine convention (ZZ...). Added IDs for
!>        tracking stations Goldstone (399001), Canberra (399002),
!>        Madrid (399003), Usuda (399004).
!>
!>-    ZZBODTRN Version 2.2.0, 01-AUG-1995 (HAN)
!>
!>        Added the IDs for Near Earth Asteroid Rendezvous (-93),
!>        Mars Pathfinder (-53), Ulysses (-55), VSOP (-58),
!>        Radioastron (-59), Cassini spacecraft (-82), and Cassini
!>        Huygens probe (-150).
!>        Mars Observer (-94) was replaced with Mars Global
!>        Surveyor (-94).
!>
!>-    ZZBODTRN Version 2.1.0, 15-MAR-1995 (KSZ) (HAN)
!>
!>        Two Shoemaker Levy 9 fragments were added, Q1 and P2
!>        (IDs 50000022 and 50000023). Two asteroids were added,
!>        Eros and Mathilde (IDs 2000433 and 2000253). The
!>        Saturnian satellite Pan (ID 618) was added.
!>
!>-    ZZBODTRN Version 2.0.0, 03-FEB-1995 (NJB)
!>
!>        The Galileo probe (ID -344) has been added to the permanent
!>        collection.
!>
!>-    ZZBODTRN Version 1.0.0, 29-APR-1994 (MJS)
!>
!>        SPICELIB symbol tables are no longer used. Instead, two order
!>        vectors are used to index the NAMES and CODES arrays. Also,
!>        this version does not support reading body name ID pairs from a
!>        file.
!>
!>-    ZZBODTRN Version 2.0.1, 10-MAR-1992 (WLT)
!>
!>        Comment section for permuted index source lines was added
!>        following the header.
!>
!>-    ZZBODTRN Version 2.0.0, 15-JUL-1991 (WLT)
!>
!>       The body id's for the Uranian satellites discovered by Voyager
!>       were modified to conform to those established by the IAU
!>       nomenclature committee.  In addition the id's for Gaspra and
!>       Ida were added.
!>
!>-    ZZBODTRN Version 1.0.0,  7-MAR-1991 (WLT)
!>
!>       Some items previously considered errors were removed
!>       and some minor modifications were made to improve the
!>       robustness of the routines.
!>
!>-    ZZBODTRN Version 1.0.0, 28-JUN-1990 (JEM)
!>
!>## ZZBODTRN (Private --- Body name and code translation)
!>
!> PEPPER: SPICELIB's umbrella routine ZZBODTRN is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_zzbodtrn. Its documentation follows.
!>
!>### Abstract
!>
!>     SPICE Private routine intended solely for the support of SPICE
!>     routines.  Users should not call this routine directly due
!>     to the volatile nature of this routine.
!>
!>     This is the umbrella routine that contains entry points to
!>     translate between body names and NAIF integer codes, and
!>     for definition of new name/code pairs.
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
!>     These parameters are defined in zzbodtrn.inc:
!>
!>     MAXL        is the maximum length of a body name.
!>
!>     MAXP        is the maximum number of additional names that may
!>                 be added via the ZZBODDEF interface.
!>
!>     NPERM       is the count of the mapping assignments built into
!>                 SPICE.
!>
!>     MAXE        is the size of the lists and hashes storing combined
!>                 built-in and ZZBODDEF-defined name/ID mappings. To
!>                 ensure efficient hashing this size is the set to the
!>                 first prime number greater than ( MAXP + NPERM ).
!>
!>     NROOM       is the size of the lists and hashes storing the
!>                 POOL-defined name/ID mappings. To ensure efficient
!>                 hashing and to provide the ability to store nearly as
!>                 many names as can fit in the POOL, this size is
!>                 set to the first prime number less than MAXLIN
!>                 defined in the POOL umbrella routine.
!>
!>     This parameter is defined in zzctr.inc:
!>
!>     CTRSIZ     is the dimension of the counter array used by various
!>                SPICE subsystems to identify changes in their states.
!>
!>### Exceptions
!>
!>     1) The error SPICE(BOGUSENTRY) is signaled if ZZBODTRN
!>        is called directly.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     ZZBODTRN should never be called, instead access the entry
!>     points:
!>
!>        ZZBODN2C      Body name to code
!>
!>        ZZBODC2N      Body code to name
!>
!>        ZZBODDEF      Body name/code definition
!>
!>        ZZBODKIK      Force an examination of the kernel pool
!>                      variables, subsequent processing and
!>                      the generation of any error messages
!>                      resultant from the processing.
!>
!>        ZZBODRST      Reset the mappings provided via the ZZBODDEF
!>                      interface.
!>
!>        ZZBCTRCK      Check and, if needed, update the caller's copy
!>                      of the ZZBODTRN state counter.
!>
!>     ZZBODN2C and ZZBODC2N perform translations between body names
!>     and their corresponding integer codes used in SPK and PCK files
!>     and associated routines.  A default set of name/code
!>     pairs are automatically defined during the first call to
!>     any of the entry points.  Additional name/code pairs may
!>     be defined via ZZBODDEF for two purposes:
!>
!>        1) to associate another, perhaps more familiar or
!>           abbreviated name with a previously defined body
!>           integer code
!>
!>        2) to define a new body integer code and name
!>
!>     Each body name maps to a unique integer code, but more than
!>     one name may map to a code.  Associating more than one
!>     integer code with a particular name creates ambiguity.
!>     Therefore the name-code mapping system establishes a
!>     clearly defined precedence structure that assures at any
!>     given instant only one code is assigned to a particular
!>     name.
!>
!>     Entries provided via the kernel pool variables are examined
!>     first to resolve name-code mappings.  The last listed entries
!>     in the kernel pool arrays NAIF_BODY_CODE and NAIF_BODY_NAME
!>     resolve any ambiguities that occur.  For example, consider
!>     the following text kernel excerpt:
!>
!>        \begindata
!>
!>           NAIF_BODY_NAME += 'NAME'
!>           NAIF_BODY_CODE += 1000
!>
!>           NAIF_BODY_NAME += 'NAME'
!>           NAIF_BODY_CODE += 1001
!>
!>        \begintext
!>
!>     If, after loading this kernel, the following calls are made:
!>
!>        CALL ZZBODN2C ( 'NAME', CODE,  NAMFND )
!>
!>        CALL ZZBODC2N ( 1000,   NAME0, FND000 )
!>        CALL ZZBODC2N ( 1001,   NAME1, FND001 )
!>
!>     The values of CODE, NAMFND, NAME0, FND000, NAME1, and FND001
!>     will be:
!>
!>        NAMFND = .TRUE.,  CODE  = 1001
!>        FND000 = .FALSE., NAME0 remains unchanged
!>        FND001 = .TRUE.,  NAME1 = 'NAME'
!>
!>     FND000 is .FALSE., because this name-code mapping is masked
!>     by the higher precedent 'NAME' <-> 1001 mapping.
!>
!>     If the name-code mapping is not resolved by the entries
!>     provided in the kernel pool, the values assigned via the
!>     ZZBODDEF interface are examined next.  As with the kernel
!>     pool, the last assignment made via the ZZBODDEF interface
!>     has the highest precedence.  Lastly, if the name-code
!>     mapping is not resolved by the contents of ZZBODDEF, the
!>     built-in mappings are examined.  In actuality, the built-in
!>     mappings represent an initial state of the ZZBODDEF listings.
!>     As changes are made to this listing, the original mappings
!>     are discarded.
!>
!>     For the case in which multiple names map to a single code, a
!>     ZZBODC2N call returns the name last assigned to that code - a
!>     LIFO situation.
!>
!>### Examples
!>
!>     1) The following code fragment shows SPKEZ compute the state
!>        (position and velocity) of Jupiter as seen from the Galileo
!>        Orbiter.  It requires the NAIF integer codes of the target
!>        and observer, so we use ZZBODN2C to convert names to integer
!>        codes for those bodies.
!>
!>           CALL ZZBODN2C ( 'JUPITER',         TARGET, FOUND )
!>
!>           CALL ZZBODN2C ( 'GALILEO ORBITER', OBSRVR, FOUND )
!>
!>           CALL SPKEZ    ( TARGET, EPOCH, FRAME, ABCORR,
!>          .                OBSRVR, STATE, LT             )
!>
!>     2) This example assumes ZZBODDEF has not been called.
!>        Thus, only the set of default name/code pairs has been
!>        defined.
!>
!>        Given these names, ZZBODN2C returns the following codes:
!>
!>           Name                         Code    Found?
!>           ------------------------   ------    ------
!>           'EARTH'                       399    Yes
!>           '  Earth '                    399    Yes
!>           'EMB'                           3    Yes
!>           'Solar System Barycenter'       0    Yes
!>           'SolarSystemBarycenter'         -    No
!>           'SSB'                           0    Yes
!>           'Voyager 2'                   -32    Yes
!>           'U.S.S. Enterprise'             -    No
!>           ' '                             -    No
!>           'Halley's Comet'                -    No
!>
!>        and, given these codes, ZZBODC2N returns the following
!>        names:
!>
!>           Code        Name                        Found?
!>           -------     -------------------         ------
!>           399         'EARTH'                     Yes
!>             0         'SOLAR SYSTEM BARYCENTER'   Yes
!>             3         'EARTH BARYCENTER'          Yes
!>           -77         'GALILEO ORBITER'           Yes
!>            11          -                          No
!>            -1         'GEOTAIL'                   Yes
!>
!>     3) This example shows the method to define a name/code pair.
!>        You may associate a new name with a previously defined
!>        code:
!>
!>           CALL ZZBODDEF ( 'JB', 5 )
!>
!>        You may also define the name and integer code for a new
!>        body:
!>
!>           CALL ZZBODDEF ( 'Asteroid Frank', 20103456 )
!>
!>        After these calls to ZZBODDEF, ZZBODN2C would return
!>        the following translations:
!>
!>           Name                         Code    Found?
!>           ------------------------   ------    ------
!>           'JB'                            5    Yes
!>           'Jupiter Barycenter'            5    Yes
!>           'ASTEROID FRANK'         20103456    Yes
!>           'ASTEROIDFRANK'                 -    No
!>           'Frank'                         -    No
!>
!>        and ZZBODC2N returns these translations:
!>
!>           Code        Name                     Found?
!>           -------     -------------------      ------
!>                  5    'JB'                     Yes
!>           20103456    'Asteroid Frank'         Yes
!>
!>        ZZBODC2N exactly returns the string as used in the
!>        body name/ID mapping definition.
!>
!>     4) To use an external IDs kernel, simply load via a FURNSH
!>        call.
!>
!>           CALL FURNSH ( 'ids.ker' )
!>
!>        With ids.ker listing data such as:
!>
!>           \begintext
!>
!>           Define an additional set of body, ID code mappings.
!>
!>           \begindata
!>
!>           NAIF_BODY_CODE  = ( 22, 23, 24, 25 )
!>
!>           NAIF_BODY_NAME  = ( 'LARRY', 'MOE', 'CURLEY', 'SHEMP' )
!>
!>        Which maps the names defined in NAIF_BODY_NAME
!>        to the corresponding index of NAIF_BODY_CODE, i.e.
!>        LARRY -> 22, MOE -> 23, etc, and the IDs in NAIF_BODY_CODE
!>        map to the corresponding index of NAIF_BODY_NAME.
!>
!>        NOTE:  When using an external NAME-ID kernel, all ID codes
!>        MUST be listed in the kernel variable NAIF_BODY_CODE, and
!>        all names MUST be listed in the kernel variable
!>        NAIF_BODY_NAME.
!>
!>     5) Suppose you ran the utility program SPACIT to summarize
!>        an SPK ephemeris file and the following data was output
!>        to the terminal screen.
!>
!>           ----------------------------------------------------------
!>           Segment identifier: JPL archive 21354
!>           Body        : -77                         Center     : 399
!>           From        : 1990 DEC 08 18:00:00.000
!>           To          : 1990 DEC 10 21:10:00.000
!>           Reference   : DE-200                      SPK Type    :1
!>           ----------------------------------------------------------
!>
!>        You could write a program to translate the body codes
!>        shown in the SPACIT output:
!>
!>           CALL ZZBODC2N ( -77, BODY,   FOUND )
!>           CALL ZZBODC2N ( 399, CENTER, FOUND )
!>
!>           IF ( FOUND ) THEN
!>
!>              WRITE ( *,* ) 'BODY:    -77 = ', BODY
!>              WRITE ( *,* ) 'CENTER:  399 = ', CENTER
!>
!>           END IF
!>
!>        You could also read the body and center codes directly from
!>        the SPK files, using the appropriate DAF routines, and then
!>        translate them, as above.
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
!>     J.E. McLean    (JPL)
!>     H.A. Neilan    (JPL)
!>     B.V. Semenov   (JPL)
!>     M.J. Spencer   (JPL)
!>     W.L. Taber     (JPL)
!>     F.S. Turner    (JPL)
!>     E.D. Wright    (JPL)
!>     K.S. Zukor     (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
!>
!>        Changed to use name- and ID-based hashes instead of the order
!>        arrays (see inline comments next to hash declarations for
!>        descriptions of the hashes).
!>
!>        Changed to keep track of the POOL's and ZZBODTRN internal
!>        states using state counters.
!>
!>        Changed to call ZZCVPOOL to bypass watcher look-ups if the
!>        POOL counter did not change.
!>
!>        Added ZZBCTRCK routine to allow a caller to check the ZZBODTRN
!>        state counter against the caller's saved counter.
!>
!>        Changed umbrella calling sequence to include USRCTR and UPDATE
!>        arguments.
!>
!>-    SPICELIB Version 4.3.0, 05-MAR-2009 (NJB)
!>
!>        Bug fixes: the entry points ZZBODN2C, ZZBODC2N, and ZZBODKIK
!>        now keep track of whether their kernel pool look-ups
!>        succeeded. If not, a kernel pool lookup is attempted on the
!>        next call to any entry point that calls ZZBODKER.
!>
!>-    SPICELIB Version 4.0.2, 19-SEP-2006 (EDW)
!>
!>        Added text to previously empty Declarations section.
!>
!>-    SPICELIB Version 4.0.1, 17-APR-2003 (EDW)
!>
!>        Corrected typo in header docs.
!>
!>-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
!>
!>        Cleaned up  ZZBODTRN routine/entry point source code
!>        and private subroutines used exclusively by ZZBODTRN
!>        to process name-code mappings.
!>
!>        ZZBODLST has been removed from this umbrella and
!>        added to the ZZBODBLT umbrella.
!>
!>        The built-in (permanent collection) of name-code
!>        mappings has been moved from this umbrella into
!>        the ZZBODBLT umbrella.  The collection is retrieved
!>        from the entry point ZZBODGET in ZZBODBLT.
!>
!>        See the Revisions section below for details.
!>
!>-    SPICELIB Version 3.2.0, 14-AUG-2002 (EDW)
!>
!>        Added the ZZBODKIK entry point.
!>
!>        Moved the NAIF_BODY_NAME/CODE to subroutine
!>        ZZBODKER. No change in logic.
!>
!>        Added logic to enforce the precedence masking;
!>        logic removes duplicate assignments of ZZBODDEF.
!>        Removed the NAMENOTUNIQUE error block.
!>
!>-    SPICELIB Version 3.1.5, 27-NOV-2001 (EDW)
!>
!>        Added to the collection:
!>        -200   CONTOUR
!>        -146   LUNAR-A
!>        -135   DRTS-W
!>
!>        Added the subroutine ZZBODLST as an entry point.
!>        The routine outputs the current name-ID mapping
!>        list to some output device.
!>
!>-    SPICELIB Version 3.1.0, 17-OCT-2001 (EDW)
!>
!>        To improve clarity, the BEGXX block initialization now
!>        exists in the include file zzbodtrn.inc.
!>
!>        Removed the comments concerning the 851, 852, ... temporary
!>        codes.
!>
!>        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME
!>        as a DATA statement.
!>
!>        Edited headers to match information in naif_ids required
!>        reading.
!>
!>        Edited headers, removed typos and bad grammar, clarified
!>        descriptions.
!>
!>        Added to the collection
!>        -41    MARS EXPRESS, MEX
!>        -44    BEAGLE 2, BEAGLE2
!>        -70    DEEP IMPACT IMPACTOR SPACECRAFT
!>        -94    MO, MARS OBSERVER
!>        -140   DEEP IMPACT FLYBY SPACECRAFT
!>        -172   SLCOMB, STARLIGHT COMBINER
!>        -205   SLCOLL, STARLIGHT COLLECTOR
!>        -253   MER-A
!>        -254   MER-B
!>
!>        Corrected typo, vehicle -188 should properly be MUSES-C,
!>        previous versions listed the name as MUSES-B.
!>
!>        Removed from collection
!>        -84    MARS SURVEYOR 01 LANDER
!>        -154   EOS-PM1
!>        -200   PLUTO EXPRESS 1, PEX1
!>        -202   PLUTO EXPRESS 2, PEX2
!>
!>-    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT)
!>
!>        The ID codes for Cluster 1, 2, 3 and 4 were added.  The
!>        ID coded for Pluto Express were removed.  The ID codes
!>        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation
!>        and Contour were added.
!>
!>-    SPICELIB Version 2.0.0, 26-JAN-1998 (EDW)
!>
!>        The Galileo probe ID -228 replaces the incorrect ID -344.
!>        DSS stations 5 through 65 added to the collection.
!>
!>        Added to the collection
!>        -107   TROPICAL RAINFALL MEASURING MISSION, TRMM
!>        -154,  EOS-PM1
!>        -142   EOS-AM1
!>        -151   AXAF
!>        -1     GEOTAIL
!>        -13    POLAR
!>        -21    SOHO
!>        -8     WIND
!>        -25    LUNAR PROSPECTOR, LPM
!>        -116   MARS POLAR LANDER, MPL
!>        -127   MARS CLIMATE ORBITER, MCO
!>        -188   MUSES-C
!>        -97    TOPEX/POSEIDON
!>        -6     PIONEER-6, P6
!>        -7     PIONEER-7, P7
!>        -20    PIONEER-8, P8
!>        -23    PIONEER-10, P10
!>        -24    PIONEER-11, P11
!>        -178   NOZOMI, PLANET-B
!>        -79    SPACE INFRARED TELESCOPE FACILITY, SIRTF
!>        -29    STARDUST, SDU
!>        -47    GENESIS
!>        -48    HUBBLE SPACE TELESCOPE, HST
!>        -200   PLUTO EXPRESS 1, PEX1
!>        -202   PLUTO EXPRESS 2, PEX2
!>        -164   YOHKOH, SOLAR-A
!>        -165   MAP
!>        -166   IMAGE
!>        -53    MARS SURVEYOR 01 ORBITER
!>         618   PAN
!>         716   CALIBAN
!>         717   SYCORAX
!>        -30    DS-1 (low priority)
!>        -58    HALCA
!>        -150   HUYGEN PROBE, CASP
!>        -55    ULS
!>
!>        Modified ZZBODC2N and ZZBODN2C so the user may load an
!>        external IDs kernel to override or supplement the standard
!>        collection.  The kernel must be loaded prior a call to
!>        ZZBODC2N or ZZBODN2C.
!>
!>-    SPICELIB Version 1.1.0, 22-MAY-1996 (WLT)
!>
!>        Added the id-code for Comet Hyakutake, Comet Hale-Bopp,
!>        Mars 96, Cassini Simulation, MGS Simulation.
!>
!>-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
!>
!>        Renamed umbrella subroutine and entry points to
!>        correspond private routine convention (ZZ...). Added IDs for
!>        tracking stations Goldstone (399001), Canberra (399002),
!>        Madrid (399003), Usuda (399004).
!>
!>-    Beta Version 2.2.0, 01-AUG-1995 (HAN)
!>
!>        Added the IDs for Near Earth Asteroid Rendezvous (-93),
!>        Mars Pathfinder (-53), Ulysses (-55), VSOP (-58),
!>        Radioastron (-59), Cassini spacecraft (-82), and Cassini
!>        Huygens probe (-150).
!>        Mars Observer (-94) was replaced with Mars Global
!>        Surveyor (-94).
!>
!>-    Beta Version 2.1.0, 15-MAR-1995 (KSZ) (HAN)
!>
!>        Two Shoemaker Levy 9 fragments were added, Q1 and P2
!>        (IDs 50000022 and 50000023). Two asteroids were added,
!>        Eros and Mathilde (IDs 2000433 and 2000253). The
!>        Saturnian satellite Pan (ID 618) was added.
!>
!>-    Beta Version 2.0.0, 03-FEB-1995 (NJB)
!>
!>        The Galileo probe (ID -344) has been added to the permanent
!>        collection.
!>
!>-    Beta Version 1.0.0, 29-APR-1994 (MJS)
!>
!>        SPICELIB symbol tables are no longer used. Instead, two order
!>        vectors are used to index the NAMES and CODES arrays. Also,
!>        this version does not support reading body name ID pairs from a
!>        file.
!>
!>-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
!>
!>        Comment section for permuted index source lines was added
!>        following the header.
!>
!>-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
!>
!>       The body id's for the Uranian satellites discovered by Voyager
!>       were modified to conform to those established by the IAU
!>       nomenclature committee.  In addition the id's for Gaspra and
!>       Ida were added.
!>
!>-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
!>
!>       Some items previously considered errors were removed
!>       and some minor modifications were made to improve the
!>       robustness of the routines.
!>
!>-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
!>
!>### Revisions
!>
!>-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
!>
!>        For clarity, some variable names have changed.  The
!>        mappings from the old names to the new are provided
!>        below:
!>
!>           Old      New     Function
!>           ---      ---     --------
!>           NAMES    DEFNAM  Name definition as provided with ZZBODDEF
!>           NORNAM   DEFNOR  Normalized name definitions
!>           CODES    DEFCOD  Integer codes mapping to entries in DEFNAM
!>           ORDCOD   DEFOCD  "Modified" order vector for DEFCOD
!>           ORDNOM   DEFONR  Order vector for DEFNOR
!>           NNAM     DEFSIZ  Size of DEFNAM, DEFNOR, DEFCOD, and DEFONR
!>           NCOD     DEFOSZ  Size of DEFOCD
!>
!>           CVALS    KERNAM  Name definition as provided from pool
!>           CVLNOM   KERNOM  Normalized name definitions
!>           IVALS    KERCOD  Integer codes mapping to entries in KERNAM
!>           XORDCD   KEROCD  "Modified" order vector for KERCOD
!>           XORNOM   KERONR  Order vector for KERNOR
!>           NUM(1)   DEFSIZ  Size of KERNAM, KERNOR, KERCOD, and KERONR
!>           NUM(2)   DEFOSZ  Size of KEROCD
!>
!>        The reason for changing the names in this fashion,
!>        is simply that these are two instances of variables
!>        that have the same properties and utility.  The first
!>        set implements the ZZBODDEF style mappings, and the
!>        second implements the kernel pool style mappings.
!>
!>        ZZBODDEF now properly signals an error when a caller
!>        attempts to use it to assign a blank string an ID code.
!>        This should have never been allowed, but somehow
!>        slipped by in previous versions.
!>
!>        The argument lists for ZZBODKER and ZZBODINI have
!>        changed as of previous versions.  Some arguments
!>        were removed, as they were no longer necessary.
!>
!>        ZZBODINI no longer normalizes the input name array;
!>        rather it simply computes the order vector for the
!>        normalized array input and the "modified" order
!>        vector for the input code array.  This was done to
!>        save from unnecessarily recomputing the normalization
!>        array.
!>
!>        An additional umbrella has been added to the set of
!>        modules of which ZZBODTRN makes use: ZZBODBLT.  This
!>        umbrella houses the data statements that used to be
!>        present in this module, which defines the "built-in"
!>        name-code mappings.  These mappings, as of the changes
!>        in N0053, store the mappings the define the initial
!>        state of the DEF* arrays.  It contains two entry
!>        points:
!>
!>           ZZBODGET    retrieve the initial values of DEFNAM,
!>                       DEFNOR, DEFCOD, and DEFSIZ.
!>
!>           ZZBODLST    dump the "built-in" codes to a device.
!>
!>        ZZBODLST used to be present in this umbrella, but the
!>        creation of ZZBODBLT made moving it there the logical
!>        choice.
!>
!>        The entry point ZZBODRST has been added to the
!>        ZZBODTRN umbrella.  This entry point resets the
!>        state of the DEF* arrays to their initial values.
!>        This effectively resets any changes made via the
!>        ZZBODDEF interface.  It does not effect the kernel
!>        pool mappings.
!>
!>        To support ZZBODRST, a logical BODCHG has been added
!>        to the list of saved variables.  This variable
!>        indicates when ZZBODDEF has been used to change the
!>        built-in body list.
!>
!>### Implementation notes
!>
!>     Length of watched POOL keywords.
!>
!>     Lower bound of the built-in/BODDEF and kernel-defined hash
!>     collision list arrays.
!>
!>     DEFNAM, DEFNOR, and DEFCOD are the name, normalized name, and ID
!>     code lists storing built-in/BODDEF name-code mappings. DEFSIZ is
!>     the current size of the lists
!>
!>     Name-based hash for built-in/BODDEF bodies. DNMLST, DNMPOL, and
!>     DNMNMS provide the index in DNMIDX which stores the index of the
!>     body name, normalized name, and ID in the arrays DEFNAM, DEFNOR,
!>     DEFCOD.
!>
!>     ID-based hash for built-in/BODDEF bodies. DIDLST, DIDPOL, and
!>     DIDIDS provide the index in DIDIDX which stores the index of the
!>     body name, normalized name, and ID in the arrays DEFNAM, DEFNOR,
!>     DEFCOD.
!>
!>     KERNAM, KERNOR, and KERCOD are the name, normalized name, and ID
!>     code lists storing kernel POOL name-code mappings. KERSIZ is the
!>     current size of the lists
!>
!>     Name-based hash for kernel POOL bodies. KNMLST, KNMPOL, and
!>     KNMNMS provide the index in KNMIDX which stores the index of the
!>     body name, normalized name, and ID in the arrays KERNAM, KERNOR,
!>     KERCOD.
!>
!>     ID-based hash for kernel POOL bodies. KIDLST, KIDPOL, and KIDIDS
!>     provide the index in KIDIDX which stores the index of the body
!>     name, normalized name, and ID in the arrays KERNAM, KERNOR,
!>     KERCOD.
!>
!>     ZZBODTRN state counter and POOL state counter.
!>
!>     Flag indicating whether the built-in list was altered by BODDEF
!>     calls.
!>
!>     Flag indicating whether valid kernel POOL defined mappings
!>     are present in the KERNAM, KERNOR, and KERCOD lists.
!>
!>     Flag indicating whether valid kernel POOL defined mappings
!>     were successfully fetched from the POOL.
submodule (pepper_instance_module) pepper_bodies

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: cdelen = 16  !! length of a body ID code as text
   integer(ip), parameter :: freidx = -1  !! index in a hash list of its first free entry
   integer(ip), parameter :: namlen = 32  !! length of a kernel variable name (NAIF_BODY_NAME ...)
   integer(ip), parameter :: sizidx = 0  !! index in a hash list of its size

contains

      !> The default SPICE body/ID mapping assignments available
      !> to the SPICE library.
      !>
      !>## Detailed Input
      !>
      !>     None.
      !>
      !>## Detailed Output
      !>
      !>     BLTCOD   The array of NPERM elements listing the body ID codes.
      !>
      !>     BLTNAM   The array of NPERM elements listing the body names
      !>              corresponding to the ID entry in BLTCOD
      !>
      !>## Parameters
      !>
      !>     NPERM    The length of both BLTCOD, BLTNAM
      !>              (read from zzbodtrn.inc).
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
      !>     Each ith entry of BLTCOD maps to the ith entry of BLTNAM.
      !>
      !>## Examples
      !>
      !>     Simple to use, a call the ZZIDMAP returns the arrays defining the
      !>     name/ID mappings.
      !>
      !>        INCLUDE            'zzbodtrn.inc'
      !>
      !>        INTEGER             ID  ( NPERM )
      !>        CHARACTER*(MAXL)    NAME( NPERM )
      !>
      !>        CALL ZZIDMAP( ID, NAME )
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
      !>     E.D. Wright, 10-DEC-2021 (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.10, 10-DEC-2021 (EDW) (JDR) (BVS)
      !>
      !>        Added:
      !>
      !>             -652   MERCURY TRANSFER MODULE
      !>             -652   MTM
      !>             -652   BEPICOLOMBO MTM
      !>             -255   PSYC
      !>             -243   VIPER
      !>             -242   LUNAR TRAILBLAZER
      !>             -240   SMART LANDER FOR INVESTIGATING MOON
      !>             -240   SLIM
      !>             -239   MARTIAN MOONS EXPLORATION
      !>             -239   MMX
      !>             -210   LICIA
      !>             -210   LICIACUBE
      !>             -197   EXOMARS_LARA
      !>             -197   LARA
      !>             -174   EXM RSP RM
      !>             -174   EXM ROVER
      !>             -174   EXOMARS ROVER
      !>             -173   EXM RSP SP
      !>             -173   EXM SURFACE PLATFORM
      !>             -173   EXOMARS SP
      !>             -172   EXM RSP SCC
      !>             -172   EXM SPACECRAFT COMPOSITE
      !>             -172   EXOMARS SCC
      !>             -168   PERSEVERANCE
      !>             -168   MARS 2020
      !>             -168   MARS2020
      !>             -168   M2020
      !>             -164   LUNAR FLASHLIGHT
      !>             -156   ADITYA
      !>             -156   ADIT
      !>             -155   KPLO
      !>             -155   KOREAN PATHFINDER LUNAR ORBITER
      !>             -153   CH2L
      !>             -153   CHANDRAYAAN-2 LANDER
      !>             -148   DFLY
      !>             -148   DRAGONFLY
      !>             -135   DART
      !>             -135   DOUBLE ASTEROID REDIRECTION TEST
      !>             -119   MARS_ORBITER_MISSION_2
      !>             -119   MOM2
      !>              -96   PARKER SOLAR PROBE
      !>              -72   JNSB
      !>              -72   JANUS_B
      !>              -57   LUNAR ICECUBE
      !>              -45   JNSA
      !>              -45   JANUS_A
      !>              -43   IMAP
      !>              -39   LUNAR POLAR HYDROGEN MAPPER
      !>              -39   LUNAH-MAP
      !>              -37   HYB2
      !>              -37   HAYABUSA 2
      !>              -37   HAYABUSA2
      !>              -33   NEOS
      !>              -33   NEO SURVEYOR
      !>           399035   DSS-35
      !>           399036   DSS-36
      !>           399056   DSS-56
      !>           399069   DSS-69
      !>          2000052   52_EUROPA
      !>          2000052   52 EUROPA
      !>          2162173   RYUGU
      !>          2486958   ARROKOTH
      !>         20000617   PATROCLUS_BARYCENTER
      !>         20000617   PATROCLUS BARYCENTER
      !>         20003548   EURYBATES_BARYCENTER
      !>         20003548   EURYBATES BARYCENTER
      !>         20011351   LEUCUS
      !>         20015094   POLYMELE
      !>         20021900   ORUS
      !>         20052246   DONALDJOHANSON
      !>         20065803   DIDYMOS_BARYCENTER
      !>         20065803   DIDYMOS BARYCENTER
      !>        120000617   MENOETIUS
      !>        120003548   QUETA
      !>        120065803   DIMORPHOS
      !>        920000617   PATROCLUS
      !>        920003548   EURYBATES
      !>        920065803   DIDYMOS
      !>
      !>        Modified assignments
      !>
      !>             -152   CH2O
      !>             -152   CHANDRAYAAN-2 ORBITER
      !>
      !>        Removed assignments
      !>
      !>             -164   YOHKOH
      !>             -164   SOLAR-A
      !>             -135   DRTS-W
      !>              -69   PSYC
      !>              -54   ASTEROID RETRIEVAL MISSION
      !>              -54   ARM
      !>
      !>        Reimplemented spelling change:
      !>
      !>           MAGACLITE to MEGACLITE
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.0.9, 04-APR-2017 (EDW)
      !>
      !>        Added information stating the frames subsystem performs
      !>        frame ID-name mappings and the DSK subsystem performs
      !>        surface ID-name mappings.
      !>
      !>        Edited body/ID assignment format to indicate whitespace
      !>        between 'NAME' and Comments.
      !>
      !>        Added:
      !>
      !>             -302   HELIOS 2
      !>             -301   HELIOS 1
      !>             -198   NASA-ISRO SAR MISSION
      !>             -198   NISAR
      !>             -159   EURC
      !>             -159   EUROPA CLIPPER
      !>             -152   CH2
      !>             -152   CHANDRAYAAN-2
      !>             -143   TRACE GAS ORBITER
      !>             -143   TGO
      !>             -143   EXOMARS 2016 TGO
      !>             -117   EDL DEMONSTRATOR MODULE
      !>             -117   EDM
      !>             -117   EXOMARS 2016 EDM
      !>              -76   CURIOSITY
      !>              -69   PSYC
      !>              -66   MCOB
      !>              -66   MARCO-B
      !>              -65   MCOA
      !>              -65   MARCO-A
      !>              -62   EMM
      !>              -62   EMIRATES MARS MISSION
      !>              -49   LUCY
      !>              -28   JUPITER ICY MOONS EXPLORER
      !>              -28   JUICE
      !>              553   DIA
      !>          2000016   PSYCHE
      !>          2101955   BENNU
      !>
      !>        Removed assignments:
      !>
      !>             -159   EUROPA ORBITER
      !>              -69   MPO
      !>              -69   MERCURY PLANETARY ORBITER
      !>
      !>        Modified assignments
      !>
      !>             -121   MERCURY PLANETARY ORBITER
      !>             -121   MPO
      !>             -121   BEPICOLOMBO MPO
      !>              -68   MERCURY MAGNETOSPHERIC ORBITER
      !>              -68   MMO
      !>              -68   BEPICOLOMBO MMO
      !>
      !>-    SPICELIB Version 1.0.8, 06-MAY-2014 (EDW)
      !>
      !>        Edited text comments in Asteroids section and Comets section.
      !>
      !>        Eliminated "PI" IAU Number from "CHARON" description.
      !>
      !>        HYROKKIN (644) spelling corrected to HYRROKKIN.
      !>
      !>        Added:
      !>
      !>             -750   SPRINT-AS
      !>             -189   NSYT
      !>             -189   INSIGHT
      !>             -170   JWST
      !>             -170   JAMES WEBB SPACE TELESCOPE
      !>             -144   SOLO
      !>             -144   SOLAR ORBITER
      !>              -96   SPP
      !>              -96   SOLAR PROBE PLUS
      !>              -64   ORX
      !>              -64   OSIRIS-REX
      !>              -54   ARM
      !>              -54   ASTEROID RETRIEVAL MISSION
      !>              -12   LADEE
      !>               -3   MOM
      !>               -3   MARS ORBITER MISSION
      !>                0   SOLAR_SYSTEM_BARYCENTER
      !>                1   MERCURY_BARYCENTER
      !>                2   VENUS_BARYCENTER
      !>                3   EARTH_BARYCENTER
      !>                4   MARS_BARYCENTER
      !>                5   JUPITER_BARYCENTER
      !>                6   SATURN_BARYCENTER
      !>                7   URANUS_BARYCENTER
      !>                8   NEPTUNE_BARYCENTER
      !>                9   PLUTO_BARYCENTER
      !>              644   HYRROKKIN
      !>              904   KERBEROS
      !>              905   STYX
      !>          1003228   C/2013 A1
      !>          1003228   SIDING SPRING
      !>          2000002   PALLAS
      !>          2000511   DAVIDA
      !>
      !>        Removed assignments:
      !>
      !>             -486   HERSCHEL
      !>             -489   PLANCK
      !>             -187   SOLAR PROBE
      !>
      !>-    SPICELIB Version 1.0.7, 20-MAY-2010 (EDW)
      !>
      !>        Edit to vehicle ID list to correct -76 not in proper
      !>        numerical (descending) order.
      !>
      !>        Added:
      !>
      !>               -5   AKATSUKI
      !>               -5   VCO
      !>             -121   BEPICOLOMBO
      !>             -177   GRAIL-A
      !>             -181   GRAIL-B
      !>             -202   MAVEN
      !>             -205   SOIL MOISTURE ACTIVE AND PASSIVE
      !>             -205   SMAP
      !>             -362   RADIATION BELT STORM PROBE A
      !>             -362   RBSP_A
      !>             -363   RADIATION BELT STORM PROBE B
      !>             -363   RBSP_B
      !>              550   HERSE
      !>              653   AEGAEON
      !>          1000093   TEMPEL_1
      !>          2000021   LUTETIA
      !>          2004179   TOUTATIS
      !>
      !>-    SPICELIB Version 1.0.6, 08-APR-2009 (EDW)
      !>
      !>        Added:
      !>
      !>               -5   PLC
      !>               -5   PLANET-C
      !>              -68   MMO
      !>              -68   MERCURY MAGNETOSPHERIC ORBITER
      !>              -69   MPO
      !>              -69   MERCURY PLANETARY ORBITER
      !>          2002867   STEINS
      !>             -140   EPOCH
      !>             -140   DIXI
      !>
      !>-    SPICELIB Version 1.0.5, 09-JAN-2008 (EDW)
      !>
      !>        Added:
      !>
      !>              -18   LCROSS
      !>              -29   NEXT
      !>              -86   CH1
      !>              -86   CHANDRAYAAN-1
      !>             -131   KAGUYA
      !>             -140   EPOXI
      !>             -151   CHANDRA
      !>             -187   SOLAR PROBE
      !>              636   AEGIR
      !>              637   BEBHIONN
      !>              638   BERGELMIR
      !>              639   BESTLA
      !>              640   FARBAUTI
      !>              641   FENRIR
      !>              642   FORNJOT
      !>              643   HATI
      !>              644   HYROKKIN
      !>              645   KARI
      !>              646   LOGE
      !>              647   SKOLL
      !>              648   SURTUR
      !>              649   ANTHE
      !>              650   JARNSAXA
      !>              651   GREIP
      !>              652   TARQEQ
      !>              809   HALIMEDE
      !>              810   PSAMATHE
      !>              811   SAO
      !>              812   LAOMEDEIA
      !>              813   NESO
      !>
      !>        NAIF modified the Jovian system listing to conform to the
      !>        current (as of this date) name/body mapping.
      !>
      !>              540   MNEME
      !>              541   AOEDE
      !>              542   THELXINOE
      !>              543   ARCHE
      !>              544   KALLICHORE
      !>              545   HELIKE
      !>              546   CARPO
      !>              547   EUKELADE
      !>              548   CYLLENE
      !>              549   KORE
      !>
      !>        Removed assignments:
      !>
      !>             -172   SPACETECH-3 COMBINER
      !>             -174   PLUTO-KUIPER EXPRESS
      !>             -175   PLUTO-KUIPER EXPRESS SIMULATION
      !>             -205   SPACETECH-3 COLLECTOR
      !>              514   1979J2
      !>              515   1979J1
      !>              516   1979J3
      !>              610   1980S1
      !>              611   1980S3
      !>              612   1980S6
      !>              613   1980S13
      !>              614   1980S25
      !>              615   1980S28
      !>              616   1980S27
      !>              617   1980S26
      !>              706   1986U7
      !>              707   1986U8
      !>              708   1986U9
      !>              709   1986U4
      !>              710   1986U6
      !>              711   1986U3
      !>              712   1986U1
      !>              713   1986U2
      !>              714   1986U5
      !>              715   1985U1
      !>              718   1986U10
      !>              901   1978P1
      !>
      !>        Spelling correction:
      !>
      !>           MAGACLITE to MEGACLITE
      !>
      !>        Rename:
      !>
      !>           ERRIAPO to ERRIAPUS
      !>           STV-1 to STV51
      !>           STV-2 to STV52
      !>           STV-3 to STV53
      !>
      !>-    SPICELIB Version 1.0.4, 01-NOV-2006 (EDW)
      !>
      !>        NAIF removed several provisional name/ID mappings from
      !>        the Jovian system listing:
      !>
      !>           539         'HEGEMONE'              JXXXIX
      !>           540         'MNEME'                 JXL
      !>           541         'AOEDE'                 JXLI
      !>           542         'THELXINOE'             JXLII
      !>           543         'ARCHE'                 JXLIII
      !>           544         'KALLICHORE'            JXLIV
      !>           545         'HELIKE'                JXLV
      !>           546         'CARPO'                 JXLVI
      !>           547         'EUKELADE'              JXLVII
      !>           548         'CYLLENE'               JXLVIII
      !>
      !>        The current mapping set for the range 539-561:
      !>
      !>              540   ARCHE
      !>              541   EUKELADE
      !>              546   HELIKE
      !>              547   AOEDE
      !>              548   HEGEMONE
      !>              551   KALLICHORE
      !>              553   CYLLENE
      !>              560   CARPO
      !>              561   MNEME
      !>
      !>        The new mapping leaves the IDs 539, 542-545, 549, 550, 552,
      !>        554-559 unassigned.
      !>
      !>        Added:
      !>
      !>              635   DAPHNIS
      !>              722   FRANCISCO
      !>              723   MARGARET
      !>              724   FERDINAND
      !>              725   PERDITA
      !>              726   MAB
      !>              727   CUPID
      !>              -61   JUNO
      !>              -76   MSL
      !>              -76   MARS SCIENCE LABORATORY
      !>             -212   STV-1
      !>             -213   STV-2
      !>             -214   STV-3
      !>              902   NIX
      !>              903   HYDRA
      !>             -85    LRO
      !>             -85    LUNAR RECON ORBITER
      !>             -85    LUNAR RECONNAISSANCE ORBITER
      !>
      !>        Spelling correction
      !>
      !>              632   METHODE to METHONE
      !>
      !>-    SPICELIB Version 1.0.3, 14-NOV-2005 (EDW)
      !>
      !>        Added:
      !>
      !>              539   HEGEMONE
      !>              540   MNEME
      !>              541   AOEDE
      !>              542   THELXINOE
      !>              543   ARCHE
      !>              544   KALLICHORE
      !>              545   HELIKE
      !>              546   CARPO
      !>              547   EUKELADE
      !>              548   CYLLENE
      !>              631   NARVI
      !>              632   METHODE
      !>              633   PALLENE
      !>              634   POLYDEUCES
      !>          2025143   ITOKAWA
      !>              -98   NEW HORIZONS
      !>             -248   VENUS EXPRESS, VEX
      !>             -500   RSAT, SELENE Relay Satellite, SELENE Rstar, Rstar
      !>             -502   VSAT, SELENE VLBI Radio Satellite,
      !>                    SELENE VRAD Satellite, SELENE Vstar
      !>           399064   DSS-64
      !>
      !>        Change in spelling:
      !>
      !>              623   SUTTUNG to SUTTUNGR
      !>              627   SKADI   to SKATHI
      !>              630   THRYM   to THRYMR
      !>
      !>-    SPICELIB Version 1.0.2, 20-DEC-2004 (EDW)
      !>
      !>        Edited parse code to correctly process embedded
      !>        parentheses in a body name.
      !>
      !>        Added:
      !>
      !>           Due to the previous definition of Parkes with DSS-05,
      !>           the Parkes ID remains 399005.
      !>
      !>             -486   HERSCHEL
      !>             -489   PLANCK
      !>           399049   DSS-49
      !>           399055   DSS-55
      !>             -203   DAWN
      !>          1000012   67P/CHURYUMOV-GERASIMENKO (1969 R1)
      !>          1000012   CHURYUMOV-GERASIMENKO
      !>          398989    NOTO
      !>             -84    PHOENIX
      !>            -131    SELENE
      !>            -238    SMART-1, S1, SM1, SMART1
      !>            -130    HAYABUSA
      !>
      !>-    SPICELIB Version 1.0.1, 19-DEC-2003 (EDW)
      !>
      !>        Added:
      !>              -79   SPITZER
      !>          2000216   KLEOPATRA
      !>
      !>-    SPICELIB Version 1.0.0, 27-JUL-2003 (EDW)
      !>
      !>        Added:
      !>              -47   GNS
      !>              -74   MRO
      !>              -74   MARS RECON ORBITER
      !>             -130   MUSES-C
      !>             -142   TERRA
      !>             -154   AQUA
      !>             -159   EUROPA ORBITER
      !>             -190   SIM
      !>             -198   INTEGRAL
      !>             -227   KEPLER
      !>             -234   STEREO AHEAD
      !>             -235   STEREO BEHIND
      !>             -253   OPPORTUNITY
      !>             -254   SPIRIT
      !>              528   AUTONOE
      !>              529   THYONE
      !>              530   HERMIPPE
      !>              531   AITNE
      !>              532   EURYDOME
      !>              533   EUANTHE
      !>              534   EUPORIE
      !>              535   ORTHOSIE
      !>              536   SPONDE
      !>              537   KALE
      !>              538   PASITHEE
      !>              619   YMIR
      !>              620   PAALIAQ
      !>              621   TARVOS
      !>              622   IJIRAQ
      !>              623   SUTTUNG
      !>              624   KIVIUQ
      !>              625   MUNDILFARI
      !>              626   ALBIORIX
      !>              627   SKADI
      !>              628   ERRIAPO
      !>              629   SIARNAQ
      !>              630   THRYM
      !>              718   PROSPERO
      !>              719   SETEBOS
      !>              720   STEPHANO
      !>              721   TRINCULO
      !>           398990   NEW NORCIA
      !>          2431011   DACTYL
      !>          2000001   CERES
      !>          2000004   VESTA
      !>
      !>        Renamed:
      !>
      !>              -25   LPM to
      !>              -25   LP
      !>
      !>             -180   MUSES-C to
      !>             -130   MUSES-B
      !>
      !>             -172   STARLIGHT COMBINER to
      !>             -172   SPACETECH-3 COMBINER
      !>
      !>             -205   STARLIGHT COLLECTOR to
      !>             -205   SPACETECH-3 COLLECTOR
      !>
      !>        Removed:
      !>             -172   SLCOMB
      module subroutine zzidmap ( ctx, bltcod, bltnam )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(out) :: bltcod(nperm)  !! List of default integer ID codes.
      character(maxl), intent(out) :: bltnam(nperm)  !! List of default names.

!
!     A script generates this file. Do not edit by hand.
!     Edit the creation script to modify the contents of
!     ZZIDMAP.
!

      bltcod(1) =   0
      bltnam(1) =  'SOLAR_SYSTEM_BARYCENTER'

      bltcod(2) =   0
      bltnam(2) =  'SSB'

      bltcod(3) =   0
      bltnam(3) =  'SOLAR SYSTEM BARYCENTER'

      bltcod(4) =   1
      bltnam(4) =  'MERCURY_BARYCENTER'

      bltcod(5) =   1
      bltnam(5) =  'MERCURY BARYCENTER'

      bltcod(6) =   2
      bltnam(6) =  'VENUS_BARYCENTER'

      bltcod(7) =   2
      bltnam(7) =  'VENUS BARYCENTER'

      bltcod(8) =   3
      bltnam(8) =  'EARTH_BARYCENTER'

      bltcod(9) =   3
      bltnam(9) =  'EMB'

      bltcod(10) =   3
      bltnam(10) =  'EARTH MOON BARYCENTER'

      bltcod(11) =   3
      bltnam(11) =  'EARTH-MOON BARYCENTER'

      bltcod(12) =   3
      bltnam(12) =  'EARTH BARYCENTER'

      bltcod(13) =   4
      bltnam(13) =  'MARS_BARYCENTER'

      bltcod(14) =   4
      bltnam(14) =  'MARS BARYCENTER'

      bltcod(15) =   5
      bltnam(15) =  'JUPITER_BARYCENTER'

      bltcod(16) =   5
      bltnam(16) =  'JUPITER BARYCENTER'

      bltcod(17) =   6
      bltnam(17) =  'SATURN_BARYCENTER'

      bltcod(18) =   6
      bltnam(18) =  'SATURN BARYCENTER'

      bltcod(19) =   7
      bltnam(19) =  'URANUS_BARYCENTER'

      bltcod(20) =   7
      bltnam(20) =  'URANUS BARYCENTER'

      bltcod(21) =   8
      bltnam(21) =  'NEPTUNE_BARYCENTER'

      bltcod(22) =   8
      bltnam(22) =  'NEPTUNE BARYCENTER'

      bltcod(23) =   9
      bltnam(23) =  'PLUTO_BARYCENTER'

      bltcod(24) =   9
      bltnam(24) =  'PLUTO BARYCENTER'

      bltcod(25) =   10
      bltnam(25) =  'SUN'

      bltcod(26) =   199
      bltnam(26) =  'MERCURY'

      bltcod(27) =   299
      bltnam(27) =  'VENUS'

      bltcod(28) =   399
      bltnam(28) =  'EARTH'

      bltcod(29) =   301
      bltnam(29) =  'MOON'

      bltcod(30) =   499
      bltnam(30) =  'MARS'

      bltcod(31) =   401
      bltnam(31) =  'PHOBOS'

      bltcod(32) =   402
      bltnam(32) =  'DEIMOS'

      bltcod(33) =   599
      bltnam(33) =  'JUPITER'

      bltcod(34) =   501
      bltnam(34) =  'IO'

      bltcod(35) =   502
      bltnam(35) =  'EUROPA'

      bltcod(36) =   503
      bltnam(36) =  'GANYMEDE'

      bltcod(37) =   504
      bltnam(37) =  'CALLISTO'

      bltcod(38) =   505
      bltnam(38) =  'AMALTHEA'

      bltcod(39) =   506
      bltnam(39) =  'HIMALIA'

      bltcod(40) =   507
      bltnam(40) =  'ELARA'

      bltcod(41) =   508
      bltnam(41) =  'PASIPHAE'

      bltcod(42) =   509
      bltnam(42) =  'SINOPE'

      bltcod(43) =   510
      bltnam(43) =  'LYSITHEA'

      bltcod(44) =   511
      bltnam(44) =  'CARME'

      bltcod(45) =   512
      bltnam(45) =  'ANANKE'

      bltcod(46) =   513
      bltnam(46) =  'LEDA'

      bltcod(47) =   514
      bltnam(47) =  'THEBE'

      bltcod(48) =   515
      bltnam(48) =  'ADRASTEA'

      bltcod(49) =   516
      bltnam(49) =  'METIS'

      bltcod(50) =   517
      bltnam(50) =  'CALLIRRHOE'

      bltcod(51) =   518
      bltnam(51) =  'THEMISTO'

      bltcod(52) =   519
      bltnam(52) =  'MEGACLITE'

      bltcod(53) =   520
      bltnam(53) =  'TAYGETE'

      bltcod(54) =   521
      bltnam(54) =  'CHALDENE'

      bltcod(55) =   522
      bltnam(55) =  'HARPALYKE'

      bltcod(56) =   523
      bltnam(56) =  'KALYKE'

      bltcod(57) =   524
      bltnam(57) =  'IOCASTE'

      bltcod(58) =   525
      bltnam(58) =  'ERINOME'

      bltcod(59) =   526
      bltnam(59) =  'ISONOE'

      bltcod(60) =   527
      bltnam(60) =  'PRAXIDIKE'

      bltcod(61) =   528
      bltnam(61) =  'AUTONOE'

      bltcod(62) =   529
      bltnam(62) =  'THYONE'

      bltcod(63) =   530
      bltnam(63) =  'HERMIPPE'

      bltcod(64) =   531
      bltnam(64) =  'AITNE'

      bltcod(65) =   532
      bltnam(65) =  'EURYDOME'

      bltcod(66) =   533
      bltnam(66) =  'EUANTHE'

      bltcod(67) =   534
      bltnam(67) =  'EUPORIE'

      bltcod(68) =   535
      bltnam(68) =  'ORTHOSIE'

      bltcod(69) =   536
      bltnam(69) =  'SPONDE'

      bltcod(70) =   537
      bltnam(70) =  'KALE'

      bltcod(71) =   538
      bltnam(71) =  'PASITHEE'

      bltcod(72) =   539
      bltnam(72) =  'HEGEMONE'

      bltcod(73) =   540
      bltnam(73) =  'MNEME'

      bltcod(74) =   541
      bltnam(74) =  'AOEDE'

      bltcod(75) =   542
      bltnam(75) =  'THELXINOE'

      bltcod(76) =   543
      bltnam(76) =  'ARCHE'

      bltcod(77) =   544
      bltnam(77) =  'KALLICHORE'

      bltcod(78) =   545
      bltnam(78) =  'HELIKE'

      bltcod(79) =   546
      bltnam(79) =  'CARPO'

      bltcod(80) =   547
      bltnam(80) =  'EUKELADE'

      bltcod(81) =   548
      bltnam(81) =  'CYLLENE'

      bltcod(82) =   549
      bltnam(82) =  'KORE'

      bltcod(83) =   550
      bltnam(83) =  'HERSE'

      bltcod(84) =   553
      bltnam(84) =  'DIA'

      bltcod(85) =   699
      bltnam(85) =  'SATURN'

      bltcod(86) =   601
      bltnam(86) =  'MIMAS'

      bltcod(87) =   602
      bltnam(87) =  'ENCELADUS'

      bltcod(88) =   603
      bltnam(88) =  'TETHYS'

      bltcod(89) =   604
      bltnam(89) =  'DIONE'

      bltcod(90) =   605
      bltnam(90) =  'RHEA'

      bltcod(91) =   606
      bltnam(91) =  'TITAN'

      bltcod(92) =   607
      bltnam(92) =  'HYPERION'

      bltcod(93) =   608
      bltnam(93) =  'IAPETUS'

      bltcod(94) =   609
      bltnam(94) =  'PHOEBE'

      bltcod(95) =   610
      bltnam(95) =  'JANUS'

      bltcod(96) =   611
      bltnam(96) =  'EPIMETHEUS'

      bltcod(97) =   612
      bltnam(97) =  'HELENE'

      bltcod(98) =   613
      bltnam(98) =  'TELESTO'

      bltcod(99) =   614
      bltnam(99) =  'CALYPSO'

      bltcod(100) =   615
      bltnam(100) =  'ATLAS'

      bltcod(101) =   616
      bltnam(101) =  'PROMETHEUS'

      bltcod(102) =   617
      bltnam(102) =  'PANDORA'

      bltcod(103) =   618
      bltnam(103) =  'PAN'

      bltcod(104) =   619
      bltnam(104) =  'YMIR'

      bltcod(105) =   620
      bltnam(105) =  'PAALIAQ'

      bltcod(106) =   621
      bltnam(106) =  'TARVOS'

      bltcod(107) =   622
      bltnam(107) =  'IJIRAQ'

      bltcod(108) =   623
      bltnam(108) =  'SUTTUNGR'

      bltcod(109) =   624
      bltnam(109) =  'KIVIUQ'

      bltcod(110) =   625
      bltnam(110) =  'MUNDILFARI'

      bltcod(111) =   626
      bltnam(111) =  'ALBIORIX'

      bltcod(112) =   627
      bltnam(112) =  'SKATHI'

      bltcod(113) =   628
      bltnam(113) =  'ERRIAPUS'

      bltcod(114) =   629
      bltnam(114) =  'SIARNAQ'

      bltcod(115) =   630
      bltnam(115) =  'THRYMR'

      bltcod(116) =   631
      bltnam(116) =  'NARVI'

      bltcod(117) =   632
      bltnam(117) =  'METHONE'

      bltcod(118) =   633
      bltnam(118) =  'PALLENE'

      bltcod(119) =   634
      bltnam(119) =  'POLYDEUCES'

      bltcod(120) =   635
      bltnam(120) =  'DAPHNIS'

      bltcod(121) =   636
      bltnam(121) =  'AEGIR'

      bltcod(122) =   637
      bltnam(122) =  'BEBHIONN'

      bltcod(123) =   638
      bltnam(123) =  'BERGELMIR'

      bltcod(124) =   639
      bltnam(124) =  'BESTLA'

      bltcod(125) =   640
      bltnam(125) =  'FARBAUTI'

      bltcod(126) =   641
      bltnam(126) =  'FENRIR'

      bltcod(127) =   642
      bltnam(127) =  'FORNJOT'

      bltcod(128) =   643
      bltnam(128) =  'HATI'

      bltcod(129) =   644
      bltnam(129) =  'HYRROKKIN'

      bltcod(130) =   645
      bltnam(130) =  'KARI'

      bltcod(131) =   646
      bltnam(131) =  'LOGE'

      bltcod(132) =   647
      bltnam(132) =  'SKOLL'

      bltcod(133) =   648
      bltnam(133) =  'SURTUR'

      bltcod(134) =   649
      bltnam(134) =  'ANTHE'

      bltcod(135) =   650
      bltnam(135) =  'JARNSAXA'

      bltcod(136) =   651
      bltnam(136) =  'GREIP'

      bltcod(137) =   652
      bltnam(137) =  'TARQEQ'

      bltcod(138) =   653
      bltnam(138) =  'AEGAEON'

      bltcod(139) =   799
      bltnam(139) =  'URANUS'

      bltcod(140) =   701
      bltnam(140) =  'ARIEL'

      bltcod(141) =   702
      bltnam(141) =  'UMBRIEL'

      bltcod(142) =   703
      bltnam(142) =  'TITANIA'

      bltcod(143) =   704
      bltnam(143) =  'OBERON'

      bltcod(144) =   705
      bltnam(144) =  'MIRANDA'

      bltcod(145) =   706
      bltnam(145) =  'CORDELIA'

      bltcod(146) =   707
      bltnam(146) =  'OPHELIA'

      bltcod(147) =   708
      bltnam(147) =  'BIANCA'

      bltcod(148) =   709
      bltnam(148) =  'CRESSIDA'

      bltcod(149) =   710
      bltnam(149) =  'DESDEMONA'

      bltcod(150) =   711
      bltnam(150) =  'JULIET'

      bltcod(151) =   712
      bltnam(151) =  'PORTIA'

      bltcod(152) =   713
      bltnam(152) =  'ROSALIND'

      bltcod(153) =   714
      bltnam(153) =  'BELINDA'

      bltcod(154) =   715
      bltnam(154) =  'PUCK'

      bltcod(155) =   716
      bltnam(155) =  'CALIBAN'

      bltcod(156) =   717
      bltnam(156) =  'SYCORAX'

      bltcod(157) =   718
      bltnam(157) =  'PROSPERO'

      bltcod(158) =   719
      bltnam(158) =  'SETEBOS'

      bltcod(159) =   720
      bltnam(159) =  'STEPHANO'

      bltcod(160) =   721
      bltnam(160) =  'TRINCULO'

      bltcod(161) =   722
      bltnam(161) =  'FRANCISCO'

      bltcod(162) =   723
      bltnam(162) =  'MARGARET'

      bltcod(163) =   724
      bltnam(163) =  'FERDINAND'

      bltcod(164) =   725
      bltnam(164) =  'PERDITA'

      bltcod(165) =   726
      bltnam(165) =  'MAB'

      bltcod(166) =   727
      bltnam(166) =  'CUPID'

      bltcod(167) =   899
      bltnam(167) =  'NEPTUNE'

      bltcod(168) =   801
      bltnam(168) =  'TRITON'

      bltcod(169) =   802
      bltnam(169) =  'NEREID'

      bltcod(170) =   803
      bltnam(170) =  'NAIAD'

      bltcod(171) =   804
      bltnam(171) =  'THALASSA'

      bltcod(172) =   805
      bltnam(172) =  'DESPINA'

      bltcod(173) =   806
      bltnam(173) =  'GALATEA'

      bltcod(174) =   807
      bltnam(174) =  'LARISSA'

      bltcod(175) =   808
      bltnam(175) =  'PROTEUS'

      bltcod(176) =   809
      bltnam(176) =  'HALIMEDE'

      bltcod(177) =   810
      bltnam(177) =  'PSAMATHE'

      bltcod(178) =   811
      bltnam(178) =  'SAO'

      bltcod(179) =   812
      bltnam(179) =  'LAOMEDEIA'

      bltcod(180) =   813
      bltnam(180) =  'NESO'

      bltcod(181) =   999
      bltnam(181) =  'PLUTO'

      bltcod(182) =   901
      bltnam(182) =  'CHARON'

      bltcod(183) =   902
      bltnam(183) =  'NIX'

      bltcod(184) =   903
      bltnam(184) =  'HYDRA'

      bltcod(185) =   904
      bltnam(185) =  'KERBEROS'

      bltcod(186) =   905
      bltnam(186) =  'STYX'

      bltcod(187) =   -1
      bltnam(187) =  'GEOTAIL'

      bltcod(188) =   -3
      bltnam(188) =  'MOM'

      bltcod(189) =   -3
      bltnam(189) =  'MARS ORBITER MISSION'

      bltcod(190) =   -5
      bltnam(190) =  'AKATSUKI'

      bltcod(191) =   -5
      bltnam(191) =  'VCO'

      bltcod(192) =   -5
      bltnam(192) =  'PLC'

      bltcod(193) =   -5
      bltnam(193) =  'PLANET-C'

      bltcod(194) =   -6
      bltnam(194) =  'P6'

      bltcod(195) =   -6
      bltnam(195) =  'PIONEER-6'

      bltcod(196) =   -7
      bltnam(196) =  'P7'

      bltcod(197) =   -7
      bltnam(197) =  'PIONEER-7'

      bltcod(198) =   -8
      bltnam(198) =  'WIND'

      bltcod(199) =   -12
      bltnam(199) =  'VENUS ORBITER'

      bltcod(200) =   -12
      bltnam(200) =  'P12'

      bltcod(201) =   -12
      bltnam(201) =  'PIONEER 12'

      bltcod(202) =   -12
      bltnam(202) =  'LADEE'

      bltcod(203) =   -13
      bltnam(203) =  'POLAR'

      bltcod(204) =   -18
      bltnam(204) =  'MGN'

      bltcod(205) =   -18
      bltnam(205) =  'MAGELLAN'

      bltcod(206) =   -18
      bltnam(206) =  'LCROSS'

      bltcod(207) =   -20
      bltnam(207) =  'P8'

      bltcod(208) =   -20
      bltnam(208) =  'PIONEER-8'

      bltcod(209) =   -21
      bltnam(209) =  'SOHO'

      bltcod(210) =   -23
      bltnam(210) =  'P10'

      bltcod(211) =   -23
      bltnam(211) =  'PIONEER-10'

      bltcod(212) =   -24
      bltnam(212) =  'P11'

      bltcod(213) =   -24
      bltnam(213) =  'PIONEER-11'

      bltcod(214) =   -25
      bltnam(214) =  'LP'

      bltcod(215) =   -25
      bltnam(215) =  'LUNAR PROSPECTOR'

      bltcod(216) =   -27
      bltnam(216) =  'VK1'

      bltcod(217) =   -27
      bltnam(217) =  'VIKING 1 ORBITER'

      bltcod(218) =   -28
      bltnam(218) =  'JUPITER ICY MOONS EXPLORER'

      bltcod(219) =   -28
      bltnam(219) =  'JUICE'

      bltcod(220) =   -29
      bltnam(220) =  'STARDUST'

      bltcod(221) =   -29
      bltnam(221) =  'SDU'

      bltcod(222) =   -29
      bltnam(222) =  'NEXT'

      bltcod(223) =   -30
      bltnam(223) =  'VK2'

      bltcod(224) =   -30
      bltnam(224) =  'VIKING 2 ORBITER'

      bltcod(225) =   -30
      bltnam(225) =  'DS-1'

      bltcod(226) =   -31
      bltnam(226) =  'VG1'

      bltcod(227) =   -31
      bltnam(227) =  'VOYAGER 1'

      bltcod(228) =   -32
      bltnam(228) =  'VG2'

      bltcod(229) =   -32
      bltnam(229) =  'VOYAGER 2'

      bltcod(230) =   -33
      bltnam(230) =  'NEOS'

      bltcod(231) =   -33
      bltnam(231) =  'NEO SURVEYOR'

      bltcod(232) =   -37
      bltnam(232) =  'HYB2'

      bltcod(233) =   -37
      bltnam(233) =  'HAYABUSA 2'

      bltcod(234) =   -37
      bltnam(234) =  'HAYABUSA2'

      bltcod(235) =   -39
      bltnam(235) =  'LUNAR POLAR HYDROGEN MAPPER'

      bltcod(236) =   -39
      bltnam(236) =  'LUNAH-MAP'

      bltcod(237) =   -40
      bltnam(237) =  'CLEMENTINE'

      bltcod(238) =   -41
      bltnam(238) =  'MEX'

      bltcod(239) =   -41
      bltnam(239) =  'MARS EXPRESS'

      bltcod(240) =   -43
      bltnam(240) =  'IMAP'

      bltcod(241) =   -44
      bltnam(241) =  'BEAGLE2'

      bltcod(242) =   -44
      bltnam(242) =  'BEAGLE 2'

      bltcod(243) =   -45
      bltnam(243) =  'JNSA'

      bltcod(244) =   -45
      bltnam(244) =  'JANUS_A'

      bltcod(245) =   -46
      bltnam(245) =  'MS-T5'

      bltcod(246) =   -46
      bltnam(246) =  'SAKIGAKE'

      bltcod(247) =   -47
      bltnam(247) =  'PLANET-A'

      bltcod(248) =   -47
      bltnam(248) =  'SUISEI'

      bltcod(249) =   -47
      bltnam(249) =  'GNS'

      bltcod(250) =   -47
      bltnam(250) =  'GENESIS'

      bltcod(251) =   -48
      bltnam(251) =  'HUBBLE SPACE TELESCOPE'

      bltcod(252) =   -48
      bltnam(252) =  'HST'

      bltcod(253) =   -49
      bltnam(253) =  'LUCY'

      bltcod(254) =   -53
      bltnam(254) =  'MARS PATHFINDER'

      bltcod(255) =   -53
      bltnam(255) =  'MPF'

      bltcod(256) =   -53
      bltnam(256) =  'MARS ODYSSEY'

      bltcod(257) =   -53
      bltnam(257) =  'MARS SURVEYOR 01 ORBITER'

      bltcod(258) =   -55
      bltnam(258) =  'ULYSSES'

      bltcod(259) =   -57
      bltnam(259) =  'LUNAR ICECUBE'

      bltcod(260) =   -58
      bltnam(260) =  'VSOP'

      bltcod(261) =   -58
      bltnam(261) =  'HALCA'

      bltcod(262) =   -59
      bltnam(262) =  'RADIOASTRON'

      bltcod(263) =   -61
      bltnam(263) =  'JUNO'

      bltcod(264) =   -62
      bltnam(264) =  'EMM'

      bltcod(265) =   -62
      bltnam(265) =  'EMIRATES MARS MISSION'

      bltcod(266) =   -64
      bltnam(266) =  'ORX'

      bltcod(267) =   -64
      bltnam(267) =  'OSIRIS-REX'

      bltcod(268) =   -65
      bltnam(268) =  'MCOA'

      bltcod(269) =   -65
      bltnam(269) =  'MARCO-A'

      bltcod(270) =   -66
      bltnam(270) =  'VEGA 1'

      bltcod(271) =   -66
      bltnam(271) =  'MCOB'

      bltcod(272) =   -66
      bltnam(272) =  'MARCO-B'

      bltcod(273) =   -67
      bltnam(273) =  'VEGA 2'

      bltcod(274) =   -68
      bltnam(274) =  'MERCURY MAGNETOSPHERIC ORBITER'

      bltcod(275) =   -68
      bltnam(275) =  'MMO'

      bltcod(276) =   -68
      bltnam(276) =  'BEPICOLOMBO MMO'

      bltcod(277) =   -70
      bltnam(277) =  'DEEP IMPACT IMPACTOR SPACECRAFT'

      bltcod(278) =   -72
      bltnam(278) =  'JNSB'

      bltcod(279) =   -72
      bltnam(279) =  'JANUS_B'

      bltcod(280) =   -74
      bltnam(280) =  'MRO'

      bltcod(281) =   -74
      bltnam(281) =  'MARS RECON ORBITER'

      bltcod(282) =   -76
      bltnam(282) =  'CURIOSITY'

      bltcod(283) =   -76
      bltnam(283) =  'MSL'

      bltcod(284) =   -76
      bltnam(284) =  'MARS SCIENCE LABORATORY'

      bltcod(285) =   -77
      bltnam(285) =  'GLL'

      bltcod(286) =   -77
      bltnam(286) =  'GALILEO ORBITER'

      bltcod(287) =   -78
      bltnam(287) =  'GIOTTO'

      bltcod(288) =   -79
      bltnam(288) =  'SPITZER'

      bltcod(289) =   -79
      bltnam(289) =  'SPACE INFRARED TELESCOPE FACILITY'

      bltcod(290) =   -79
      bltnam(290) =  'SIRTF'

      bltcod(291) =   -81
      bltnam(291) =  'CASSINI ITL'

      bltcod(292) =   -82
      bltnam(292) =  'CAS'

      bltcod(293) =   -82
      bltnam(293) =  'CASSINI'

      bltcod(294) =   -84
      bltnam(294) =  'PHOENIX'

      bltcod(295) =   -85
      bltnam(295) =  'LRO'

      bltcod(296) =   -85
      bltnam(296) =  'LUNAR RECON ORBITER'

      bltcod(297) =   -85
      bltnam(297) =  'LUNAR RECONNAISSANCE ORBITER'

      bltcod(298) =   -86
      bltnam(298) =  'CH1'

      bltcod(299) =   -86
      bltnam(299) =  'CHANDRAYAAN-1'

      bltcod(300) =   -90
      bltnam(300) =  'CASSINI SIMULATION'

      bltcod(301) =   -93
      bltnam(301) =  'NEAR EARTH ASTEROID RENDEZVOUS'

      bltcod(302) =   -93
      bltnam(302) =  'NEAR'

      bltcod(303) =   -94
      bltnam(303) =  'MO'

      bltcod(304) =   -94
      bltnam(304) =  'MARS OBSERVER'

      bltcod(305) =   -94
      bltnam(305) =  'MGS'

      bltcod(306) =   -94
      bltnam(306) =  'MARS GLOBAL SURVEYOR'

      bltcod(307) =   -95
      bltnam(307) =  'MGS SIMULATION'

      bltcod(308) =   -96
      bltnam(308) =  'PARKER SOLAR PROBE'

      bltcod(309) =   -96
      bltnam(309) =  'SPP'

      bltcod(310) =   -96
      bltnam(310) =  'SOLAR PROBE PLUS'

      bltcod(311) =   -97
      bltnam(311) =  'TOPEX/POSEIDON'

      bltcod(312) =   -98
      bltnam(312) =  'NEW HORIZONS'

      bltcod(313) =   -107
      bltnam(313) =  'TROPICAL RAINFALL MEASURING MISSION'

      bltcod(314) =   -107
      bltnam(314) =  'TRMM'

      bltcod(315) =   -112
      bltnam(315) =  'ICE'

      bltcod(316) =   -116
      bltnam(316) =  'MARS POLAR LANDER'

      bltcod(317) =   -116
      bltnam(317) =  'MPL'

      bltcod(318) =   -117
      bltnam(318) =  'EDL DEMONSTRATOR MODULE'

      bltcod(319) =   -117
      bltnam(319) =  'EDM'

      bltcod(320) =   -117
      bltnam(320) =  'EXOMARS 2016 EDM'

      bltcod(321) =   -119
      bltnam(321) =  'MARS_ORBITER_MISSION_2'

      bltcod(322) =   -119
      bltnam(322) =  'MOM2'

      bltcod(323) =   -121
      bltnam(323) =  'MERCURY PLANETARY ORBITER'

      bltcod(324) =   -121
      bltnam(324) =  'MPO'

      bltcod(325) =   -121
      bltnam(325) =  'BEPICOLOMBO MPO'

      bltcod(326) =   -127
      bltnam(326) =  'MARS CLIMATE ORBITER'

      bltcod(327) =   -127
      bltnam(327) =  'MCO'

      bltcod(328) =   -130
      bltnam(328) =  'MUSES-C'

      bltcod(329) =   -130
      bltnam(329) =  'HAYABUSA'

      bltcod(330) =   -131
      bltnam(330) =  'SELENE'

      bltcod(331) =   -131
      bltnam(331) =  'KAGUYA'

      bltcod(332) =   -135
      bltnam(332) =  'DART'

      bltcod(333) =   -135
      bltnam(333) =  'DOUBLE ASTEROID REDIRECTION TEST'

      bltcod(334) =   -140
      bltnam(334) =  'EPOCH'

      bltcod(335) =   -140
      bltnam(335) =  'DIXI'

      bltcod(336) =   -140
      bltnam(336) =  'EPOXI'

      bltcod(337) =   -140
      bltnam(337) =  'DEEP IMPACT FLYBY SPACECRAFT'

      bltcod(338) =   -142
      bltnam(338) =  'TERRA'

      bltcod(339) =   -142
      bltnam(339) =  'EOS-AM1'

      bltcod(340) =   -143
      bltnam(340) =  'TRACE GAS ORBITER'

      bltcod(341) =   -143
      bltnam(341) =  'TGO'

      bltcod(342) =   -143
      bltnam(342) =  'EXOMARS 2016 TGO'

      bltcod(343) =   -144
      bltnam(343) =  'SOLO'

      bltcod(344) =   -144
      bltnam(344) =  'SOLAR ORBITER'

      bltcod(345) =   -146
      bltnam(345) =  'LUNAR-A'

      bltcod(346) =   -148
      bltnam(346) =  'DFLY'

      bltcod(347) =   -148
      bltnam(347) =  'DRAGONFLY'

      bltcod(348) =   -150
      bltnam(348) =  'CASSINI PROBE'

      bltcod(349) =   -150
      bltnam(349) =  'HUYGENS PROBE'

      bltcod(350) =   -150
      bltnam(350) =  'CASP'

      bltcod(351) =   -151
      bltnam(351) =  'AXAF'

      bltcod(352) =   -151
      bltnam(352) =  'CHANDRA'

      bltcod(353) =   -152
      bltnam(353) =  'CH2O'

      bltcod(354) =   -152
      bltnam(354) =  'CHANDRAYAAN-2 ORBITER'

      bltcod(355) =   -153
      bltnam(355) =  'CH2L'

      bltcod(356) =   -153
      bltnam(356) =  'CHANDRAYAAN-2 LANDER'

      bltcod(357) =   -154
      bltnam(357) =  'AQUA'

      bltcod(358) =   -155
      bltnam(358) =  'KPLO'

      bltcod(359) =   -155
      bltnam(359) =  'KOREAN PATHFINDER LUNAR ORBITER'

      bltcod(360) =   -156
      bltnam(360) =  'ADITYA'

      bltcod(361) =   -156
      bltnam(361) =  'ADIT'

      bltcod(362) =   -159
      bltnam(362) =  'EURC'

      bltcod(363) =   -159
      bltnam(363) =  'EUROPA CLIPPER'

      bltcod(364) =   -164
      bltnam(364) =  'LUNAR FLASHLIGHT'

      bltcod(365) =   -165
      bltnam(365) =  'MAP'

      bltcod(366) =   -166
      bltnam(366) =  'IMAGE'

      bltcod(367) =   -168
      bltnam(367) =  'PERSEVERANCE'

      bltcod(368) =   -168
      bltnam(368) =  'MARS 2020'

      bltcod(369) =   -168
      bltnam(369) =  'MARS2020'

      bltcod(370) =   -168
      bltnam(370) =  'M2020'

      bltcod(371) =   -170
      bltnam(371) =  'JWST'

      bltcod(372) =   -170
      bltnam(372) =  'JAMES WEBB SPACE TELESCOPE'

      bltcod(373) =   -172
      bltnam(373) =  'EXM RSP SCC'

      bltcod(374) =   -172
      bltnam(374) =  'EXM SPACECRAFT COMPOSITE'

      bltcod(375) =   -172
      bltnam(375) =  'EXOMARS SCC'

      bltcod(376) =   -173
      bltnam(376) =  'EXM RSP SP'

      bltcod(377) =   -173
      bltnam(377) =  'EXM SURFACE PLATFORM'

      bltcod(378) =   -173
      bltnam(378) =  'EXOMARS SP'

      bltcod(379) =   -174
      bltnam(379) =  'EXM RSP RM'

      bltcod(380) =   -174
      bltnam(380) =  'EXM ROVER'

      bltcod(381) =   -174
      bltnam(381) =  'EXOMARS ROVER'

      bltcod(382) =   -177
      bltnam(382) =  'GRAIL-A'

      bltcod(383) =   -178
      bltnam(383) =  'PLANET-B'

      bltcod(384) =   -178
      bltnam(384) =  'NOZOMI'

      bltcod(385) =   -181
      bltnam(385) =  'GRAIL-B'

      bltcod(386) =   -183
      bltnam(386) =  'CLUSTER 1'

      bltcod(387) =   -185
      bltnam(387) =  'CLUSTER 2'

      bltcod(388) =   -188
      bltnam(388) =  'MUSES-B'

      bltcod(389) =   -189
      bltnam(389) =  'NSYT'

      bltcod(390) =   -189
      bltnam(390) =  'INSIGHT'

      bltcod(391) =   -190
      bltnam(391) =  'SIM'

      bltcod(392) =   -194
      bltnam(392) =  'CLUSTER 3'

      bltcod(393) =   -196
      bltnam(393) =  'CLUSTER 4'

      bltcod(394) =   -197
      bltnam(394) =  'EXOMARS_LARA'

      bltcod(395) =   -197
      bltnam(395) =  'LARA'

      bltcod(396) =   -198
      bltnam(396) =  'INTEGRAL'

      bltcod(397) =   -198
      bltnam(397) =  'NASA-ISRO SAR MISSION'

      bltcod(398) =   -198
      bltnam(398) =  'NISAR'

      bltcod(399) =   -200
      bltnam(399) =  'CONTOUR'

      bltcod(400) =   -202
      bltnam(400) =  'MAVEN'

      bltcod(401) =   -203
      bltnam(401) =  'DAWN'

      bltcod(402) =   -205
      bltnam(402) =  'SOIL MOISTURE ACTIVE AND PASSIVE'

      bltcod(403) =   -205
      bltnam(403) =  'SMAP'

      bltcod(404) =   -210
      bltnam(404) =  'LICIA'

      bltcod(405) =   -210
      bltnam(405) =  'LICIACUBE'

      bltcod(406) =   -212
      bltnam(406) =  'STV51'

      bltcod(407) =   -213
      bltnam(407) =  'STV52'

      bltcod(408) =   -214
      bltnam(408) =  'STV53'

      bltcod(409) =   -226
      bltnam(409) =  'ROSETTA'

      bltcod(410) =   -227
      bltnam(410) =  'KEPLER'

      bltcod(411) =   -228
      bltnam(411) =  'GLL PROBE'

      bltcod(412) =   -228
      bltnam(412) =  'GALILEO PROBE'

      bltcod(413) =   -234
      bltnam(413) =  'STEREO AHEAD'

      bltcod(414) =   -235
      bltnam(414) =  'STEREO BEHIND'

      bltcod(415) =   -236
      bltnam(415) =  'MESSENGER'

      bltcod(416) =   -238
      bltnam(416) =  'SMART1'

      bltcod(417) =   -238
      bltnam(417) =  'SM1'

      bltcod(418) =   -238
      bltnam(418) =  'S1'

      bltcod(419) =   -238
      bltnam(419) =  'SMART-1'

      bltcod(420) =   -239
      bltnam(420) =  'MARTIAN MOONS EXPLORATION'

      bltcod(421) =   -239
      bltnam(421) =  'MMX'

      bltcod(422) =   -240
      bltnam(422) =  'SMART LANDER FOR INVESTIGATING MOON'

      bltcod(423) =   -240
      bltnam(423) =  'SLIM'

      bltcod(424) =   -242
      bltnam(424) =  'LUNAR TRAILBLAZER'

      bltcod(425) =   -243
      bltnam(425) =  'VIPER'

      bltcod(426) =   -248
      bltnam(426) =  'VEX'

      bltcod(427) =   -248
      bltnam(427) =  'VENUS EXPRESS'

      bltcod(428) =   -253
      bltnam(428) =  'OPPORTUNITY'

      bltcod(429) =   -253
      bltnam(429) =  'MER-1'

      bltcod(430) =   -254
      bltnam(430) =  'SPIRIT'

      bltcod(431) =   -254
      bltnam(431) =  'MER-2'

      bltcod(432) =   -255
      bltnam(432) =  'PSYC'

      bltcod(433) =   -301
      bltnam(433) =  'HELIOS 1'

      bltcod(434) =   -302
      bltnam(434) =  'HELIOS 2'

      bltcod(435) =   -362
      bltnam(435) =  'RADIATION BELT STORM PROBE A'

      bltcod(436) =   -362
      bltnam(436) =  'RBSP_A'

      bltcod(437) =   -363
      bltnam(437) =  'RADIATION BELT STORM PROBE B'

      bltcod(438) =   -363
      bltnam(438) =  'RBSP_B'

      bltcod(439) =   -500
      bltnam(439) =  'RSAT'

      bltcod(440) =   -500
      bltnam(440) =  'SELENE Relay Satellite'

      bltcod(441) =   -500
      bltnam(441) =  'SELENE Rstar'

      bltcod(442) =   -500
      bltnam(442) =  'Rstar'

      bltcod(443) =   -502
      bltnam(443) =  'VSAT'

      bltcod(444) =   -502
      bltnam(444) =  'SELENE VLBI Radio Satellite'

      bltcod(445) =   -502
      bltnam(445) =  'SELENE VRAD Satellite'

      bltcod(446) =   -502
      bltnam(446) =  'SELENE Vstar'

      bltcod(447) =   -502
      bltnam(447) =  'Vstar'

      bltcod(448) =   -550
      bltnam(448) =  'MARS-96'

      bltcod(449) =   -550
      bltnam(449) =  'M96'

      bltcod(450) =   -550
      bltnam(450) =  'MARS 96'

      bltcod(451) =   -550
      bltnam(451) =  'MARS96'

      bltcod(452) =   -652
      bltnam(452) =  'MERCURY TRANSFER MODULE'

      bltcod(453) =   -652
      bltnam(453) =  'MTM'

      bltcod(454) =   -652
      bltnam(454) =  'BEPICOLOMBO MTM'

      bltcod(455) =   -750
      bltnam(455) =  'SPRINT-A'

      bltcod(456) =   50000001
      bltnam(456) =  'SHOEMAKER-LEVY 9-W'

      bltcod(457) =   50000002
      bltnam(457) =  'SHOEMAKER-LEVY 9-V'

      bltcod(458) =   50000003
      bltnam(458) =  'SHOEMAKER-LEVY 9-U'

      bltcod(459) =   50000004
      bltnam(459) =  'SHOEMAKER-LEVY 9-T'

      bltcod(460) =   50000005
      bltnam(460) =  'SHOEMAKER-LEVY 9-S'

      bltcod(461) =   50000006
      bltnam(461) =  'SHOEMAKER-LEVY 9-R'

      bltcod(462) =   50000007
      bltnam(462) =  'SHOEMAKER-LEVY 9-Q'

      bltcod(463) =   50000008
      bltnam(463) =  'SHOEMAKER-LEVY 9-P'

      bltcod(464) =   50000009
      bltnam(464) =  'SHOEMAKER-LEVY 9-N'

      bltcod(465) =   50000010
      bltnam(465) =  'SHOEMAKER-LEVY 9-M'

      bltcod(466) =   50000011
      bltnam(466) =  'SHOEMAKER-LEVY 9-L'

      bltcod(467) =   50000012
      bltnam(467) =  'SHOEMAKER-LEVY 9-K'

      bltcod(468) =   50000013
      bltnam(468) =  'SHOEMAKER-LEVY 9-J'

      bltcod(469) =   50000014
      bltnam(469) =  'SHOEMAKER-LEVY 9-H'

      bltcod(470) =   50000015
      bltnam(470) =  'SHOEMAKER-LEVY 9-G'

      bltcod(471) =   50000016
      bltnam(471) =  'SHOEMAKER-LEVY 9-F'

      bltcod(472) =   50000017
      bltnam(472) =  'SHOEMAKER-LEVY 9-E'

      bltcod(473) =   50000018
      bltnam(473) =  'SHOEMAKER-LEVY 9-D'

      bltcod(474) =   50000019
      bltnam(474) =  'SHOEMAKER-LEVY 9-C'

      bltcod(475) =   50000020
      bltnam(475) =  'SHOEMAKER-LEVY 9-B'

      bltcod(476) =   50000021
      bltnam(476) =  'SHOEMAKER-LEVY 9-A'

      bltcod(477) =   50000022
      bltnam(477) =  'SHOEMAKER-LEVY 9-Q1'

      bltcod(478) =   50000023
      bltnam(478) =  'SHOEMAKER-LEVY 9-P2'

      bltcod(479) =   1000001
      bltnam(479) =  'AREND'

      bltcod(480) =   1000002
      bltnam(480) =  'AREND-RIGAUX'

      bltcod(481) =   1000003
      bltnam(481) =  'ASHBROOK-JACKSON'

      bltcod(482) =   1000004
      bltnam(482) =  'BOETHIN'

      bltcod(483) =   1000005
      bltnam(483) =  'BORRELLY'

      bltcod(484) =   1000006
      bltnam(484) =  'BOWELL-SKIFF'

      bltcod(485) =   1000007
      bltnam(485) =  'BRADFIELD'

      bltcod(486) =   1000008
      bltnam(486) =  'BROOKS 2'

      bltcod(487) =   1000009
      bltnam(487) =  'BRORSEN-METCALF'

      bltcod(488) =   1000010
      bltnam(488) =  'BUS'

      bltcod(489) =   1000011
      bltnam(489) =  'CHERNYKH'

      bltcod(490) =   1000012
      bltnam(490) =  '67P/CHURYUMOV-GERASIMENKO (1969 R1)'

      bltcod(491) =   1000012
      bltnam(491) =  'CHURYUMOV-GERASIMENKO'

      bltcod(492) =   1000013
      bltnam(492) =  'CIFFREO'

      bltcod(493) =   1000014
      bltnam(493) =  'CLARK'

      bltcod(494) =   1000015
      bltnam(494) =  'COMAS SOLA'

      bltcod(495) =   1000016
      bltnam(495) =  'CROMMELIN'

      bltcod(496) =   1000017
      bltnam(496) =  'D''ARREST'

      bltcod(497) =   1000018
      bltnam(497) =  'DANIEL'

      bltcod(498) =   1000019
      bltnam(498) =  'DE VICO-SWIFT'

      bltcod(499) =   1000020
      bltnam(499) =  'DENNING-FUJIKAWA'

      bltcod(500) =   1000021
      bltnam(500) =  'DU TOIT 1'

      bltcod(501) =   1000022
      bltnam(501) =  'DU TOIT-HARTLEY'

      bltcod(502) =   1000023
      bltnam(502) =  'DUTOIT-NEUJMIN-DELPORTE'

      bltcod(503) =   1000024
      bltnam(503) =  'DUBIAGO'

      bltcod(504) =   1000025
      bltnam(504) =  'ENCKE'

      bltcod(505) =   1000026
      bltnam(505) =  'FAYE'

      bltcod(506) =   1000027
      bltnam(506) =  'FINLAY'

      bltcod(507) =   1000028
      bltnam(507) =  'FORBES'

      bltcod(508) =   1000029
      bltnam(508) =  'GEHRELS 1'

      bltcod(509) =   1000030
      bltnam(509) =  'GEHRELS 2'

      bltcod(510) =   1000031
      bltnam(510) =  'GEHRELS 3'

      bltcod(511) =   1000032
      bltnam(511) =  'GIACOBINI-ZINNER'

      bltcod(512) =   1000033
      bltnam(512) =  'GICLAS'

      bltcod(513) =   1000034
      bltnam(513) =  'GRIGG-SKJELLERUP'

      bltcod(514) =   1000035
      bltnam(514) =  'GUNN'

      bltcod(515) =   1000036
      bltnam(515) =  'HALLEY'

      bltcod(516) =   1000037
      bltnam(516) =  'HANEDA-CAMPOS'

      bltcod(517) =   1000038
      bltnam(517) =  'HARRINGTON'

      bltcod(518) =   1000039
      bltnam(518) =  'HARRINGTON-ABELL'

      bltcod(519) =   1000040
      bltnam(519) =  'HARTLEY 1'

      bltcod(520) =   1000041
      bltnam(520) =  'HARTLEY 2'

      bltcod(521) =   1000042
      bltnam(521) =  'HARTLEY-IRAS'

      bltcod(522) =   1000043
      bltnam(522) =  'HERSCHEL-RIGOLLET'

      bltcod(523) =   1000044
      bltnam(523) =  'HOLMES'

      bltcod(524) =   1000045
      bltnam(524) =  'HONDA-MRKOS-PAJDUSAKOVA'

      bltcod(525) =   1000046
      bltnam(525) =  'HOWELL'

      bltcod(526) =   1000047
      bltnam(526) =  'IRAS'

      bltcod(527) =   1000048
      bltnam(527) =  'JACKSON-NEUJMIN'

      bltcod(528) =   1000049
      bltnam(528) =  'JOHNSON'

      bltcod(529) =   1000050
      bltnam(529) =  'KEARNS-KWEE'

      bltcod(530) =   1000051
      bltnam(530) =  'KLEMOLA'

      bltcod(531) =   1000052
      bltnam(531) =  'KOHOUTEK'

      bltcod(532) =   1000053
      bltnam(532) =  'KOJIMA'

      bltcod(533) =   1000054
      bltnam(533) =  'KOPFF'

      bltcod(534) =   1000055
      bltnam(534) =  'KOWAL 1'

      bltcod(535) =   1000056
      bltnam(535) =  'KOWAL 2'

      bltcod(536) =   1000057
      bltnam(536) =  'KOWAL-MRKOS'

      bltcod(537) =   1000058
      bltnam(537) =  'KOWAL-VAVROVA'

      bltcod(538) =   1000059
      bltnam(538) =  'LONGMORE'

      bltcod(539) =   1000060
      bltnam(539) =  'LOVAS 1'

      bltcod(540) =   1000061
      bltnam(540) =  'MACHHOLZ'

      bltcod(541) =   1000062
      bltnam(541) =  'MAURY'

      bltcod(542) =   1000063
      bltnam(542) =  'NEUJMIN 1'

      bltcod(543) =   1000064
      bltnam(543) =  'NEUJMIN 2'

      bltcod(544) =   1000065
      bltnam(544) =  'NEUJMIN 3'

      bltcod(545) =   1000066
      bltnam(545) =  'OLBERS'

      bltcod(546) =   1000067
      bltnam(546) =  'PETERS-HARTLEY'

      bltcod(547) =   1000068
      bltnam(547) =  'PONS-BROOKS'

      bltcod(548) =   1000069
      bltnam(548) =  'PONS-WINNECKE'

      bltcod(549) =   1000070
      bltnam(549) =  'REINMUTH 1'

      bltcod(550) =   1000071
      bltnam(550) =  'REINMUTH 2'

      bltcod(551) =   1000072
      bltnam(551) =  'RUSSELL 1'

      bltcod(552) =   1000073
      bltnam(552) =  'RUSSELL 2'

      bltcod(553) =   1000074
      bltnam(553) =  'RUSSELL 3'

      bltcod(554) =   1000075
      bltnam(554) =  'RUSSELL 4'

      bltcod(555) =   1000076
      bltnam(555) =  'SANGUIN'

      bltcod(556) =   1000077
      bltnam(556) =  'SCHAUMASSE'

      bltcod(557) =   1000078
      bltnam(557) =  'SCHUSTER'

      bltcod(558) =   1000079
      bltnam(558) =  'SCHWASSMANN-WACHMANN 1'

      bltcod(559) =   1000080
      bltnam(559) =  'SCHWASSMANN-WACHMANN 2'

      bltcod(560) =   1000081
      bltnam(560) =  'SCHWASSMANN-WACHMANN 3'

      bltcod(561) =   1000082
      bltnam(561) =  'SHAJN-SCHALDACH'

      bltcod(562) =   1000083
      bltnam(562) =  'SHOEMAKER 1'

      bltcod(563) =   1000084
      bltnam(563) =  'SHOEMAKER 2'

      bltcod(564) =   1000085
      bltnam(564) =  'SHOEMAKER 3'

      bltcod(565) =   1000086
      bltnam(565) =  'SINGER-BREWSTER'

      bltcod(566) =   1000087
      bltnam(566) =  'SLAUGHTER-BURNHAM'

      bltcod(567) =   1000088
      bltnam(567) =  'SMIRNOVA-CHERNYKH'

      bltcod(568) =   1000089
      bltnam(568) =  'STEPHAN-OTERMA'

      bltcod(569) =   1000090
      bltnam(569) =  'SWIFT-GEHRELS'

      bltcod(570) =   1000091
      bltnam(570) =  'TAKAMIZAWA'

      bltcod(571) =   1000092
      bltnam(571) =  'TAYLOR'

      bltcod(572) =   1000093
      bltnam(572) =  'TEMPEL_1'

      bltcod(573) =   1000093
      bltnam(573) =  'TEMPEL 1'

      bltcod(574) =   1000094
      bltnam(574) =  'TEMPEL 2'

      bltcod(575) =   1000095
      bltnam(575) =  'TEMPEL-TUTTLE'

      bltcod(576) =   1000096
      bltnam(576) =  'TRITTON'

      bltcod(577) =   1000097
      bltnam(577) =  'TSUCHINSHAN 1'

      bltcod(578) =   1000098
      bltnam(578) =  'TSUCHINSHAN 2'

      bltcod(579) =   1000099
      bltnam(579) =  'TUTTLE'

      bltcod(580) =   1000100
      bltnam(580) =  'TUTTLE-GIACOBINI-KRESAK'

      bltcod(581) =   1000101
      bltnam(581) =  'VAISALA 1'

      bltcod(582) =   1000102
      bltnam(582) =  'VAN BIESBROECK'

      bltcod(583) =   1000103
      bltnam(583) =  'VAN HOUTEN'

      bltcod(584) =   1000104
      bltnam(584) =  'WEST-KOHOUTEK-IKEMURA'

      bltcod(585) =   1000105
      bltnam(585) =  'WHIPPLE'

      bltcod(586) =   1000106
      bltnam(586) =  'WILD 1'

      bltcod(587) =   1000107
      bltnam(587) =  'WILD 2'

      bltcod(588) =   1000108
      bltnam(588) =  'WILD 3'

      bltcod(589) =   1000109
      bltnam(589) =  'WIRTANEN'

      bltcod(590) =   1000110
      bltnam(590) =  'WOLF'

      bltcod(591) =   1000111
      bltnam(591) =  'WOLF-HARRINGTON'

      bltcod(592) =   1000112
      bltnam(592) =  'LOVAS 2'

      bltcod(593) =   1000113
      bltnam(593) =  'URATA-NIIJIMA'

      bltcod(594) =   1000114
      bltnam(594) =  'WISEMAN-SKIFF'

      bltcod(595) =   1000115
      bltnam(595) =  'HELIN'

      bltcod(596) =   1000116
      bltnam(596) =  'MUELLER'

      bltcod(597) =   1000117
      bltnam(597) =  'SHOEMAKER-HOLT 1'

      bltcod(598) =   1000118
      bltnam(598) =  'HELIN-ROMAN-CROCKETT'

      bltcod(599) =   1000119
      bltnam(599) =  'HARTLEY 3'

      bltcod(600) =   1000120
      bltnam(600) =  'PARKER-HARTLEY'

      bltcod(601) =   1000121
      bltnam(601) =  'HELIN-ROMAN-ALU 1'

      bltcod(602) =   1000122
      bltnam(602) =  'WILD 4'

      bltcod(603) =   1000123
      bltnam(603) =  'MUELLER 2'

      bltcod(604) =   1000124
      bltnam(604) =  'MUELLER 3'

      bltcod(605) =   1000125
      bltnam(605) =  'SHOEMAKER-LEVY 1'

      bltcod(606) =   1000126
      bltnam(606) =  'SHOEMAKER-LEVY 2'

      bltcod(607) =   1000127
      bltnam(607) =  'HOLT-OLMSTEAD'

      bltcod(608) =   1000128
      bltnam(608) =  'METCALF-BREWINGTON'

      bltcod(609) =   1000129
      bltnam(609) =  'LEVY'

      bltcod(610) =   1000130
      bltnam(610) =  'SHOEMAKER-LEVY 9'

      bltcod(611) =   1000131
      bltnam(611) =  'HYAKUTAKE'

      bltcod(612) =   1000132
      bltnam(612) =  'HALE-BOPP'

      bltcod(613) =   1003228
      bltnam(613) =  'C/2013 A1'

      bltcod(614) =   1003228
      bltnam(614) =  'SIDING SPRING'

      bltcod(615) =   2000001
      bltnam(615) =  'CERES'

      bltcod(616) =   2000002
      bltnam(616) =  'PALLAS'

      bltcod(617) =   2000004
      bltnam(617) =  'VESTA'

      bltcod(618) =   2000016
      bltnam(618) =  'PSYCHE'

      bltcod(619) =   2000021
      bltnam(619) =  'LUTETIA'

      bltcod(620) =   2000052
      bltnam(620) =  '52_EUROPA'

      bltcod(621) =   2000052
      bltnam(621) =  '52 EUROPA'

      bltcod(622) =   2000216
      bltnam(622) =  'KLEOPATRA'

      bltcod(623) =   2000253
      bltnam(623) =  'MATHILDE'

      bltcod(624) =   2000433
      bltnam(624) =  'EROS'

      bltcod(625) =   2000511
      bltnam(625) =  'DAVIDA'

      bltcod(626) =   2002867
      bltnam(626) =  'STEINS'

      bltcod(627) =   2004015
      bltnam(627) =  'WILSON-HARRINGTON'

      bltcod(628) =   2004179
      bltnam(628) =  'TOUTATIS'

      bltcod(629) =   2009969
      bltnam(629) =  '1992KD'

      bltcod(630) =   2009969
      bltnam(630) =  'BRAILLE'

      bltcod(631) =   2025143
      bltnam(631) =  'ITOKAWA'

      bltcod(632) =   2101955
      bltnam(632) =  'BENNU'

      bltcod(633) =   2162173
      bltnam(633) =  'RYUGU'

      bltcod(634) =   2431010
      bltnam(634) =  'IDA'

      bltcod(635) =   2431011
      bltnam(635) =  'DACTYL'

      bltcod(636) =   2486958
      bltnam(636) =  'ARROKOTH'

      bltcod(637) =   9511010
      bltnam(637) =  'GASPRA'

      bltcod(638) =   20000617
      bltnam(638) =  'PATROCLUS_BARYCENTER'

      bltcod(639) =   20000617
      bltnam(639) =  'PATROCLUS BARYCENTER'

      bltcod(640) =   20003548
      bltnam(640) =  'EURYBATES_BARYCENTER'

      bltcod(641) =   20003548
      bltnam(641) =  'EURYBATES BARYCENTER'

      bltcod(642) =   20011351
      bltnam(642) =  'LEUCUS'

      bltcod(643) =   20015094
      bltnam(643) =  'POLYMELE'

      bltcod(644) =   20021900
      bltnam(644) =  'ORUS'

      bltcod(645) =   20052246
      bltnam(645) =  'DONALDJOHANSON'

      bltcod(646) =   20065803
      bltnam(646) =  'DIDYMOS_BARYCENTER'

      bltcod(647) =   20065803
      bltnam(647) =  'DIDYMOS BARYCENTER'

      bltcod(648) =   120000617
      bltnam(648) =  'MENOETIUS'

      bltcod(649) =   120003548
      bltnam(649) =  'QUETA'

      bltcod(650) =   120065803
      bltnam(650) =  'DIMORPHOS'

      bltcod(651) =   920000617
      bltnam(651) =  'PATROCLUS'

      bltcod(652) =   920003548
      bltnam(652) =  'EURYBATES'

      bltcod(653) =   920065803
      bltnam(653) =  'DIDYMOS'

      bltcod(654) =   398989
      bltnam(654) =  'NOTO'

      bltcod(655) =   398990
      bltnam(655) =  'NEW NORCIA'

      bltcod(656) =   399001
      bltnam(656) =  'GOLDSTONE'

      bltcod(657) =   399002
      bltnam(657) =  'CANBERRA'

      bltcod(658) =   399003
      bltnam(658) =  'MADRID'

      bltcod(659) =   399004
      bltnam(659) =  'USUDA'

      bltcod(660) =   399005
      bltnam(660) =  'DSS-05'

      bltcod(661) =   399005
      bltnam(661) =  'PARKES'

      bltcod(662) =   399012
      bltnam(662) =  'DSS-12'

      bltcod(663) =   399013
      bltnam(663) =  'DSS-13'

      bltcod(664) =   399014
      bltnam(664) =  'DSS-14'

      bltcod(665) =   399015
      bltnam(665) =  'DSS-15'

      bltcod(666) =   399016
      bltnam(666) =  'DSS-16'

      bltcod(667) =   399017
      bltnam(667) =  'DSS-17'

      bltcod(668) =   399023
      bltnam(668) =  'DSS-23'

      bltcod(669) =   399024
      bltnam(669) =  'DSS-24'

      bltcod(670) =   399025
      bltnam(670) =  'DSS-25'

      bltcod(671) =   399026
      bltnam(671) =  'DSS-26'

      bltcod(672) =   399027
      bltnam(672) =  'DSS-27'

      bltcod(673) =   399028
      bltnam(673) =  'DSS-28'

      bltcod(674) =   399033
      bltnam(674) =  'DSS-33'

      bltcod(675) =   399034
      bltnam(675) =  'DSS-34'

      bltcod(676) =   399035
      bltnam(676) =  'DSS-35'

      bltcod(677) =   399036
      bltnam(677) =  'DSS-36'

      bltcod(678) =   399042
      bltnam(678) =  'DSS-42'

      bltcod(679) =   399043
      bltnam(679) =  'DSS-43'

      bltcod(680) =   399045
      bltnam(680) =  'DSS-45'

      bltcod(681) =   399046
      bltnam(681) =  'DSS-46'

      bltcod(682) =   399049
      bltnam(682) =  'DSS-49'

      bltcod(683) =   399053
      bltnam(683) =  'DSS-53'

      bltcod(684) =   399054
      bltnam(684) =  'DSS-54'

      bltcod(685) =   399055
      bltnam(685) =  'DSS-55'

      bltcod(686) =   399056
      bltnam(686) =  'DSS-56'

      bltcod(687) =   399061
      bltnam(687) =  'DSS-61'

      bltcod(688) =   399063
      bltnam(688) =  'DSS-63'

      bltcod(689) =   399064
      bltnam(689) =  'DSS-64'

      bltcod(690) =   399065
      bltnam(690) =  'DSS-65'

      bltcod(691) =   399066
      bltnam(691) =  'DSS-66'

      bltcod(692) =   399069
      bltnam(692) =  'DSS-69'

      return
      end subroutine zzidmap

      !> The function returns the hash value associated with N.
      !>
      !> This function returns the hash value corresponding to an integer.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     N           is an integer number.
      !>
      !>     M           is the divisor of the hashing function. This value
      !>                 defines the spread of the hash values, that spread
      !>                 covering the interval [1, M]. M must be a positive
      !>                 number. If it is not, the function signals an error
      !>                 and returns 0.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the hash value of N as computed using M
      !>     as the hash function divisor.
      !>
      !>     This function is sign-insensitive, i.e. it computes the same
      !>     hash value for A as is it does for -A.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1) If the input divisor value is not in the allowed range, the
      !>        error 'SPICE(INVALIDDIVISOR)' will be signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine computes the hash value of an integer number using
      !>     the user specified hash divisor value.
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
      !>     E.D. Wright     (JPL)
      !>     B.V. Semenov    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 02-AUG-2013 (BVS)
      module integer(ip) function zzhashi ( ctx, n, m )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: n  !! An integer number.
      integer(ip), intent(in) :: m  !! Divisor used for the hash function

!
!     Check divisor.
!
      if ( m <= 0 ) then

         zzhashi = 0

         call ctx%setmsg ( 'The input hash function divisor was not '    // &
                       'a positive number. It was #.'                )
         call ctx%errint ( '#', m                                        )
         call ctx%sigerr ( 'SPICE(INVALIDDIVISOR)'                       )
         return

      end if

!
!     Use simple division method -- h(k) = k mod m.
!
      zzhashi = mod(abs(n),m) + 1

      return
      end function zzhashi

      !> Initialize an add-only character hash.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HASHSZ      is the hash size. For efficiency reasons it must be a
      !>                 prime number in the range from 1 to INTMAX/68 - 1.
      !>
      !>     HEDLST
      !>     COLLST      are the head node pointer list and the node collision
      !>                 list components of an add-only character hash.
      !>
      !>## Detailed Output
      !>
      !>     HEDLST
      !>     COLLST      are the head node pointer list and the node collision
      !>                 list components of an add-only character hash with all
      !>                 elements of the head node pointer list HEDLST set to
      !>                 zero and size and first-free elements of the head
      !>                 node pointer list COLLST set to HASHSZ and 1
      !>                 correspondingly.
      !>
      !>## Parameters
      !>
      !>     LBPOOL      is the lower bound of the collision list array.
      !>
      !>## Exceptions
      !>
      !>     1) If HASHSZ is less than 1 or greater than the value that is
      !>        guaranteed not to result in eventual integer overflow in
      !>        ZZHASH2, the error will be signaled by ZZHASH2.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See the header of the umbrella routine ZZHSC.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZHSC.
      !>
      !>## Restrictions
      !>
      !>     See the header of the umbrella routine ZZHSC.
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
      !>-    SPICELIB Version 1.1.0, 12-OCT-2020 (BVS)
      !>
      !>        Changed to not check RETURN() on entry. The entry point
      !>        now initializes the hash as long as the size parameter
      !>        is greater than zero.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
      module subroutine zzhscini ( ctx, hashsz, hedlst, collst )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: hashsz  !! Hash size
      integer(ip), intent(inout) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip), intent(inout) :: collst  ( lbpool : * )  !! Hash node collision list
      logical :: return
      integer(ip) :: i
      integer(ip) :: lookat
      integer(ip) :: node
      logical :: full
      logical :: lfound
      if ( hashsz > 0 ) then

!
!        Wipe out head node pointer list.
!
         do i = 1, hashsz
            hedlst(i) = 0
         end do

!
!        Reset control area.
!
         collst( sizidx ) = hashsz
         collst( freidx ) = 1

      end if

!
!     The requested number of nodes must be in the valid range (as
!     ZZHASH2 checked it; PEPPER hashes with NAME_HASH, which does not).
!
      if ( hashsz <= 0 .or. hashsz > intmax / 68 - 1 ) then
         call ctx%setmsg ( 'The input hash function divisor was not '    // &
                       'in the allowed range from 1 to #. It was #.' )
         call ctx%errint ( '#', intmax / 68 - 1                        )
         call ctx%errint ( '#', hashsz                                   )
         call ctx%sigerr ( 'SPICE(INVALIDDIVISOR)'                       )
         return
      end if

      return
      end subroutine zzhscini

      !> Find or add, if not present, an item to an add-only character
      !> hash.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HEDLST
      !>     COLLST
      !>     ITEMS       are the components of an add-only character
      !>                 hash.
      !>
      !>     ITEM        is an item to find and, if needed, add to the hash.
      !>
      !>## Detailed Output
      !>
      !>     HEDLST
      !>     COLLST
      !>     ITEMS       are the components of an add-only character
      !>                 hash with the new item added to it if needed.
      !>
      !>     ITEMAT      is the index of the item in the node collision and
      !>                 item lists. If the item could not be found in or
      !>                 added to the hash, ITEMAT is set to 0.
      !>
      !>     NEW         is a flag indicting if item was added to the hash.
      !>
      !>## Parameters
      !>
      !>     LBPOOL      is the lower bound of the collision list array.
      !>
      !>## Exceptions
      !>
      !>     1) If the hash is full, the error SPICE(HASHISFULL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See the header of the umbrella routine ZZHSC.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZHSC.
      !>
      !>## Restrictions
      !>
      !>     See the header of the umbrella routine ZZHSC.
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
      !>-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
      module subroutine zzhscadd ( ctx, hedlst, collst, items, item, itemat, new )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip), intent(inout) :: collst  ( lbpool : * )  !! Hash node collision list
      character(*), intent(inout) :: items   (          * )  !! Hash item list
      character(*), intent(in) :: item  !! Item to be checked for/added
      integer(ip), intent(out) :: itemat  !! Item index in node collision and item lists
      logical, intent(out) :: new  !! Flag indicting if item was added
      integer(ip) :: i
      integer(ip) :: lookat
      integer(ip) :: node
      logical :: full
      logical :: lfound
      if ( ctx%failed() ) return

!
!     Set flag indicating whether the hash is full.
!
      full   = collst ( freidx ) > collst ( sizidx )

!
!     Use hash function to get index of the head node.
!
      lookat = name_hash ( item, collst ( sizidx ) )
      node   = hedlst  ( lookat )

!
!     Set initial values.
!
      lfound = .false.
      new    = .false.

!
!     See if this item (or one colliding with it in the hash scheme)
!     has already been stored in the item list.
!
      if ( node > 0 ) then

!
!        Start at the head node and check each item saved for this hash
!        value until we find a item that matches or run out of items in
!        this conflict resolution list.
!
         do while ( node > 0 .and. .not. lfound )

            lfound = items ( node ) == item
            itemat = node
            node   = collst ( node )

         end do

!
!        If we didn't find this item on the conflict resolution list
!        and our hash is not full we will add this item to it.
!
         if ( .not. lfound .and. .not. full   ) then

!
!           Get next free node.
!
            node = collst ( freidx )

!
!           Increment next free node pointer.
!
            collst ( freidx ) = collst ( freidx ) + 1

!
!           Set current node pointer to the node we just picked for
!           this item.
!
            collst ( itemat ) = node

!
!           Set new node pointer to 0, just in case.
!
            collst ( node ) = 0

!
!           Save item.
!
            items ( node ) = item

!
!           Set output node ID and new and found flags.
!
            itemat =  node
            new    = .true.
            lfound = .true.

         end if

      else if ( .not. full ) then

!
!        Nothing like this item (in the hashing sense) has been stored
!        so far and the hash is not full.
!
!        Get next free node.
!
         node = collst ( freidx )

!
!        Increment next free node pointer.
!
         collst ( freidx ) = collst ( freidx ) + 1

!
!        Set new node pointer to 0, just in case.
!
         collst ( node ) = 0

!
!        Set the head node pointer to this node.
!
         hedlst ( lookat ) = node

!
!        Save item.
!
         items ( node ) = item

!
!        Set output node ID and new and found flags.
!
         itemat =  node
         new    = .true.
         lfound = .true.

      end if

!
!     Set ITEMAT to 0 if LFOUND is FALSE.
!
      if ( .not. lfound ) then

         itemat = 0

      end if

!
!     If the item hash was full and we didn't find this item we've got
!     an error. Report it and return.
!
      if ( full .and. .not. lfound ) then

         call ctx%setmsg ( 'The hash has no room for any more items.')
         call ctx%sigerr ( 'SPICE(HASHISFULL)' )

         return

      end if

      return
      end subroutine zzhscadd

      !> Find an item in an add-only character hash.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HEDLST
      !>     COLLST
      !>     ITEMS       are the components of an add-only character
      !>                 hash.
      !>
      !>     ITEM        is an item to find in the hash.
      !>
      !>## Detailed Output
      !>
      !>     ITEMAT      is the index of the item in the node collision and
      !>                 item lists. If item is not in the hash, ITEMAT is set
      !>                 to 0.
      !>
      !>## Parameters
      !>
      !>     LBPOOL      is the lower bound of the collision list array.
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
      !>     See the header of the umbrella routine ZZHSC.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZHSC.
      !>
      !>## Restrictions
      !>
      !>     See the header of the umbrella routine ZZHSC.
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
      !>-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
      module subroutine zzhscchk ( ctx, hedlst, collst, items, item, itemat )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
      character(*), intent(in) :: items   (          * )  !! Hash item list
      character(*), intent(in) :: item  !! Item to find
      integer(ip), intent(out) :: itemat  !! Item index in node collision and item lists
      integer(ip) :: i
      integer(ip) :: lookat
      integer(ip) :: node
      logical :: full
      logical :: lfound
      if ( ctx%failed() ) return

!
!     Use hash function to get index of the head node.
!
      lookat = name_hash ( item, collst ( sizidx ) )
      node   = hedlst  ( lookat )

!
!     Set initial values.
!
      lfound  = .false.

!
!     See if this item (or one colliding with it in the hash scheme) is
!     in the item list.
!
      if ( node > 0 ) then

!
!        Start at the head node and check each item saved for this hash
!        value until we find a item that matches or run out of items in
!        this conflict resolution list.
!
         do while ( node > 0 .and. .not. lfound )

            lfound = items ( node ) == item
            itemat = node
            node   = collst ( node )

         end do

      end if

!
!     If LFOUND is false, set ITEMAT to 0.
!
      if ( .not. lfound ) then

         itemat = 0

      end if

      return
      end subroutine zzhscchk

      !> Get room available in an add-only character hash.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     COLLST      is the add-only character hash node collision
      !>                 list.
      !>
      !>## Detailed Output
      !>
      !>     AVAIL       is the room (number of vacant slots) available in
      !>                 the hash.
      !>
      !>## Parameters
      !>
      !>     LBPOOL      is the lower bound of the collision list array.
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
      !>     See the header of the umbrella routine ZZHSC.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZHSC.
      !>
      !>## Restrictions
      !>
      !>     See the header of the umbrella routine ZZHSC.
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
      !>-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
      module subroutine zzhscavl ( ctx, collst, avail )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
      integer(ip), intent(out) :: avail  !! Room available in the hash
      logical :: return
      integer(ip) :: i
      integer(ip) :: lookat
      integer(ip) :: node
      logical :: full
      logical :: lfound
      avail = collst ( sizidx ) - collst ( freidx ) + 1

      return
      end subroutine zzhscavl

      !> Get information about an add-only character hash.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HEDLST
      !>     COLLST
      !>     ITEMS       are the components of an add-only character
      !>                 hash.
      !>
      !>     ITEM        is the parameter to report:
      !>
      !>                    'HASH SIZE'
      !>                    'USED HEADNODE COUNT'
      !>                    'UNUSED HEADNODE COUNT'
      !>                    'USED ITEM COUNT'
      !>                    'UNUSED ITEM COUNT'
      !>                    'LONGEST LIST SIZE'
      !>
      !>## Detailed Output
      !>
      !>     AVAIL       is the value of the parameter of interest.
      !>
      !>## Parameters
      !>
      !>     LBPOOL      is the lower bound of the collision list array.
      !>
      !>## Exceptions
      !>
      !>     1) If the input ITEM is not recognized, the error
      !>        SPICE(ITEMNOTRECOGNIZED) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See the header of the umbrella routine ZZHSC.
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     See the header of the umbrella routine ZZHSC.
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
      !>-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
      module subroutine zzhscinf ( ctx, hedlst, collst, items, item, avail )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_constants, only: intmax
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
      character(*), intent(in) :: items   (          * )  !! Hash item list
      character(*), intent(in) :: item  !! parameter to report
      integer(ip), intent(out) :: avail  !! parameter Value
      logical :: return
      integer(ip) :: i
      integer(ip) :: lookat
      integer(ip) :: node
      logical :: full
      logical :: lfound
      if      ( item == 'HASH SIZE' ) then

         avail = collst ( sizidx )

!
!     Get the count of used nodes in the head list.
!
      else if ( item == 'USED HEADNODE COUNT' ) then

         avail = 0
         do i = 1, collst ( sizidx )
            if ( hedlst ( i ) /= 0 ) then
               avail = avail + 1
            end if
         end do

!
!     Get the count of unused nodes in the head list.
!
      else if ( item == 'UNUSED HEADNODE COUNT' ) then

         avail = 0
         do i = 1, collst ( sizidx )
            if ( hedlst ( i ) == 0 ) then
               avail = avail + 1
            end if
         end do

!
!     Get the count of used slots in the item list.
!
      else if ( item == 'USED ITEM COUNT' ) then

         avail = collst ( freidx ) - 1

!
!     Get the count of unused slots in the item list.
!
      else if ( item == 'UNUSED ITEM COUNT' ) then

         avail = collst ( sizidx ) - collst ( freidx ) + 1

!
!     Get the size of the longest item list for any hash value.
!
      else if ( item == 'LONGEST LIST SIZE' ) then

         avail = 0
         do i = 1, collst ( sizidx )
            node = hedlst ( i )
            lookat = 0
            do while ( node > 0 )
               lookat = lookat + 1
               node   = collst ( node )
            end do
            avail = max( avail, lookat )
         end do

!
!     This parameter is not supported.
!
      else

         avail = 0

         call ctx%setmsg ( 'Parameter ''#'' is not recognized.' )
         call ctx%errch  ( '#', item                            )
         call ctx%sigerr ( 'SPICE(ITEMNOTRECOGNIZED)'           )

      end if

      return
      end subroutine zzhscinf

      !> Initialize an add-only integer hash.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HASHSZ      is the hash size. For efficiency reasons it must be a
      !>                 positive prime number.
      !>
      !>     HEDLST
      !>     COLLST      are the head node pointer list and the node collision
      !>                 list components of an add-only integer hash.
      !>
      !>## Detailed Output
      !>
      !>     HEDLST
      !>     COLLST      are the head node pointer list and the node collision
      !>                 list components of an add-only integer hash with all
      !>                 elements of the head node pointer list HEDLST set to
      !>                 zero and size and first-free elements of the head
      !>                 node pointer list COLLST set to HASHSZ and 1
      !>                 correspondingly.
      !>
      !>## Parameters
      !>
      !>     LBPOOL      is the lower bound of the collision list array.
      !>
      !>## Exceptions
      !>
      !>     1) If HASHSZ is less than 1, the error SPICE(INVALIDCOUNT) is
      !>        signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See the header of the umbrella routine ZZHSI.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZHSI.
      !>
      !>## Restrictions
      !>
      !>     See the header of the umbrella routine ZZHSI.
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
      !>-    SPICELIB Version 1.1.0, 07-OCT-2020 (JDR) (BVS) (NJB)
      !>
      !>        This entry point no longer checks RETURN() on entry. It
      !>        initializes the hash as long as the size parameter is greater
      !>        than zero.
      !>
      !>-    SPICELIB Version 1.0.0, 01-AUG-2013 (BVS)
      module subroutine zzhsiini ( ctx, hashsz, hedlst, collst )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: hashsz  !! Hash size
      integer(ip), intent(inout) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip), intent(inout) :: collst  ( lbpool : * )  !! Hash node collision list
      logical :: return
      integer(ip) :: i
      integer(ip) :: lookat
      integer(ip) :: node
      logical :: full
      logical :: lfound
      if ( hashsz > 0 ) then
!
!        Wipe out head node pointer list.
!
         do i = 1, hashsz
            hedlst(i) = 0
         end do
!
!        Reset control area.
!
         collst( sizidx ) = hashsz
         collst( freidx ) = 1

      end if

!
!     The requested number of nodes must be valid. ZZHASHI will check
!     that.
!
      i = zzhashi( ctx, 1, hashsz )

      if ( ctx%failed() ) return

      return
      end subroutine zzhsiini

      !> Find or add, if not present, an item to an add-only integer
      !> hash.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HEDLST
      !>     COLLST
      !>     ITEMS       are the components of an add-only integer
      !>                 hash.
      !>
      !>     ITEM        is an item to find and, if needed, add to the hash.
      !>
      !>## Detailed Output
      !>
      !>     HEDLST
      !>     COLLST
      !>     ITEMS       are the components of an add-only integer
      !>                 hash with the new item added to it if needed.
      !>
      !>     ITEMAT      is the index of the item in the node collision and
      !>                 item lists. If the item could not be found in or
      !>                 added to the hash, ITEMAT is set to 0.
      !>
      !>     NEW         is a flag indicting if item was added to the hash.
      !>
      !>## Parameters
      !>
      !>     LBPOOL      is the lower bound of the collision list array.
      !>
      !>## Exceptions
      !>
      !>     1) If the hash is full, the error SPICE(HASHISFULL) is signaled.
      !>
      !>     2) If the hash size saved in the control area of the collision
      !>        list is less than 1, the error SPICE(UNINITIALIZEDHASH) is
      !>        signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See the header of the umbrella routine ZZHSI.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZHSI.
      !>
      !>## Restrictions
      !>
      !>     See the header of the umbrella routine ZZHSI.
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
      !>-    SPICELIB Version 1.0.0, 01-AUG-2013 (BVS)
      module subroutine zzhsiadd ( ctx, hedlst, collst, items, item, itemat, new )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip), intent(inout) :: collst  ( lbpool : * )  !! Hash node collision list
      integer(ip), intent(inout) :: items   (          * )  !! Hash item list
      integer(ip), intent(in) :: item  !! Item to be checked for/added
      integer(ip), intent(out) :: itemat  !! Item index in node collision and item lists
      logical, intent(out) :: new  !! Flag indicting if item was added
      integer(ip) :: i
      integer(ip) :: lookat
      integer(ip) :: node
      logical :: full
      logical :: lfound
      if ( ctx%failed() ) return

!
!     Set flag indicating whether the hash is full.
!
      full   = collst ( freidx ) > collst ( sizidx )

!
!     Use simple division hash function to get index of the head node.
!
      if ( collst ( sizidx ) < 1 ) then

         call ctx%setmsg ( 'Uninitialized hash. Size was #.' )
         call ctx%errint ( '#', collst ( sizidx )            )
         call ctx%sigerr ( 'SPICE(UNINITIALIZEDHASH)'        )
         return

      end if

      lookat = zzhashi ( ctx, item , collst ( sizidx ) )
      node   = hedlst  ( lookat )

!
!     Set initial values.
!
      lfound = .false.
      new    = .false.

!
!     See if this item (or one colliding with it in the hash scheme)
!     has already been stored in the item list.
!
      if ( node > 0 ) then

!
!        Start at the head node and check each item saved for this hash
!        value until we find a item that matches or run out of items in
!        this conflict resolution list.
!
         do while ( node > 0 .and. .not. lfound )

            lfound = items ( node ) == item
            itemat = node
            node   = collst ( node )

         end do

!
!        If we didn't find this item on the conflict resolution list
!        and our hash is not full we will add this item to it.
!
         if ( .not. lfound .and. .not. full   ) then

!
!           Get next free node.
!
            node = collst ( freidx )

!
!           Increment next free node pointer.
!
            collst ( freidx ) = collst ( freidx ) + 1

!
!           Set current node pointer to the node we just picked for
!           this item.
!
            collst ( itemat ) = node

!
!           Set new node pointer to 0, just in case.
!
            collst ( node ) = 0

!
!           Save item.
!
            items ( node ) = item

!
!           Set output node ID and new and found flags.
!
            itemat =  node
            new    = .true.
            lfound = .true.

         end if

      else if ( .not. full ) then

!
!        Nothing like this item (in the hashing sense) has been stored
!        so far and the hash is not full.
!
!        Get next free node.
!
         node = collst ( freidx )

!
!        Increment next free node pointer.
!
         collst ( freidx ) = collst ( freidx ) + 1

!
!        Set new node pointer to 0, just in case.
!
         collst ( node ) = 0

!
!        Set the head node pointer to this node.
!
         hedlst ( lookat ) = node

!
!        Save item.
!
         items ( node ) = item

!
!        Set output node ID and new and found flags.
!
         itemat =  node
         new    = .true.
         lfound = .true.

      end if

!
!     Set ITEMAT to 0 if LFOUND is FALSE.
!
      if ( .not. lfound ) then

         itemat = 0

      end if

!
!     If the item hash was full and we didn't find this item we've got
!     an error. Report it and return.
!
      if ( full .and. .not. lfound ) then

         call ctx%setmsg ( 'The hash has no room for any more items.')
         call ctx%sigerr ( 'SPICE(HASHISFULL)' )

         return

      end if

      return
      end subroutine zzhsiadd

      !> Find an item in an add-only integer hash.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HEDLST
      !>     COLLST
      !>     ITEMS       are the components of an add-only integer
      !>                 hash.
      !>
      !>     ITEM        is an item to find in the hash.
      !>
      !>## Detailed Output
      !>
      !>     ITEMAT      is the index of the item in the node collision and
      !>                 item lists. If item is not in the hash, ITEMAT is set
      !>                 to 0.
      !>
      !>## Parameters
      !>
      !>     LBPOOL      is the lower bound of the collision list array.
      !>
      !>## Exceptions
      !>
      !>     1) If the hash size saved in the control area of the collision
      !>        list is less than 1, the error SPICE(UNINITIALIZEDHASH) is
      !>        signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See the header of the umbrella routine ZZHSI.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZHSI.
      !>
      !>## Restrictions
      !>
      !>     See the header of the umbrella routine ZZHSI.
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
      !>-    SPICELIB Version 1.1.0, 14-OCT-2021 (BVS)
      !>
      !>        Bug fix: fixed routine name in CHKIN/CHKOUT calls
      !>        (ZZHSIADD -> ZZHSICHK).
      !>
      !>-    SPICELIB Version 1.0.0, 01-AUG-2013 (BVS)
      module subroutine zzhsichk ( ctx, hedlst, collst, items, item, itemat )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
      integer(ip), intent(in) :: items   (          * )  !! Hash item list
      integer(ip), intent(in) :: item  !! Item to find
      integer(ip), intent(out) :: itemat  !! Item index in node collision and item lists
      integer(ip) :: i
      integer(ip) :: lookat
      integer(ip) :: node
      logical :: full
      logical :: lfound
      if ( ctx%failed() ) return

!
!     Use simple division hash function to get index of the head node.
!
      if ( collst ( sizidx ) < 1 ) then

         call ctx%setmsg ( 'Uninitialized hash. Size was #.' )
         call ctx%errint ( '#', collst ( sizidx )            )
         call ctx%sigerr ( 'SPICE(UNINITIALIZEDHASH)'        )
         return

      end if

      lookat = zzhashi ( ctx, item , collst ( sizidx ) )
      node   = hedlst  ( lookat )

!
!     Set initial values.
!
      lfound  = .false.

!
!     See if this item (or one colliding with it in the hash scheme) is
!     in the item list.
!
      if ( node > 0 ) then

!
!        Start at the head node and check each item saved for this hash
!        value until we find a item that matches or run out of items in
!        this conflict resolution list.
!
         do while ( node > 0 .and. .not. lfound )

            lfound = items ( node ) == item
            itemat = node
            node   = collst ( node )

         end do

      end if

!
!     If LFOUND is false, set ITEMAT to 0.
!
      if ( .not. lfound ) then

         itemat = 0

      end if

      return
      end subroutine zzhsichk

      !> Get room available in an add-only integer hash.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     COLLST      is the add-only integer hash node collision
      !>                 list.
      !>
      !>## Detailed Output
      !>
      !>     AVAIL       is the room (number of vacant slots) available in
      !>                 the hash.
      !>
      !>## Parameters
      !>
      !>     LBPOOL      is the lower bound of the collision list array.
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
      !>     See the header of the umbrella routine ZZHSI.
      !>
      !>## Examples
      !>
      !>     See the header of the umbrella routine ZZHSI.
      !>
      !>## Restrictions
      !>
      !>     See the header of the umbrella routine ZZHSI.
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
      !>-    SPICELIB Version 1.0.0, 01-AUG-2013 (BVS)
      module subroutine zzhsiavl ( ctx, collst, avail )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
      integer(ip), intent(out) :: avail  !! Room available in the hash
      logical :: return
      integer(ip) :: i
      integer(ip) :: lookat
      integer(ip) :: node
      logical :: full
      logical :: lfound
      avail = collst ( sizidx ) - collst ( freidx ) + 1

      return
      end subroutine zzhsiavl

      !> Get information about an add-only integer hash.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     HEDLST
      !>     COLLST
      !>     ITEMS       are the components of an add-only integer
      !>                 hash.
      !>
      !>     ITEM        is the parameter to report:
      !>
      !>                    'HASH SIZE'
      !>                    'USED HEADNODE COUNT'
      !>                    'UNUSED HEADNODE COUNT'
      !>                    'USED ITEM COUNT'
      !>                    'UNUSED ITEM COUNT'
      !>                    'LONGEST LIST SIZE'
      !>
      !>## Detailed Output
      !>
      !>     AVAIL       is the value of the parameter of interest.
      !>
      !>## Parameters
      !>
      !>     LBPOOL      is the lower bound of the collision list array.
      !>
      !>## Exceptions
      !>
      !>     1) If the input ITEM is not recognized, the error
      !>        SPICE(ITEMNOTRECOGNIZED) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     See the header of the umbrella routine ZZHSC.
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     See the header of the umbrella routine ZZHSC.
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
      !>-    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS)
      module subroutine zzhsiinf ( ctx, hedlst, collst, items, param, avail )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
      integer(ip), intent(in) :: items   (          * )  !! Hash item list
      character(*), intent(in) :: param  !! parameter to report
      integer(ip), intent(out) :: avail  !! parameter Value
      logical :: return
      integer(ip) :: i
      integer(ip) :: lookat
      integer(ip) :: node
      logical :: full
      logical :: lfound
      if      ( param == 'HASH SIZE' ) then

         avail = collst ( sizidx )

!
!     Get the count of used nodes in the head list.
!
      else if ( param == 'USED HEADNODE COUNT' ) then

         avail = 0
         do i = 1, collst ( sizidx )
            if ( hedlst ( i ) /= 0 ) then
               avail = avail + 1
            end if
         end do

!
!     Get the count of unused nodes in the head list.
!
      else if ( param == 'UNUSED HEADNODE COUNT' ) then

         avail = 0
         do i = 1, collst ( sizidx )
            if ( hedlst ( i ) == 0 ) then
               avail = avail + 1
            end if
         end do

!
!     Get the count of used slots in the item list.
!
      else if ( param == 'USED ITEM COUNT' ) then

         avail = collst ( freidx ) - 1

!
!     Get the count of unused slots in the item list.
!
      else if ( param == 'UNUSED ITEM COUNT' ) then

         avail = collst ( sizidx ) - collst ( freidx ) + 1

!
!     Get the size of the longest item list for any hash value.
!
      else if ( param == 'LONGEST LIST SIZE' ) then

         avail = 0
         do i = 1, collst ( sizidx )
            node = hedlst ( i )
            lookat = 0
            do while ( node > 0 )
               lookat = lookat + 1
               node   = collst ( node )
            end do
            avail = max( avail, lookat )
         end do

!
!     This parameter is not supported.
!
      else

         avail = 0

         call ctx%setmsg ( 'Parameter ''#'' is not recognized.' )
         call ctx%errch  ( '#', param                           )
         call ctx%sigerr ( 'SPICE(ITEMNOTRECOGNIZED)'           )

      end if

      return
      end subroutine zzhsiinf

      !> Initialize the name-based and ID-based hashes used for efficient
      !> access to body-name mapping arrays. This routine should be called
      !> by ZZBODTRN and ZZBODKER only.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     NAMES     is the array of body names. This array is parallel to
      !>               NORNAM and CODES.
      !>
      !>     NORNAM    is the array of normalized body names, made from
      !>               elements of NAMES by upper-casing, left-justifying, and
      !>               compressing groups of spaces to a single space. This
      !>               represents the canonical member of the equivalence
      !>               class each parallel entry in NAMES belongs.
      !>
      !>     CODES     is the array of body codes extracted. This array is
      !>               parallel to NAMES and NORNAM.
      !>
      !>     NVALS     is the number of items contained in NAMES, NORNAM,
      !>               CODES.
      !>
      !>     MAXVAL    is the output hash size.
      !>
      !>## Detailed Output
      !>
      !>     All output arrays must be declared with the dimension MAXVAL.
      !>     MAXVAL must be greater than or equal to NVALS.
      !>
      !>     BNMLST
      !>     BNMPOL
      !>     BNMNMS    are the body name-based hash head node pointer, node
      !>               collision, and item lists. Together they return the
      !>               index of the element in the BNMIDX index storage array
      !>               that stores the index of the body items in the input
      !>               storage arrays.
      !>
      !>     BNMIDX    is the body name-based hash index storage array
      !>               containing at the index determined by the hash for a
      !>               given normalized name the index corresponding to this
      !>               name in the NAMES, NORNAM, and CODES arrays.
      !>
      !>     BIDLST
      !>     BIDPOL
      !>     BIDIDS    are the body ID-based hash head node pointer, node
      !>               collision, and item lists. Together they return the
      !>               index of the element in the BNMIDX index storage array
      !>               that stores the index of the body items in the input
      !>               storage arrays.
      !>
      !>     BIDIDX    is the body ID-based hash index storage array
      !>               containing at the index determined by the hash for a
      !>               given ID the index corresponding to the same ID in the
      !>               NAMES, NORNAM, and CODES arrays.
      !>
      !>## Parameters
      !>
      !>     LBPOOL    is the lower bound of the hashes' collision list array.
      !>
      !>     MAXL      is the maximum length of a body name. Defined in the
      !>               include file 'zzbodtrn.inc'.
      !>
      !>## Exceptions
      !>
      !>     1) If the input number of bodies NVALS is not less than or equal
      !>        to the size of the output hash, the error 'SPICE(BUG1)' will be
      !>        signaled.
      !>
      !>     2) If registering an ID in the output ID-based hash fails, the
      !>        error 'SPICE(BUG2)' will be signaled.
      !>
      !>     3) If registering an name in the output name-based hash fails,
      !>        the error 'SPICE(BUG3)' will be signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a utility routine used for initializing the hashes
      !>     facilitating efficient body name-ID translation in ZZBODTRN.
      !>
      !>     The order of mapping in the input arrays determines the priority,
      !>     with the mapping with the lowest priority being first and the
      !>     mapping with the highest priority being last.
      !>
      !>     If more than one entry with a particular normalized name is
      !>     present in the input arrays, only the latest entry is registered
      !>     in the name-based hash.
      !>
      !>     If more than one entry with a particular ID is present in the
      !>     input arrays, only the latest entry that maps to a not-yet
      !>     registered normalized name is registered in the ID-based hash.
      !>     Registering IDs only for not-yet registered names achieves masking
      !>     all IDs with the lower priority in cases when a single normalized
      !>     name maps to more than one ID.
      !>
      !>## Examples
      !>
      !>     See the routine ZZBODTRN.
      !>
      !>## Restrictions
      !>
      !>     1) This routine is intended only for use by ZZBODTRN and
      !>        ZZBODKER.
      !>
      !>     2) All output hash arrays must be declared with the same dimension
      !>        which is greater than or equal to MAXVAL.
      !>
      !>     3) The order of mappings in the input arrays determines the
      !>        priority, with the mapping with the lowest priority being
      !>        the first and the mapping with the highest priority being
      !>        the last.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     B.V. Semenov       (JPL)
      !>     M.J. Spencer       (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.0.0, 16-SEP-2013 (BVS)
      !>
      !>        Changed routine's calling sequence by dropping name and ID
      !>        order vectors and adding name- and ID-based hashes and
      !>        modified it to initialize hashes instead of the order arrays.
      !>
      !>-    SPICELIB Version 3.0.0, 23-AUG-2002 (FST)
      !>
      !>        Implemented changes to support the new precedence
      !>        system.
      !>
      !>        Altered the calling sequence of ZZBODINI to remove
      !>        unused arguments.  This routine also no longer computes
      !>        NORNAM from NAMES, since it is used in a more general
      !>        capacity.
      !>
      !>        Updated module header and comments to document additional
      !>        assumptions this module now makes about its inputs.
      !>
      !>        This routine is now error free.
      !>
      !>-    SPICELIB Version 2.1.1, 07-MAR-2002 (EDW)
      !>
      !>        Modified error logic to allow duplicate
      !>        NAME -> CODE mappings without signaling an error.
      !>        The mapping operation is a no-op, but might
      !>        cause a user problems if an error signals.
      !>
      !>-    SPICELIB Version 2.1.0, 12-AUG-2001 (EDW)
      !>
      !>        Modified logic for all ZZBOD routines to function with
      !>        equivalence class concept. A body name now exists
      !>        as a member of an equivalence class named by the
      !>        normalized form of the body name. To facilitate this
      !>        concept, an addition name vector, NORNAM, and
      !>        order vector, ORDNOM, now exist.
      !>
      !>-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) (WLT)
      !>
      !>        Renamed to ZZBODINI and filled out the comments on what this
      !>        routine does and how it works.
      module subroutine zzbodini ( ctx, names, nornam, codes, nvals, maxval, bnmlst, bnmpol, bnmnms, bnmidx, bidlst, bidpol, bidids, bididx )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(maxl), intent(in) :: names  (          * )  !! Array of names
      character(maxl), intent(in) :: nornam (          * )  !! Array of normalized names
      integer(ip), intent(in) :: codes  (          * )  !! Array of ID codes for NAMES/NORNAM
      integer(ip), intent(in) :: nvals  !! Length of NAMES, NORNAM, and CODES arrays
      integer(ip), intent(in) :: maxval  !! Size of the hash arrays
      integer(ip), intent(out) :: bnmlst (          * )  !! Body name-based hash head node
                                                         !! pointer list
      integer(ip), intent(out) :: bnmpol ( lbpool : * )  !! Body name-based hash node collision
                                                         !! list
      character(maxl), intent(out) :: bnmnms (          * )  !! Body name-based hash item list
      integer(ip), intent(out) :: bnmidx (          * )  !! Body name-based hash index storage
                                                         !! array
      integer(ip), intent(out) :: bidlst (          * )  !! Body ID-based hash head node pointer
                                                         !! list
      integer(ip), intent(out) :: bidpol ( lbpool : * )  !! Body ID-based hash node collision
                                                         !! list
      integer(ip), intent(out) :: bidids (          * )  !! Body ID-based hash item list
      integer(ip), intent(out) :: bididx (          * )  !! Body ID-based hash index storage
                                                         !! array


      integer(ip) :: i
      integer(ip) :: item

      logical :: new

!
!     Consistency check.
!
      if ( maxval < nvals ) then
         call ctx%setmsg ( 'There is an inconsistency between the '  // &
                       'number of input bodies and the size of ' // &
                       'the output hashes. The number of input ' // &
                       'bodies was #. The size of the output '   // &
                       'hashes was #.' )
         call ctx%errint ( '#', nvals  )
         call ctx%errint ( '#', maxval )
         call ctx%sigerr ( 'SPICE(BUG1)' )
         return
      end if

!
!     Initialize output hashes.
!
      call zzhsiini ( ctx, maxval, bidlst, bidpol )
      call zzhscini ( ctx, maxval, bnmlst, bnmpol )

!
!     Loop through the input arrays to populate hashes. We do it
!     backwards to pick and register only the highest priority (latest)
!     values for each normalized name.
!
      do i = nvals, 1, -1

!
!        Register this normalized name, but only if it is not already
!        in the hash.
!
         call zzhscadd ( ctx, bnmlst, bnmpol, bnmnms, nornam(i), item, new )
         if ( new ) then
            if ( item /= 0 ) then
               bnmidx( item ) = i
            else
               call ctx%setmsg ( 'Could not add name # to the hash.' )
               call ctx%errch  ( '#', nornam(i) )
               call ctx%sigerr ( 'SPICE(BUG3)' )
            end if
         end if

!
!        We may have a situation when a single normalized name maps to
!        more than one ID. In such cases we want to completely mask all
!        IDs with the lower priority. This is easy to do by simply not
!        attempting to register any more IDs if the name is already
!        registered.
!
         if ( new ) then

!
!           Register this ID, but only if it is not already in the hash.
!
            call zzhsiadd ( ctx, bidlst, bidpol, bidids, codes(i), item, new)
            if ( new ) then
               if ( item /= 0 ) then
                  bididx( item ) = i
               else
                  call ctx%setmsg ( 'Could not add ID # to the hash.' )
                  call ctx%errint ( '#', codes(i) )
                  call ctx%sigerr ( 'SPICE(BUG2)' )
                  return
               end if
            end if

         end if

      end do

      return
      end subroutine zzbodini

      !> PEPPER: fills KER (SPICELIB returned arrays of NROOM entries and
      !> their hash tables; see [[body_names]]).
      !>
      !> This routine processes the kernel pool vectors NAIF_BODY_NAME
      !> and NAIF_BODY_CODE into the lists and hashes required by ZZBODTRN
      !> to successfully compute code-name mappings.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzbodker ( ctx, ker )
      use pepper_kinds, only: ip
      use pepper_strings, only: ljucrs
      use pepper_inc_zzbodtrn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(body_names), intent(inout) :: ker  !! the names and codes from the kernel pool
      integer(ip), parameter :: keylen = 32
      character(keylen), parameter :: nbc = 'NAIF_BODY_CODE'
      character(keylen), parameter :: nbn = 'NAIF_BODY_NAME'
      character(1) :: type ( 2 )
      integer(ip) :: i
      integer(ip) :: iname
      integer(ip) :: nsiz ( 2 )
      integer(ip) :: num
      logical :: found
      logical :: plfind ( 2 )
!
!     PEPPER: the names are taken whole from the kernel pool (SPICELIB
!     read them with GCPOOL into MAXL-character strings), and there is
!     no limit on their number (SPICELIB: NROOM, SPICE(KERVARTOOBIG)).
!     The codes are read with GIPOOL, as by SPICELIB.
!
      if ( ctx%failed() ) return
      call ker%clear ( )
      iname     = ctx%spice%pool%find ( nbn )
      plfind(1) = .false.
      if ( iname > 0 ) plfind(1) = ctx%spice%pool%vars(iname)%kind == pool_character
      call dtpool ( ctx, nbc, found, nsiz(2), type(2) )
      plfind(2) = found .and. type(2) == 'N'
      if ( plfind(2) ) then
         allocate ( ker%codes(nsiz(2)) )
         call gipool ( ctx, nbc, 1, nsiz(2), num, ker%codes, found )
      end if
      if ( ctx%failed() ) return
      if ( plfind(1) .neqv. plfind(2) ) then
         call ctx%setmsg ( 'The kernel pool vector, #, used in ' &
         //            'mapping between names and ID-codes ' &
         //            'is absent, while # is not.  This is ' &
         //            'often due to an improperly constructed ' &
         //            'text kernel.  Check loaded kernels for ' &
         //            'these keywords.'                         )
         if ( plfind(1) ) then
            call ctx%errch ( '#', nbc )
            call ctx%errch ( '#', nbn )
         else
            call ctx%errch ( '#', nbn )
            call ctx%errch ( '#', nbc )
         end if
         call ctx%sigerr ( 'SPICE(MISSINGKPV)' )
         return
      else if ( .not. plfind(1) ) then
         return
      end if
      nsiz(1) = ctx%spice%pool%vars(iname)%n
      if ( nsiz(1) /= nsiz(2) ) then
         call ctx%setmsg ( 'The kernel pool vectors used for ' &
         //            'mapping between names and ID-codes ' &
         //            'are not the same size.  The size ' &
         //            'of the name vector, NAIF_BODY_NAME ' &
         //            'is #. The size of the ID-code ' &
         //            'vector, NAIF_BODY_CODE is #. You ' &
         //            'need to examine the ID-code kernel ' &
         //            'you loaded and correct the mismatch.' )
         call ctx%errint ( '#', nsiz(1)                           )
         call ctx%errint ( '#', nsiz(2)                           )
         call ctx%sigerr ( 'SPICE(BADDIMENSIONS)'                 )
         return
      end if
      ker%nvals = nsiz(1)
      ker%names = ctx%spice%pool%vars(iname)%cvals(1:ker%nvals)
      allocate ( ker%nornam(ker%nvals) )
      do i = 1, ker%nvals
         if ( ker%names(i)%s == ' ' ) then
            call ctx%setmsg ( 'An attempt to assign the code, #, to ' &
            //            'a blank string was made.  Check loaded ' &
            //            'text kernels for a blank string in ' &
            //            'the NAIF_BODY_NAME array.'               )
            call ctx%errint ( '#', i                                    )
            call ctx%sigerr ( 'SPICE(BLANKNAMEASSIGNED)'                )
            call ker%clear ( )
            return
         end if
         call normalize ( ker%names(i)%s, ker%nornam(i)%s )
      end do
      call ker%index ( )
      ker%extker = .true.
      return

      contains

!
!        A body name normalized as by LJUCRS (left-justified, uppercase,
!        single blanks), without trailing blanks.
!
!        (A subroutine, not a function with a deferred-length result:
!        gfortran keeps that length in a static variable at the call
!        site, which is not thread-safe.)
!
         pure subroutine normalize ( name, nor )
            character(*), intent(in) :: name  !! the name
            character(:), allocatable, intent(out) :: nor  !! the normalized name
            character(len(name)) :: buf
            call ljucrs ( 1, name, buf )
            nor = trim ( buf )
         end subroutine normalize
      end subroutine zzbodker

      !> Retrieve a copy of the built-in body name-code mapping lists.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     ROOM       is the maximum number of entries that NAMES, NORNAM,
      !>                and CODES may receive.
      !>
      !>## Detailed Output
      !>
      !>     NAMES      the array of built-in names.  This array is parallel
      !>                to NORNAM and CODES.
      !>
      !>     NORNAM     the array of normalized built-in body names.  This
      !>                array is computed from the NAMES array by compressing
      !>                groups of spaces into a single space, left-justifying
      !>                the name, and uppercasing the letters.
      !>
      !>     CODES      the array of built-in codes associated with NAMES
      !>                and NORNAM entries.
      !>
      !>     NVALS      the number of items returned in NAMES, NORNAM,
      !>                and CODES.
      !>
      !>## Parameters
      !>
      !>     NPERM      the number of permanent, or built-in, body name-code
      !>                mappings.
      !>
      !>## Exceptions
      !>
      !>     1) SPICE(BUG) is signaled if ROOM is less than NPERM, the
      !>        amount of space required to store the entire list of
      !>        body names and codes.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine simply copies it's local buffered version of the
      !>     built-in name-code mappings to the output arguments.
      !>
      !>## Examples
      !>
      !>     See ZZBODTRN for sample usage.
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
      !>-    SPICELIB Version 2.1.0, 17-MAR-2003 (EDW)
      !>
      !>        Added a call to ZZIDMAP to retrieve the default
      !>        mapping list. "zzbodtrn.inc" no longer
      !>        contains the default mapping list.
      !>
      !>-    SPICELIB Version 2.0.0, 23-AUG-2002 (FST)
      module subroutine zzbodget ( ctx, room, names, nornam, codes, nvals )
      use pepper_kinds, only: ip
      use pepper_strings, only: cmprss_inplace, ucase_inplace
      use pepper_inc_zzbodtrn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: room  !! Space available in NAMES, NORNAM, and CODES.
      character(*), intent(out) :: names  ( * )  !! Array of built-in body names.
      character(*), intent(out) :: nornam ( * )  !! Array of normalized built-in body names.
      integer(ip), intent(out) :: codes  ( * )  !! Array of built-in ID codes for NAMES/NORNAM.
      integer(ip), intent(out) :: nvals  !! Length of NAMES, NORNAM, CODES, and ORDNOM arrays.
      integer(ip) :: i
      if ( .not. allocated ( ctx%zzbodblt ) ) then
         allocate ( ctx%zzbodblt )
         call init_zzbodblt_state ( ctx%zzbodblt )
      end if
      associate ( bltnam => ctx%zzbodblt%bltnam, &
                  bltnor => ctx%zzbodblt%bltnor, &
                  bltcod => ctx%zzbodblt%bltcod, &
                  first => ctx%zzbodblt%first )
      if ( ctx%failed() ) return

!
!     On the first invocation compute the normalized forms of BLTNAM
!     and store them in BLTNOR.
!
      if ( first ) then

!
!        Retrieve the default mapping list.
!
         call zzidmap( ctx, bltcod, bltnam )

         do i = 1, nperm
            bltnor(i) = adjustl(bltnam(i))
            call ucase_inplace ( bltnor(i) )
            call cmprss_inplace ( ' ', 1, bltnor(i) )
         end do

!
!        Do not do this again.
!
         first = .false.

      end if

!
!     Copy the contents of BLTNAM, BLTNOR, and BLTCOD to the output
!     arguments, but only if there is sufficient room.
!
      if ( room < nperm ) then

         call ctx%setmsg ( 'Insufficient room to copy the stored ' &
         //            'body name-code mappings to the output ' &
         //            'arguments.  Space required is #, but ' &
         //            'the caller supplied #.'                 )
         call ctx%errint ( '#', nperm                               )
         call ctx%errint ( '#', room                                )
         call ctx%sigerr ( 'SPICE(BUG)'                             )
         return

      end if

      names(1:nperm) = bltnam
      nornam(1:nperm) = bltnor
      codes(1:nperm) = bltcod

      nvals = nperm

      return
      end associate
      end subroutine zzbodget

      !> Output the complete list of built-in body/ID mappings to
      !> some output device. Thw routine generates 2 lists: one
      !> sorted by ID number, one sorted by name.
      !>
      !>## Detailed Input
      !>
      !>     DEVICE         identifies the device to receive the
      !>                    body/ID mapping list. WRLINE performs the
      !>                    output function and so DEVICE may have
      !>                    the values 'SCREEN' (to generate a screen dump),
      !>                    'NULL' (do nothing), or a device name (a
      !>                    file, or any other name valid in a FORTRAN OPEN
      !>                    statement).
      !>
      !>     REQST          A case insensitive string indicating the data
      !>                    set to output. REQST may have the value 'ID',
      !>                    'NAME', or 'BOTH'. 'ID' outputs the name/ID mapping
      !>                    ordered by ID number from least to highest value.
      !>                    'NAME' outputs the name/ID mapping ordered by ASCII
      !>                    sort on the name string. 'BOTH' outputs both
      !>                    ordered lists.
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
      !>     The entry point outputs ordered lists of the name/ID mappings
      !>     defined in ZZBODTRN.
      !>
      !>## Examples
      !>
      !>     1. Write both sorted lists to screen.
      !>
      !>     PROGRAM X
      !>
      !>     CALL ZZBODLST( 'SCREEN', 'BOTH' )
      !>
      !>     END
      !>
      !>     2. Write an ID number sorted list to a file, "body.txt".
      !>
      !>     PROGRAM X
      !>
      !>     CALL ZZBODLST( 'body.txt', 'ID' )
      !>
      !>     END
      !>
      !> With SCREEN output of the form:
      !>
      !>   Total number of name/ID mappings: 414
      !>
      !>   ID to name mappings.
      !>   -550                                 | M96
      !>   -550                                 | MARS 96
      !>   -550                                 | MARS-96
      !>   -550                                 | MARS96
      !>   -254                                 | MER-2
      !>   -253                                 | MER-1
      !>
      !>     ..                                   ..
      !>
      !>   50000020                             | SHOEMAKER-LEVY 9-B
      !>   50000021                             | SHOEMAKER-LEVY 9-A
      !>   50000022                             | SHOEMAKER-LEVY 9-Q1
      !>   50000023                             | SHOEMAKER-LEVY 9-P2
      !>
      !>   Name to ID mappings.
      !>   1978P1                               | 901
      !>   1979J1                               | 515
      !>
      !>     ..                                   ..
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
      !>     F.S. Turner    (JPL)
      !>     E.D. Wright    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.1.2, 01-OCT-2021 (NJB)
      !>
      !>        Corrected typos in comments.
      !>
      !>-    SPICELIB Version 2.1.1, 27-FEB-2007 (EDW)
      !>
      !>        Completed the ZZBODLST declarations section.
      !>
      !>-    SPICELIB Version 2.1.0, 17-MAR-2003 (EDW)
      !>
      !>        Added a call to ZZIDMAP to retrieve the default
      !>        mapping list. "zzbodtrn.inc" no longer
      !>        contains the default mapping list.
      !>
      !>-    SPICELIB Version 2.0.0, 23-AUG-2002 (FST)
      !>
      !>        This entry point was moved into ZZBODBLT and some
      !>        variable names were changed to refer to variables
      !>        in the umbrella.
      !>
      !>-    SPICELIB Version 1.0.0, 26-NOV-2001 (EDW)
      module subroutine zzbodlst ( ctx, device, reqst )
      use pepper_kinds, only: ip
      use pepper_arrays, only: orderc, orderi
      use pepper_strings, only: eqstr, intstr, ucase
      use pepper_output, only: wrline
      use pepper_inc_zzbodtrn
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: device  !! Device name to receive the output.
      character(*), intent(in) :: reqst  !! Data list name to output.
      character(2*maxl+3) :: zzline
      character(4) :: zzrqst
      integer(ip) :: zzocod ( nperm )
      integer(ip) :: zzonam ( nperm )
      character(maxl) :: zzint
      integer(ip) :: i
      if ( .not. allocated ( ctx%zzbodblt ) ) then
         allocate ( ctx%zzbodblt )
         call init_zzbodblt_state ( ctx%zzbodblt )
      end if
      associate ( bltnam => ctx%zzbodblt%bltnam, &
                  bltcod => ctx%zzbodblt%bltcod )
      if ( ctx%failed() ) return

!
!     Upper case the ZZRQST value.
!
      call ucase  ( reqst, zzrqst )
      call intstr ( nperm, zzint  )

      zzline = 'Total number of name/ID mappings: '//zzint

      call wrline ( device, zzline(1: len_trim(zzline) ) )

!
!     Retrieve the current set of name/ID mappings
!
      call zzidmap( ctx, bltcod, bltnam )

!
!      Branch as defined by the value of ZZRQST. 'ID' or 'BOTH'.
!
      if ( eqstr( zzrqst , 'ID') .or. eqstr( zzrqst , 'BOTH') ) then

         call orderi ( bltcod, nperm, zzocod )

         call wrline ( device, ' ' )
         call wrline ( device, 'ID to name mappings.' )

         do i = 1, nperm

            call intstr ( bltcod(zzocod(i)) , zzint )

            zzline = zzint // ' | ' // bltnam(zzocod(i))

            call wrline ( device, zzline(1: len_trim(zzline) ) )

         end do

      end if

!
!     ... 'NAME' or 'BOTH'.
!
      if ( eqstr( zzrqst, 'NAME') .or. eqstr( zzrqst, 'BOTH') ) then

         call orderc ( bltnam, nperm, zzonam )

         call wrline ( device, ' ' )
         call wrline ( device, 'Name to ID mappings.' )

         do i = 1, nperm

            call intstr ( bltcod(zzonam(i)) , zzint )

            zzline = bltnam(zzonam(i)) // ' | ' // zzint

            call wrline ( device, zzline(1:len_trim(zzline) ) )

         end do

      end if

      return
      end associate
      end subroutine zzbodlst

      !> Translate a body name to the corresponding SPICE integer code.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     NAME       is an arbitrary name of a body which could be
      !>                a planet, satellite, barycenter, spacecraft,
      !>                asteroid, comet, or other ephemeris object.
      !>
      !>                Case and leading and trailing blanks in a name
      !>                are not significant.  However, when a name consists
      !>                of more than one word, they must be separated by
      !>                at least one blank, i.e., all of the following
      !>                strings are equivalent names:
      !>
      !>                   'JUPITER BARYCENTER'
      !>                   'Jupiter Barycenter'
      !>                   'JUPITER BARYCENTER   '
      !>                   'JUPITER    BARYCENTER'
      !>                   '   JUPITER BARYCENTER'
      !>
      !>                However, 'JUPITERBARYCENTER' is not equivalent to
      !>                the names above.
      !>
      !>                When ignoring trailing blanks, NAME must have fewer
      !>                than MAXL characters.
      !>
      !>## Detailed Output
      !>
      !>     CODE       is the NAIF or user defined integer code for the
      !>                named body.
      !>
      !>     FOUND      return as true if NAME has a translation.
      !>                Otherwise, FOUND returns as false.
      !>
      !>## Parameters
      !>
      !>     MAXL       is the maximum length of a body name.  Defined in
      !>                the include file 'zzbodtrn.inc'.
      !>
      !>## Exceptions
      !>
      !>     Errors may be signaled by routines in the call tree of this
      !>     routine.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     ZZBODN2C is one of three related entry points,
      !>
      !>        ZZBODN2C      Body name to code
      !>
      !>        ZZBODC2N      Body code to name
      !>
      !>        ZZBODDEF      Body name/code definition
      !>
      !>     ZZBODN2C and ZZBODC2N perform translations between body names
      !>     and their corresponding integer codes used in SPK and PCK files
      !>     and associated routines.  A default set of name/code
      !>     pairs are automatically defined during the first call to
      !>     any of the entry points.  Additional name/code pairs may
      !>     be defined via ZZBODDEF.
      !>
      !>## Examples
      !>
      !>     See the Examples section of the ZZBODTRN umbrella header.
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
      !>     J.E. McLean    (JPL)
      !>     B.V. Semenov   (JPL)
      !>     M.J. Spencer   (JPL)
      !>     W.L. Taber     (JPL)
      !>     F.S. Turner    (JPL)
      !>     E.D. Wright    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
      !>
      !>        Changed to use name-based hashes instead of the order arrays.
      !>
      !>        Changed to keep track of the POOL's and ZZBODTRN internal
      !>        states using state counters.
      !>
      !>        Changed to call ZZCVPOOL to bypass watcher look-ups if the
      !>        POOL counter did not change.
      !>
      !>-    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB)
      !>
      !>        Bug fix: this routine now keeps track of whether its
      !>        kernel pool look-up succeeded. If not, a kernel pool
      !>        lookup is attempted on the next call to any entry
      !>        point that calls ZZBODKER.
      !>
      !>-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
      !>
      !>        Cleaned up module header and source.  See the Revisions
      !>        section of ZZBODTRN for detailed update information.
      !>
      !>-    SPICELIB Version 3.1.0, 12-FEB-2001 (EDW)
      !>
      !>        Added logic to ensure the routine returns the NAME string
      !>        in the same format as when defined (case and space).
      !>        Added logic to handle error response in ZZBODINI.
      !>
      !>        To improve clarity, the BEGXX block initialization now
      !>        exists in the include file zzbodtrn.inc.
      !>
      !>        Removed the comments concerning the 851, 852, ... temporary
      !>        codes.
      !>
      !>        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME
      !>        as a DATA statement.
      !>
      !>        Edited headers, removed typos and bad grammar, clarified
      !>        descriptions.
      !>
      !>-    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT)
      !>
      !>        The ID codes for Cluster 1, 2, 3 and 4 were added.  The
      !>        ID coded for Pluto Express were removed.  The ID codes
      !>        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation
      !>        and Contour were added.
      !>
      !>-    SPICELIB Version 2.0.0, 21-JAN-1999 (EDW)
      !>
      !>        Added code to use the external name/ID kernel.
      !>
      !>-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
      !>
      !>        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
      !>
      !>-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
      !>
      !>        Renamed to ZZBODN2C (BVS)
      !>
      !>-    Beta Version 1.0.0, 29-APR-1994 (MJS)
      !>
      !>        SPICELIB symbol tables are no longer used. Instead, two order
      !>        vectors are used to index the NAMES and CODES arrays.
      !>
      !>-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
      !>
      !>       The body id's for the Uranian satellites discovered by Voyager
      !>       were modified to conform to those established by the IAU
      !>       nomenclature committee.  In addition the id's for Gaspra and
      !>       Ida were added.
      !>
      !>-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
      !>
      !>       Items previously considered errors were downgraded
      !>       to simply be exceptions.  Any NAME is a legitimate input now.
      !>       If its not in the table, the FOUND flag is just set to .FALSE.
      !>
      !>-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
      module subroutine zzbodn2c ( ctx, name, code, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: ljucrs
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Body name to be translated.
      integer(ip), intent(out) :: code  !! Integer code for that body.
      logical, intent(out) :: found  !! True if translated, otherwise false.
      character(:), allocatable :: tmpnam
      logical :: lupdte
!     PEPPER: apply the body names defined through other contexts.
      call sync_boddefs ( ctx )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
      if ( .not. allocated ( ctx%zzbodtrn ) ) then
         allocate ( ctx%zzbodtrn )
         call init_zzbodtrn_state ( ctx%zzbodtrn )
      end if
      associate ( def => ctx%zzbodtrn%def, &
                  ker => ctx%zzbodtrn%ker, &
                  subctr => ctx%zzbodtrn%subctr, &
                  pulctr => ctx%zzbodtrn%pulctr, &
                  nodata => ctx%zzbodtrn%nodata, &
                  i => ctx%zzbodtrn%i, &
                  first => ctx%zzbodtrn%first )
      if ( ctx%failed() ) return

!
!     Assume we will not find the code we seek.
!
      found = .false.

!
!     On the first pass through this entry point, initialize the
!     built-in arrays, set the kernel pool watchers, and state
!     counters.
!
      if ( first ) then

!
!        Initialize counters. Set ZZBODTRN state counter, for
!        which this umbrella is the owner, to subsystem values. Set
!        POOL counter, for which this umbrella is the user, to user
!        values.
!
         call zzctrsin ( ctx, subctr )
         call zzctruin ( ctx, pulctr )

!
!        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD
!        arrays from the built-in code-name list.
!
         call body_builtins ( ctx, def )

         if ( ctx%failed() ) return

!
!        Populate the initial built-in code-name hashes.
!

         if ( ctx%failed() ) return

!
!        Set up the watchers for the kernel pool name-code mapping
!        variables.
!
!        PEPPER: the watch is the context's own (SWPOOL keeps the watches
!        of each context; phase 3 had checked the pool's state counter
!        instead, and rebuilt the tables after any change of the pool).
!
         call swpool ( ctx, 'ZZBODTRN', 2, &
                       [character(14) :: 'NAIF_BODY_NAME', 'NAIF_BODY_CODE'] )

!
!        Set FIRST to .FALSE. to not repeat initialization again.
!
         first = .false.

      end if

!
!     Check for updates to the kernel pool variables. Note: the first
!     call to ZZCVPOOL after initialization always returns .TRUE. for
!     LUPDTE.  This ensures that any initial assignments are properly
!     processed.
!
      call zzcvpool ( ctx, 'ZZBODTRN', pulctr, lupdte )

      if ( lupdte .or. nodata ) then

!
!        Conservatively increment the ZZBODTRN state counter
!        in expectation of successful update.
!
         call zzctrinc ( ctx, subctr )

!
!        Update kernel pool mapping lists and hashes.
!
         call zzbodker ( ctx, ker )

         if ( ctx%failed() ) then

            nodata = .true.

            return

         end if

         nodata = .false.

      end if

!
!     Normalize the input argument NAME. We will look this normalized
!     name up in the built-in and kernel pool names hashes.
!
      block
         character(len(name)) :: buf
         call ljucrs ( 1, name, buf )
         tmpnam = trim ( buf )
      end block

!
!     If necessary, first examine the contents of the kernel pool
!     name-code mapping list.
!
      if ( ker%extker ) then

!
!        Check if this name is in the kernel pool names hash.
!
         i = ker%name_index ( tmpnam )

         if ( i /= 0 ) then
            code  = ker%codes( i )
            found = .true.
            return
         end if

      end if

!
!     If we reach here, we did not find this name in the kernel pool
!     names hash. Check the built-in names hash.
!
      i = def%name_index ( tmpnam )

      if ( i /= 0 ) then

         code  = def%codes( i )
         found = .true.

      end if

      return
      end associate
      end subroutine zzbodn2c

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzbodc2n ( ctx, code, name, found )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(out) :: name  !! Common name for the body identified by CODE.
      integer(ip), intent(in) :: code  !! Integer code to be translated.
      logical, intent(out) :: found  !! True if translated, otherwise false.
      character(:), allocatable :: full
      call body_name_of ( ctx, code, full, found )
      if ( found ) name = full
      end subroutine zzbodc2n

!> The name of a body from its ID code, of any length (PEPPER; the
!> body of [[zzbodc2n]], which truncates to its argument's length).
!>
!> Translate the integer code of a body into a common name for
!> that body.
!>
!> SPICE Private routine intended solely for the support of SPICE
!> routines.  Users should not call this routine directly due
!> to the volatile nature of this routine.
!>
!>## Detailed Input
!>
!>     CODE       is an integer code for a body ---
!>                a planet, satellite, barycenter, spacecraft,
!>                asteroid, comet, or other ephemeris object.
!>
!>## Detailed Output
!>
!>     NAME       is the common name of the body identified by CODE.
!>                If CODE has more than one translation, then the
!>                most recently defined NAME corresponding to CODE
!>                is returned.  The routine returns NAME in the exact
!>                format (case and blanks) as used when defining
!>                the name/code pair.
!>
!>     FOUND      returns as true if NAME has a translation.
!>                Otherwise, FOUND returns as false.
!>
!>## Parameters
!>
!>     MAXL       is the maximum length of a body name.  Defined in
!>                the include file 'zzbodtrn.inc'.
!>
!>## Exceptions
!>
!>     Errors may be signaled by routines in the call tree of this
!>     routine.
!>
!>## Files
!>
!>     None.
!>
!>## Particulars
!>
!>     ZZBODC2N is one of three related entry points,
!>
!>        ZZBODN2C      Body name to code
!>
!>        ZZBODC2N      Body code to name
!>
!>        ZZBODDEF      Body name/code definition
!>
!>     ZZBODN2C and ZZBODC2N perform translations between body names
!>     and their corresponding integer codes used in SPK and PCK files
!>     and associated routines.  A default set of name/code
!>     pairs are automatically defined during the first call to
!>     any of the entry points.  Additional name/code pairs may
!>     be defined via ZZBODDEF.
!>
!>     For the case in which multiple names map to a single code, a
!>     ZZBODC2N call returns the name last assigned to that code - a
!>     LIFO situation.
!>
!>## Examples
!>
!>     See Examples section of ZZBODTRN umbrella header.
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
!>     J.E. McLean    (JPL)
!>     B.V. Semenov   (JPL)
!>     M.J. Spencer   (JPL)
!>     W.L. Taber     (JPL)
!>     F.S. Turner    (JPL)
!>     E.D. Wright    (JPL)
!>
!>## Version
!>
!>-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
!>
!>        Changed to use name and ID-based hashes instead of the order
!>        arrays.
!>
!>        Changed to keep track of the POOL's and ZZBODTRN internal
!>        state using state counters.
!>
!>        Changed to call ZZCVPOOL to bypass watcher look-ups if the
!>        POOL counter did not change.
!>
!>-    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB)
!>
!>        Bug fix: this routine now keeps track of whether its
!>        kernel pool look-up succeeded. If not, a kernel pool
!>        lookup is attempted on the next call to any entry
!>        point that calls ZZBODKER.
!>
!>-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
!>
!>        Cleaned up module header and source code.  See the Revisions
!>        section of ZZBODTRN for detailed update information.
!>
!>-    SPICELIB Version 3.2.0, 19-JUL-2002 (EDW)
!>
!>        Added logic to enforce the precedence masking.
!>
!>-    SPICELIB Version 3.1.0, 5-SEP-2001 (EDW)
!>
!>        Added logic to ensure the routine returns the NAME string
!>        in the same format as when defined (case and space).
!>        Added logic to handle error response in ZZBODINI.
!>
!>        To improve clarity, the BEGXX block initialization now
!>        exists in the include file zzbodtrn.inc.
!>
!>        Removed the comments concerning the 851, 852, ... temporary
!>        codes.
!>
!>        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME
!>        as a DATA statement.
!>
!>        Edited headers, removed typos and bad grammar, clarified
!>        descriptions.
!>
!>-    SPICELIB Version 3.0.0, 29-MAR-2000 (WLT)
!>
!>        The ID codes for Cluster 1, 2, 3 and 4 were added.  The
!>        ID coded for Pluto Express were removed.  The ID codes
!>        for Pluto-Kuiper Express, Pluto-Kuiper Express Simulation
!>        and Contour were added.
!>
!>-    SPICELIB Version 2.0.0, 21-JAN-1999 (EDW)
!>
!>        Added code to use the external name/ID kernel.
!>
!>-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
!>
!>        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
!>
!>-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
!>
!>        Renamed to ZZBODC2N (BVS)
!>
!>-    Beta Version 1.0.0, 29-APR-1994 (MJS)
!>
!>        SPICELIB symbol tables are no longer used. Instead, two order
!>        vectors are used to index the NAMES and CODES arrays.
!>
!>-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
!>
!>        Comment section for permuted index source lines was added
!>        following the header.
!>
!>-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
!>
!>       The body id's for the Uranian satellites discovered by Voyager
!>       were modified to conform to those established by the IAU
!>       nomenclature committee.  In addition the id's for Gaspra and
!>       Ida were added.
!>
!>-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
!>
!>       Checks to see the input integer code can be represented
!>       as a character string were removed along with the exceptions
!>       associated with these checks.  It is now the responsibility
!>       of a maintenance programmer to make sure MAXL is large
!>       enough to allow any integer to be converted to a string
!>       representation.
!>
!>-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
      module subroutine body_name_of ( ctx, code, name, found )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: code  !! the body ID code
      character(:), allocatable, intent(out) :: name  !! the body name (empty if not found)
      logical, intent(out) :: found  !! the code has a name
      logical :: lupdte
      integer(ip) :: j
!     PEPPER: apply the body names defined through other contexts.
      name = ''
      found = .false.
      call sync_boddefs ( ctx )
      if ( ctx%failed() ) return
      if ( .not. allocated ( ctx%zzbodtrn ) ) then
         allocate ( ctx%zzbodtrn )
         call init_zzbodtrn_state ( ctx%zzbodtrn )
      end if
      associate ( def => ctx%zzbodtrn%def, &
                  ker => ctx%zzbodtrn%ker, &
                  subctr => ctx%zzbodtrn%subctr, &
                  pulctr => ctx%zzbodtrn%pulctr, &
                  nodata => ctx%zzbodtrn%nodata, &
                  i => ctx%zzbodtrn%i, &
                  first => ctx%zzbodtrn%first )
      if ( ctx%failed() ) return

!
!     Assume we will not find the name we seek.
!
      found = .false.

!
!     On the first pass through this entry point, initialize the
!     built-in arrays, set the kernel pool watchers, and state
!     counters.
!
      if ( first ) then

!
!        Initialize counters. Set ZZBODTRN state counter, for
!        which this umbrella is the owner, to subsystem values. Set
!        POOL counter, for which this umbrella is the user, to user
!        values.
!
         call zzctrsin ( ctx, subctr )
         call zzctruin ( ctx, pulctr )

!
!        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD
!        arrays from the built-in code list.
!
         call body_builtins ( ctx, def )

         if ( ctx%failed() ) return

!
!        Populate the initial built-in code-name hashes.
!

         if ( ctx%failed() ) return

!
!        Set up the watchers for the kernel pool name-code mapping
!        variables.
!
!        PEPPER: the watch is the context's own (SWPOOL keeps the watches
!        of each context; phase 3 had checked the pool's state counter
!        instead, and rebuilt the tables after any change of the pool).
!
         call swpool ( ctx, 'ZZBODTRN', 2, &
                       [character(14) :: 'NAIF_BODY_NAME', 'NAIF_BODY_CODE'] )

!
!        Set FIRST to .FALSE. to not repeat initialization again.
!
         first = .false.

      end if

!
!     Check for updates to the kernel pool variables. Note: the first
!     call to ZZCVPOOL after initialization always returns .TRUE. for
!     LUPDTE. This ensures that any initial assignments are properly
!     processed.
!
      call zzcvpool ( ctx, 'ZZBODTRN', pulctr, lupdte )

      if ( lupdte .or. nodata ) then

!
!        Conservatively increment the ZZBODTRN state counter
!        in expectation of successful update.
!
         call zzctrinc ( ctx, subctr )

!
!        Update kernel pool mapping lists and hashes.
!
         call zzbodker ( ctx, ker )

         if ( ctx%failed() ) then

            nodata = .true.

            return

         end if

         nodata = .false.

      end if

!
!     If necessary, first examine the contents of the kernel pool
!     name-code mapping list.
!
      if ( ker%extker ) then

!
!        Check if this code is in the kernel pool codes hash.
!
         i = ker%code_index ( code )

         if ( i /= 0 ) then
            name  = ker%names(i)%s
            found = .true.
            return
         end if

      end if

!
!     If we reach here, we did not find this code in the kernel pool
!     codes hash. Check the built-in codes hash.
!
      i = def%code_index ( code )

!
!     If we find a match, verify that it is not masked by a kernel pool
!     entry before returning.
!
      if ( i /= 0 ) then

         if ( ker%extker ) then

!
!           Only bother performing this check if there are actually
!           mappings present in the kernel pool lists.
!
            j = ker%name_index ( def%nornam(i)%s )

            if ( j /= 0 ) then

!
!              This name is defined in the kernel pool mappings. Set
!              FOUND to .FALSE., as the contents of the kernel pool
!              have higher precedence than any entries in the built-in
!              mapping list.
!
               found = .false.

            else

!
!              No match for this name in the kernel pool mapping list.
!              Return the name.
!
               name  = def%names(i)%s
               found = .true.

            end if

         else

!
!           No kernel pool mappings were defined, simply return
!           return the name.
!
            name  = def%names(i)%s
            found = .true.

         end if

      end if

      return
      end associate
      end subroutine body_name_of

      !> Add a name/code pair to this context's table of defined names (the
      !> code of SPICELIB's [[zzboddef]]; zzboddef also records it in the instance).
      !>
      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     Define a body name/code pair for later translation by
      !>     ZZBODN2C or ZZBODC2N.
      !>
      !>## Detailed Input
      !>
      !>     NAME       is an arbitrary name of a body which could be
      !>                a planet, satellite, barycenter, spacecraft,
      !>                asteroid, comet, or other ephemeris object.
      !>
      !>                The case and positions of blanks in a name
      !>                are significant. ZZBODC2N returns the exact
      !>                string (case and space) last mapped to a code.
      !>                When a name is made up of more than one word,
      !>                the words require separation by at least one blank,
      !>                i.e., all of the following strings belong to
      !>                the same equivalence class:
      !>
      !>                   'JUPITER BARYCENTER'
      !>                   'Jupiter Barycenter'
      !>                   'JUPITER BARYCENTER   '
      !>                   'JUPITER    BARYCENTER'
      !>                   '   JUPITER BARYCENTER'
      !>
      !>                However, 'JUPITERBARYCENTER' is not equivalent to
      !>                the names above.
      !>
      !>                When ignoring trailing blanks, NAME must have fewer
      !>                than MAXL characters.
      !>
      !>     CODE       is the integer code for the named body.
      !>
      !>                CODE may already have a name as defined by a
      !>                previous call to ZZBODDEF or as part of the set of
      !>                default definitions.  That previous definition
      !>                remains and a translation of that name still
      !>                returns the same CODE.  However, future translations
      !>                of CODE will give the new NAME instead of the
      !>                previous one.  This feature is useful for assigning
      !>                a more familiar or abbreviated name to a body.
      !>                For example, in addition to the default name for
      !>                body 5, 'JUPITER BARYCENTER', you could define the
      !>                abbreviation 'JB' to mean 5.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     The following relevant parameters are defined in zzbodtrn.inc:
      !>
      !>     MAXL        is the maximum length of a body name.
      !>
      !>     MAXP        is the maximum number of additional names that may
      !>                 be added via the ZZBODDEF interface.
      !>
      !>## Exceptions
      !>
      !>     1) If the maximum number of definitions is exceeded, a the
      !>        error SPICE(TOOMANYPAIRS) is signaled.
      !>
      !>     2) If an attempt to assign a blank string an ID code is made,
      !>        the error SPICE(BLANKNAMEASSIGNED) is signaled.
      !>
      !>     3) Routines in the call tree of this routine may signal
      !>        errors.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     ZZBODDEF is one of three related entry points,
      !>
      !>        ZZBODN2C      Body name to code
      !>
      !>        ZZBODC2N      Body code to name
      !>
      !>        ZZBODDEF      Body name/code definition
      !>
      !>     ZZBODN2C and ZZBODC2N perform translations between body names
      !>     and their corresponding integer codes used in SPK and PCK files
      !>     and associated routines.  A default set of name/code
      !>     pairs are automatically defined during the first call to
      !>     any of the entry points.  Additional name/code pairs may
      !>     be defined via ZZBODDEF for two purposes:
      !>
      !>        1.  to associate another, perhaps more familiar or
      !>            abbreviated name with a previously defined body integer
      !>            code or
      !>
      !>        2.  to define a new body integer code and name,
      !>
      !>     Each body has a unique integer code, but may have several
      !>     names.  Thus you may associate more than one name with
      !>     a particular integer code.  However, associating more
      !>     than one integer code with a particular name creates ambiguity.
      !>     Therefore, once a name has been defined, it may not be redefined
      !>     with a different integer code.
      !>
      !>     For example, Europa is the name of the second satellite of
      !>     Jupiter, and has the NAIF integer code 502.  Thus (EUROPA, 502)
      !>     is one of the default definitions.  Europa is also the name
      !>     of an asteroid.  Suppose you were able to associate the asteroid
      !>     integer code with the name EUROPA.  Then when you call ZZBODN2C to
      !>     translate the name EUROPA, which code should be returned?  That
      !>     of the asteroid or 502?
      !>
      !>     ZZBODDEF prevents this ambiguity by signaling an error
      !>     if the specified name has already been defined with a
      !>     different code.  In the case of EUROPA, you may want to use the
      !>     name ASTEROID EUROPA.  The set of default definitions are listed
      !>     in DATA statements in the umbrella routine ZZBODTRN for easy
      !>     reference.
      !>
      !>## Examples
      !>
      !>     See the Examples section of the ZZBODTRN umbrella header.
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
      !>     J.E. McLean    (JPL)
      !>     B.V. Semenov   (JPL)
      !>     W.L. Taber     (JPL)
      !>     F.S. Turner    (JPL)
      !>     E.D. Wright    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
      !>
      !>        Changed to use name and ID-based hashes instead of the order
      !>        arrays.
      !>
      !>        Changed to keep track of the POOL's and ZZBODTRN internal
      !>        state using state counters.
      !>
      !>        Changed to call ZZCVPOOL to bypass watcher look-ups if the
      !>        POOL counter did not change.
      !>
      !>-    SPICELIB Version 4.0.1, 17-APR-2003 (EDW)
      !>
      !>        Correct typo in header docs.
      !>
      !>     SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
      !>
      !>        Cleaned up module header and source code.  See the Revisions
      !>        section of ZZBODTRN for detailed update information.
      !>
      !>        Added the error SPICE(BLANKNAMEASSIGNED), when the caller
      !>        attempts to assign an ID code to a blank string.
      !>
      !>-    SPICELIB Version 1.3.0, 14-AUG-2002 (EDW)
      !>
      !>        Added logic to enforce the precedence masking;
      !>        logic removes duplicate assignments of ZZBODDEF.
      !>        Removed the NAMENOTUNIQUE error block.
      !>
      !>-    SPICELIB Version 1.2.0, 5-SEP-2001 (EDW)
      !>
      !>        Added logic to ensure the routine returns the NAME string
      !>        in the same format as when defined (case and space).
      !>        Added logic to handle error response from ZZBODINI.
      !>
      !>        To improve clarity, the BEGXX block initialization now
      !>        exists in the include file zzbodtrn.inc.
      !>
      !>        Removed the comments concerning the 851, 852, ... temporary
      !>        codes.
      !>
      !>        Set the WNAMES assignment to NAIF_BODY_CODE, NAIF_BODY_NAME
      !>        as a DATA statement.
      !>
      !>        Edited headers, removed typos and bad grammar, clarified
      !>        descriptions.
      !>
      !>-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
      !>
      !>        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
      !>
      !>-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
      !>
      !>        Renamed to ZZBODDEF (BVS). More careful checking for overflow
      !>        of the recognized names is now performed.
      !>
      !>-    Beta Version 1.0.0, 29-APR-1994 (MJS)
      !>
      !>        SPICELIB symbol tables are no longer used. Instead, two order
      !>        vectors are used to index the NAMES and CODES arrays.
      !>
      !>-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
      !>
      !>       The body id's for the Uranian satellites discovered by Voyager
      !>       were modified to conform to those established by the IAU
      !>       nomenclature committee.  In addition the id's for Gaspra and
      !>       Ida were added.
      !>
      !>-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
      !>
      !>       Checks to see an integer code can be represented
      !>       as a character string were removed along with the exceptions
      !>       associated with these checks.  It is now the responsibility
      !>       of a maintenance programmer to make sure MAXL is large
      !>       enough to allow any integer to be converted to a string
      !>       representation.
      !>
      !>-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
      subroutine zzboddef_apply ( ctx, name, code )
      use pepper_kinds, only: ip
      use pepper_strings, only: ljucrs
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: name  !! common name of some body
         integer(ip), intent(in) :: code  !! integer code for that body
      integer(ip) :: index
      character(:), allocatable :: tmpnam
      integer(ip) :: codidx
      logical :: removed
      if ( .not. allocated ( ctx%zzbodtrn ) ) then
         allocate ( ctx%zzbodtrn )
         call init_zzbodtrn_state ( ctx%zzbodtrn )
      end if
      associate ( def => ctx%zzbodtrn%def, &
                  subctr => ctx%zzbodtrn%subctr, &
                  pulctr => ctx%zzbodtrn%pulctr, &
                  bodchg => ctx%zzbodtrn%bodchg, &
                  i => ctx%zzbodtrn%i, &
                  first => ctx%zzbodtrn%first )
      if ( ctx%failed() ) return

!
!     On the first pass through this entry point, initialize the
!     built-in arrays, set the kernel pool watchers, and state
!     counters.
!
      if ( first ) then

!
!        Initialize counters. Set ZZBODTRN state counter, for
!        which this umbrella is the owner, to subsystem values. Set
!        POOL counter, for which this umbrella is the user, to user
!        values.
!
         call zzctrsin ( ctx, subctr )
         call zzctruin ( ctx, pulctr )

!
!        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD
!        arrays from the built-in code list.
!
         call body_builtins ( ctx, def )

         if ( ctx%failed() ) return

!
!        Populate the initial built-in code-name hashes.
!

         if ( ctx%failed() ) return

!
!        Set up the watchers for the kernel pool name-code mapping
!        variables.
!
!        PEPPER: the watch is the context's own (SWPOOL keeps the watches
!        of each context; phase 3 had checked the pool's state counter
!        instead, and rebuilt the tables after any change of the pool).
!
         call swpool ( ctx, 'ZZBODTRN', 2, &
                       [character(14) :: 'NAIF_BODY_NAME', 'NAIF_BODY_CODE'] )

!
!        Set FIRST to .FALSE. to not repeat initialization again.
!
         first = .false.

      end if

!
!     Begin by verifying that the user is not attempting to assign a
!     blank string a code.
!
      if ( name == ' ' ) then

         call ctx%setmsg ( 'An attempt to assign the code, #, to ' &
         //            'a blank string was made.  Check loaded ' &
         //            'text kernels for a blank string in ' &
         //            'the NAIF_BODY_NAME array.'               )
         call ctx%errint ( '#', i                                    )
         call ctx%sigerr ( 'SPICE(BLANKNAMEASSIGNED)'                )
         return

      end if

!
!     Conservatively increment the ZZBODTRN state counter in
!     expectation of successful addition.
!
      call zzctrinc ( ctx, subctr )

!
!     Get normalized form of the input NAME.
!
      block
         character(len(name)) :: buf
         call ljucrs ( 1, name, buf )
         tmpnam = trim ( buf )
      end block
!
!     Determine if we are going to replace an entry currently present
!     in the definitions.
!
      index = def%name_index ( tmpnam )
      removed = index /= 0
      if ( index /= 0 ) then
!
!        We are going to replace an existing entry.  There are
!        two possible ways in which a replace operation can
!        happen:
!
!           1) The caller is attempting to replace the highest
!              precedent name-code mapping for a particular ID code.
!              When this happens, we need only change the name at
!              position INDEX. The user is simply changing the name
!              that maps to the same normalized name.
!
!           2) The caller is attempting to change the code
!              associated with a name, bump a lower precedence
!              name-code mapping to highest precedence, or some
!              combination of the two.
!
!        See if we should handle 1) first.
!
         codidx = def%code_index ( code )
         if ( codidx == index ) then
!
!           We altered the built-in body list. Set BODCHG to .TRUE.
!
            bodchg = .true.
            def%names(index)%s = trim ( name )
            return
         end if
!
!        At this point we have to replace the mapping at INDEX: remove
!        it (the maps are rebuilt below).
!
         call def%remove ( index )
      end if
!
!     PEPPER: the definitions grow (SPICELIB signaled SPICE(TOOMANYPAIRS)
!     when it had MAXE, 853, of them).
!
!     We are changing the body list, inform ZZBODRST by setting BODCHG
!     to .TRUE.
!
      bodchg = .true.
!
!     Now, we need to add the new entry on to the end of the list, and
!     rebuild the maps (SPICELIB's ZZBODINI).
!
!     PEPPER: a new name is added to the maps (SPICELIB rebuilt them on
!     every definition: O(n**2) for n definitions); they are rebuilt only
!     when an assignment was removed (the indices after it changed).
!
      call def%append ( trim ( name ), tmpnam, code )
      if ( removed ) then
         call def%index ( )
      else
         call def%index_last ( )
      end if
      return
      end associate
      end subroutine zzboddef_apply

      !> This routine executes the kernel pool read instructions
      !> if necessary.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     NONE.
      !>
      !>## Detailed Output
      !>
      !>     NONE.
      !>
      !>## Parameters
      !>
      !>     NONE.
      !>
      !>## Exceptions
      !>
      !>     NONE.
      !>
      !>## Files
      !>
      !>     NONE.
      !>
      !>## Particulars
      !>
      !>     This entry point provides a mechanism to allow a caller
      !>     to force the examination of the kernel pool variables that
      !>     define name-code mappings.  This is useful, if once a new
      !>     mapping is defined, diagnostics at the time of definition
      !>     are useful.  The way the system performs otherwise, the
      !>     diagnostics are not provided until a name-code conversion
      !>     is attempted.
      !>
      !>## Examples
      !>
      !>     See ZZLDKER for sample usage.
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
      !>     B.V. Semenov   (JPL)
      !>     F.S. Turner    (JPL)
      !>     E.D. Wright    (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
      !>
      !>        Changed to use name and ID-based hashes instead of the order
      !>        arrays.
      !>
      !>        Changed to keep track of the POOL's and ZZBODTRN internal
      !>        state using state counters.
      !>
      !>        Changed to call ZZCVPOOL to bypass watcher look-ups if the
      !>        POOL counter did not change.
      !>
      !>-    SPICELIB Version 4.1.0, 05-MAR-2009 (NJB)
      !>
      !>        Bug fix: this routine now keeps track of whether its
      !>        kernel pool look-up succeeded. If not, a kernel pool
      !>        lookup is attempted on the next call to any entry
      !>        point that calls ZZBODKER.
      !>
      !>-    SPICELIB Version 4.0.2, 19-SEP-2006 (EDW)
      !>
      !>        Added text to previously empty Declarations section.
      !>
      !>-    SPICELIB Version 4.0.0, 23-AUG-2002 (FST)
      !>
      !>        Added checks to watchers and the initialization
      !>        block.
      !>
      !>-    SPICELIB Version 1.0.0, 16-JUN-2002 (EDW)
      module subroutine zzbodkik ( ctx )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      logical :: lupdte
      if ( .not. allocated ( ctx%zzbodtrn ) ) then
         allocate ( ctx%zzbodtrn )
         call init_zzbodtrn_state ( ctx%zzbodtrn )
      end if
      associate ( def => ctx%zzbodtrn%def, &
                  ker => ctx%zzbodtrn%ker, &
                  subctr => ctx%zzbodtrn%subctr, &
                  pulctr => ctx%zzbodtrn%pulctr, &
                  nodata => ctx%zzbodtrn%nodata, &
                  first => ctx%zzbodtrn%first )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     On the first pass through this entry point, initialize the
!     built-in arrays, set the kernel pool watchers, and state
!     counters.
!
      if ( first ) then

!
!        Initialize counters. Set ZZBODTRN state counter, for
!        which this umbrella is the owner, to subsystem values. Set
!        POOL counter, for which this umbrella is the user, to user
!        values.
!
         call zzctrsin ( ctx, subctr )
         call zzctruin ( ctx, pulctr )

!
!        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD
!        arrays from the built-in code list.
!
         call body_builtins ( ctx, def )

         if ( ctx%failed() ) return

!
!        Populate the initial built-in/BODDEF hashes.
!

         if ( ctx%failed() ) return

!
!        Set up the watchers for the kernel pool name-code mapping
!        variables.
!
!        PEPPER: the watch is the context's own (SWPOOL keeps the watches
!        of each context; phase 3 had checked the pool's state counter
!        instead, and rebuilt the tables after any change of the pool).
!
         call swpool ( ctx, 'ZZBODTRN', 2, &
                       [character(14) :: 'NAIF_BODY_NAME', 'NAIF_BODY_CODE'] )

!
!        Set FIRST to .FALSE. to not repeat initialization again.
!
         first = .false.

      end if

!
!     Check for updates to the kernel pool variables. Note: the first
!     call to ZZCVPOOL after initialization always returns .TRUE. for
!     LUPDTE. This ensures that any initial assignments are properly
!     processed.
!
      call zzcvpool ( ctx, 'ZZBODTRN', pulctr, lupdte )

      if ( lupdte .or. nodata ) then

!
!        Conservatively increment the ZZBODTRN state counter
!        in expectation of successful update.
!
         call zzctrinc ( ctx, subctr )

!
!        Update kernel pool mapping lists and hashes.
!
         call zzbodker ( ctx, ker )

         if ( ctx%failed() ) then

            nodata = .true.

            return

         end if

         nodata = .false.

      end if

      return
      end associate
      end subroutine zzbodkik

      !> Reset this context's table of defined names to the built-in names
      !> (the code of SPICELIB's [[zzbodrst]]).
      !>
      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     This routine resets the built-in body list, removing any
      !>     assignments or alterations made by the ZZBODDEF entry point.
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
      !>     1) Routines in the call tree of this routine may signal errors.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     ZZBODRST resets the built-in body name-code mapping list.  This
      !>     list may only be modified by ZZBODDEF.  Further, any assignments
      !>     made through the kernel pool mechanism remain unaltered as a
      !>     result of invoking this routine.
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
      !>     B.V. Semenov    (JPL)
      !>     F.S. Turner     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
      !>
      !>        Changed to use name and ID-based hashes instead of the order
      !>        arrays.
      !>
      !>        Changed to keep track of the POOL's and ZZBODTRN internal
      !>        state using state counters.
      !>
      !>-    SPICELIB Version 4.0.0, 26-AUG-2002 (FST)
      subroutine zzbodrst_apply ( ctx )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
         class(pepper_context), intent(inout) :: ctx  !! the context
      if ( .not. allocated ( ctx%zzbodtrn ) ) then
         allocate ( ctx%zzbodtrn )
         call init_zzbodtrn_state ( ctx%zzbodtrn )
      end if
      associate ( def => ctx%zzbodtrn%def, &
                  subctr => ctx%zzbodtrn%subctr, &
                  pulctr => ctx%zzbodtrn%pulctr, &
                  bodchg => ctx%zzbodtrn%bodchg, &
                  first => ctx%zzbodtrn%first )
      if ( ctx%failed() ) return

!
!     On the first pass through this entry point, initialize the
!     built-in arrays, set the kernel pool watchers, and state
!     counters.
!
      if ( first ) then

!
!        Initialize counters. Set ZZBODTRN state counter, for
!        which this umbrella is the owner, to subsystem values. Set
!        POOL counter, for which this umbrella is the user, to user
!        values.
!
         call zzctrsin ( ctx, subctr )
         call zzctruin ( ctx, pulctr )

!
!        Populate the initial values of the DEFNAM, DEFNOR, and DEFCOD
!        arrays from the built-in code list.
!
         call body_builtins ( ctx, def )

         if ( ctx%failed() ) return

!
!        Populate the initial built-in code-name hashes.
!

         if ( ctx%failed() ) return

!
!        Set up the watchers for the kernel pool name-code mapping
!        variables.
!
!        PEPPER: the watch is the context's own (SWPOOL keeps the watches
!        of each context; phase 3 had checked the pool's state counter
!        instead, and rebuilt the tables after any change of the pool).
!
         call swpool ( ctx, 'ZZBODTRN', 2, &
                       [character(14) :: 'NAIF_BODY_NAME', 'NAIF_BODY_CODE'] )

!
!        Set FIRST to .FALSE. to not repeat initialization again.
!
         first = .false.

      end if

!
!     See if the body list needs to be reset.
!
      if ( bodchg ) then

         bodchg = .false.

!
!        Conservatively increment the ZZBODTRN state counter
!        in expectation of successful update.
!
         call zzctrinc ( ctx, subctr )

!
!        Fetch the initial body name-code mapping list. Note: we need
!        not check FAILED() here, because if an error had occurred due
!        to the improper specification of MAXE it would have been
!        signaled already to the user.
!
         call body_builtins ( ctx, def )

!
!        Reset the built-in/BODDEF hashes.
!

      end if

      return
      end associate
      end subroutine zzbodrst_apply

      !> Check and update the ZZBODTRN state counter tracked by a caller
      !> (user) routine.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     USRCTR      is the value of the ZZBODTRN state counter tracked by
      !>                 (saved in) the caller (user) routine.
      !>
      !>## Detailed Output
      !>
      !>     USRCTR      is the current ZZBODTRN state counter.
      !>
      !>     UPDATE      is the logical flag indicating whether the input
      !>                 ZZBODTRN state counter was different from the current
      !>                 ZZBODTRN state counter and, therefore, had to be
      !>                 updated (UPDATE = .TRUE.) or if it was the same
      !>                 (UPDATE = .FALSE.).
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
      !>     1) Routines in the call tree of this routine may signal errors.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is not part of the SPICELIB API. This routine may be
      !>     removed in a later version of the SPICE Toolkit, or its interface
      !>     may change.
      !>
      !>     SPICE-based application code should not call this routine.
      !>
      !>     This routine allows other routines to be aware of ZZBODTRN state
      !>     change due to addition or deletion body name-code mappings via
      !>     ZZBODTRN calls and/or kernel POOL NAIF_BODY_NAME/NAIF_BODY_CODE
      !>     variables. Such awareness is needed to be able to locally save
      !>     some ZZBODTRN-based data (e.g. a particular body name/ID mapping)
      !>     and only update these locally saved values if the ZZBODTRN state
      !>     has changed.
      !>
      !>     To make use of the ZZBODTRN state counter to achieve this goal the
      !>     caller routines save the ZZBODTRN state counter returned by the
      !>     first call to this routine and then check that saved value
      !>     against the current ZZBODTRN state counter and update it by
      !>     subsequent calls this routine.
      !>
      !>     This routine checks if the watched POOL name-ID mapping keywords
      !>     changed and if so updates the internally buffered POOL-based
      !>     name-ID mappings.
      !>
      !>## Examples
      !>
      !>     The routines that need to be aware of and act on the ZZBODTRN
      !>     state change initialize a local ZZBODTRN counter array using
      !>     ZZCTRUIN, save it, and check it against the current ZZBODTRN
      !>     state counter and update it, if needed, using this entry point,
      !>     as follows:
      !>
      !>        C
      !>        C     Include zzctr.inc to access CTRSIZ.
      !>        C
      !>              INCLUDE              'zzctr.inc'
      !>              ...
      !>
      !>        C
      !>        C     In local variable declarations declare and save
      !>        C     the local ZZBODTRN state counter. Also declare the
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
      !>        C     initialize the local ZZBODTRN state counter using
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
      !>        C     the ZZBODTRN state change call this entry to
      !>        C     check and update the local POOL state counter.
      !>        C
      !>              CALL ZZBCTRCK ( USRCTR, UPDATE )
      !>
      !>              IF ( UPDATE ) THEN
      !>
      !>        C
      !>        C        It the ZZBODTRN state changed, do what needs to
      !>        C        be done to deal with saved values based
      !>        C        on ZZBODTRN data.
      !>        C
      !>                 ...
      !>
      !>              END IF
      !>
      !>## Restrictions
      !>
      !>     1) This is a private routine. See $Particulars above.
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
      !>-    SPICELIB Version 5.0.0, 16-SEP-2013 (BVS)
      module subroutine zzbctrck ( ctx, usrctr, update )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: usrctr    ( ctrsiz )  !! ZZBODTRN state counter tracked by
                                                          !! the caller
      logical, intent(out) :: update  !! Flag indicating if input counter was updated
      logical :: lupdte
!     PEPPER: apply the body names defined through other contexts.
      call sync_boddefs ( ctx )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return
      if ( .not. allocated ( ctx%zzbodtrn ) ) then
         allocate ( ctx%zzbodtrn )
         call init_zzbodtrn_state ( ctx%zzbodtrn )
      end if
      associate ( ker => ctx%zzbodtrn%ker, &
                  subctr => ctx%zzbodtrn%subctr, &
                  pulctr => ctx%zzbodtrn%pulctr, &
                  nodata => ctx%zzbodtrn%nodata )
      if ( ctx%failed() ) return

!
!     Check for updates to the kernel pool variables.
!
      call zzcvpool ( ctx, 'ZZBODTRN', pulctr, lupdte )

      if ( lupdte .or. nodata ) then

!
!        Check in because ZZBODKER can fail.
!

!
!        Conservatively increment the ZZBODTRN state counter in
!        expectation of successful update.
!
         call zzctrinc ( ctx, subctr )

!
!        Update kernel pool mapping lists and hashes.
!
         call zzbodker ( ctx, ker )

         if ( ctx%failed() ) then

            nodata = .true.

            return

         end if

         nodata = .false.

      end if

!
!     Check the input counter against the ZZBODTRN counter.
!
      call zzctrchk ( ctx, subctr, usrctr, update )

      return
      end associate
      end subroutine zzbctrck

      !> Translate the name of a body or object to the corresponding SPICE
      !> integer ID code.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of a body or object, such as a planet,
      !>              satellite, comet, asteroid, barycenter, DSN station,
      !>              spacecraft, or instrument, that is "known" to the
      !>              SPICE system, whether through hard-coded
      !>              registration or run-time registration in the SPICE
      !>              kernel pool.
      !>
      !>              Case and leading and trailing blanks in a name
      !>              are not significant. However when a name is made
      !>              up of more than one word, they must be separated by
      !>              at least one blank. That is, all of the following
      !>              strings are equivalent names:
      !>
      !>                  'JUPITER BARYCENTER'
      !>                  'Jupiter Barycenter'
      !>                  'JUPITER BARYCENTER   '
      !>                  'JUPITER    BARYCENTER'
      !>                  '   JUPITER BARYCENTER'
      !>
      !>              However, 'JUPITERBARYCENTER' is not equivalent to
      !>              the names above.
      !>
      !>## Detailed Output
      !>
      !>     CODE     is the SPICE or user-defined integer ID code for the
      !>              named body.
      !>
      !>     FOUND    is .TRUE. if NAME has a translation. Otherwise, FOUND
      !>              is .FALSE.
      !>
      !>## Parameters
      !>
      !>     MAXL     is the maximum allowable length of a body name. The
      !>              current value of this parameter is 36. See the SPICELIB
      !>              include file zzbodtrn.inc for details.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is any problem with the body name-ID mapping kernel
      !>         variables present in the kernel pool, an error is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>     2)  Body name strings are upper-cased, their leading and trailing
      !>         blanks removed, and embedded blanks are compressed out, after
      !>         which they get truncated to the maximum body name length MAXL.
      !>         Therefore, two body names that differ only after that maximum
      !>         length are considered equal.
      !>
      !>## Files
      !>
      !>     Body-name mappings may be defined at run time by loading text
      !>     kernels containing kernel variable assignments of the form
      !>
      !>        NAIF_BODY_NAME += ( <name 1>, ... )
      !>        NAIF_BODY_CODE += ( <code 1>, ... )
      !>
      !>     See naif_ids.req for details.
      !>
      !>## Particulars
      !>
      !>     BODN2C is one of five related subroutines,
      !>
      !>        BODS2C      Body string to code
      !>        BODC2S      Body code to string
      !>        BODN2C      Body name to code
      !>        BODC2N      Body code to name
      !>        BODDEF      Body name/code definition
      !>
      !>     BODS2C, BODC2S, BODN2C, and BODC2N perform translations between
      !>     body names and their corresponding integer ID codes which are
      !>     used in SPICE files and routines.
      !>
      !>     BODS2C is a slightly more general version of BODN2C: support
      !>     for strings containing ID codes in string format enables a caller
      !>     to identify a body using a string, even when no name is
      !>     associated with that body.
      !>
      !>     BODC2S is a general version of BODC2N; the routine returns either
      !>     the name assigned in the body ID to name mapping or a string
      !>     representation of the CODE value if no mapping exists.
      !>
      !>     BODDEF assigns a body name to ID mapping. The mapping has
      !>     priority in name-to-ID and ID-to-name translations.
      !>
      !>     Programmers writing user interface code should consider using the
      !>     SPICELIB routine BODS2C. BODS2C provides more flexibility in
      !>     handling input strings, since it accepts both body names and
      !>     strings representing integer ID codes, for example '399'.
      !>
      !>     Refer to naif_ids.req for the list of name/code associations built
      !>     into SPICE, and for details concerning adding new name/code
      !>     associations at run time by loading text kernels.
      !>
      !>## Examples
      !>
      !>     1)  In the following code fragment, BODVCD returns the radii
      !>         of Jupiter. BODVCD requires the SPICE integer ID code for
      !>         Jupiter, so we use BODN2C to convert the name to
      !>         its corresponding integer ID code.
      !>
      !>            CALL BODN2C ( 'JUPITER', JUPID,  FOUND )
      !>
      !>            CALL BODVCD ( JUPID, 'RADII', 3, N, RADII )
      !>
      !>     2)  In this example, we assume that only the set of default
      !>         name/code pairs has been defined.
      !>
      !>         Given these names, BODN2C will return the following codes:
      !>
      !>            Name                         Code    Found?
      !>            ------------------------   ------    ------
      !>            'EARTH'                       399    Yes
      !>            '  Earth '                    399    Yes
      !>            'EMB'                           3    Yes
      !>            'Solar System Barycenter'       0    Yes
      !>            'SolarSystemBarycenter'         -    No
      !>            'SSB'                           0    Yes
      !>            'Voyager 2'                   -32    Yes
      !>            'U.S.S. Enterprise'             -    No
      !>            ' '                             -    No
      !>            'Halley's Comet'                -    No
      !>
      !>         Given these codes, BODC2N will return the following names:
      !>
      !>            Code        Name                        Found?
      !>            -------     -------------------         ------
      !>            399         'EARTH'                     Yes
      !>              0         'SOLAR SYSTEM BARYCENTER'   Yes
      !>              3         'EARTH BARYCENTER'          Yes
      !>            -77         'GALILEO ORBITER'           Yes
      !>             11          -                          No
      !>
      !>## Restrictions
      !>
      !>     1)  See exception <2>.
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
      !>     K.R. Gehringer     (JPL)
      !>     B.V. Semenov       (JPL)
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        description of MAXL parameter and added $Exceptions and
      !>        $Restrictions.
      !>
      !>-    SPICELIB Version 1.0.8, 16-MAY-2009 (EDW)
      !>
      !>        Edit to $Particulars section to document the BODC2S routine.
      !>
      !>-    SPICELIB Version 1.0.7, 28-FEB-2008 (BVS)
      !>
      !>        Corrected the contents of the $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.6, 31-JAN-2008 (NJB)
      !>
      !>        References to the routine BODS2C were added to the header.
      !>
      !>-    SPICELIB Version 1.0.5, 24-OCT-2005 (NJB)
      !>
      !>        Header update: changed references to BODVAR to references
      !>        to BODVCD.
      !>
      !>-    SPICELIB Version 1.0.4, 20-JUL-2004 (EDW)
      !>
      !>        Removed unneeded assignment of FOUND = .FALSE.
      !>
      !>-    SPICELIB Version 1.0.3, 29-JUL-2003 (NJB) (CHA)
      !>
      !>        Various header changes were made to improve clarity. Some
      !>        minor header corrections were made.
      !>
      !>-    SPICELIB Version 1.0.2, 26-AUG-2002 (FST)
      !>
      !>        Added discussion of MAXL to the $Parameters section.
      !>
      !>-    SPICELIB Version 1.0.1, 22-AUG-2001 (EDW)
      !>
      !>        Corrected ENDIF to END IF.
      !>
      !>-    SPICELIB Version 1.0.0, 23-JAN-1996 (KRG)
      !>
      !>        This was the BODN2C entry point from the original BODTRN
      !>        subroutine that was in the NAIF toolkit SUPPORT library.
      !>        When the private subroutine ZZBODTRN was added to SPICELIB,
      !>        superseding the BODTRN from SUPPORT, the body ID code/name
      !>        translation interface from the original BODTRN was moved to
      !>        SPICELIB so that ID codes did not have to be hard coded by
      !>        users of the Toolkit.
      !>
      !>        This subroutine simply calls the private subroutine ZZBODN2C
      !>        to perform its job.
      module subroutine bodn2c ( ctx, name, code, found )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Body name to be translated into a SPICE ID code.
      integer(ip), intent(out) :: code  !! SPICE integer ID code for the named body.
      logical, intent(out) :: found  !! .TRUE. if translated, otherwise false.

!     Standard SPICELIB error handling.
!
      if ( ctx%failed() ) return

      call zzbodn2c ( ctx, name, code, found )
!
!     No need for any error checking, since all we do is check out
!     and return anyway. We leave the error checking to the caller.
!
      return

      end subroutine bodn2c

      !> Translate the SPICE integer code of a body into a common name
      !> for that body.
      !>
      !>## Detailed Input
      !>
      !>     CODE     is an integer code for a body ---
      !>              a planet, satellite, barycenter, spacecraft,
      !>              asteroid, comet, or other ephemeris object.
      !>
      !>## Detailed Output
      !>
      !>     NAME     is a common name of the body identified by CODE.
      !>              If CODE has more than one translation, then the
      !>              most recently defined NAME corresponding to CODE
      !>              is returned. NAME will have the exact format (case
      !>              and blanks) as when the name/code pair was defined.
      !>              If the input value of CODE is not recognized, NAME
      !>              will remain unchanged from its input value.
      !>
      !>     FOUND    is .TRUE. if CODE has a translation. Otherwise, FOUND
      !>              is .FALSE.
      !>
      !>## Parameters
      !>
      !>     MAXL     is the maximum allowable length of a body name.
      !>              This amount of storage space should be declared
      !>              to receive NAME, otherwise truncation may occur.
      !>              The current value of this parameter is 36. See
      !>              the SPICELIB include file zzbodtrn.inc for details.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is any problem with the body name-ID mapping kernel
      !>         variables present in the kernel pool, an error is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     Body-name mappings may be defined at run time by loading text
      !>     kernels containing kernel variable assignments of the form
      !>
      !>        NAIF_BODY_NAME += ( <name 1>, ... )
      !>        NAIF_BODY_CODE += ( <code 1>, ... )
      !>
      !>     See naif_ids.req for details.
      !>
      !>## Particulars
      !>
      !>     BODS2N is one of five related subroutines,
      !>
      !>        BODS2C      Body string to code
      !>        BODC2S      Body code to string
      !>        BODN2C      Body name to code
      !>        BODC2N      Body code to name
      !>        BODDEF      Body name/code definition
      !>
      !>     BODS2C, BODC2S, BODN2C, and BODC2N perform translations between
      !>     body names and their corresponding integer ID codes which are
      !>     used in SPICE files and routines.
      !>
      !>     BODS2C is a slightly more general version of BODN2C: support
      !>     for strings containing ID codes in string format enables a caller
      !>     to identify a body using a string, even when no name is
      !>     associated with that body.
      !>
      !>     BODC2S is a general version of BODC2N; the routine returns either
      !>     the name assigned in the body ID to name mapping or a string
      !>     representation of the CODE value if no mapping exists.
      !>
      !>     BODDEF assigns a body name to ID mapping. The mapping has
      !>     priority in name-to-ID and ID-to-name translations.
      !>
      !>     Refer to NAIF_IDs for the list of name/code associations built
      !>     into SPICE, and for details concerning adding new name/code
      !>     associations at run time by loading text kernels.
      !>
      !>## Examples
      !>
      !>     1. Suppose you ran the utility program SPACIT to summarize
      !>         an SPK ephemeris file and the following data was output
      !>         to the terminal screen.
      !>
      !>            ----------------------------------------------------------
      !>            Segment identifier: JPL archive 21354
      !>            Body        : -77                         Center     : 399
      !>            From        : 1990 DEC 08 18:00:00.000
      !>            To          : 1990 DEC 10 21:10:00.000
      !>            Reference   : DE-200                      SPK Type    :1
      !>            ----------------------------------------------------------
      !>
      !>        You could write a program to translate the body codes
      !>        shown in the SPACIT output:
      !>
      !>           CALL BODC2N ( -77, BODY,   FOUND )
      !>           CALL BODC2N ( 399, CENTER, FOUND )
      !>
      !>           IF ( FOUND ) THEN
      !>
      !>              WRITE ( *,* ) 'BODY:    -77 = ', BODY
      !>              WRITE ( *,* ) 'CENTER:  399 = ', CENTER
      !>
      !>           END IF
      !>
      !>        You could also read the body and center codes directly from
      !>        the SPK files, using the appropriate DAF routines, and then
      !>        translate them, as above.
      !>
      !>     2. In this example, we assume that BODDEF has not been called,
      !>         so only the set of default name/code pairs has
      !>         been defined.
      !>
      !>         Given these names, BODN2C will return the following codes:
      !>
      !>            Name                         Code    Found?
      !>            ------------------------   ------    ------
      !>            'EARTH'                       399    Yes
      !>            '  Earth '                    399    Yes
      !>            'EMB'                           3    Yes
      !>            'Solar System Barycenter'       0    Yes
      !>            'SolarSystemBarycenter'         -    No
      !>            'SSB'                           0    Yes
      !>            'Voyager 2'                   -32    Yes
      !>            'U.S.S. Enterprise'             -    No
      !>            ' '                             -    No
      !>            'Halley's Comet'                -    No
      !>
      !>         Given these codes, BODC2N will return the following names:
      !>
      !>            Code        Name                        Found?
      !>            -------     -------------------         ------
      !>            399         'EARTH'                     Yes
      !>              0         'SOLAR SYSTEM BARYCENTER'   Yes
      !>              3         'EARTH BARYCENTER'          Yes
      !>            -77         'GALILEO ORBITER'           Yes
      !>             11          -                          No
      !>             -1          -                          No
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
      !>     B.V. Semenov       (JPL)
      !>     W.L. Taber         (JPL)
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        description of MAXL parameter and added $Exceptions,
      !>        $Restrictions and $Files.
      !>
      !>-    SPICELIB Version 1.0.4, 16-MAY-2009 (EDW)
      !>
      !>        Edit to $Particulars section to document the BODC2S routine.
      !>
      !>-    SPICELIB Version 1.0.3, 28-FEB-2008 (BVS)
      !>
      !>        Corrected the contents of the $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.2, 26-AUG-2002 (FST)
      !>
      !>        Added documentation discussing the parameter MAXL.
      !>
      !>-    SPICELIB Version 1.0.1, 01-DEC-1998 (WLT)
      !>
      !>        Added documentation that describes the output NAME if CODE
      !>        is not a recognized body ID.
      !>
      !>-    SPICELIB Version 1.0.0, 23-JAN-1996 (KRG)
      !>
      !>        This was the BODC2N entry point from the original BODTRN
      !>        subroutine that was in the NAIF toolkit SUPPORT library.
      !>        When the private subroutine ZZBODTRN was added to SPICELIB,
      !>        superseding the BODTRN from SUPPORT, the body ID code/name
      !>        translation interface from the original BODTRN was moved to
      !>        SPICELIB so that ID codes did not have to be hard coded by
      !>        users of the toolkit.
      !>
      !>        This subroutine simply calls the private subroutine ZZBODC2N
      !>        to perform its job.
      module subroutine bodc2n ( ctx, code, name, found )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: code  !! Integer ID code to be translated into a name.
      character(*), intent(out) :: name  !! A common name for the body identified by CODE.
      logical, intent(out) :: found  !! .TRUE. if translated, otherwise false.

!
!     Standard SPICELIB error handling.
!
      if ( ctx%failed() ) return

      call zzbodc2n ( ctx, code, name, found )
!
!     No need for any error checking, since all we do is check out
!     and return anyway. We leave the error checking to the caller.
!
      return

      end subroutine bodc2n

      !> Translate a string containing a body name or ID code to an
      !> integer code.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is a string containing the name or ID code of a
      !>              body or object, such as a planet, satellite, comet,
      !>              asteroid, barycenter, DSN station, spacecraft, or
      !>              instrument.
      !>
      !>              If NAME contains the name of a body or object, that
      !>              name must be "known" to the SPICE system, whether
      !>              through hard-coded registration or run-time
      !>              registration in the SPICE kernel pool.
      !>
      !>              Case and leading and trailing blanks in a name are
      !>              not significant. However when a name is made up of
      !>              more than one word, adjacent words must be separated
      !>              by at least one blank. That is, all of the following
      !>              strings are equivalent names:
      !>
      !>                 'JUPITER BARYCENTER'
      !>                 'Jupiter Barycenter'
      !>                 'JUPITER BARYCENTER   '
      !>                 'JUPITER    BARYCENTER'
      !>                 '   JUPITER BARYCENTER'
      !>
      !>              However, 'JUPITERBARYCENTER' is not equivalent to
      !>              the names above.
      !>
      !>              If NAME is a string representation of an integer,
      !>              for example
      !>
      !>                 '399'
      !>
      !>              the string will be translated to the equivalent
      !>              INTEGER datum. The input integer need not be one
      !>              recognized by the SPICE system: the integer need not
      !>              be a built-in NAIF ID code, nor need it be associated
      !>              with a name via run-time registration.
      !>
      !>## Detailed Output
      !>
      !>     CODE     is, if NAME contains the name of a body or object,
      !>              the corresponding NAIF or user-defined integer ID
      !>              code, as determined by the SPICE name-code mapping
      !>              subsystem. If NAME represents an integer, the same
      !>              integer is returned in CODE.
      !>
      !>              CODE is assigned a value only if FOUND is returned
      !>              as .TRUE.; otherwise it is returned unchanged.
      !>
      !>     FOUND    is .TRUE. if NAME has a translation or represents an
      !>              integer within the bounds of representable integers
      !>              as defined by the SPICELIB routines INTMAX and INTMIN.
      !>              Otherwise, FOUND is .FALSE.
      !>
      !>## Parameters
      !>
      !>     MAXL     is the maximum allowable length of a body name. The
      !>              current value of this parameter is 36. See the SPICELIB
      !>              include file zzbodtrn.inc for details.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is any problem with the body name-ID mapping kernel
      !>         variables present in the kernel pool, an error is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>     2)  Body name strings are upper-cased, their leading and trailing
      !>         blanks removed, and embedded blanks are compressed out, after
      !>         which they get truncated to the maximum body name length MAXL.
      !>         Therefore, two body names that differ only after that maximum
      !>         length are considered equal.
      !>
      !>## Files
      !>
      !>     Body-name mappings may be defined at run time by loading text
      !>     kernels containing kernel variable assignments of the form
      !>
      !>        NAIF_BODY_NAME += ( <name 1>, ... )
      !>        NAIF_BODY_CODE += ( <code 1>, ... )
      !>
      !>     See naif_ids.req for details.
      !>
      !>## Particulars
      !>
      !>     BODS2C is one of five related subroutines,
      !>
      !>        BODS2C      Body string to code
      !>        BODC2S      Body code to string
      !>        BODN2C      Body name to code
      !>        BODC2N      Body code to name
      !>        BODDEF      Body name/code definition
      !>
      !>     BODS2C, BODC2S, BODN2C, and BODC2N perform translations between
      !>     body names and their corresponding integer ID codes which are
      !>     used in SPICE files and routines.
      !>
      !>     BODS2C is a slightly more general version of BODN2C: support
      !>     for strings containing ID codes in string format enables a caller
      !>     to identify a body using a string, even when no name is
      !>     associated with that body.
      !>
      !>     BODC2S is a general version of BODC2N; the routine returns either
      !>     the name assigned in the body ID to name mapping or a string
      !>     representation of the CODE value if no mapping exists.
      !>
      !>     BODDEF assigns a body name to ID mapping. The mapping has
      !>     priority in name-to-ID and ID-to-name translations.
      !>
      !>     Refer to naif_ids.req for the list of name/code associations built
      !>     into SPICE, and for details concerning adding new name/code
      !>     associations at run time by loading text kernels.
      !>
      !>## Examples
      !>
      !>     1)  In the following code fragment, BODEUL returns the Euler
      !>         angles representing the orientation of Jupiter relative to
      !>         the J2000 reference frame. BODEUL requires the NAIF integer
      !>         ID code for Jupiter, so we use BODS2C to convert the name to
      !>         its corresponding integer ID code.
      !>
      !>         We know Jupiter has a built-in name-code mapping, so we
      !>         needn't check the FOUND flag.
      !>
      !>            CALL BODS2C ( 'JUPITER', JUPID, FOUND )
      !>
      !>            CALL BODEUL ( JUPID, ET, RA, DEC, W, LAMBDA )
      !>
      !>     2)  In this example, we assume that only the set of default
      !>         name/code pairs has been defined.
      !>
      !>         Given these names, BODS2C will return the following codes:
      !>
      !>            Name                             Code    Found?
      !>            ------------------------       ------    ------
      !>            'EARTH'                           399    Yes
      !>            '  Earth '                        399    Yes
      !>            '399'                             399    Yes
      !>            ' 399 '                           399    Yes
      !>            'EMB'                               3    Yes
      !>            '3'                                 3    Yes
      !>            '1000000000'               1000000000    Yes
      !>            'Solar System Barycenter'           0    Yes
      !>            'SolarSystemBarycenter'             -    No
      !>            'SSB'                               0    Yes
      !>            'Voyager 2'                       -32    Yes
      !>            'U.S.S. Enterprise'                 -    No
      !>            ' '                                 -    No
      !>            'Halley's Comet'                    -    No
      !>
      !>         Given these codes, BODC2N will return the following names:
      !>
      !>            Code        Name                        Found?
      !>            -------     -------------------         ------
      !>            399         'EARTH'                     Yes
      !>              0         'SOLAR SYSTEM BARYCENTER'   Yes
      !>              3         'EARTH BARYCENTER'          Yes
      !>            -77         'GALILEO ORBITER'           Yes
      !>             11          -                          No
      !>     1000000000          -                          No
      !>
      !>## Restrictions
      !>
      !>     1)  See exception <2>.
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
      !>     K.R. Gehringer     (JPL)
      !>     B.V. Semenov       (JPL)
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 07-AUG-2020 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Updated output argument FOUND description in $Detailed_Output.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed minor
      !>        typos in header. Added description of MAXL parameter. Added
      !>        $Exceptions and $Restrictions.
      !>
      !>-    SPICELIB Version 1.0.2, 16-MAY-2009 (EDW)
      !>
      !>        Edit to $Particulars section to document the BODC2S routine.
      !>
      !>-    SPICELIB Version 1.0.1, 28-FEB-2008 (BVS)
      !>
      !>        Corrected the contents of the $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.0.0, 23-JUL-2003 (CHA) (NJB) (KRG) (FST) (EDW)
      module subroutine bods2c ( ctx, name, code, found )
      use pepper_kinds, only: ip
      use pepper_strings, only: beint, nparsi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! String to be translated to an ID code.
      integer(ip), intent(out) :: code  !! Integer ID code corresponding to NAME.
      logical, intent(out) :: found  !! Flag indicating whether translation succeeded.

      integer(ip), parameter :: msglen = 1

      character(msglen) :: errmsg
      integer(ip) :: ptr

!
!     Standard SPICELIB error handling.
!
      if ( ctx%failed() ) return

!
!     Attempt to translate the input name to an integer code.  Call
!     the private routine ZZBODN2C to avoid additional CHKIN and
!     CHKOUT calls.
!
      call zzbodn2c ( ctx, name, code, found )

      if ( .not. found ) then
!
!        It's possible the name is a string representation
!        of an integer, for example, '999'.  If so, find
!        the equivalent datum of INTEGER type.
!
         if ( beint(name) ) then
!
!           The input conforms to the syntax of an integer, but it may
!           be outside of the range of the INTEGER data type.
!           Therefore we use the non-error-signaling routine NPARSI
!           rather than the cleaner PRSINT to attempt to convert the
!           string to an INTEGER.
!
            call nparsi ( name, code, errmsg, ptr )

!
!           We have an ID code if and only if PTR is zero.
!
            found = ptr == 0

         end if

      end if

!
!     FOUND is set.  CODE is set if NAME was a recognized name
!     or a string representation of an integer.
!

      return
      end subroutine bods2c

      !> Translate a body ID code to either the corresponding name
      !> or if no name to ID code mapping exists, the string
      !> representation of the body ID value.
      !>
      !>## Detailed Input
      !>
      !>     CODE     is the integer code for a body: planet, satellite,
      !>              barycenter, spacecraft, asteroid, comet, or
      !>              other ephemeris object.
      !>
      !>## Detailed Output
      !>
      !>     NAME     is the string name of the body identified by CODE
      !>              if a mapping between CODE and a body name exists
      !>              within SPICE.
      !>
      !>              If CODE has more than one translation, then the
      !>              most recently defined NAME corresponding to CODE
      !>              is returned. NAME will have the exact format (case
      !>              and blanks) as when the name/code pair was defined.
      !>
      !>              If the input value of CODE does not map to a body
      !>              name, NAME returns the string representation
      !>              of CODE.
      !>
      !>## Parameters
      !>
      !>     MAXL     is the maximum allowable length of a body name. The
      !>              current value of this parameter is 36. See the SPICELIB
      !>              include file zzbodtrn.inc for details.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is any problem with the body name-ID mapping kernel
      !>         variables present in the kernel pool, an error is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     Body-name mappings may be defined at run time by loading text
      !>     kernels containing kernel variable assignments of the form
      !>
      !>        NAIF_BODY_NAME += ( <name 1>, ... )
      !>        NAIF_BODY_CODE += ( <code 1>, ... )
      !>
      !>     See naif_ids.req for details.
      !>
      !>## Particulars
      !>
      !>     BODC2S is one of five related subroutines,
      !>
      !>        BODS2C      Body string to code
      !>        BODC2S      Body code to string
      !>        BODN2C      Body name to code
      !>        BODC2N      Body code to name
      !>        BODDEF      Body name/code definition
      !>
      !>     BODS2C, BODC2S, BODN2C, and BODC2N perform translations between
      !>     body names and their corresponding integer ID codes which are
      !>     used in SPICE files and routines.
      !>
      !>     BODS2C is a slightly more general version of BODN2C: support
      !>     for strings containing ID codes in string format enables a caller
      !>     to identify a body using a string, even when no name is
      !>     associated with that body.
      !>
      !>     BODC2S is a general version of BODC2N; the routine returns either
      !>     the name assigned in the body ID to name mapping or a string
      !>     representation of the CODE value if no mapping exists.
      !>
      !>     BODDEF assigns a body name to ID mapping. The mapping has
      !>     priority in name-to-ID and ID-to-name translations.
      !>
      !>     Refer to naif_ids.req for the list of name/code associations built
      !>     into SPICE, and for details concerning adding new name/code
      !>     associations at run time by loading text kernels.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Apply the BODC2S call to several IDs representing codes
      !>        included in the default SPICE ID-name lists and codes not
      !>        included in the list.
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM BODC2S_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      CHARACTER*(32)         NAME
      !>      INTEGER                CODE (7)
      !>      INTEGER                I
      !>
      !>C
      !>C     Assign an array of body IDs. Not all the listed IDs
      !>C     map to a body name.
      !>C
      !>      CODE(1) = 399
      !>      CODE(2) = 0
      !>      CODE(3) = 3
      !>      CODE(4) = -77
      !>      CODE(5) = 11
      !>      CODE(6) = -1
      !>      CODE(7) = 6000001
      !>
      !>C
      !>C     Loop over the CODE array, call BODC2S for each
      !>C     element of CODE.
      !>C
      !>      WRITE(*,*) 'Code      Name'
      !>      WRITE(*,*) '-------   -----------------------'
      !>
      !>      DO I= 1, 7
      !>
      !>         CALL BODC2S( CODE(I), NAME )
      !>
      !>         WRITE(*, '(I8,3x,A)' ) CODE(I),  NAME
      !>
      !>      END DO
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>         Code      Name
      !>         -------   -----------------------
      !>             399   EARTH
      !>               0   SOLAR SYSTEM BARYCENTER
      !>               3   EARTH BARYCENTER
      !>             -77   GALILEO ORBITER
      !>              11   11
      !>              -1   GEOTAIL
      !>         6000001   6000001
      !>
      !>        Note that the codes 11 and 6000001 did not map to a name so the
      !>        call returns as NAME the string expression of the codes.
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 07-JUL-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed minor
      !>        typos in header. Added description of MAXL parameter. Added
      !>        $Exceptions and $Restrictions.
      !>
      !>-    SPICELIB Version 1.0.1, 18-APR-2014 (BVS)
      !>
      !>        Minor header edits.
      !>
      !>-    SPICELIB Version 1.0.0, 10-APR-2010 (EDW)
      module subroutine bodc2s ( ctx, code, name )
      use pepper_kinds, only: ip
      use pepper_strings, only: intstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: code  !! Integer ID code to translate to a string.
      character(*), intent(out) :: name  !! String corresponding to CODE.

      logical :: found

!
!     Standard SPICELIB error handling.
!
      if ( ctx%failed() ) return

!
!     Fortran. No type check available for CODE. Bother.
!

!
!     Attempt to translate the input CODE to a name. Use
!     the private routine ZZBODC2N.
!
      call zzbodc2n ( ctx, code, name, found )

      if ( found ) then

!
!        Success. CODE maps to NAME. Return.
!
         return

      end if

!
!     If execution reaches this level, the SPICE body ID
!     to name mapping lacks an assignment for CODE. Convert
!     CODE to a string representation of the integer value.
!
      call intstr ( code, name )

      return

      end subroutine bodc2s

      !> Define a body name/ID code pair for later translation via
      !> [[bodn2c]] or [[bodc2n]].
      !>
      !>## Detailed Input
      !>
      !>     NAME     is an arbitrary name of a body which could be
      !>              a planet, satellite, barycenter, spacecraft,
      !>              asteroid, comet, or other ephemeris object.
      !>
      !>              The case and positions of blanks in a name are
      !>              significant. BODC2N returns the same string
      !>              (case and space) most recently mapped to a code.
      !>              When NAME consists of more than one word, the
      !>              words require separation by at least one blank.
      !>
      !>              The kernel sub-system stores NAME as described in
      !>              the BODDEF call, but creates an equivalence class
      !>              based on NAME for comparisons in BODN2C. This class
      !>              ignores leading/trailing whitespace, compresses
      !>              interior whitespace to a single space, and ignores
      !>              character case.
      !>
      !>              The following strings belong to the same equivalence
      !>              class:
      !>
      !>                      'JUPITER BARYCENTER'
      !>                      'Jupiter Barycenter'
      !>                      'JUPITER BARYCENTER   '
      !>                      'JUPITER    BARYCENTER'
      !>                      '   JUPITER BARYCENTER'
      !>
      !>              However, 'JUPITERBARYCENTER' is distinct from
      !>              the names above.
      !>
      !>              When ignoring trailing blanks, NAME must be short
      !>              enough to fit into the space defined by parameter
      !>              MAXL.
      !>
      !>     CODE     is the integer ID code for assignment to body NAME.
      !>
      !>## Detailed Output
      !>
      !>     None.
      !>
      !>## Parameters
      !>
      !>     MAXL     is the maximum allowed length of a body NAME.
      !>              Names exceeding this length will be truncated
      !>              on assignment to a code with BODDEF. The value
      !>              of this parameter may be found in the include
      !>              file 'zzbodtrn.inc'.
      !>
      !>## Exceptions
      !>
      !>     1)  If improper inputs are supplied, or if there is insufficient
      !>         room to store the requested addition, an error is signaled by
      !>         a routine in the call tree of this routine.
      !>
      !>     2)  If the length of NAME exceeds the maximum allowed length for a
      !>         body name, the name stored in the kernel pool will be
      !>         truncated on the right.
      !>
      !>     3)  If a name-code definition inserted into this routine seems to
      !>         have no effect, it is possible that the contents of the
      !>         definition are masked by the higher precedence kernel pool
      !>         assignments. See the $Particulars section of this document
      !>         for more information.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     BODDEF is one of five related subroutines,
      !>
      !>        BODS2C      Body string to code
      !>        BODC2S      Body code to string
      !>        BODN2C      Body name to code
      !>        BODC2N      Body code to name
      !>        BODDEF      Body name/code definition
      !>
      !>     BODS2C, BODC2S, BODN2C, and BODC2N perform translations between
      !>     body names and their corresponding integer ID codes which are
      !>     used in SPICE files and routines.
      !>
      !>     BODS2C is a slightly more general version of BODN2C: support
      !>     for strings containing ID codes in string format enables a caller
      !>     to identify a body using a string, even when no name is
      !>     associated with that body.
      !>
      !>     BODC2S is a general version of BODC2N; the routine returns either
      !>     the name assigned in the body ID to name mapping or a string
      !>     representation of the CODE value if no mapping exists.
      !>
      !>     BODDEF assigns a body name to ID mapping. The mapping has
      !>     priority in name-to-ID and ID-to-name translations.
      !>
      !>     Refer to NAIF_IDs for the list of name/code associations built
      !>     into SPICE, and for details concerning adding new name/code
      !>     associations at run time by loading text kernels.
      !>
      !>     Modifying the SPICE name-ID mapping set
      !>     =======================================
      !>
      !>     Each body has a unique integer CODE, but may have several
      !>     names. Thus you may associate more than one name with
      !>     a particular integer code.
      !>
      !>     CODE may already have a name as defined by a previous
      !>     call to BODDEF or as part of the set of default
      !>     definitions. That previous definition will remain,
      !>     and a translation of that name will still give the
      !>     same CODE. However, future translations of CODE will
      !>     give the new NAME instead of the previous one. This
      !>     feature is useful for assigning a more familiar or
      !>     abbreviated name to a body. For example, in addition
      !>     to the default name for body 5, 'JUPITER BARYCENTER',
      !>     you could define the abbreviation 'JB' to mean 5.
      !>
      !>     Note: In the case where BODDEF performs a name-to-ID mapping
      !>     assignment for an unused body name and unused ID value,
      !>     any subsequent assignment to NAME destroys the previous
      !>     mapping.
      !>
      !>        BODDEF( 'spud', 22)
      !>
      !>     then
      !>
      !>        BODDEF( 'spud', 23)
      !>
      !>     results in the state 'spud' maps to 23, 23 maps to 'spud',
      !>     and 22 maps to nothing (FOUND in BODC2N returns .FALSE.).
      !>
      !>## Examples
      !>
      !>     You may associate a new name for a previously defined code:
      !>
      !>            CALL BODDEF ( 'JB', 5 )
      !>
      !>     You may also define the name and integer code for a new body:
      !>
      !>            CALL BODDEF ( 'Asteroid Frank', 20103456 )
      !>
      !>     After these calls to BODDEF, BODN2C would return the following
      !>     translations:
      !>
      !>        Name                         Code    Found?
      !>        ------------------------   ------    ------
      !>        'JB'                            5    Yes
      !>        'Jupiter Barycenter'            5    Yes
      !>        'ASTEROID FRANK'         20103456    Yes
      !>        'ASTEROIDFRANK'                 -    No
      !>        'Frank'                         -    No
      !>
      !>     and BODC2N will return these translations:
      !>
      !>        Code        Name                     Found?
      !>        -------     -------------------      ------
      !>               5    'JB'                     Yes
      !>        20103456    'Asteroid Frank'         Yes
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
      !>     B.V. Semenov       (JPL)
      !>     F.S. Turner        (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.0, 13-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added entry #2
      !>        to $Exceptions section.
      !>
      !>-    SPICELIB Version 1.1.2, 16-MAY-2009 (EDW)
      !>
      !>        Edit to $Particulars section to document the BODC2S routine.
      !>
      !>-    SPICELIB Version 1.1.1, 28-FEB-2008 (BVS)
      !>
      !>        Corrected the contents of the $Required_Reading section.
      !>
      !>-    SPICELIB Version 1.1.0, 23-JAN-2004 (EDW)
      !>
      !>        Rewrote header for clarity with regards to the
      !>        current capabilities of the kernel subsystem.
      !>
      !>-    SPICELIB Version 1.0.2, 26-AUG-2002 (FST)
      !>
      !>        Updated header to describe the parameter MAXL and
      !>        its effect on this module. The $Exceptions section
      !>        was updated to include a more general discussion
      !>        of errors that routines in the call tree of this
      !>        routine may signal.
      !>
      !>-    SPICELIB Version 1.0.1, 12-AUG-2001 (EDW)
      !>
      !>        Updated header with information on new functionality.
      !>        The code-to-name retrieval routines now return the exact
      !>        string as defined in the last code/name mapping (case
      !>        and space).
      !>
      !>-    SPICELIB Version 1.0.0, 23-JAN-1996 (KRG)
      !>
      !>        This was the BODDEF entry point from the original BODTRN
      !>        subroutine that was in the NAIF toolkit SUPPORT library.
      !>        When the private subroutine ZZBODTRN was added to SPICELIB,
      !>        superseding the BODTRN from SUPPORT, the body ID code/name
      !>        translation interface from the original BODTRN was moved to
      !>        SPICELIB so that ID codes did not have to be hard coded by
      !>        users of the toolkit.
      !>
      !>        This subroutine simply calls the private subroutine ZZBODDEF
      !>        to perform its job.
      module subroutine boddef ( ctx, name, code )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Common name of some body.
      integer(ip), intent(in) :: code  !! Integer code for that body.

!
!     Standard SPICELIB error handling.
!
      if ( ctx%failed() ) return

      call zzboddef ( ctx, name, code )

!
!     No need for any error checking, since all we do is check out
!     and return anyway. We leave the error checking to the caller.
!
      return

      end subroutine boddef

      !> Determine whether values exist for some item for any body
      !> in the kernel pool.
      !>
      !> The function returns .TRUE. if ITEM is in the kernel pool, .FALSE.
      !> otherwise.
      !>
      !>## Detailed Input
      !>
      !>     BODY     is the ID code of the body for which the item is
      !>              requested.
      !>
      !>     ITEM     is the item to be returned. Together, the body and item
      !>              name combine to form a variable name, e.g.,
      !>
      !>                    'BODY599_RADII'
      !>                    'BODY4_POLE_RA'
      !>
      !>## Detailed Output
      !>
      !>     The function returns .TRUE. if ITEM is in the kernel pool, and
      !>     .FALSE. if it is not.
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
      !>     BODVCD, which returns values from the kernel pool, causes an
      !>     error to be signaled whenever the specified item is not found.
      !>     In many cases, this is appropriate. However, sometimes the
      !>     program may attempt to recover, by providing default values,
      !>     prompting for replacements, and so on.
      !>
      !>## Examples
      !>
      !>     In the following example, default values are substituted for
      !>     bodies for which axes are not found.
      !>
      !>        IF ( BODFND ( TARGET, 'RADII' ) ) THEN
      !>           CALL BODVCD ( TARGET, 'RADII', 3, N, RADII )
      !>        ELSE
      !>           CALL VPACK ( 100.D0, 100.D0, 100.D0, RADII )
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
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.3.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        input argument BODY detailed description. Added SPK and PCK to
      !>        the list of required readings.
      !>
      !>-    SPICELIB Version 1.2.1, 24-OCT-2005 (NJB)
      !>
      !>        Header update: calls to BODVAR in example code were replaced
      !>        with calls to BODVCD. The string 'AXES' and variable AXES
      !>        were replaced with the string 'RADII' and variable 'RADII'
      !>        throughout the header.
      !>
      !>-    SPICELIB Version 1.2.0, 15-MAR-2002 (NJB)
      !>
      !>        Bug fix: routine was updated to work with string-valued
      !>        kernel variables.
      !>
      !>-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
      !>
      !>        If the value of the function RETURN is .TRUE. upon execution of
      !>        this module, this function is assigned a default value of
      !>        either 0, 0.0D0, .FALSE., or blank depending on the type of
      !>        the function.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      module logical function bodfnd ( ctx, body, item )
      use pepper_kinds, only: ip
      use pepper_strings, only: intstr, suffix
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: body  !! ID code of body.
      character(*), intent(in) :: item  !! Item to find ('RADII', 'NUT_AMP_RA', etc.).

      integer(ip), parameter :: vnmlen = 32

      integer(ip), parameter :: numlen = 16

      character(1) :: dtype
      character(numlen) :: code
      character(vnmlen) :: varnam

      integer(ip) :: n

      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
        bodfnd = .false.
        return
      end if

!
!     Construct the variable name from BODY and ITEM.
!
      varnam = 'BODY'

      call intstr ( body, code )
      call suffix ( code, 0, varnam )
      call suffix ( '_',  0, varnam )
      call suffix ( item, 0, varnam )

!
!     Search the kernel pool for the item.
!
      call dtpool ( ctx, varnam, found, n, dtype )

!
!     Was anything there?
!
      bodfnd = found

      return
      end function bodfnd

      !> Translate a string containing a body name or ID code to an
      !> integer code, but bypass calling BODS2C and return saved values
      !> provided by the caller if the name is the same as the saved name
      !> and the ZZBODTRN state did not change.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     USRCTR      is the value of the ZZBODTRN state counter tracked by
      !>                 (saved in) the caller (user) routine specifically for
      !>                 this body name/ID/found flag triplet of variables.
      !>
      !>     SAVNAM      is the body name saved in the caller routine
      !>                 specifically for this body name/ID/found flag triplet
      !>                 of variables. For detailed description of allowed
      !>                 values see description of the NAME argument in
      !>                 BODS2C.
      !>
      !>     SAVCDE      is the body ID code saved in the caller routine
      !>                 specifically for this body name/ID/found flag triplet
      !>                 of variables. For detailed description of allowed
      !>                 values see description of the CODE argument in
      !>                 BODS2C.
      !>
      !>     SAVFND      is the body name to ID translation success flag saved
      !>                 in the caller routine specifically for this body
      !>                 name/ID/found flag triplet of variables. SAVFND
      !>                 should .TRUE. if NAME had a translation or represents
      !>                 an integer.
      !>
      !>     NAME        is the input body name. For detailed description of
      !>                 allowed values see description of the NAME argument
      !>                 in BODS2C.
      !>
      !>## Detailed Output
      !>
      !>     USRCTR      is the current ZZBODTRN state counter.
      !>
      !>     SAVNAM      is the body name saved in the caller routine
      !>                 specifically for this body name/ID/found flag triplet
      !>                 of variables. On the output SAVNAM always equals
      !>                 NAME.
      !>
      !>     SAVCDE      is the body ID code saved in the caller routine
      !>                 specifically for this body name/ID/found flag triplet
      !>                 of variables. On the output SAVCDE always equals
      !>                 CODE.
      !>
      !>     SAVFND      is the body name to ID translation success flag saved
      !>                 in the caller routine specifically for this body
      !>                 name/ID/found flag triplet of variables. On the
      !>                 output SAVFND always equals FOUND.
      !>
      !>     CODE        is the output body ID code. For detailed description
      !>                 of possible values see description of the CODE
      !>                 argument in BODS2C. On the output CODE always equals
      !>                 SAVCDE.
      !>
      !>     FOUND       is the body name to ID translation success flag.
      !>                 FOUND is .TRUE. if NAME has a translation or
      !>                 represents an integer. Otherwise, FOUND is .FALSE.
      !>                 On the output FOUND always equals SAVFND.
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
      !>     This routine translates a string containing a body name or ID code
      !>     to an integer code but bypasses calling BODS2C to do the
      !>     translation and simply returns the saved ID value and translation
      !>     success flag provided by the caller if the input name is the same
      !>     as the saved name provided by the caller, the saved translation
      !>     success flag provided by the caller is .TRUE. and the ZZBODTRN
      !>     state counter tracked by the caller is the same as the current
      !>     ZZBODTRN state counter.
      !>
      !>     The ZZBODTRN state counter and name/ID/found flag triplet of
      !>     saved variables tracked by the caller must be specific for each
      !>     body of interest. I.e. if the caller routine needs to call this
      !>     routine to do translations for a target body and for an observer
      !>     body, the caller routine mush use must have its own set of saved
      !>     ZZBODTRN state counter and name/ID/found flag variables for each
      !>     of the two bodies .
      !>
      !>## Examples
      !>
      !>     This example shows how a routine that needs to do body name-ID
      !>     conversion for two distinct bodies can do it using ZZBODS2C.
      !>
      !>           SUBROUTINE <name> ( TARG, OBS, ... )
      !>           ...
      !>           INCLUDE               'zzctr.inc'
      !>           ...
      !>     C
      !>     C     Saved body name length.
      !>     C
      !>           INTEGER               MAXL
      !>           PARAMETER           ( MAXL  = 36 )
      !>           ....
      !>     C
      !>     C
      !>     C     Saved name/ID item declarations.
      !>     C
      !>           INTEGER               SVCTR1 ( CTRSIZ )
      !>           CHARACTER*(MAXL)      SVTARG
      !>           INTEGER               SVTGID
      !>           LOGICAL               SVFND1
      !>           INTEGER               SVCTR2 ( CTRSIZ )
      !>           CHARACTER*(MAXL)      SVOBSN
      !>           INTEGER               SVOBSI
      !>           LOGICAL               SVFND2
      !>           LOGICAL               FIRST
      !>     C
      !>     C     Saved name/ID items.
      !>     C
      !>           SAVE                  SVCTR1
      !>           SAVE                  SVTARG
      !>           SAVE                  SVTGID
      !>           SAVE                  SVFND1
      !>           SAVE                  SVCTR2
      !>           SAVE                  SVOBSN
      !>           SAVE                  SVOBSI
      !>           SAVE                  SVFND2
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
      !>     C        Initialize ZZBODTRN counters.
      !>     C
      !>              CALL ZZCTRUIN( SVCTR1 )
      !>              CALL ZZCTRUIN( SVCTR2 )
      !>              FIRST = .FALSE.
      !>           END IF
      !>     C
      !>     C     Starting from translation of target name to ID.
      !>     C
      !>           CALL ZZBODS2C ( SVCTR1, SVTARG, SVTGID, SVFND1,
      !>          .                TARG, TARGID, FOUND )
      !>
      !>           IF ( .NOT. FOUND ) THEN
      !>              CALL SETMSG ( '...' )
      !>              CALL SIGERR ( 'SPICE(TARGIDCODENOTFOUND)' )
      !>              CALL CHKOUT ( '<name>' )
      !>              RETURN
      !>           END IF
      !>     C
      !>     C     Now do the same for observer
      !>     C
      !>           CALL ZZBODS2C ( SVCTR2, SVOBSN, SVOBSI, SVFND2,
      !>          .                OBS, OBSID, FOUND )
      !>
      !>           IF ( .NOT. FOUND ) THEN
      !>              CALL SETMSG ( '...' )
      !>              CALL SIGERR ( 'SPICE(OBSIDCODENOTFOUND)' )
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
      !>-    SPICELIB Version 1.0.0, 21-SEP-2013 (BVS)
      module subroutine zzbods2c ( ctx, usrctr, savnam, savcde, savfnd, name, code, found )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(inout) :: usrctr    ( ctrsiz )  !! ZZBODTRN state counter saved by the
                                                          !! caller.
      character(*), intent(inout) :: savnam  !! Body name saved by the caller.
      integer(ip), intent(inout) :: savcde  !! Body ID code saved by the caller.
      logical, intent(inout) :: savfnd  !! Translation success flag saved in the caller.
      character(*), intent(in) :: name  !! Body name.
      integer(ip), intent(out) :: code  !! Body ID code.
      logical, intent(out) :: found  !! Translation success flag.

!
!     Local variables.
!
      logical :: update

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check/update ZZBODTRN state counter.
!
      call zzbctrck ( ctx, usrctr, update )

!
!     Check update flag, saved found flag, and saved name against the
!     input.
!
      if ( .not. update     .and. &
           savfnd           .and. &
           savnam == name      ) then

!
!        No change in body-name mapping state, the saved name was
!        successfully resolved earlier, and input and saved names are
!        the same. Return saved ID and FOUND.
!
         code   = savcde
         found  = savfnd

      else

!
!        Check in because BODS2C may fail.
!

!
!        Body-name mapping state changed, or the saved name was never
!        successfully resolved earlier, or input and saved names are
!        different. Call BODS2C to look up ID and FOUND and reset saved
!        values.
!
         call bods2c( ctx, name, code, found )

         savnam = name
         savcde = code
         savfnd = found

!
!        Check out.
!

      end if

      return

      end subroutine zzbods2c

      !> Fetch from the kernel pool the double precision values of an item
      !> associated with a body, where the body is specified by an integer
      !> ID code. Perform lookup only if kernel variable or kernel pool
      !> state has changed; otherwise, bypass kernel pool lookup and
      !> return input values.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !>## Detailed Input
      !>
      !>     BODYID     is the NAIF integer ID code for a body of interest.
      !>                For example, if the body is the earth, the code is
      !>                399.
      !>
      !>     ITEM       is the item to be returned. Together, the NAIF ID
      !>                code of the body and the item name combine to form a
      !>                kernel variable name, e.g.,
      !>
      !>                      'BODY599_RADII'
      !>                      'BODY401_POLE_RA'
      !>
      !>                The values associated with the kernel variable having
      !>                the name constructed as shown are sought. Below
      !>                we'll take the shortcut of calling this kernel variable
      !>                the "requested kernel variable."
      !>
      !>                Note that ITEM *is* case-sensitive. This attribute
      !>                is inherited from the case-sensitivity of kernel
      !>                variable names.
      !>
      !>     MAXN       is the maximum number of values that may be returned.
      !>                The output array VALUES must be declared with size at
      !>                least MAXN.  It's an error to supply an output array
      !>                that is too small to hold all of the values associated
      !>                with the requested kernel variable.
      !>
      !>     VARCTR     is, on input, a counter used to represent the kernel
      !>                pool state at the time the kernel variable designated
      !>                by BODYID and ITEM was set.
      !>
      !>                Note that a distinct actual argument corresponding to
      !>                VARCTR is required for each kernel variable to be
      !>                updated by this routine.
      !>
      !>     N          is, on input, the dimension of the saved kernel
      !>                variable designated by BODYID and ITEM.
      !>
      !>     VALUES     is, on input, the set of values associated with the
      !>                kernel variable designated by BODYID and ITEM.
      !>
      !>## Detailed Output
      !>
      !>     VARCTR     is, on output, a counter used to represent the current
      !>                kernel pool state.
      !>
      !>     N          is, on output, the dimension of the kernel variable
      !>                designated by BODYID and ITEM.
      !>
      !>     VALUES     is, on output, the set of values associated with the
      !>                kernel variable designated by BODYID and ITEM.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the requested kernel variable is not found in the kernel
      !>         pool, the error SPICE(KERNELVARNOTFOUND) is signaled.
      !>
      !>     2)  If the requested kernel variable is found but the associated
      !>         values aren't numeric, an error is signaled by a routine in
      !>         the call tree of this routine.
      !>
      !>     3)  The output array VALUES must be declared with sufficient size
      !>         to contain all of the values associated with the requested
      !>         kernel variable.  If the dimension of VALUES indicated by
      !>         MAXN is too small to contain the requested values, an error
      !>         is signaled by a routine in the call tree of this routine.
      !>
      !>     4)  If the input dimension MAXN indicates there is more room in
      !>         VALUES than there really is---for example, if MAXN is 10 but
      !>         values is declared with dimension 5---and the dimension of
      !>         the requested kernel variable is larger than the actual
      !>         dimension of VALUES, then this routine may overwrite memory.
      !>         The results are unpredictable.
      !>
      !>     5)  If a lookup of the requested variable fails, the argument
      !>         N is set to zero.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine is meant to enable efficient use of BODVCD. This
      !>     routine calls BODVCD only if the requested kernel variable values
      !>     may have changed from their previous values.
      !>
      !>     This routine simplifies looking up PCK kernel variables by
      !>     constructing names of requested kernel variables and by
      !>     performing error checking.
      !>
      !>     This routine is intended for use in cases where the maximum
      !>     number of values that may be returned is known at compile time.
      !>     The caller fetches all of the values associated with the
      !>     specified kernel variable via a single call to this routine. If
      !>     the number of values to be fetched cannot be known until run
      !>     time, the lower-level routine GDPOOL (an entry point of POOL)
      !>     should be used instead. GDPOOL supports fetching arbitrary
      !>     amounts of data in multiple "chunks."
      !>
      !>     This routine is intended for use in cases where the requested
      !>     kernel variable is expected to be present in the kernel pool. If
      !>     the variable is not found or has the wrong data type, this
      !>     routine signals an error. In cases where it is appropriate to
      !>     indicate absence of an expected kernel variable by returning a
      !>     boolean "found flag" with the value .FALSE., again the routine
      !>     GDPOOL should be used.
      !>
      !>## Examples
      !>
      !>     1)  The call below can be used to fetch initial values of the
      !>         radii for the earth (body 399).
      !>
      !>         When the kernel variable
      !>
      !>            BODY399_RADII
      !>
      !>         is present in the kernel pool---normally because a PCK
      !>         defining this variable has been loaded---the code fragment
      !>
      !>            CALL ZZCTRUIN ( CTR1 )
      !>
      !>            BODYID = 399
      !>            ITEM   = 'RADII'
      !>
      !>            CALL ZZBODVCD ( BODYID, ITEM, MAXN, CTR1, N, RADII )
      !>
      !>         returns the dimension and values associated with the variable
      !>         'BODY399_RADII', for example,
      !>
      !>            DIM      = 3
      !>            RADII(1) = 6378.140
      !>            RADII(2) = 6378.140
      !>            RADII(3) = 6356.755
      !>
      !>         The call to ZZCTRUIN initializes CTR1, which forces ZZBODVCD
      !>         to update the output values
      !>
      !>     2)  After the call in example (1) has been made, the variable
      !>
      !>            BODY399_RADII
      !>
      !>         can be set to its current value by the call
      !>
      !>            CALL ZZBODVCD ( BODYID, ITEM, MAXN, CTR1, N, RADII )
      !>
      !>         If the kernel pool has not been modified since the previous
      !>         call, the arguments
      !>
      !>            CTR1
      !>            N
      !>            RADII
      !>
      !>         are unchanged on output.
      !>
      !>     3)  After the call in example (2) has been made, the variable
      !>
      !>            BODY499_POLE_RA
      !>
      !>         can be set to its current value by the code fragment
      !>
      !>            CALL ZZCTRUIN ( CTR2 )
      !>
      !>            BODY   = 499
      !>            ITEM   = 'POLE_RA'
      !>
      !>            CALL ZZBODVCD ( BODYID, ITEM, MAXN, CTR2, N, RA )
      !>
      !>         On output, the arguments
      !>
      !>            N
      !>            RA
      !>
      !>         will be updated to (based on PCK pck00010.tpc):
      !>
      !>            3
      !>            499
      !>            'POLE_RA'
      !>            317.68143   -0.1061      0.
      !>
      !>         CTR2 will be updated if the kernel pool contents have
      !>         changed.
      !>
      !>     4) The call
      !>
      !>            BODY   = 399
      !>            ITEM   = 'radii'
      !>
      !>            CALL ZZBODVCD ( BODYID, ITEM, MAXN, CTR1, N, VALUES )
      !>
      !>        usually will cause a SPICE(KERNELVARNOTFOUND) error to be
      !>        signaled, because this call will attempt to look up the
      !>        values associated with a kernel variable of the name
      !>
      !>           'BODY399_radii'
      !>
      !>        Since kernel variable names are case sensitive, this
      !>        name is not considered to match the name
      !>
      !>           'BODY399_RADII'
      !>
      !>        which normally would be present after a text PCK
      !>        containing data for all planets and satellites has
      !>        been loaded.
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
      !>     I.M. Underwood  (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.0, 20-OCT-2015 (NJB) (BVS) (WLT) (IMU)
      module subroutine zzbodvcd ( ctx, bodyid, item, maxn, varctr, n, values )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: bodyid  !! Body ID code.
      character(*), intent(in) :: item  !! Item for which values are desired.
      integer(ip), intent(in) :: maxn  !! Maximum number of values that may be returned.
      integer(ip), intent(inout) :: varctr ( ctrsiz )  !! POOL state counter saved by the caller.
      integer(ip), intent(inout) :: n  !! Number of values returned.
      real(wp), intent(inout) :: values ( * )  !! Array of double precision values.

      logical :: update

      if ( ctx%failed() ) return

!
!     See whether the kernel pool state has changed since the
!     user counter was set. Update the user counter if so.
!
      call zzpctrck ( ctx, varctr, update )

!
!     If the pool was updated, or if we're looking at a new variable,
!     update the kernel variable values, size, and found flag.
!     Otherwise do nothing.
!
      if ( update ) then

         call bodvcd ( ctx, bodyid, item, maxn, n, values )

         if ( ctx%failed() ) then

            n = 0

         end if

      end if

      return
      end subroutine zzbodvcd

      !> **Deprecated:** This routine has been superseded by [[bodvcd]] and
      !> [[bodvrd]]. This routine is supported for purposes of backward
      !> compatibility only.
      !>
      !> Return the values of some item for any body in the
      !> kernel pool.
      !>
      !>## Detailed Input
      !>
      !>     BODY     is the ID code of the body for which ITEM is
      !>              requested.
      !>
      !>     ITEM     is the item to be returned. Together, the body and
      !>              item name combine to form a variable name, e.g.,
      !>
      !>                 'BODY599_RADII'
      !>                 'BODY401_POLE_RA'
      !>
      !>## Detailed Output
      !>
      !>     DIM      is the number of values associated with the variable.
      !>
      !>     VALUES   are the values associated with the variable.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the requested item is not found, the error
      !>         SPICE(KERNELVARNOTFOUND) is signaled.
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
      !>     1) Retrieve the Earth's radii values from the kernel pool
      !>
      !>        Use the PCK kernel below to load the required triaxial
      !>        ellipsoidal shape model for the Earth.
      !>
      !>           pck00008.tpc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM BODVAR_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     SPICELIB functions
      !>C
      !>      LOGICAL               BODFND
      !>
      !>C
      !>C     Local constants.
      !>C
      !>      INTEGER               BODYID
      !>      PARAMETER           ( BODYID = 399 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      RADII  (3)
      !>
      !>      INTEGER               DIM
      !>
      !>      LOGICAL               FOUND
      !>
      !>C
      !>C     Load a PCK file.
      !>C
      !>      CALL FURNSH ( 'pck00008.tpc' )
      !>
      !>C
      !>C     Test if Earth's radii values exist in the
      !>C     kernel pool.
      !>C
      !>C     The procedure searches for the kernel variable
      !>C     BODY399_RADII.
      !>C
      !>      FOUND = BODFND( BODYID, 'RADII' )
      !>
      !>C
      !>C     If found, retrieve the values.
      !>C
      !>      IF ( FOUND ) THEN
      !>
      !>         CALL BODVAR ( BODYID, 'RADII', DIM, RADII )
      !>
      !>         WRITE(*,'(I3,A,3F11.3)') BODYID, ' RADII:', RADII(1),
      !>     .                            RADII(2), RADII(3)
      !>
      !>      ELSE
      !>
      !>         WRITE(*,*) 'No RADII data found for object ', BODYID
      !>
      !>      END IF
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        399 RADII:   6378.140   6378.140   6356.750
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
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.6, 17-JUN-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard. Added
      !>        complete code example. Updated input argument BODY
      !>        detailed description. Added SPK to the list of required
      !>        readings.
      !>
      !>-    SPICELIB Version 1.0.5, 18-MAY-2010 (BVS)
      !>
      !>        Index lines now state that this routine is deprecated.
      !>
      !>-    SPICELIB Version 1.0.4, 27-OCT-2005 (NJB)
      !>
      !>        Routine is now deprecated.
      !>
      !>-    SPICELIB Version 1.0.3, 08-JAN-2004 (EDW)
      !>
      !>        Trivial typo corrected.
      !>
      !>-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.1, 08-AUG-1990 (HAN)
      !>
      !>        Detailed Input section of the header was updated. The
      !>        description for the variable BODY was incorrect.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
      module subroutine bodvar ( ctx, body, item, dim, values )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: intstr, suffix
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: body  !! ID code of body.
      character(*), intent(in) :: item  !! Item for which values are desired.
      integer(ip), intent(out) :: dim  !! Number of values returned.
      real(wp), intent(out) :: values  ( * )  !! Values.

      character(16) :: code
      character(32) :: varnam
      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Construct the variable name from BODY and ITEM.
!
      varnam = 'BODY'

      call intstr ( body, code )
      call suffix ( code, 0, varnam )
      call suffix ( '_',  0, varnam )
      call suffix ( item, 0, varnam )

!
!     Grab the items. Complain if they aren't there.
!
      call rtpool ( ctx, varnam, dim, values, found )

      if ( .not. found ) then
         call ctx%setmsg ( 'The variable # could not be found in the ' // &
                       'kernel pool.'                             )
         call ctx%errch  ( '#', varnam                                )
         call ctx%sigerr ( 'SPICE(KERNELVARNOTFOUND)'                 )
      end if

      return
      end subroutine bodvar

      !> Fetch from the kernel pool the double precision values
      !> of an item associated with a body, where the body is
      !> specified by an integer ID code.
      !>
      !>## Detailed Input
      !>
      !>     BODYID   is the NAIF integer ID code for a body of interest.
      !>              For example, if the body is the earth, the code is
      !>              399.
      !>
      !>     ITEM     is the item to be returned. Together, the NAIF ID
      !>              code of the body and the item name combine to form a
      !>              kernel variable name, e.g.,
      !>
      !>                    'BODY599_RADII'
      !>                    'BODY401_POLE_RA'
      !>
      !>              The values associated with the kernel variable having
      !>              the name constructed as shown are sought. Below
      !>              we'll take the shortcut of calling this kernel variable
      !>              the "requested kernel variable."
      !>
      !>              Note that ITEM *is* case-sensitive. This attribute
      !>              is inherited from the case-sensitivity of kernel
      !>              variable names.
      !>
      !>     MAXN     is the maximum number of values that may be returned.
      !>              The output array VALUES must be declared with size at
      !>              least MAXN. It's an error to supply an output array
      !>              that is too small to hold all of the values associated
      !>              with the requested kernel variable.
      !>
      !>## Detailed Output
      !>
      !>     DIM      is the number of values returned; this is always the
      !>              number of values associated with the requested kernel
      !>              variable unless an error has been signaled.
      !>
      !>     VALUES   is the array of values associated with the requested
      !>              kernel variable. If VALUES is too small to hold all
      !>              of the values associated with the kernel variable, the
      !>              returned values of DIM and VALUES are undefined.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the requested kernel variable is not found in the kernel
      !>         pool, the error SPICE(KERNELVARNOTFOUND) is signaled.
      !>
      !>     2)  If the requested kernel variable is found but the associated
      !>         values aren't numeric, the error SPICE(TYPEMISMATCH) is
      !>         signaled.
      !>
      !>     3)  If the dimension of VALUES indicated by MAXN is too small to
      !>         contain the requested values, the error SPICE(ARRAYTOOSMALL)
      !>         is signaled. The output array VALUES must be declared with
      !>         sufficient size to contain all of the values associated with
      !>         the requested kernel variable.
      !>
      !>     4)  If the input dimension MAXN indicates there is more room
      !>         in VALUES than there really is---for example, if MAXN is
      !>         10 but values is declared with dimension 5---and the dimension
      !>         of the requested kernel variable is larger than the actual
      !>         dimension of VALUES, then this routine may overwrite
      !>         memory. The results are unpredictable.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine simplifies looking up PCK kernel variables by
      !>     constructing names of requested kernel variables and by
      !>     performing error checking.
      !>
      !>     This routine is intended for use in cases where the maximum number
      !>     of values that may be returned is known at compile time. The
      !>     caller fetches all of the values associated with the specified
      !>     kernel variable via a single call to this routine. If the number
      !>     of values to be fetched cannot be known until run time, the
      !>     lower-level routine GDPOOL should be used instead. GDPOOL
      !>     supports fetching arbitrary amounts of data in multiple "chunks."
      !>
      !>     This routine is intended for use in cases where the requested
      !>     kernel variable is expected to be present in the kernel pool. If
      !>     the variable is not found or has the wrong data type, this
      !>     routine signals an error. In cases where it is appropriate to
      !>     indicate absence of an expected kernel variable by returning a
      !>     boolean "found flag" with the value .FALSE., again the routine
      !>     GDPOOL should be used.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Retrieve the radii of the Earth from the kernel pool, using
      !>        both 'RADII' and 'radii' as the item name to return. Since
      !>        the ITEM variable possesses case sensitivity, the later case
      !>        should fail. Trap the error and print it to the output.
      !>
      !>        Use the PCK kernel below to load the required triaxial
      !>        ellipsoidal shape model for the Earth.
      !>
      !>           pck00008.tpc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM BODVCD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NVALS
      !>      PARAMETER           ( NVALS = 3 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      VALUES (NVALS)
      !>
      !>      INTEGER               DIM
      !>
      !>C
      !>C     Load a PCK.
      !>C
      !>      CALL FURNSH ( 'pck00008.tpc' )
      !>
      !>C
      !>C     When the kernel variable
      !>C
      !>C        BODY399_RADII
      !>C
      !>C     is present in the kernel pool---normally because a PCK
      !>C     defining this variable has been loaded (as is the case
      !>C     here)---the call
      !>C
      !>      CALL BODVCD ( 399, 'RADII', 3, DIM, VALUES )
      !>
      !>C
      !>C     returns the dimension and values associated with the
      !>C     variable 'BODY399_RADII'
      !>C
      !>      WRITE(*,'(A,3F10.3)') '399 RADII: ', VALUES
      !>
      !>C
      !>C     The ITEM variable possesses case sensitivity. This
      !>C     call should cause an error.
      !>C
      !>      CALL BODVRD ( 'EARTH', 'radii', 3, DIM, VALUES )
      !>      WRITE(*,'(A,3F10.3)') '399 radii: ', VALUES
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        399 RADII:   6378.140  6378.140  6356.750
      !>
      !>        ============================================================***
      !>
      !>        Toolkit version: N0066
      !>
      !>        SPICE(KERNELVARNOTFOUND) -- The Variable Was not Found in th***
      !>        Pool.
      !>
      !>        The variable BODY399_radii could not be found in the kernel ***
      !>
      !>        A traceback follows.  The name of the highest level module i***
      !>        BODVRD
      !>
      !>        Oh, by the way:  The SPICELIB error handling actions are USER-
      !>        TAILORABLE.  You can choose whether the Toolkit aborts or co***
      !>        when errors occur, which error messages to output, and where***
      !>        the output.  Please read the ERROR "Required Reading" file, ***
      !>        the routines ERRACT, ERRDEV, and ERRPRT.
      !>
      !>        ============================================================***
      !>
      !>        Warning: incomplete output. 8 lines extended past the right
      !>        margin of the header and have been truncated. These lines are
      !>        marked by "***" at the end of each line.
      !>
      !>        Note that, usually, the last call will cause a
      !>        SPICE(KERNELVARNOTFOUND) error to be signaled, because this
      !>        call will attempt to look up the values associated with a
      !>        kernel variable of the name
      !>
      !>           'BODY399_radii'
      !>
      !>        Since kernel variable names are case sensitive, this
      !>        name is not considered to match the name
      !>
      !>           'BODY399_RADII'
      !>
      !>        which normally would be present after a text PCK
      !>        containing data for all planets and satellites has
      !>        been loaded.
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.1, 07-SEP-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete example code based on the existing fragments.
      !>
      !>        Removed note about GDPOOL being entry point of POOL from
      !>        $Particulars section.
      !>
      !>-    SPICELIB Version 1.0.0, 24-OCT-2004 (NJB) (BVS) (WLT) (IMU)
      module subroutine bodvcd ( ctx, bodyid, item, maxn, dim, values )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: intstr, suffix
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: bodyid  !! Body ID code.
      character(*), intent(in) :: item  !! Item for which values are desired. ('RADII',
                                        !! 'NUT_PREC_ANGLES', etc. )
      integer(ip), intent(in) :: maxn  !! Maximum number of values that may be returned.
      integer(ip), intent(out) :: dim  !! Number of values returned.
      real(wp), intent(out) :: values  ( * )  !! Values.



      character(cdelen) :: code
      character(1) :: type
      character(namlen) :: varnam

      logical :: found

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Construct the variable name from BODY and ITEM.
!
      varnam = 'BODY'

      call intstr ( bodyid,    code   )
      call suffix ( code,   0, varnam )
      call suffix ( '_',    0, varnam )
      call suffix ( item,   0, varnam )

!
!     Make sure the item is present in the kernel pool.
!
      call dtpool ( ctx, varnam, found, dim, type )

      if ( .not. found ) then

         call ctx%setmsg ( 'The variable # could not be found in the ' // &
                       'kernel pool.'                              )
         call ctx%errch  ( '#', varnam                                 )
         call ctx%sigerr ( 'SPICE(KERNELVARNOTFOUND)'                  )
         return

      end if

!
!     Make sure the item's data type is numeric.
!
      if ( type /= 'N' ) then

         call ctx%setmsg ( 'The data associated with variable # ' // &
                       'are not of numeric type.'             )
         call ctx%errch  ( '#', varnam                            )
         call ctx%sigerr ( 'SPICE(TYPEMISMATCH)'                  )
         return

      end if

!
!     Make sure there's enough room in the array VALUES to hold
!     the requested data.
!
      if ( maxn < dim ) then

         call ctx%setmsg ( 'The data array associated with variable # ' // &
                       'has dimension #, which is larger than the ' // &
                       'available space # in the output array.'    )
         call ctx%errch  ( '#', varnam                                 )
         call ctx%errint ( '#', dim                                    )
         call ctx%errint ( '#', maxn                                   )
         call ctx%sigerr ( 'SPICE(ARRAYTOOSMALL)'                      )
         return

      end if

!
!     Grab the values.  We know at this point they're present in
!     the kernel pool, so we don't check the FOUND flag.
!
      call gdpool ( ctx, varnam, 1, maxn, dim, values, found )

      return
      end subroutine bodvcd

      !> Fetch from the kernel pool the double precision values
      !> of an item associated with a body.
      !>
      !>## Detailed Input
      !>
      !>     BODYNM   is the name of the body for which ITEM is requested.
      !>              BODYNM is case-insensitive, and leading and trailing
      !>              blanks in BODYNM are not significant. Optionally, you
      !>              may supply the integer ID code for the object as an
      !>              integer string. For example both 'MOON' and '301' are
      !>              legitimate strings that indicate the moon is the body
      !>              of interest.
      !>
      !>     ITEM     is the item to be returned. Together, the NAIF ID
      !>              code of the body and the item name combine to form a
      !>              kernel variable name, e.g.,
      !>
      !>                    'BODY599_RADII'
      !>                    'BODY401_POLE_RA'
      !>
      !>              The values associated with the kernel variable having
      !>              the name constructed as shown are sought. Below
      !>              we'll take the shortcut of calling this kernel variable
      !>              the "requested kernel variable."
      !>
      !>              Note that ITEM *is* case-sensitive. This attribute
      !>              is inherited from the case-sensitivity of kernel
      !>              variable names.
      !>
      !>     MAXN     is the maximum number of values that may be returned.
      !>              The output array VALUES must be declared with size at
      !>              least MAXN. It's an error to supply an output array
      !>              that is too small to hold all of the values associated
      !>              with the requested kernel variable.
      !>
      !>## Detailed Output
      !>
      !>     DIM      is the number of values returned; this is always the
      !>              number of values associated with the requested kernel
      !>              variable unless an error has been signaled.
      !>
      !>     VALUES   is the array of values associated with the requested
      !>              kernel variable. If VALUES is too small to hold all
      !>              of the values associated with the kernel variable, the
      !>              returned values of DIM and VALUES are undefined.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the input body name cannot be translated to an ID code,
      !>         and if the name is not a string representation of an integer
      !>         (for example, '399'), the error SPICE(NOTRANSLATION) is
      !>         signaled.
      !>
      !>     2)  If the requested kernel variable is not found in the kernel
      !>         pool, the error SPICE(KERNELVARNOTFOUND) is signaled.
      !>
      !>     3)  If the requested kernel variable is found but the associated
      !>         values aren't numeric, the error SPICE(TYPEMISMATCH) is
      !>         signaled.
      !>
      !>     4)  If the dimension of VALUES indicated by MAXN is too small to
      !>         contain the requested values, the error SPICE(ARRAYTOOSMALL)
      !>         is signaled. The output array VALUES must be declared with
      !>         sufficient size to contain all of the values associated with
      !>         the requested kernel variable.
      !>
      !>     5)  If the input dimension MAXN indicates there is more room
      !>         in VALUES than there really is---for example, if MAXN is
      !>         10 but values is declared with dimension 5---and the dimension
      !>         of the requested kernel variable is larger than the actual
      !>         dimension of VALUES, then this routine may overwrite
      !>         memory. The results are unpredictable.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine simplifies looking up PCK kernel variables by
      !>     constructing names of requested kernel variables and by
      !>     performing error checking.
      !>
      !>     This routine is intended for use in cases where the maximum number
      !>     of values that may be returned is known at compile time. The
      !>     caller fetches all of the values associated with the specified
      !>     kernel variable via a single call to this routine. If the number
      !>     of values to be fetched cannot be known until run time, the
      !>     lower-level routine GDPOOL should be used instead. GDPOOL supports
      !>     fetching arbitrary amounts of data in multiple "chunks."
      !>
      !>     This routine is intended for use in cases where the requested
      !>     kernel variable is expected to be present in the kernel pool. If
      !>     the variable is not found or has the wrong data type, this
      !>     routine signals an error. In cases where it is appropriate to
      !>     indicate absence of an expected kernel variable by returning a
      !>     boolean "found flag" with the value .FALSE., again the routine
      !>     GDPOOL should be used.
      !>
      !>## Examples
      !>
      !>     The numerical results shown for this example may differ across
      !>     platforms. The results depend on the SPICE kernels used as
      !>     input, the compiler and supporting libraries, and the machine
      !>     specific arithmetic implementation.
      !>
      !>     1) Retrieve the radii of the Earth from the kernel pool, using
      !>        both 'RADII' and 'radii' as the item name to return. Since
      !>        the ITEM variable possesses case sensitivity, the later case
      !>        should fail. Trap the error and print it to the output.
      !>
      !>        Use the PCK kernel below to load the required triaxial
      !>        ellipsoidal shape model for the Earth.
      !>
      !>           pck00008.tpc
      !>
      !>        Example code begins here.
      !>
      !>```fortran
      !>      PROGRAM BODVRD_EX1
      !>      IMPLICIT NONE
      !>
      !>C
      !>C     Local parameters.
      !>C
      !>      INTEGER               NVALS
      !>      PARAMETER           ( NVALS = 3 )
      !>
      !>C
      !>C     Local variables.
      !>C
      !>      DOUBLE PRECISION      VALUES (NVALS)
      !>
      !>      INTEGER               DIM
      !>
      !>C
      !>C     Load a PCK.
      !>C
      !>      CALL FURNSH ( 'pck00008.tpc' )
      !>
      !>C
      !>C     When the kernel variable
      !>C
      !>C        BODY399_RADII
      !>C
      !>C     is present in the kernel pool---normally because a PCK
      !>C     defining this variable has been loaded (as is the case
      !>C     here)---the call
      !>C
      !>      CALL BODVRD ( 'EARTH', 'RADII', 3, DIM, VALUES )
      !>
      !>C
      !>C     returns the dimension and values associated with the
      !>C     variable 'BODY399_RADII'
      !>C
      !>      WRITE(*,'(A,3F10.3)') 'EARTH RADII: ', VALUES
      !>
      !>C
      !>C     The call
      !>C
      !>      CALL BODVRD ( 'earth', 'RADII', 3, DIM, VALUES )
      !>
      !>C
      !>C     will produce the same results since the case of the
      !>C     input argument BODYNM is not significant.
      !>C
      !>      WRITE(*,'(A,3F10.3)') 'earth RADII: ', VALUES
      !>
      !>C
      !>C     The call
      !>C
      !>      CALL BODVRD ( '399', 'RADII', 3, DIM, VALUES )
      !>
      !>C
      !>C     will also produce the same results since strings
      !>C     containing integer codes are accepted
      !>C     by this routine.
      !>C
      !>      WRITE(*,'(A,3F10.3)') '''399'' RADII: ', VALUES
      !>
      !>C
      !>C     The ITEM variable possesses case sensitivity. This
      !>C     call should cause an error.
      !>C
      !>      CALL BODVRD ( 'EARTH', 'radii', 3, DIM, VALUES )
      !>      WRITE(*,'(A,3F10.3)') 'EARTH radii: ', VALUES
      !>
      !>      END
      !>```
      !>
      !>        When this program was executed on a Mac/Intel/gfortran/64-bit
      !>        platform, the output was:
      !>
      !>        EARTH RADII:   6378.140  6378.140  6356.750
      !>        earth RADII:   6378.140  6378.140  6356.750
      !>        '399' RADII:   6378.140  6378.140  6356.750
      !>
      !>        ============================================================***
      !>
      !>        Toolkit version: N0066
      !>
      !>        SPICE(KERNELVARNOTFOUND) -- The Variable Was not Found in th***
      !>        Pool.
      !>
      !>        The variable BODY399_radii could not be found in the kernel ***
      !>
      !>        A traceback follows.  The name of the highest level module i***
      !>        BODVRD
      !>
      !>        Oh, by the way:  The SPICELIB error handling actions are USER-
      !>        TAILORABLE.  You can choose whether the Toolkit aborts or co***
      !>        when errors occur, which error messages to output, and where***
      !>        the output.  Please read the ERROR "Required Reading" file, ***
      !>        the routines ERRACT, ERRDEV, and ERRPRT.
      !>
      !>        ============================================================***
      !>
      !>        Warning: incomplete output. 8 lines extended past the right
      !>        margin of the header and have been truncated. These lines are
      !>        marked by "***" at the end of each line.
      !>
      !>        Note that, usually, the last call will cause a
      !>        SPICE(KERNELVARNOTFOUND) error to be signaled, because this
      !>        call will attempt to look up the values associated with a
      !>        kernel variable of the name
      !>
      !>           'BODY399_radii'
      !>
      !>        Since kernel variable names are case sensitive, this
      !>        name is not considered to match the name
      !>
      !>           'BODY399_RADII'
      !>
      !>        which normally would be present after a text PCK
      !>        containing data for all planets and satellites has
      !>        been loaded.
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
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.2.1, 27-AUG-2021 (JDR)
      !>
      !>        Edited the header to comply with NAIF standard.
      !>        Added complete example code based on the existing fragments.
      !>
      !>        Removed note about GDPOOL being entry point of POOL from
      !>        $Particulars section.
      !>
      !>-    SPICELIB Version 1.2.0, 19-SEP-2013 (BVS)
      !>
      !>        Updated to save the input body name and ZZBODTRN state counter
      !>        and to do name-ID conversion only if the counter has changed.
      !>
      !>-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB)
      !>
      !>        Updated to use BODS2C.
      !>
      !>-    SPICELIB Version 1.0.0, 23-FEB-2004 (NJB) (BVS) (WLT) (IMU)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB)
      !>
      !>        Updated to use BODS2C. This simplifies the name-to-ID
      !>        mapping code.
      module subroutine bodvrd ( ctx, bodynm, item, maxn, dim, values )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: intstr, suffix
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: bodynm  !! Body name.
      character(*), intent(in) :: item  !! Item for which values are desired. ('RADII',
                                        !! 'NUT_PREC_ANGLES', etc. )
      integer(ip), intent(in) :: maxn  !! Maximum number of values that may be returned.
      integer(ip), intent(out) :: dim  !! Number of values returned.
      real(wp), intent(out) :: values  ( * )  !! Values.
!
!     Local variables
!
      character(cdelen) :: code
      character(1) :: type
      character(namlen) :: varnam
      integer(ip) :: bodyid
      logical :: found
      if ( .not. allocated ( ctx%bodvrd_cache ) ) allocate ( ctx%bodvrd_cache )
      associate ( svctr1 => ctx%bodvrd_cache%svctr1, &
                  svbdnm => ctx%bodvrd_cache%svbdnm, &
                  svbdid => ctx%bodvrd_cache%svbdid, &
                  svfnd1 => ctx%bodvrd_cache%svfnd1, &
                  first => ctx%bodvrd_cache%first )
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( ctx, svctr1 )

         first = .false.

      end if

!
!     Translate the input name to an ID code.
!
      call zzbods2c ( ctx, svctr1, svbdnm, svbdid, svfnd1, &
                      bodynm, bodyid, found    )

      if ( .not. found ) then

         call ctx%setmsg ( 'The body name # could not be translated ' // &
                       'to a NAIF ID code.  The cause of this '   // &
                       'problem may be that you need an updated ' // &
                       'version of the SPICE Toolkit.'            )
         call ctx%errch  ( '#', bodynm                                )
         call ctx%sigerr ( 'SPICE(NOTRANSLATION)'                     )
         return

      end if

!
!     Construct the variable name from BODY and ITEM.
!
      varnam = 'BODY'

      call intstr ( bodyid,    code   )
      call suffix ( code,   0, varnam )
      call suffix ( '_',    0, varnam )
      call suffix ( item,   0, varnam )

!
!     Make sure the item is present in the kernel pool.
!
      call dtpool ( ctx, varnam, found, dim, type )

      if ( .not. found ) then

         call ctx%setmsg ( 'The variable # could not be found in the ' // &
                       'kernel pool.'                              )
         call ctx%errch  ( '#', varnam                                 )
         call ctx%sigerr ( 'SPICE(KERNELVARNOTFOUND)'                  )
         return

      end if

!
!     Make sure the item's data type is numeric.
!
      if ( type /= 'N' ) then

         call ctx%setmsg ( 'The data associated with variable # ' // &
                       'are not of numeric type.'             )
         call ctx%errch  ( '#', varnam                            )
         call ctx%sigerr ( 'SPICE(TYPEMISMATCH)'                  )
         return

      end if

!
!     Make sure there's enough room in the array VALUES to hold
!     the requested data.
!
      if ( maxn < dim ) then

         call ctx%setmsg ( 'The data array associated with variable # ' // &
                       'has dimension #, which is larger than the ' // &
                       'available space # in the output array.'    )
         call ctx%errch  ( '#', varnam                                 )
         call ctx%errint ( '#', dim                                    )
         call ctx%errint ( '#', maxn                                   )
         call ctx%sigerr ( 'SPICE(ARRAYTOOSMALL)'                      )
         return

      end if

!
!     Grab the values.  We know at this point they're present in
!     the kernel pool, so we don't check the FOUND flag.
!
      call gdpool ( ctx, varnam, 1, maxn, dim, values, found )

      return
      end associate
      end subroutine bodvrd

      !> All the values of a body constant, the body given by name (PEPPER;
      !> [[bodvrd]] with the size taken from the pool). Bound as the generic
      !> `body_values`.
      !>
      !> The errors are those of [[bodvrd]], except SPICE(ARRAYTOOSMALL).
      module subroutine body_values_by_name ( ctx, body, item, values )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: body  !! the body name (or ID code as a string)
      character(*), intent(in) :: item  !! the item, e.g. 'RADII' (the variable BODYnnn_<item>)
      real(wp), allocatable, intent(out) :: values(:)  !! the values (size 0 after an error)
      integer(ip) :: code
      logical :: found
      if ( ctx%failed() ) then
         allocate ( values(0) )
         return
      end if
      call bods2c ( ctx, body, code, found )
      if ( .not. found ) then
         allocate ( values(0) )
         if ( ctx%failed() ) return
         call ctx%setmsg ( 'The body name # could not be translated ' // &
                       'to a NAIF ID code.  The cause of this '   // &
                       'problem may be that you need an updated ' // &
                       'version of the SPICE Toolkit.'            )
         call ctx%errch  ( '#', body                                  )
         call ctx%sigerr ( 'SPICE(NOTRANSLATION)'                     )
         return
      end if
      call body_values_by_code ( ctx, code, item, values )
      end subroutine body_values_by_name

      !> All the values of a body constant, the body given by its ID code
      !> (PEPPER; [[bodvcd]] with the size taken from the pool). Bound as the
      !> generic `body_values`.
      !>
      !> The errors are those of [[bodvcd]], except SPICE(ARRAYTOOSMALL).
      module subroutine body_values_by_code ( ctx, body, item, values )
      use pepper_kinds, only: ip
      use pepper_strings, only: intstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: body  !! the body ID code
      character(*), intent(in) :: item  !! the item, e.g. 'RADII' (the variable BODYnnn_<item>)
      real(wp), allocatable, intent(out) :: values(:)  !! the values (size 0 after an error)
      character(16) :: code
      character(:), allocatable :: varnam
      character(1) :: type
      integer(ip) :: n
      logical :: found
      if ( ctx%failed() ) then
         allocate ( values(0) )
         return
      end if
!
!     The variable BODYnnn_<item>, of any length (BODVCD's name is cut
!     at 32 characters).
!
      call intstr ( body, code )
      varnam = 'BODY' // trim(code) // '_' // trim(item)
      call dtpool ( ctx, varnam, found, n, type )
      if ( .not. found ) then
         allocate ( values(0) )
         call ctx%setmsg ( 'The variable # could not be found in the ' // &
                       'kernel pool.'                              )
         call ctx%errch  ( '#', varnam                                 )
         call ctx%sigerr ( 'SPICE(KERNELVARNOTFOUND)'                  )
         return
      end if
      if ( type /= 'N' ) then
         allocate ( values(0) )
         call ctx%setmsg ( 'The data associated with variable # ' // &
                       'are not of numeric type.'             )
         call ctx%errch  ( '#', varnam                            )
         call ctx%sigerr ( 'SPICE(TYPEMISMATCH)'                  )
         return
      end if
      call pool_values ( ctx, varnam, values, found )
      end subroutine body_values_by_code

      !> Add a name/code pair to the defined names (SPICELIB's [[zzboddef]]). The
      !> pair is recorded in the instance, so that all its contexts see it.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzboddef ( ctx, name, code )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Common name of some body.
      integer(ip), intent(in) :: code  !! Integer code for that body.
      call sync_boddefs ( ctx )
      if ( ctx%failed() ) return
      call zzboddef_apply ( ctx, name, code )
      if ( ctx%failed() ) return
      call record_boddef ( ctx%spice, name, code, 0_ip )
      ctx%zzbodtrn%boddef_epoch = ctx%spice%boddef_epoch
      ctx%zzbodtrn%boddef_count = ctx%spice%boddef_count
      end subroutine zzboddef

      !> Add a body name/code pair defined by a kernel built in memory, owned
      !> by it (its handle): BODDEF's definition, removed with
      !> [[remove_owned_boddefs]] when the kernel is unloaded (PEPPER).
      module subroutine owned_boddef ( ctx, name, code, owner )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! the body's name
      integer(ip), intent(in) :: code  !! its NAIF ID code
      integer(ip), intent(in) :: owner  !! the handle of the kernel that defines it
      call sync_boddefs ( ctx )
      if ( ctx%failed() ) return
      call zzboddef_apply ( ctx, name, code )
      if ( ctx%failed() ) return
      call record_boddef ( ctx%spice, name, code, owner )
      ctx%zzbodtrn%boddef_epoch = ctx%spice%boddef_epoch
      ctx%zzbodtrn%boddef_count = ctx%spice%boddef_count
      end subroutine owned_boddef

      !> Remove the body name/code pairs a kernel defined ([[owned_boddef]]):
      !> every context rebuilds its name tables from the built-in names and
      !> the definitions that remain, in their order (PEPPER).
      !>
      !> PEPPER: remove the pairs a kernel defined. The record loses them
      !> and the epoch changes while the count drops, so every context's
      !> next look (sync_boddefs) rebuilds its tables from the built-in
      !> names and the remaining definitions, in order: an earlier
      !> definition of the same name or code is in effect again.
      module subroutine remove_owned_boddefs ( ctx, owner )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      integer(ip), intent(in) :: owner  !! the handle of the kernel unloaded
      integer(ip) :: k, n
      associate ( s => ctx%spice )
         if ( s%boddef_count == 0 .or. owner == 0 ) return
         if ( .not. allocated ( s%boddef_owners ) ) return
         if ( .not. any ( s%boddef_owners(1:s%boddef_count) == owner ) ) return
         n = 0
         do k = 1, s%boddef_count
            if ( s%boddef_owners(k) == owner ) cycle
            n = n + 1
            if ( n /= k ) then
               call move_alloc ( s%boddef_names(k)%s, s%boddef_names(n)%s )
               s%boddef_codes(n) = s%boddef_codes(k)
               s%boddef_owners(n) = s%boddef_owners(k)
            end if
         end do
         s%boddef_count = n
         s%boddef_epoch = s%boddef_epoch + 1
      end associate
      end subroutine remove_owned_boddefs

      !> Reset the defined names to the built-in names (SPICELIB's
      !> [[zzbodrst]]), in the instance and so for all its contexts.
      !>
      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzbodrst ( ctx )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      ctx%spice%boddef_count = 0
      ctx%spice%boddef_epoch = ctx%spice%boddef_epoch + 1
      call zzbodrst_apply ( ctx )
      if ( ctx%failed() ) return
      ctx%zzbodtrn%boddef_epoch = ctx%spice%boddef_epoch
      ctx%zzbodtrn%boddef_count = 0
      end subroutine zzbodrst

      !> Bring this context's table of defined body names up to date with the
      !> instance: if names were defined or reset (through the instance) since
      !> this context last looked, apply the definitions made since, or after
      !> a reset rebuild the table from the built-in names and the instance's
      !> record of definitions.
      subroutine sync_boddefs ( ctx )
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip) :: k, first
      if ( allocated ( ctx%zzbodtrn ) ) then
         if ( ctx%zzbodtrn%boddef_epoch == ctx%spice%boddef_epoch ) return
      else if ( ctx%spice%boddef_epoch == 0 ) then
         return
      end if
!
!     The epoch changes by one for each definition and for each reset,
!     and a reset sets the count to zero: the definitions made since the
!     context last looked are all there is to apply when the epoch and
!     the count changed by the same amount.
!
      first = 1
      if ( allocated ( ctx%zzbodtrn ) ) then
         if (       ( ctx%zzbodtrn%boddef_count <= ctx%spice%boddef_count ) &
              .and. (   ctx%spice%boddef_epoch - ctx%zzbodtrn%boddef_epoch     &
                     == ctx%spice%boddef_count - ctx%zzbodtrn%boddef_count ) ) then
            first = ctx%zzbodtrn%boddef_count + 1
         end if
      end if
      if ( first == 1 ) call zzbodrst_apply ( ctx )
      do k = first, ctx%spice%boddef_count
         if ( ctx%failed() ) return
         call zzboddef_apply ( ctx, ctx%spice%boddef_names(k)%s, &
                               ctx%spice%boddef_codes(k) )
      end do
      ctx%zzbodtrn%boddef_epoch = ctx%spice%boddef_epoch
      ctx%zzbodtrn%boddef_count = ctx%spice%boddef_count
      end subroutine sync_boddefs

      !> Append a name/code pair to the instance's record of definitions.
      subroutine record_boddef ( spice, name, code, owner )
      use pepper_containers, only: grow
      type(pepper_instance), intent(inout) :: spice  !! the instance
      character(*), intent(in) :: name  !! common name of some body
      integer(ip), intent(in) :: code  !! integer code for that body
      integer(ip), intent(in) :: owner  !! PEPPER: the handle of the kernel that defines it (0: the program)
      integer(ip) :: n
      n = spice%boddef_count + 1
      call grow ( spice%boddef_names, n )
      call grow ( spice%boddef_codes, n )
      call grow ( spice%boddef_owners, n )
      spice%boddef_names(n)%s = trim ( name )
      spice%boddef_codes(n) = code
      spice%boddef_owners(n) = owner
      spice%boddef_count = n
      spice%boddef_epoch = spice%boddef_epoch + 1
      end subroutine record_boddef

      !> PEPPER: set the defined names to the built-in names ([[zzbodget]]),
      !> with their maps (SPICELIB's [[zzbodini]]).
      subroutine body_builtins ( ctx, def )
      use pepper_inc_zzbodtrn
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(body_names), intent(inout) :: def  !! the defined names
      character(maxl), allocatable :: names(:), nornam(:)
      integer(ip), allocatable :: codes(:)
      integer(ip) :: n, i
      allocate ( names(maxe), nornam(maxe), codes(maxe) )
      call zzbodget ( ctx, maxe, names, nornam, codes, n )
      call def%clear ( )
      if ( ctx%failed() ) return
      do i = 1, n
         call def%append ( trim ( names(i) ), trim ( nornam(i) ), codes(i) )
      end do
      call def%index ( )
      end subroutine body_builtins

end submodule pepper_bodies
