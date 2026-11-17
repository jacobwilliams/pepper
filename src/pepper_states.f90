!> The saved state of SPICELIB's routines: one derived type for each
!> routine (or umbrella routine with its entry points) whose saved
!> variables are real state. Each is an allocatable component of
!> [[pepper_instance]] (the state of the instance, e.g. the kernel pool) or of
!> [[pepper_context]] (the state of a computation in progress), allocated on
!> first use.
!>
!> The components start with SPICELIB's DATA values, and the others as
!> zero (SPICELIB relies on saved storage starting as zero). A type of
!> at most 16 KiB has default initialization. A larger one has a
!> routine init_<name>_state that sets the initial values after
!> allocate: for a type with default initialization, gfortran builds a
!> temporary of the whole type on the stack in allocate (and a template
!> of it in the executable).
!>
!> The parameters are private. Those that several states define with
!> different values have the name of the state as a prefix.
!> (tools/merge_states.py merged the modules pepper_state_<name> that
!> tools/modularize_ctx.py had generated.)
module pepper_states

   use, intrinsic :: iso_fortran_env, only: output_unit
   use, intrinsic :: iso_fortran_env, only: int64
   use pepper_kinds, only: ip, wp
   use pepper_containers, only: grow, string, string_map, int_map
   use pepper_segment_index, only: segment_index, segment_reuse
   use pepper_inc_das, only: ftsize
   use pepper_inc_dla, only: dladsz
   use pepper_inc_dskdsc, only: dskdsz, nsypar
   use pepper_inc_dsktol, only: angmrg, lonali, ptmemm, sgpadm, sgreed, xfract
   use pepper_inc_ekcnamsz, only: cnamsz
   use pepper_inc_ekcoldsc, only: cdscsz
   use pepper_inc_ekglimit, only: mxclsg
   use pepper_inc_ekpage, only: pgsizc, pgsizd, pgsizi
   use pepper_inc_ekquery, only: eqvdsz
   use pepper_inc_eksegdsc, only: sdscsz
   use pepper_inc_ektnamsz, only: tnamsz
   use pepper_inc_gf, only: annulr, any, full, noctyp, oclln, partl, shplen
   use pepper_inc_ninert, only: ninert
   use pepper_inc_nninrt, only: nninrt
   use pepper_inc_sclk, only: mxnfld, mxpart
   use pepper_inc_sgparam, only: mxmeta
   use pepper_inc_zzabcorr, only: corlen, nabcor
   use pepper_inc_zzbodtrn, only: maxe, maxl, nperm
   use pepper_inc_zzctr, only: ctrsiz
   use pepper_inc_zzddhman, only: numamh, numarc, numbff, strsiz, utsize
   use pepper_inc_zzdsk, only: acllen, ellshp, mthlen
   use pepper_inc_zzdyn, only: kvnmln, kvx, kvy, kvz, kwatol, kweac1, kweac2, kweac3
   use pepper_inc_zzftprms, only: sizdlm, sizend, sizftp, sizstr
   use pepper_inc_zzgf, only: cylsys, geosys, latsys, mxbegm, mxendm, pgrsys, radsys, recsys, sphsys
   use pepper_inc_zzholdd, only: nid
   use pepper_inc_zzsc01, only: dplen, lbsngl, nivals
   use pepper_inc_zzswtchf, only: maxbas, maxfrm

   implicit none

   private

   public :: init_dafana_state, dafana_grow, &
       init_dafec_state, init_daffa_state, daffa_grow, init_dafrwd_state, init_dasec_state, &
       init_dasrwr_state, init_ekqmgr_state, init_ekqmgr_context_state, &
       init_framex_state, init_limbpt_state, &
       init_sc01_state, &
       init_termpt_state, init_tisbod_state, init_tkfram_state, &
       init_zzbodblt_state, init_zzbodtrn_state, &
       init_zzdsksbf_state, init_zzekjtst_state, &
       init_zzeksca_state, init_zzelvupy_state, init_zzgffvu_state, &
       init_zzswfxfm_state

   ! parameters of several states
   integer(ip), parameter :: crdlen = 32
   integer(ip), parameter :: crlen = 1000
   integer(ip), parameter :: errlen = 240
   integer(ip), parameter :: evlfln = 25
   integer(ip), parameter :: filsiz = 255
   integer(ip), parameter :: lbcell = -5
   integer(ip), parameter :: lbpool = -5
   integer(ip), parameter :: maxivl = 1000
   integer(ip), parameter :: mrklen = 10
   integer(ip), parameter :: mxphas = 4
   integer(ip), parameter :: mxspec = 512
   integer(ip), parameter :: nc = 7
   integer(ip), parameter :: nevflg = 3
   integer(ip), parameter :: nflags = 9
   integer(ip), parameter :: nil = -1
   integer(ip), parameter :: nsys = 7
   integer(ip), parameter :: pstftp = 297
   integer(ip), parameter :: sidlen = 40
   integer(ip), parameter :: slen = 40
   integer(ip), parameter :: typlen = 8

   ! ------------------------------------------------------------------ ANA

   integer(ip), parameter :: ana_wdsize = 32

   !> Return the correct article "a" or "an" used to modify a word
   !> and return it capitalized, lower case, or upper case.
   !>
   !> The saved variables of SPICELIB's [[ana]] routines (entry points ana), held
   !> in [[pepper_context]] (one per thread).
   type, public :: ana_state
      character(ana_wdsize) :: start ( 7 ) = ' '  !! work: the first 1 to 7 characters of the input word
   end type ana_state

   ! ------------------------------------------------------------------ AZLCPO

   !> Return the azimuth/elevation coordinates of a specified target
   !> relative to an "observer," where the observer has constant
   !> position in a specified reference frame. The observer's position
   !> is provided by the calling program rather than by loaded SPK
   !> files.
   !>
   !> The saved variables of SPICELIB's [[azlcpo]] routines (entry points azlcpo),
   !> held in [[pepper_context]] (one per thread).
   type, public :: azlcpo_state
      real(wp) :: z ( 3    ) = [ 0.00_wp, 0.00_wp, 1.00_wp ]  !! constant +Z unit vector (0,0,1)
   end type azlcpo_state

   ! ------------------------------------------------------------------ BLTFRM

   integer(ip), parameter :: nframe = ninert + nninrt

   !> Return a set containing the frame IDs of all built-in frames of a
   !> specified class.
   !>
   !> The saved variables of SPICELIB's [[bltfrm]] routines (entry points bltfrm),
   !> held in [[pepper_context]] (one per thread).
   type, public :: bltfrm_state
      integer(ip) :: fclass ( nframe ) = 0  !! frame classes of the built-in frames
      integer(ip) :: fcode ( nframe ) = 0  !! frame ID codes of the built-in frames
      logical :: pass1 = .true.  !! first call: the built-in frame tables not yet loaded ([[zzfdat]])
   end type bltfrm_state

   ! ------------------------------------------------------------------ BODEUL

   !> Return the Euler angles needed to compute the transformation from
   !> inertial to body-fixed coordinates for any body in the kernel
   !> pool.
   !>
   !> The saved variables of SPICELIB's [[bodeul]] routines (entry points bodeul),
   !> held in [[pepper_context]] (one per thread).
   type, public :: bodeul_state
      integer(ip) :: j2code = 0  !! inertial frame code ([[irfnum]]) of J2000
      integer(ip) :: nphase = 0  !! number of nutation/precession angles (phase polynomials) last loaded
      integer(ip) :: nphsco = 0  !! number of coefficients per nutation/precession angle (max phase degree + 1)
      logical :: first = .true.  !! first call: J2000 frame code not yet looked up
   end type bodeul_state

   ! ------------------------------------------------------------------ BODVRD

   integer(ip), parameter :: bodvrd_maxl = 36

   !> Fetch from the kernel pool the double precision values
   !> of an item associated with a body.
   !>
   !> The saved variables of SPICELIB's [[bodvrd]] routines (entry points bodvrd),
   !> held in [[pepper_context]] (one per thread).
   type, public :: bodvrd_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]])
      character(bodvrd_maxl) :: svbdnm = ' '  !! the body name translated last
      integer(ip) :: svbdid = 0  !! its NAIF ID code
      logical :: svfnd1 = .false.  !! the name was found
      logical :: first = .true.  !! no name has been translated yet
   end type bodvrd_state

   ! ------------------------------------------------------------------ CHGIRF

   integer(ip), parameter :: maxf = ninert

   !> Support changes among a standard set of inertial coordinate
   !> reference frames.
   !>
   !> The saved variables of SPICELIB's CHGIRF routines (entry points irfrot,
   !> irfnum, irfnam, irfdef), held in [[pepper_instance]] (and, for some variables,
   !> in [[pepper_context]]).
   type, public :: chgirf_state
      integer(ip) :: dframe = 0  !! the default inertial frame (set by [[irfdef]]; 0: none)
   end type chgirf_state

   !> The saved variables of CHGIRF that are per context.
   type, public :: chgirf_context_state
      logical :: ready = .false.  !! the rotations have been computed
      real(wp) :: trans ( 9, maxf ) = 0.0_wp  !! rotation (3x3, by columns) from J2000 to each frame
   end type chgirf_context_state

   ! ------------------------------------------------------------------ CKBSR

   !> Load and unload files for use by the readers. Buffer segments
   !> for readers.
   !>
   !> SPICELIB's CKBSR (entry points cklpf, ckupf, ckbss, cksns, ckhave):
   !> the loaded CK files and the index of their segments by instrument,
   !> held in [[pepper_instance]].
   !>
   !> PEPPER: SPICELIB found an instrument's segments when it was first
   !> searched for, reading the summaries of every loaded file, and kept
   !> them in per-context tables of 5000 instruments and 100000 segments
   !> (9.9 MB per context). The segments are indexed when a file is loaded.
   type, public :: ckbsr_state
      type(segment_index) :: index  !! the segments of the loaded files, by instrument
   end type ckbsr_state

   !> The per-context state of CKBSR: the search in progress ([[ckbss]],
   !> [[cksns]]), and the segment returned last for each instrument.
   type, public :: ckbsr_context_state
      type(segment_reuse) :: reuse  !! the segment returned last for each instrument, and its re-use interval
      logical :: active = .false.  !! a search was begun by [[ckbss]] (and no file was loaded or unloaded since)
      integer(ip) :: epoch = -1  !! the instance's segment_epoch at [[ckbss]]
      integer(ip) :: scinst = 0  !! the instrument of the search
      real(wp) :: reqt = 0.0_wp  !! the request time (encoded spacecraft clock) of the search
      real(wp) :: savtol = 0.0_wp  !! the time tolerance of the search
      real(wp) :: alpha = 0.0_wp  !! start of the interval of interest (request time minus tolerance)
      real(wp) :: omega = 0.0_wp  !! end of the interval of interest (request time plus tolerance)
      logical :: avneed = .false.  !! the search requires angular velocity
      logical :: newsch = .false.  !! no segment was returned yet by this search
      logical :: fresh = .false.  !! the re-use interval was not checked yet by this search ([[cksns]] checks it once, at its first call)
      integer(ip) :: list = 0  !! the instrument's segment list in the index
      integer(ip) :: pos = 0  !! the position in the list to continue from (searched downwards)
      real(wp) :: lb = 0.0_wp  !! lower bound of the re-use interval being built
      real(wp) :: ub = 0.0_wp  !! upper bound of the re-use interval being built
   end type ckbsr_context_state

   ! ------------------------------------------------------------------ CKGP

   integer(ip), parameter :: ckgp_frnmln = 32

   !> Get pointing (attitude) for a specified spacecraft clock time.
   !>
   !> The saved variables of SPICELIB's [[ckgp]] routines (entry points ckgp),
   !> held in [[pepper_context]] (one per thread).
   type, public :: ckgp_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(ckgp_frnmln) :: svref = ' '  !! name of the reference frame looked up last ([[zznamfrm]])
      integer(ip) :: svrefr = 0  !! ID code of the reference frame looked up last
      logical :: first = .true.  !! first call: the pool counter is not yet initialized
   end type ckgp_state

   ! ------------------------------------------------------------------ CKGPAV

   integer(ip), parameter :: ckgpav_frnmln = 32

   !> Get pointing (attitude) and angular velocity for a specified
   !> spacecraft clock time.
   !>
   !> The saved variables of SPICELIB's [[ckgpav]] routines (entry points ckgpav),
   !> held in [[pepper_context]] (one per thread).
   type, public :: ckgpav_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(ckgpav_frnmln) :: svref = ' '  !! name of the reference frame looked up last ([[zznamfrm]])
      integer(ip) :: svrefr = 0  !! ID code of the reference frame looked up last
      logical :: first = .true.  !! first call: the pool counter is not yet initialized
   end type ckgpav_state

   ! ------------------------------------------------------------------ CKMETA

   integer(ip), parameter :: ckmeta_wdsize = 32
   integer(ip), parameter :: nck = 30

   !> Return (depending upon the user's request) the ID code of either
   !> the spacecraft or spacecraft clock associated with a C-Kernel ID
   !> code.
   !>
   !> The saved variables of SPICELIB's [[ckmeta]] routines (entry points ckmeta),
   !> held in [[pepper_context]] (one per thread).
   type, public :: ckmeta_state
      character(ckmeta_wdsize) :: lookup ( 2, nck ) = ' '  !! names of the kernel variables CK_<id>_SCLK and CK_<id>_SPK of each buffered CK ID
      character(ckmeta_wdsize) :: agent (    nck ) = ' '  !! kernel pool watcher (agent) name of each buffered CK ID
      integer(ip) :: cks (    nck ) = 0  !! buffered CK IDs
      integer(ip) :: cksord (    nck ) = 0  !! order vector of cks (for the binary search)
      integer(ip) :: currnt = 0  !! number of buffered CK IDs
      integer(ip) :: last = 0  !! buffer slot filled last (slots are reused round-robin when the buffer is full)
      integer(ip) :: sclks (    nck ) = 0  !! SCLK ID of each buffered CK ID
      integer(ip) :: spks (    nck ) = 0  !! SPK ID of each buffered CK ID
      integer(ip) :: usrctr ( ctrsiz, nck ) = 0  !! kernel pool state counter of each buffered CK ID
      logical :: first = .true.  !! this is the first call (the counters and agents are not initialized yet)
      logical :: found (    2   ) = .false.  !! SCLK and SPK kernel variables were found (a work variable; not used between calls)
      logical :: nodata = .true.  !! the last kernel pool lookup failed (the buffered IDs must be fetched again)
   end type ckmeta_state

   ! ------------------------------------------------------------------ CKR03

   !> Read a pointing record from a CK segment, data type 3.
   !>
   !> The saved variables of SPICELIB's [[ckr03]] routines (entry points ckr03),
   !> held in [[pepper_context]] (one per thread).
   type, public :: ckr03_state
      real(wp) :: prevs = -1.0_wp  !! start time of the interpolation interval found on the last search
      real(wp) :: prevn = -1.0_wp  !! start time of the interpolation interval following the one starting at prevs
      integer(ip) :: lhand = 0  !! handle of the file of the segment searched last
      integer(ip) :: lbeg = -1  !! begin address of the segment searched last
      integer(ip) :: lend = -1  !! end address of the segment searched last
   end type ckr03_state

   ! ------------------------------------------------------------------ CKR05

   !> Read a single CK data record from a segment of type 05
   !> (MEX/Rosetta Attitude file interpolation).
   !>
   !> The saved variables of SPICELIB's [[ckr05]] routines (entry points ckr05),
   !> held in [[pepper_context]] (one per thread).
   type, public :: ckr05_state
      real(wp) :: prevs = -1.0_wp  !! start time of the interpolation interval found on the last search
      real(wp) :: prevn = -1.0_wp  !! start time of the interpolation interval following the one starting at prevs
      real(wp) :: prevnn = -1.0_wp  !! start time of the interpolation interval following the one starting at prevn
      integer(ip) :: lhand = 0  !! handle of the file of the segment searched last
      integer(ip) :: lbeg = -1  !! begin address of the segment searched last
      integer(ip) :: lend = -1  !! end address of the segment searched last
   end type ckr05_state

   ! ------------------------------------------------------------------ CKR06

   !> Read a single CK data record from a segment of type 6
   !> (ESOC/DDID Piecewise Interpolation).
   !>
   !> The saved variables of SPICELIB's [[ckr06]] routines (entry points ckr06),
   !> held in [[pepper_context]] (one per thread).
   type, public :: ckr06_state
      logical :: pass1 = .true.  !! this is the first call (the record size check has not been done yet)
      integer(ip) :: svbeg = -1  !! begin address of the segment used last
      real(wp) :: svbtim = 0.0_wp  !! start time of the mini-segment interval used last
      real(wp) :: svetim = -1.0_wp  !! end time of the mini-segment interval used last (at most its last epoch)
      logical :: svfnd = .false.  !! value of FOUND returned last (the saved values are valid only if true)
      integer(ip) :: svhan = 0  !! handle of the file of the segment used last
      logical :: svlast = .false.  !! interval selection flag of the segment used last (select the last applicable interval)
      integer(ip) :: svmiix = -1  !! index of the mini-segment (interval) used last
      integer(ip) :: svminb = -1  !! begin address of the mini-segment used last
      integer(ip) :: svn = -1  !! number of mini-segments in the segment used last
      integer(ip) :: svnpkt = -1  !! number of packets in the mini-segment used last
      integer(ip) :: svpkdb = -1  !! base address of the packet directory of the mini-segment used last
      integer(ip) :: svpknd = -1  !! number of packet directory entries of the mini-segment used last
      integer(ip) :: svpksz = -1  !! packet size of the mini-segment used last
      real(wp) :: svrate = -1.0_wp  !! nominal SCLK rate of the mini-segment used last
      integer(ip) :: svstyp = -1  !! subtype of the mini-segment used last
      integer(ip) :: svwnsz = -1  !! interpolation window size of the mini-segment used last
   end type ckr06_state

   ! ------------------------------------------------------------------ DAFAH

   !> Assign handles to DAFs as they are opened.
   !>
   !> The saved variables of SPICELIB's DAFAH routines (entry points dafopr,
   !> dafopw, dafonw, dafopn, dafcls, dafhsf, dafhlu, dafluh, dafhfn, daffnh,
   !> dafhof, dafsih), held in [[pepper_instance]].
   !>
   !> PEPPER: SPICELIB's file table was four parallel arrays of FTSIZE
   !> (5000) entries, plus a set (FHLIST) of the same handles; it is an
   !> array of these records that grows as DAFs are opened.
   type, public :: daf_file
      integer(ip) :: handle = 0  !! the handle
      integer(ip) :: links = 0  !! number of links (opens) of the DAF
      integer(ip) :: nd = 0  !! number of double precision components of the summaries
      integer(ip) :: ni = 0  !! number of integer components of the summaries
   end type daf_file

   !> The DAF file table: the open DAFs in increasing order of
   !> abs(handle), so that a handle is found by binary search.
   type, public :: dafah_state
      integer(ip) :: nft = 0  !! number of DAFs in the file table
      type(daf_file), allocatable :: files(:)  !! the DAFs (1 to nft are used)
   contains
      procedure :: find => daf_find
      procedure :: add => daf_add
      procedure :: remove => daf_remove
   end type dafah_state

   ! ------------------------------------------------------------------ DAFANA

   integer(ip), parameter :: dafana_tbsize = 4  !! PEPPER: initial size of the state table (it grows)
   integer(ip), parameter :: ifnlen = 60
   integer(ip), parameter :: maxsum = 125

   !> Add a new array to an existing DAF.
   !>
   !> The saved variables of SPICELIB's DAFANA routines (entry points dafbna,
   !> dafada, dafena, dafcad), held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization (41776 bytes): see [[init_dafana_state]].
   type, public :: dafana_state
      ! PEPPER: the state table starts with DAFANA_TBSIZE entries and grows
      ! (dafana_grow); SPICELIB had 20 and signaled SPICE(STFULL) when a
      ! 21st DAF was written to at the same time.
      integer(ip), allocatable :: stfh (:)  !! handle of each DAF being written
      character(ifnlen), allocatable :: stifnm (:)  !! internal file name of each DAF being
                                                             !! written
      logical, allocatable :: staddg (:)  !! an array is being added to each DAF (between [[dafbna]]
                                                   !! and [[dafena]])
      integer(ip), allocatable :: stfrst (:)  !! record number of the first summary record of each
                                                       !! DAF
      integer(ip), allocatable :: stlast (:)  !! record number of the last summary record of each
                                                       !! DAF
      integer(ip), allocatable :: stfree (:)  !! address of the next free word of each DAF
      integer(ip), allocatable :: stbegn (:)  !! beginning address of the new array of each DAF
      real(wp), allocatable :: stlsum (:, :)  !! summary of the new array of each DAF
      character(crlen), allocatable :: stname (:)  !! name of the new array of each DAF
      integer(ip), allocatable :: stpool (:)  !! linked-list pool of the state table (active and free
                                               !! entries)
      integer(ip) :: sthead  !! head of the list of active entries of the state table
      integer(ip) :: stfptr  !! head of the list of free entries of the state table
   end type dafana_state

   ! ------------------------------------------------------------------ DAFEC

   integer(ip), parameter :: dafec_mxcrec = 1000

   !> Extract comments from the comment area of a binary DAF.
   !>
   !> The saved variables of SPICELIB's [[dafec]] routines (entry points dafec),
   !> held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization (101016 bytes): see [[init_dafec_state]].
   type, public :: dafec_state
      ! PEPPER: the tables of files grow (SPICELIB: FTSIZE, 5000, entries).
      character(dafec_mxcrec) :: crecrd  !! the comment record read last
      character(1) :: eocmrk  !! the end-of-comments marker
      character(1) :: eolmrk  !! the end-of-line marker
      integer(ip), allocatable :: filchr (:)  !! the file table declarations for keeping track of which files
                                            !! are currently in the process of having comments extracted.
      integer(ip), allocatable :: filcnt (:)  !! number of comment lines read from each file
      integer(ip), allocatable :: filhan (:)  !! handles of the files whose comments are being read
      integer(ip) :: lsthan  !! the handle read last
      integer(ip), allocatable :: lstpos (:)  !! position in the last comment record of each file
      integer(ip), allocatable :: lstrec (:)  !! the comment record read last of each file
      integer(ip) :: nfiles  !! number of files whose comments are being read
      logical :: first  !! the tables have not been set up yet (they are set up on the first call)
   end type dafec_state

   ! ------------------------------------------------------------------ DAFFA

   integer(ip), parameter :: daffa_tbsize = 16  !! PEPPER: initial size of the state table (it grows)
   integer(ip), parameter :: dprsiz = 128

   !> Find arrays in a DAF.
   !>
   !> The saved variables of SPICELIB's DAFFA routines (entry points dafbfs,
   !> daffna, dafbbs, daffpa, dafgs, dafgn, dafgh, dafrs, dafrn, dafws,
   !> dafcs), held in [[pepper_context]] (one per thread).
   !>
   !> [[init_daffa_state]] allocates the state table (SPICELIB's was 10280016
   !> bytes per context).
   type, public :: daffa_state
      ! PEPPER: SPICELIB's state table had FTSIZE (5000) entries in each
      ! context (about 10 MB); it starts with DAFFA_TBSIZE entries and
      ! doubles when it is full of searches of open DAFs (daffa_grow).
      integer(ip), allocatable :: stfh (:)  !! handle of each DAF being searched
      integer(ip), allocatable :: stprev (:)  !! record number of the previous summary record of each search
      integer(ip), allocatable :: stthis (:)  !! record number of the current summary record of each search
      integer(ip), allocatable :: stnext (:)  !! record number of the next summary record of each search
      integer(ip), allocatable :: stnseg (:)  !! number of summaries in the current summary record of
                                              !! each search
      integer(ip), allocatable :: stcurr (:)  !! index of the current summary within its record, for
                                              !! each search
      character(crlen), allocatable :: stnr (:)  !! the name record read last, for each search
      logical, allocatable :: sthvnr (:)  !! the name record of the current array is buffered (stnr),
                                          !! for each search
      real(wp), allocatable :: stsr (:, :)  !! the summary record read last, for each search
                                            !! (dprsiz, entries)
      integer(ip), allocatable :: stpool (:)  !! linked-list pool of the state table (active and free
                                              !! entries)
      integer(ip) :: sthead = nil  !! head of the list of active entries of the state table
      integer(ip) :: stfptr = nil  !! head of the list of free entries of the state table
   end type daffa_state

   ! ------------------------------------------------------------------ DAFRWD

   integer(ip), parameter :: rbsize = 100

   !> Read, write, and rewrite double precision records to and
   !> from DAFs.
   !>
   !> The saved variables of SPICELIB's DAFRWD routines (entry points dafgdr,
   !> dafgsr, dafrdr, dafwdr, dafnrr), held in [[pepper_context]] (one per
   !> thread).
   !>
   !> No default initialization (103616 bytes): see [[init_dafrwd_state]].
   type, public :: dafrwd_state
      integer(ip) :: rbhan (      rbsize )  !! handle of the file of each buffered record
      integer(ip) :: rbrec (      rbsize )  !! record number of each buffered record
      integer(ip) :: rbreq (      rbsize )  !! request number of the last use of each buffered
                                            !! record (to replace the oldest)
      real(wp) :: rbdat ( 128, rbsize )  !! the buffered records
      integer(ip) :: rbnbr  !! number of records in the buffer
      integer(ip) :: nread  !! number of records read from files
      integer(ip) :: nreq  !! number of requests
   end type dafrwd_state

   ! ------------------------------------------------------------------ DAFWDA

   integer(ip), parameter :: bsize = 128

   !> Write or rewrite the double precision data bounded by two
   !> addresses within a DAF.
   !>
   !> The saved variables of SPICELIB's [[dafwda]] routines (entry points dafwda),
   !> held in [[pepper_context]] (one per thread).
   type, public :: dafwda_state
      real(wp) :: buffer ( bsize ) = 0.0_wp  !! work buffer of one record (saved in SPICELIB to keep it off
                                             !! the stack)
   end type dafwda_state

   ! ------------------------------------------------------------------ DASA2L

   integer(ip), parameter :: dasa2l_nwc = 1024
   integer(ip), parameter :: dasa2l_nwd = 128
   integer(ip), parameter :: dasa2l_nwi = 256
   integer(ip), parameter :: chrrng = 3
   integer(ip), parameter :: dprng = chrrng + 2
   integer(ip), parameter :: intrng = dprng  + 2
   integer(ip), parameter :: dasa2l_maxfil = 20

   !> Map a DAS address to a physical location in a specified DAS file.
   !>
   !> The saved variables of SPICELIB's [[dasa2l]] routines (entry points dasa2l),
   !> held in [[pepper_context]] (one per thread).
   type, public :: dasa2l_state
      integer(ip) :: fidx = 0  !! index (in the tables) of the file looked up last
      integer(ip) :: next ( 3 ) = [ 2, 3, 1 ]  !! the next data type (character, double precision, integer,
                                               !! cyclic)
      integer(ip) :: nfiles = 0  !! number of files in the tables
      integer(ip) :: nw ( 3 ) = [ dasa2l_nwc, dasa2l_nwd, dasa2l_nwi ]  !! words per record of each data type
      integer(ip) :: prev ( 3 ) = [ 3, 1, 2 ]  !! the previous data type (cyclic)
      integer(ip) :: prvhan = 0  !! handle of the file looked up last
      integer(ip) :: rngloc ( 3 ) = [ chrrng, dprng, intrng ]  !! location of each data type's address range in a directory
                                                               !! record
      integer(ip) :: tbbase ( 3, dasa2l_maxfil ) = -1  !! first record of the single cluster of each data type,
                                                       !! for each file
      integer(ip) :: tbfwrd (    dasa2l_maxfil ) = -1  !! forward pointer of the first directory record of each
                                                       !! file
      integer(ip) :: tbhan (    dasa2l_maxfil ) = 0  !! handles of the files in the tables
      integer(ip) :: tbmxad ( 3, dasa2l_maxfil ) = -1  !! largest address of each data type, for each file
      integer(ip) :: tbsize ( 3, dasa2l_maxfil ) = -1  !! number of records of the single cluster of each data
                                                       !! type, for each file
      logical :: fast = .false.  !! the file looked up last has one cluster per data type (fast lookup)
      logical :: prvok = .false.  !! the last lookup was valid
      logical :: tbfast ( dasa2l_maxfil ) = .false.  !! each file has one cluster per data type (fast lookup)
      logical :: tbrdon ( dasa2l_maxfil ) = .false.  !! each file is open for reading only (so its tables do not
                                                     !! change)
   end type dasa2l_state

   ! ------------------------------------------------------------------ DASADC

   integer(ip), parameter :: dasadc_nwc = 1024

   !> Add character data to a DAS file.
   !>
   !> The saved variables of SPICELIB's [[dasadc]] routines (entry points dasadc),
   !> held in [[pepper_context]] (one per thread).
   type, public :: dasadc_state
      character(dasadc_nwc) :: record = ' '  !! work buffer of one character record (saved in SPICELIB to keep it
                                             !! off the stack)
   end type dasadc_state

   ! ------------------------------------------------------------------ DASADD

   integer(ip), parameter :: dasadd_nwd = 128

   !> Add an array of double precision numbers to a DAS file.
   !>
   !> The saved variables of SPICELIB's [[dasadd]] routines (entry points dasadd),
   !> held in [[pepper_context]] (one per thread).
   type, public :: dasadd_state
      real(wp) :: record ( dasadd_nwd ) = 0.0_wp  !! work buffer of one double precision record (saved in SPICELIB
                                                  !! to keep it off the stack)
   end type dasadd_state

   ! ------------------------------------------------------------------ DASADI

   integer(ip), parameter :: dasadi_nwi = 256

   !> Add an array of integers to a DAS file.
   !>
   !> The saved variables of SPICELIB's [[dasadi]] routines (entry points dasadi),
   !> held in [[pepper_context]] (one per thread).
   type, public :: dasadi_state
      integer(ip) :: record ( dasadi_nwi ) = 0  !! work buffer of one integer record (saved in SPICELIB to keep
                                                !! it off the stack)
   end type dasadi_state

   ! ------------------------------------------------------------------ DASCLS

   ! ------------------------------------------------------------------ DASEC

   integer(ip), parameter :: dasec_mxcrec = 1024

   !> Extract comments from the comment area of a binary DAS file.
   !>
   !> The saved variables of SPICELIB's [[dasec]] routines (entry points dasec),
   !> held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization (101036 bytes): see [[init_dasec_state]].
   type, public :: dasec_state
      ! PEPPER: the tables of files grow (SPICELIB: FTSIZE, 5000, entries).
      character(dasec_mxcrec) :: crecrd  !! the comment record read last
      integer(ip), allocatable :: filcnt (:)  !! the file table declarations for keeping track of which files
                                      !! are currently in the process of having comments extracted.
      integer(ip), allocatable :: filchr (:)  !! number of comment characters read from each file
      integer(ip), allocatable :: filhan (:)  !! handles of the files whose comments are being read
      integer(ip) :: lsthan  !! the handle read last
      integer(ip), allocatable :: lstpos (:)  !! position in the last comment record of each file
      integer(ip), allocatable :: lstrec (:)  !! the comment record read last of each file
      integer(ip) :: nfiles  !! number of files whose comments are being read
      logical :: first  !! the tables have not been set up yet (they are set up on the first call)
   end type dasec_state

   ! ------------------------------------------------------------------ DASFM

   integer(ip), parameter :: rrcidx = 1
   integer(ip), parameter :: rchidx = rrcidx + 1
   integer(ip), parameter :: crcidx = rchidx + 1
   integer(ip), parameter :: cchidx = crcidx + 1
   integer(ip), parameter :: freidx = cchidx + 1
   integer(ip), parameter :: llabas = freidx
   integer(ip), parameter :: lrcbas = llabas + 3
   integer(ip), parameter :: lwdbas = lrcbas + 3
   integer(ip), parameter :: sumsiz = lwdbas + 3

   !> Manage open DAS files.
   !>
   !> The saved variables of SPICELIB's DASFM routines (entry points dasopr,
   !> dasopw, dasonw, dasopn, dasops, dasllc, dashfs, dasufs, dashlu, dasluh,
   !> dashfn, dasfnh, dashof, dassih, dasham), held in [[pepper_instance]].
   !>
   !> PEPPER: SPICELIB's file table was parallel arrays of FTSIZE (5000)
   !> entries linked into a list (most recently opened first, searched
   !> from the head), plus a set (FHLIST) of the same handles; it is an
   !> array of these records that grows as DASs are opened.
   type, public :: das_file
      integer(ip) :: handle = 0  !! the handle
      integer(ip) :: access = -1  !! access method (READ, WRITE)
      integer(ip) :: links = -1  !! number of links (opens) of the DAS
      integer(ip) :: summary ( sumsiz ) = 0  !! file summary (free records and addresses, last
                                             !! records and words of each data type)
   end type das_file

   !> The DAS file table: the open DASs in increasing order of
   !> abs(handle), so that a handle is found by binary search.
   type, public :: dasfm_state
      integer(ip) :: nft = 0  !! number of DASs in the file table
      type(das_file), allocatable :: files(:)  !! the DASs (1 to nft are used)
   contains
      procedure :: find => das_find
      procedure :: add => das_add
      procedure :: remove => das_remove
   end type dasfm_state

   ! ------------------------------------------------------------------ DASRWR

   integer(ip), parameter :: dasrwr_nwd = 128
   integer(ip), parameter :: dasrwr_nwi = 256
   integer(ip), parameter :: dasrwr_nwc = 1024
   integer(ip), parameter :: bufszd = 10
   integer(ip), parameter :: bufszi = 10
   integer(ip), parameter :: bufszc = 10

   !> Read and write DAS physical records.
   !>
   !> The saved variables of SPICELIB's DASRWR routines (entry points dasrrd,
   !> dasrri, dasrrc, daswrd, daswri, daswrc, dasurd, dasuri, dasurc, daswbr),
   !> held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization (31496 bytes): see [[init_dasrwr_state]].
   type, public :: dasrwr_state
      character(dasrwr_nwc) :: rcbufc (      bufszc )  !! the buffered character records
      real(wp) :: rcbufd ( dasrwr_nwd, bufszd )  !! the buffered double precision records
      integer(ip) :: rcbufi ( dasrwr_nwi, bufszi )  !! the buffered integer records
      integer(ip) :: rnbufc (      bufszc )  !! record number of each buffered character record
      integer(ip) :: rnbufd (      bufszd )  !! record number of each buffered double precision
                                             !! record
      integer(ip) :: rnbufi (      bufszi )  !! record number of each buffered integer record
      integer(ip) :: hnbufc (      bufszc )  !! file handle of each buffered character record
      integer(ip) :: hnbufd (      bufszd )  !! file handle of each buffered double precision record
      integer(ip) :: hnbufi (      bufszi )  !! file handle of each buffered integer record
      logical :: upbufc (      bufszc )  !! each buffered character record was updated and is not
                                         !! written to the file yet
      logical :: upbufd (      bufszd )  !! each buffered double precision record was updated and is
                                         !! not written to the file yet
      logical :: upbufi (      bufszi )  !! each buffered integer record was updated and is not
                                         !! written to the file yet
      integer(ip) :: poolc ( 2, lbpool : bufszc )  !! linked-list pool of the character buffer
                                                   !! (most recently used first)
      integer(ip) :: poold ( 2, lbpool : bufszd )  !! linked-list pool of the double precision
                                                   !! buffer (most recently used first)
      integer(ip) :: pooli ( 2, lbpool : bufszi )  !! linked-list pool of the integer buffer (most
                                                   !! recently used first)
      integer(ip) :: headc  !! head of the list of the character buffer's entries
      integer(ip) :: headd  !! head of the list of the double precision buffer's entries
      integer(ip) :: headi  !! head of the list of the integer buffer's entries
      integer(ip) :: usedc  !! number of entries of the character buffer in use
      integer(ip) :: usedd  !! number of entries of the double precision buffer in use
      integer(ip) :: usedi  !! number of entries of the integer buffer in use
      logical :: pass1  !! the buffers have not been initialized yet
   end type dasrwr_state

   ! ------------------------------------------------------------------ DELTET

   !> Return the value of Delta ET (ET-UTC) for an input epoch.
   !>
   !> The saved variables of SPICELIB's [[deltet]] routines (entry points deltet),
   !> held in [[pepper_context]] (one per thread).
   type, public :: deltet_state
      !> error message templates naming each missing [[deltet]] kernel variable
      character(20) :: missed ( 5 ) = [ character(20) :: 'DELTET/DELTA_T_A, #', 'DELTET/K, #', 'DELTET/EB, #', &
         'DELTET/M, #', 'DELTET/DELTA_AT, #' ]
      real(wp), allocatable :: dleap ( :, : )  !! the leapseconds and their epochs (DELTET/DELTA_AT); grows (SPICELIB: a local buffer of 200)
      real(wp), allocatable :: etleap ( : )  !! PEPPER: the ET from which each leapsecond counts, for ET epochs (computed once)
      integer(ip) :: nleap = 0  !! number of leapseconds in DLEAP
      real(wp) :: dta = 0.0_wp  !! DELTET/DELTA_T_A
      real(wp) :: k = 0.0_wp  !! DELTET/K
      real(wp) :: eb = 0.0_wp  !! DELTET/EB
      real(wp) :: m ( 0:1 ) = 0.0_wp  !! DELTET/M
      logical :: valid = .false.  !! the values above were all found in the kernel pool
      logical :: first = .true.  !! the pool counter is not set up yet
      integer(ip) :: usrctr ( ctrsiz ) = 0  !! the pool's state when the values were read
   end type deltet_state

   ! ------------------------------------------------------------------ DIAGS2

   !> Diagonalize a symmetric 2x2 matrix.
   !>
   !> The saved variables of SPICELIB's [[diags2]] routines (entry points diags2),
   !> held in [[pepper_context]] (one per thread).
   type, public :: diags2_state
      real(wp) :: ident ( 2, 2 ) = reshape ( [ 1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp ], [ 2, 2 ] )  !! the 2x2 identity matrix, constant
   end type diags2_state

   ! ------------------------------------------------------------------ DPGRDR

   integer(ip), parameter :: dpgrdr_maxl = 36

   !> Compute the Jacobian matrix of the transformation from
   !> rectangular to planetographic coordinates.
   !>
   !> The saved variables of SPICELIB's [[dpgrdr]] routines (entry points dpgrdr),
   !> held in [[pepper_context]] (one per thread).
   type, public :: dpgrdr_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(dpgrdr_maxl) :: svbody = ' '  !! name of the body looked up last ([[zzbods2c]])
      integer(ip) :: svbdid = 0  !! ID code of the body looked up last
      logical :: svfnd1 = .false.  !! found flag of the last body name lookup
      logical :: first = .true.  !! first call: the pool counter is not yet initialized
   end type dpgrdr_state

   ! ------------------------------------------------------------------ DPSPCE

   integer(ip), parameter :: numgeo = 8
   integer(ip), parameter :: numelm = 10

   !> **Deprecated:** This routine has been superseded by the SPICELIB
   !> routine [[evsgp4]]. *ALL* TLE evaluations should use that routine.
   !> NAIF supports [[dpspce]] only for backward compatibility.
   !>
   !> The saved variables of SPICELIB's [[dpspce]] routines (entry points dpspce),
   !> held in [[pepper_context]] (one per thread).
   type, public :: dpspce_state
      real(wp) :: ae = 0.0_wp  !! distance units per Earth radius (GEOPHS(8))
      real(wp) :: ck2 = 0.0_wp  !! J2 term coefficient: 0.5 * J2 * AE**2
      real(wp) :: xke = 0.0_wp  !! square root of GM, in Earth radii**1.5 per minute (GEOPHS(4))
      real(wp) :: xkmper = 0.0_wp  !! km per Earth radius (GEOPHS(7))
      real(wp) :: lstphs ( numgeo ) = 0.0_wp  !! geophysical constants (GEOPHS) of the last initialization
      real(wp) :: bstar = 0.0_wp  !! elements
      real(wp) :: epoch = 0.0_wp  !! epoch of the elements (ET)
      real(wp) :: omegao = 0.0_wp  !! argument of perigee at epoch (radians)
      real(wp) :: xmo = 0.0_wp  !! mean anomaly at epoch (radians)
      real(wp) :: xnodeo = 0.0_wp  !! right ascension of the ascending node at epoch (radians)
      real(wp) :: lstelm ( numelm ) = 0.0_wp  !! two-line elements (ELEMS) of the last initialization
      real(wp) :: aodp = 0.0_wp  !! original (un-Kozai) semi-major axis, from the initialization
      real(wp) :: aycof = 0.0_wp  !! long-period periodic coefficient of the AYN term
      real(wp) :: c1 = 0.0_wp  !! drag coefficient C1 (BSTAR * C2)
      real(wp) :: c4 = 0.0_wp  !! drag coefficient C4
      real(wp) :: cosepw = 0.0_wp  !! work: cosine of (E + omega) in the Kepler iteration
      real(wp) :: cosio = 0.0_wp  !! cosine of the inclination at epoch
      real(wp) :: omgdot = 0.0_wp  !! secular rate of the argument of perigee (radians/minute)
      real(wp) :: pio2 = 0.0_wp  !! pi/2, set on the first call
      real(wp) :: pix2 = 0.0_wp  !! 2*pi, set on the first call
      real(wp) :: sinepw = 0.0_wp  !! work: sine of (E + omega) in the Kepler iteration
      real(wp) :: sinio = 0.0_wp  !! sine of the inclination at epoch
      real(wp) :: t2cof = 0.0_wp  !! coefficient of the t**2 drag term of the mean longitude (1.5 * C1)
      real(wp) :: temp3 = 0.0_wp  !! work: temporary of the initialization and the Kepler iteration
      real(wp) :: temp4 = 0.0_wp  !! work: temporary of the Kepler iteration
      real(wp) :: temp5 = 0.0_wp  !! work: temporary of the Kepler iteration
      real(wp) :: temp6 = 0.0_wp  !! work: temporary of the Kepler iteration
      real(wp) :: x1mth2 = 0.0_wp  !! 1 - cos(i)**2
      real(wp) :: x3thm1 = 0.0_wp  !! 3*cos(i)**2 - 1
      real(wp) :: x7thm1 = 0.0_wp  !! 7*cos(i)**2 - 1
      real(wp) :: xlcof = 0.0_wp  !! long-period periodic coefficient of the mean longitude
      real(wp) :: xmdot = 0.0_wp  !! secular rate of the mean anomaly (radians/minute)
      real(wp) :: xnodcf = 0.0_wp  !! coefficient of the t**2 drag term of the node
      real(wp) :: xnodot = 0.0_wp  !! secular rate of the ascending node (radians/minute)
      real(wp) :: xnodp = 0.0_wp  !! original (un-Kozai) mean motion, from the initialization (radians/minute)
      logical :: doinit = .true.  !! the derived quantities must be recomputed (first call or new GEOPHS/ELEMS)
      logical :: first = .true.  !! first call: PIO2 and PIX2 not yet set
   end type dpspce_state

   ! ------------------------------------------------------------------ DRDPGR

   integer(ip), parameter :: drdpgr_maxl = 36

   !> Compute the Jacobian matrix of the transformation from
   !> planetographic to rectangular coordinates.
   !>
   !> The saved variables of SPICELIB's [[drdpgr]] routines (entry points drdpgr),
   !> held in [[pepper_context]] (one per thread).
   type, public :: drdpgr_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(drdpgr_maxl) :: svbody = ' '  !! name of the body looked up last ([[zzbods2c]])
      integer(ip) :: svbdid = 0  !! ID code of the body looked up last
      logical :: svfnd1 = .false.  !! found flag of the last body name lookup
      logical :: first = .true.  !! first call: the pool counter is not yet initialized
   end type drdpgr_state

   ! ------------------------------------------------------------------ DSKD02

   !> Fetch double precision data from a type 2 DSK segment.
   !>
   !> The saved variables of SPICELIB's [[dskd02]] routines (entry points dskd02),
   !> held in [[pepper_context]] (one per thread).
   type, public :: dskd02_state
      logical :: first = .true.  !! no segment has been read yet
      integer(ip) :: prvbas = 0  !! DLA base address of the segment read last
      integer(ip) :: prvhan = 0  !! handle of the DSK file of the segment read last
      integer(ip) :: nv = 0  !! number of vertices of the segment read last
   end type dskd02_state

   ! ------------------------------------------------------------------ DSKI02

   !> Fetch integer data from a type 2 DSK segment.
   !>
   !> The saved variables of SPICELIB's [[dski02]] routines (entry points dski02),
   !> held in [[pepper_context]] (one per thread).
   type, public :: dski02_state
      integer(ip) :: cgscal = 0  !! coarse voxel scale of the segment read last
      logical :: first = .true.  !! no segment has been read yet
      integer(ip) :: np = 0  !! number of plates of the segment read last
      integer(ip) :: nv = 0  !! number of vertices of the segment read last
      integer(ip) :: nvxtot = 0  !! number of voxels of the segment read last
      integer(ip) :: prvbas = 0  !! DLA base address of the segment read last
      integer(ip) :: prvhan = 0  !! handle of the DSK file of the segment read last
      integer(ip) :: voxnpl = 0  !! size of the voxel-plate pointer array of the segment read last
      integer(ip) :: voxnpt = 0  !! size of the voxel-plate correspondence list of the segment read last
      integer(ip) :: vtxnpl = 0  !! size of the vertex-plate correspondence list of the segment read
                                 !! last
   end type dski02_state

   ! ------------------------------------------------------------------ DSKTOL

   integer(ip), parameter :: ndspar = 6

   !> Umbrella routine for DSK tolerance and margin parameter access
   !> routines.
   !>
   !> The saved variables of SPICELIB's DSKTOL routines (entry points dskgtl,
   !> dskstl), held in [[pepper_instance]].
   type, public :: dsktol_state
      real(wp) :: dppars ( ndspar ) = [ xfract, sgreed, sgpadm, ptmemm, angmrg, lonali ]  !! the values of the DSK tolerance and margin parameters
   end type dsktol_state

   ! ------------------------------------------------------------------ DSKX02

   integer(ip), parameter :: dskx02_bufsiz = 200

   !> Determine the plate ID and body-fixed coordinates of the
   !> intersection of a specified ray with the surface defined by a
   !> type 2 DSK plate model.
   !>
   !> The saved variables of SPICELIB's [[dskx02]] routines (entry points dskx02),
   !> held in [[pepper_context]] (one per thread).
   !>
   !> SPICELIB's fixed work arrays (50000 voxels on a ray, 256000 plates
   !> in a voxel group, the 100000-entry coarse grid: 3.4 MB per context)
   !> are allocatable: the coarse grid and the voxel lists are sized from
   !> the segment's voxel grid (a ray crosses at most `nx+ny+nz` voxels),
   !> and the plate lists grow with the plates read.
   type, public :: dskx02_state
      integer(ip) :: cgrext ( 3 ) = 0  !! coarse voxel grid extents of the current segment
      integer(ip), allocatable :: cgrptr ( : )  !! coarse voxel grid pointers of the current segment
      integer(ip) :: cgscal = 0  !! coarse voxel scale of the current segment
      integer(ip) :: cgscl2 = 0  !! square of the coarse voxel scale
      integer(ip) :: corsys = 0  !! coordinate system code of the current segment
      real(wp) :: dskdsc ( dskdsz ) = 0.0_wp  !! DSK descriptor of the current segment
      real(wp) :: grdext ( 3 ) = -1.0_wp  !! voxel grid extents (km) of the current segment
      real(wp) :: grdtol = 0.0_wp  !! tolerance for the ray vertex being inside the voxel grid
      integer(ip), allocatable :: ordvec ( : )  !! work: order vector of PLATID
      integer(ip), allocatable :: platid ( : )  !! work: plate IDs of the voxels of the current group
      integer(ip) :: prvdsc ( dladsz ) = 0  !! DLA descriptor of the segment used last
      integer(ip) :: prvhan = 0  !! DSK handle of the segment used last (0 if its data were not fully read)
      integer(ip), allocatable :: source ( : )  !! work: index (into the voxel list) of the voxel each plate in PLATID came from
      real(wp) :: vbuff ( 3, dskx02_bufsiz ) = 0.0_wp  !! work: buffer of plate vertices already fetched during this call
      integer(ip) :: vgrext ( 3 ) = 0  !! voxel grid extents (in voxels) of the current segment
      real(wp) :: voxori ( 3 ) = 0.0_wp  !! voxel grid origin of the current segment
      real(wp) :: voxsiz = 0.0_wp  !! voxel size (km) of the current segment
      integer(ip), allocatable :: voxlst ( :, : )  !! work: grid coordinates of the voxels hit by the ray ([[xdda]])
      integer(ip), allocatable :: vxlcg ( :, : )  !! work: coarse voxel coordinates of the voxels hit by the ray
      integer(ip), allocatable :: vxlout ( : )  !! work: indices (into the voxel list) of the voxels with non-empty coarse voxels
      integer(ip), allocatable :: vxlstr ( : )  !! work: start pointers of the fine voxel data of the voxels hit by the ray
   end type dskx02_state

   ! ------------------------------------------------------------------ DSKXSI

   integer(ip), parameter :: dskxsi_bdnmln = 36
   integer(ip), parameter :: dskxsi_frnmln = 32

   !> Compute a ray-surface intercept using data provided by
   !> multiple loaded DSK segments. Return information about
   !> the source of the data defining the surface on which the
   !> intercept was found: DSK handle, DLA and DSK descriptors,
   !> and DSK data type-dependent parameters.
   !>
   !> The saved variables of SPICELIB's [[dskxsi]] routines (entry points dskxsi),
   !> held in [[pepper_context]] (one per thread).
   type, public :: dskxsi_state
      logical :: first = .true.  !! first call: the counters are not yet initialized
      integer(ip) :: fixfid = 0  !! ID code of the body-fixed frame FIXREF of the previous call
      integer(ip) :: frmctr ( ctrsiz ) = 0  !! pool counter of the frame name lookup of FIXREF ([[zzpctrck]])
      character(:), allocatable :: prvfrm  !! body-fixed frame name of the previous call
      integer(ip) :: prvtcd = 0  !! target ID code of the previous call
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svtfnd = .false.  !! found flag of the last target name lookup
      character(dskxsi_bdnmln) :: svtnam = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: trgctr ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the target
   end type dskxsi_state

   ! ------------------------------------------------------------------ DSKXV

   integer(ip), parameter :: dskxv_bdnmln = 36
   integer(ip), parameter :: dskxv_frnmln = 32

   !> Compute ray-surface intercepts for a set of rays, using data
   !> provided by multiple loaded DSK segments.
   !>
   !> The saved variables of SPICELIB's [[dskxv]] routines (entry points dskxv),
   !> held in [[pepper_context]] (one per thread).
   type, public :: dskxv_state
      logical :: first = .true.  !! first call: pool counters not initialized, no frame validated yet
      integer(ip) :: fixfid = 0  !! frame ID code of the last validated body-fixed frame (fixref)
      integer(ip) :: frmctr ( ctrsiz ) = 0  !! pool counter for the frame kernel variables ([[zzpctrck]]), triggers re-validating fixref
      character(:), allocatable :: prvfrm  !! name of the body-fixed frame validated last
      integer(ip) :: prvtcd = 0  !! ID code of the target for which the frame was validated last
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last ([[zzbods2c]])
      logical :: svtfnd = .false.  !! found flag of the last target name lookup
      character(dskxv_bdnmln) :: svtnam = ' '  !! name of the target looked up last
      integer(ip) :: trgctr ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the target
   end type dskxv_state

   ! ------------------------------------------------------------------ EDTERM

   integer(ip), parameter :: edterm_maxl = 36
   integer(ip), parameter :: edterm_frnmln = 32

   !> Compute a set of points on the umbral or penumbral terminator of
   !> a specified target body, where the target shape is modeled as an
   !> ellipsoid.
   !>
   !> The saved variables of SPICELIB's [[edterm]] routines (entry points edterm),
   !> held in [[pepper_context]] (one per thread).
   type, public :: edterm_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(edterm_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtgid = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the light source
      character(edterm_maxl) :: svscre = ' '  !! name of the light source looked up last
      integer(ip) :: svsrci = 0  !! ID code of the light source looked up last
      logical :: svfnd2 = .false.  !! found flag of the last light source name lookup
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(edterm_maxl) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsi = 0  !! ID code of the observer looked up last
      logical :: svfnd3 = .false.  !! found flag of the last observer name lookup
      logical :: first = .true.  !! first call: the pool counters are not yet initialized
      integer(ip) :: svctr4 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(edterm_frnmln) :: svfref = ' '  !! name of the body-fixed frame looked up last ([[zznamfrm]])
      integer(ip) :: svfrcd = 0  !! ID code of the body-fixed frame looked up last
   end type edterm_state

   ! ------------------------------------------------------------------ EKBSEG

   !> Start a new segment in an E-kernel.
   !>
   !> The saved variables of SPICELIB's [[ekbseg]] routines (entry points ekbseg),
   !> held in [[pepper_context]] (one per thread).
   type, public :: ekbseg_state
      logical :: first = .true.  !! idspec is not set yet
      integer(ip) :: idspec ( lbcell : mxspec ) = 0  !! default identifier specification (lxdfid), to check table/column names
   end type ekbseg_state

   ! ------------------------------------------------------------------ EKQMGR

   integer(ip), parameter :: mxclld = 500
   integer(ip), parameter :: ekqmgr_short = 80

   !> Manage query operations on EK files.
   !>
   !> The saved variables of SPICELIB's EKQMGR routines (entry points eklef,
   !> ekuef, ekntab, ektnam, ekccnt, ekcii, eksrch, eknelt, ekgc, ekgd, ekgi),
   !> held in [[pepper_instance]] (and, for some variables, in [[pepper_context]]).
   !>
   !> PEPPER: the five linked list pools and their parallel arrays grow
   !> when a file is loaded (EKLEF), allocated by [[init_ekqmgr_state]]
   !> (SPICELIB: 20 files, 200 segments, 100 tables, 500 column attribute
   !> entries, 10000 column descriptors; EKFILETABLEFULL, EKSEGTABLEFULL,
   !> EKTABLELISTFULL, EKCOLATTRTABLEFULL, COLDESCTABLEFULL). SPICELIB's
   !> table of the files of each table (TBFILS, TBFLSZ) is gone: it was
   !> filled and read wrongly, and unused in effect (spicelib_bugs.md).
   type, public :: ekqmgr_state
      integer(ip), allocatable :: ftpool ( :, : )  !! doubly linked list pool of the file list (head: fthead), ftpool(2, lbpool:n)
      integer(ip), allocatable :: fthan ( : )  !! handle of each loaded EK file
      integer(ip) :: fthead = 0  !! head node of the file list (0 if no files are loaded)
      integer(ip), allocatable :: tbpool ( :, : )  !! doubly linked list pool of the table list (head: tbhead)
      integer(ip), allocatable :: tbstpt ( : )  !! head node of the segment list (in stpool) of each table
      integer(ip), allocatable :: tbncol ( : )  !! number of columns of each table
      character(tnamsz), allocatable :: tbnams ( : )  !! name of each table
      integer(ip), allocatable :: tbctpt ( : )  !! head node of the column attribute list (in ctpool) of each table
      integer(ip) :: tbhead = 0  !! head node of the table list (0 if no tables are loaded)
      integer(ip), allocatable :: stpool ( :, : )  !! doubly linked list pool of the segment table (one list per table)
      integer(ip), allocatable :: sthan ( : )  !! handle of the EK file of each loaded segment
      integer(ip), allocatable :: stsidx ( : )  !! index of each loaded segment within its EK file
      integer(ip), allocatable :: stdscs ( :, : )  !! segment descriptor of each loaded segment, stdscs(sdscsz, n)
      integer(ip), allocatable :: stnrow ( : )  !! number of rows of each loaded segment
      integer(ip), allocatable :: stncol ( : )  !! number of columns of each loaded segment
      integer(ip), allocatable :: stdtpt ( : )  !! head node of the column descriptor list (in dtpool) of each loaded segment
      integer(ip), allocatable :: dtpool ( :, : )  !! doubly linked list pool of the column descriptor table (one list per segment)
      integer(ip), allocatable :: dtdscs ( :, : )  !! column descriptor of each entry of the column descriptor table, dtdscs(cdscsz, n)
      integer(ip), allocatable :: ctpool ( :, : )  !! doubly linked list pool of the column attribute table (one list per table)
      character(cnamsz), allocatable :: ctnams ( : )  !! column name of each column attribute entry
      integer(ip), allocatable :: ctclas ( : )  !! class of each column
      integer(ip), allocatable :: cttyps ( : )  !! data type of each column
      integer(ip), allocatable :: ctlens ( : )  !! string length of each column (character columns)
      logical, allocatable :: ctfixd ( : )  !! each column has fixed-size entries
      integer(ip), allocatable :: ctsizs ( : )  !! number of elements per entry of each column (fixed-size columns)
      logical, allocatable :: ctindx ( : )  !! each column is indexed
      logical, allocatable :: ctnull ( : )  !! each column may contain null values
      logical :: first = .true.  !! the linked list pools are not initialized yet (done by the first eklef or eksrch)
   end type ekqmgr_state

   !> The saved variables of EKQMGR that are per context.
   !>
   !> No default initialization: see [[init_ekqmgr_context_state]].
   !>
   !> PEPPER: the arrays of the FROM tables, SELECT and ORDER BY columns,
   !> constraints and conjunctions of a query are sized for it by [[eksrch]]
   !> (ekqmgr_query_room; SPICELIB: MAXTAB, MAXSEL, MAXORD, MAXCON, MXJOIN,
   !> MXJRS).
   type, public :: ekqmgr_context_state
      character(cnamsz) :: cnams ( mxclld )  !! work: names of the columns of the segment being loaded (eklef)
      character(cnamsz) :: cnmset ( lbcell : mxclld )  !! work: set of the segment's column names, to find duplicates (eklef)
      character(cnamsz) :: colnam  !! work: name of a column (for eklef's error messages; of a SELECT column in eksrch)
      character(tnamsz), allocatable :: frmals ( : )  !! work: alias of each FROM table of the query (eksrch)
      character(tnamsz), allocatable :: frmtab ( : )  !! work: name of each FROM table of the query (eksrch)
      character(ekqmgr_short) :: problm  !! work: code of the problem that made eklef abort loading the file
      character(tnamsz) :: tabnam  !! work: name of a table (of the segment being loaded, or of a SELECT or order-by column)
      character(tnamsz), allocatable :: tabvec ( : )  !! work: cell of the names of the FROM tables of the query (eksrch)
      real(wp), allocatable :: dvals ( : )  !! work: d.p. value of each constraint of the current conjunction (eksrch)
      integer(ip), allocatable :: cbegs ( : )  !! work: start in eqryc of the string value of each constraint of the current conjunction
      integer(ip) :: cdscrs ( cdscsz,  mxclld )  !! work: column descriptors of the segment being loaded (eklef)
      integer(ip), allocatable :: cends ( : )  !! work: end in eqryc of the string value of each constraint of the current conjunction
      integer(ip), allocatable :: cnstyp ( : )  !! work: type (column or value comparison) of each constraint of the current conjunction
      integer(ip), allocatable :: dtype ( : )  !! work: data type of the value of each constraint of the current conjunction
      integer(ip), allocatable :: ivals ( : )  !! work: integer value of each constraint of the current conjunction
      integer(ip), allocatable :: lcidx ( : )  !! work: index (in its table) of the left-hand column of each constraint
      integer(ip), allocatable :: lelts ( : )  !! element indices (all 1) of the left-hand columns of the constraints
      integer(ip), allocatable :: ltbidx ( : )  !! work: FROM table index of the left-hand column of each constraint
      integer(ip) :: ncols  !! work: number of columns of the segment being loaded (eklef)
      integer(ip) :: new  !! work: file table node of the file being loaded (eklef)
      integer(ip) :: nmatch  !! work: number of matching rows in the current segment (eksrch)
      integer(ip) :: norder  !! number of order-by columns of the last query
      integer(ip) :: npcol  !! work: number of columns of the table of the segment being loaded (for an error message)
      integer(ip) :: nsel  !! number of SELECT columns of the last query
      integer(ip) :: ntab  !! number of FROM tables of the last query
      integer(ip), allocatable :: ocols ( : )  !! index (in its table's column list) of each order-by column of the last query
      integer(ip), allocatable :: oelts ( : )  !! element indices (all 1) of the order-by columns
      integer(ip), allocatable :: ops ( : )  !! work: operator code of each constraint of the current conjunction
      integer(ip) :: ordbas  !! base address in the EK scratch area of the order vector of the sorted query results
      integer(ip), allocatable :: otabs ( : )  !! FROM table index of each order-by column of the last query
      integer(ip), allocatable :: rbas ( : )  !! work: scratch area base address of the one-table join row set of each FROM table
      integer(ip), allocatable :: rcidx ( : )  !! work: index (in its table) of the right-hand column of each constraint
      integer(ip), allocatable :: relts ( : )  !! element indices (all 1) of the right-hand columns of the constraints
      integer(ip), allocatable :: rsize ( : )  !! work: size of the one-table join row set of each FROM table
      integer(ip), allocatable :: rtbidx ( : )  !! work: FROM table index of the right-hand column of each constraint
      integer(ip), allocatable :: selcol ( : )  !! index (in its table's column list) of each SELECT column of the last query
      integer(ip), allocatable :: selctp ( : )  !! column attribute table entry (ctpool node) of each SELECT column of the last query
      integer(ip), allocatable :: seltab ( : )  !! FROM table index of each SELECT column of the last query
      integer(ip) :: segdsc ( sdscsz )  !! work: descriptor of the segment being loaded (eklef)
      integer(ip), allocatable :: sense ( : )  !! sense (ascending or descending) of each order-by column of the last query
      integer(ip) :: stnew  !! work: segment table node of the segment being loaded (eklef)
      integer(ip) :: tbcurr  !! work: the current node of the table list
      integer(ip), allocatable :: tptvec ( : )  !! cell of the table list nodes of the FROM tables of the last query
      integer(ip), allocatable :: ubase ( : )  !! base address of the join row set of each conjunction (the join row set union)
      integer(ip) :: unrows  !! number of rows that matched the last query
      integer(ip) :: usize  !! number of join row sets in the join row set union of the last query
      logical, allocatable :: activc ( : )  !! work: each column comparison constraint is active
      logical, allocatable :: activv ( : )  !! work: each value comparison constraint is active
      logical :: dosort  !! the results of the last query must be sorted at the next fetch
      logical :: fnd  !! work: the searched for table, file or column was found
      logical :: presnt  !! work: the table of the segment being loaded is in the table list already (eklef)
      logical :: sorted  !! the results of the last query are sorted (ordbas holds their order vector)
   end type ekqmgr_context_state

   ! ------------------------------------------------------------------ ET2UTC

   !> Convert an input time from ephemeris seconds past J2000
   !> to Calendar, Day-of-Year, or Julian Date format, UTC.
   !>
   !> The saved variables of SPICELIB's [[et2utc]] routines (entry points et2utc),
   !> held in [[pepper_context]] (one per thread).
   type, public :: et2utc_state
      character(80) :: str = ' '  !! work: output string of the Julian date format
      real(wp) :: tvec ( 10 ) = 0.0_wp  !! work: time vector converted from TDB to JDUTC
   end type et2utc_state

   ! ------------------------------------------------------------------ ETCAL

   !> Convert from an ephemeris epoch measured in seconds past
   !> the epoch of J2000 to a calendar string format using a
   !> formal calendar free of leapseconds.
   !>
   !> The saved variables of SPICELIB's [[etcal]] routines (entry points etcal),
   !> held in [[pepper_context]] (one per thread).
   type, public :: etcal_state
      real(wp) :: dmnint = 0.0_wp  !! smallest integer, as a double precision number (computed on the first call)
      real(wp) :: dmxint = 0.0_wp  !! largest integer, as a double precision number (computed on the first call)
      real(wp) :: halfd = 0.0_wp  !! number of seconds in half a day
      real(wp) :: secspd = 0.0_wp  !! number of seconds in a day
      integer(ip) :: dn2000 = 0  !! day number past 1 Jan 1 AD of Jan 1, 2000 (computed on the first call)
      integer(ip) :: offset = 0  !! scratch: number of 400-year periods by which a B.C. day number was shifted
      logical :: first = .true.  !! the first call has not set up the constants yet
   end type etcal_state

   ! ------------------------------------------------------------------ EV2LIN

   integer(ip), parameter :: nmodl = 6
   integer(ip), parameter :: kae = 8
   integer(ip), parameter :: ngeocn = kae
   integer(ip), parameter :: ev2lin_kepoch = 10
   integer(ip), parameter :: ev2lin_nelems = ev2lin_kepoch
   integer(ip), parameter :: prsize = 29

   !> **Deprecated:** This routine has been superseded by the SPICELIB
   !> routine [[evsgp4]]. *ALL* TLE evaluations should use that routine.
   !> NAIF supports [[ev2lin]] only for backward compatibility.
   !>
   !> The saved variables of SPICELIB's [[ev2lin]] routines (entry points ev2lin),
   !> held in [[pepper_context]] (one per thread).
   type, public :: ev2lin_state
      real(wp) :: ke = 0.0_wp  !! sqrt(GM) of the earth in (earth radii)^1.5/min, from the geophysical constants
      real(wp) :: er = 0.0_wp  !! equatorial radius of the earth (km), from the geophysical constants
      real(wp) :: ae = 0.0_wp  !! distance units per earth radius, from the geophysical constants
      real(wp) :: ck2 = 0.0_wp  !! J2 coefficient term J2*ae^2/2
      real(wp) :: a3ovk2 = 0.0_wp  !! J3/J2 term -2*J3*ae/J2
      real(wp) :: ck4 = 0.0_wp  !! J4 coefficient term -3/8*J4*ae^4
      real(wp) :: qoms2t = 0.0_wp  !! atmospheric density parameter ((QO-SO)*ae/er)^4
      real(wp) :: s = 0.0_wp  !! atmospheric density parameter ae*(1+SO/er)
      real(wp) :: d2 = 0.0_wp  !! work: drag coefficient D2 of the current elements (t^2 term, perigee >= 220 km)
      real(wp) :: d3 = 0.0_wp  !! work: drag coefficient D3 of the current elements (t^3 term, perigee >= 220 km)
      real(wp) :: d4 = 0.0_wp  !! work: drag coefficient D4 of the current elements (t^4 term, perigee >= 220 km)
      real(wp) :: elemnt ( ev2lin_nelems, nmodl ) = 0.0_wp  !! element sets of the cached models (matched against the input to reuse PRELIM)
      real(wp) :: fl = 0.0_wp  !! work: Kepler equation residual at the lower bisection bound
      real(wp) :: fu = 0.0_wp  !! work: Kepler equation residual at the upper bisection bound
      real(wp) :: lower = 0.0_wp  !! work: lower bracket of the Kepler equation bisection
      real(wp) :: lstgeo ( ngeocn ) = 0.0_wp  !! geophysical constants of the previous call (derived constants recomputed on change)
      real(wp) :: pix2 = 0.0_wp  !! 2*pi, set on the first call
      real(wp) :: prelim ( prsize, nmodl ) = 0.0_wp  !! element-dependent preliminary quantities of each cached model
      real(wp) :: t3cof = 0.0_wp  !! work: t^3 mean longitude coefficient of the current elements (perigee >= 220 km)
      real(wp) :: t4cof = 0.0_wp  !! work: t^4 mean longitude coefficient of the current elements (perigee >= 220 km)
      real(wp) :: t5cof = 0.0_wp  !! work: t^5 mean longitude coefficient of the current elements (perigee >= 220 km)
      real(wp) :: tokm = 0.0_wp  !! kilometers per distance unit (er/ae)
      real(wp) :: tokmps = 0.0_wp  !! conversion from distance units/min to km/s (tokm/60)
      real(wp) :: upper = 0.0_wp  !! work: upper bracket of the Kepler equation bisection
      integer(ip) :: head = 0  !! head of the most-recently-used list of cached models
      integer(ip) :: last = 0  !! work: last model visited in the cache search (reused when no match)
      integer(ip) :: list ( 2, nmodl ) = 0  !! previous/next links of the most-recently-used list of cached models
      logical :: doinit = .true.  !! first call: the model cache and list are not yet initialized
      logical :: recog = .false.  !! work: the input elements match a cached model
   end type ev2lin_state

   ! ------------------------------------------------------------------ FOVTRG

   !> Determine if a specified ephemeris object is within the
   !> field-of-view (FOV) of a specified instrument at a given time.
   !>
   !> The saved variables of SPICELIB's [[fovtrg]] routines (entry points fovtrg),
   !> held in [[pepper_context]] (one per thread).
   type, public :: fovtrg_state
      real(wp) :: raydir ( 3 ) = 0.0_wp  !! ray direction vector required by [[zzgffvin]]. This is an unused variable as far is this routine is concerned: the target is an ephemeris object. We initialize the ray to prevent portability problems.
   end type fovtrg_state

   ! ------------------------------------------------------------------ FRAMEX

   integer(ip), parameter :: non = nninrt
   integer(ip), parameter :: ncount = ninert + non
   integer(ip), parameter :: framex_frnmln = 32
   integer(ip), parameter :: maxbfr = 149

   !> This is an umbrella routine for the entry points available for
   !> manipulating different reference frames. It should not be called
   !> directly.
   !>
   !> The saved variables of SPICELIB's FRAMEX routines (entry points namfrm,
   !> frmnam, frinfo, cidfrm, cnmfrm, ccifrm), held in [[pepper_context]] (one per
   !> thread).
   !>
   !> No default initialization (537488 bytes): see [[init_framex_state]].
   type, public :: framex_state
      character(framex_frnmln) :: name ( ncount )  !! official names of the built-in frames (inertial and non-inertial)
      integer(ip) :: center ( ncount )  !! body ID codes of the centers of the built-in frames (parallel to name)
      integer(ip) :: centrd ( ncount )  !! order vector of center (indices of center sorted in increasing order)
      integer(ip) :: idcode ( ncount )  !! frame ID codes of the built-in frames (parallel to name)
      integer(ip) :: type ( ncount )  !! frame class (INERTL, PCK, CK, TK, DYN, ...) of the built-in frames (parallel to name)
      integer(ip) :: typeid ( ncount )  !! frame class IDs of the built-in frames (parallel to name)
      logical :: first  !! true until the built-in frame tables and hashes have been set up
      integer(ip) :: pulctr ( ctrsiz )  !! POOL state counter.
      integer(ip) :: bnmlst (          maxbfr )  !! heads of the name hash collision lists of the built-in frames (index into bnmidx)
      integer(ip) :: bnmpol ( lbpool : maxbfr )  !! name-based hash for built-in frames: collision lists
      character(framex_frnmln) :: bnmnms (          maxbfr )  !! name-based hash for built-in frames: the names (uppercase)
      integer(ip) :: bnmidx (          maxbfr )  !! index in the built-in frame arrays (name, idcode, ...) of each name in bnmnms
      integer(ip) :: bidlst (          maxbfr )  !! heads of the ID hash collision lists of the built-in frames (index into bididx)
      integer(ip) :: bidpol ( lbpool : maxbfr )  !! ID-based hash for built-in frames: collision lists
      integer(ip) :: bidids (          maxbfr )  !! ID-based hash for built-in frames: the frame ID codes
      integer(ip) :: bididx (          maxbfr )  !! index in the built-in frame arrays (name, idcode, ...) of each ID in bidids
      ! PEPPER: the caches of kernel pool frames (emptied when the pool
      ! changes) grow: SPICELIB's hash tables had MAXKFR (5209) entries and
      ! signaled SPICE(HASHISFULL) for the next distinct frame.
      type(string_map) :: knmmap  !! index in knmids of each kernel frame name looked up
      integer(ip), allocatable :: knmids(:)  !! frame ID code of each name in knmmap
      integer(ip) :: nknm = 0  !! number of names in knmmap
      type(int_map) :: kidmap  !! index in kname, kcent, ... of each kernel frame ID looked up
      type(string), allocatable :: kname(:)  !! names of the kernel pool frames in kidmap
      integer(ip), allocatable :: kcent(:)  !! center body ID codes of the kernel pool frames in kidmap
      integer(ip), allocatable :: kclass(:)  !! frame classes of the kernel pool frames in kidmap
      integer(ip), allocatable :: kclsid(:)  !! frame class IDs of the kernel pool frames in kidmap
      integer(ip) :: nkid = 0  !! number of IDs in kidmap
   contains
      procedure :: kreset => framex_kreset
      procedure :: kname_find => framex_kname_find
      procedure :: kname_add => framex_kname_add
      procedure :: kid_find => framex_kid_find
      procedure :: kid_add => framex_kid_add
   end type framex_state

   ! ------------------------------------------------------------------ FRMGET

   !> Find the transformation from a user specified frame to
   !> another frame at a user specified epoch.
   !>
   !> The saved variables of SPICELIB's [[frmget]] routines (entry points frmget),
   !> held in [[pepper_context]] (one per thread).
   type, public :: frmget_state
      real(wp) :: idnt66 ( 6, 6 ) = 0.0_wp  !! 6x6 identity matrix, set on the first call
      logical :: pass1 = .true.  !! true until the first-call initialization is done
      character(6) :: versn = '5.0.0'  !! [[frmget]] version string, used in error messages
   end type frmget_state

   ! ------------------------------------------------------------------ GETFVN

   integer(ip), parameter :: getfvn_maxl = 36

   !> Return the field-of-view (FOV) parameters for a specified
   !> instrument. The instrument is specified by name.
   !>
   !> The saved variables of SPICELIB's [[getfvn]] routines (entry points getfvn),
   !> held in [[pepper_context]] (one per thread).
   type, public :: getfvn_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(getfvn_maxl) :: svinst = ' '  !! name of the instrument looked up last ([[zzbods2c]])
      integer(ip) :: svinid = 0  !! ID code of the instrument looked up last
      logical :: svfnd1 = .false.  !! found flag of the last instrument name lookup
      logical :: first = .true.  !! first call: the pool counter is not yet initialized
   end type getfvn_state

   ! ------------------------------------------------------------------ GFEVNT

   integer(ip), parameter :: nq = 8
   integer(ip), parameter :: gfevnt_maxcln = 80

   !> Determine time intervals when a specified geometric quantity
   !> satisfies a specified mathematical condition.
   !>
   !> The saved variables of SPICELIB's [[gfevnt]] routines (entry points gfevnt),
   !> held in [[pepper_context]] (one per thread).
   type, public :: gfevnt_state
      logical :: first = .true.  !! first call: progress report prefixes and suffixes not yet set up
      !> recognized relational operators (constant)
      character(gfevnt_maxcln) :: cnames ( nc ) = [ character(gfevnt_maxcln) :: '>', '=', '<', 'ABSMAX', &
         'ABSMIN', 'LOCMAX', 'LOCMIN' ]
      character(gfevnt_maxcln) :: dref = ' '  !! frame of the DVEC ray for coordinate searches (DREF; kept if not supplied)
      !> recognized geometric quantity names (constant)
      character(gfevnt_maxcln) :: qnames ( nq     ) = [ character(gfevnt_maxcln) :: 'ANGULAR SEPARATION', &
         'DISTANCE', 'COORDINATE', 'RANGE RATE', 'PHASE ANGLE', 'ILLUMINATION ANGLE', ' ', ' ' ]
      character(mxbegm) :: srcpre ( 2, nq ) = ' '  !! progress report prefixes for each pass of each quantity
      character(mxendm) :: srcsuf ( 2, nq ) = ' '  !! progress report suffixes for each pass of each quantity
   end type gfevnt_state

   ! ------------------------------------------------------------------ GFRPRT

   !> The entry points contained under this routine provide users
   !> information regarding the status of a GF search in progress.
   !>
   !> The saved variables of SPICELIB's GFRPRT routines (entry points gfrepi,
   !> gfrepu, gfrepf), held in [[pepper_context]] (one per thread).
   type, public :: gfrprt_state
      character(mxbegm) :: copyb = ' '  !! progress report begin message saved by [[gfrepi]] for [[gfrepf]]
      character(mxendm) :: copye = ' '  !! progress report end message saved by [[gfrepi]] for [[gfrepf]]
      real(wp) :: t0 = 0.0_wp  !! time of the last progress update (start of the search window at first)
      real(wp) :: remain = 0.0_wp  !! measure of the current interval remaining after t0
   end type gfrprt_state

   ! ------------------------------------------------------------------ GFSTEP

   !> Return the time step set by the most recent call to [[gfsstp]].
   !>
   !> The saved variables of SPICELIB's [[gfstep]] routines (entry points gfstep,
   !> gfsstp), held in [[pepper_context]] (one per thread).
   type, public :: gfstep_state
      real(wp) :: svstep = -1.0_wp  !! the step size (set by [[gfsstp]])
      logical :: svinit = .false.  !! the step size has been set
   end type gfstep_state

   ! ------------------------------------------------------------------ GFTFOV

   !> Determine time intervals when a specified ephemeris object
   !> intersects the space bounded by the field-of-view (FOV) of a
   !> specified instrument.
   !>
   !> The saved variables of SPICELIB's [[gftfov]] routines (entry points gftfov),
   !> held in [[pepper_context]] (one per thread).
   type, public :: gftfov_state
      real(wp) :: raydir ( 3 ) = 0.0_wp  !! ray direction vector required by [[gffove]]. This is an unused variable as far is this routine is concerned: the target is an ephemeris object. We initialize the ray to prevent portability problems.
   end type gftfov_state

   ! ------------------------------------------------------------------ HX2DP

   integer(ip), parameter :: maxman = 31

   !> Convert a string representing a double precision number in a
   !> base 16 "scientific notation" into its equivalent double
   !> precision number.
   !>
   !> The saved variables of SPICELIB's [[hx2dp]] routines (entry points hx2dp),
   !> held in [[pepper_context]] (one per thread).
   type, public :: hx2dp_state
      real(wp) :: maxdp = 0.0_wp  !! largest number divided by the base
      real(wp) :: mindp = 0.0_wp  !! smallest number divided by the base
      real(wp) :: scales (maxman) = 0.0_wp  !! negative powers of the base (16) for the digits of the mantissa
      integer(ip) :: digbeg = 0  !! character code of '0'
      integer(ip) :: digend = 0  !! character code of '9'
      integer(ip) :: iexpch = 0  !! character code of the exponent character
      integer(ip) :: iminus = 0  !! character code of '-'
      integer(ip) :: iplus = 0  !! character code of '+'
      integer(ip) :: ispace = 0  !! character code of ' '
      integer(ip) :: lccbeg = 0  !! character code of 'a'
      integer(ip) :: lccend = 0  !! character code of 'f'
      integer(ip) :: uccbeg = 0  !! character code of 'A'
      integer(ip) :: uccend = 0  !! character code of 'F'
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
   end type hx2dp_state

   ! ------------------------------------------------------------------ HYPTOF

   !> Solve the time of flight equation MA = e sinh(F) - F for the
   !> hyperbolic eccentric anomaly F, given the mean anomaly, MA,
   !> and the eccentricity, e.
   !>
   !> The saved variables of SPICELIB's [[hyptof]] routines (entry points hyptof),
   !> held in [[pepper_context]] (one per thread).
   type, public :: hyptof_state
      logical :: first = .true.  !! first call: maxlog not yet computed
      real(wp) :: maxlog = 0.0_wp  !! log(dpmax), the natural log of the largest double precision number
   end type hyptof_state

   ! ------------------------------------------------------------------ ILLUM

   integer(ip), parameter :: illum_maxl = 36

   !> **Deprecated:** This routine has been superseded by the SPICELIB
   !> routine [[ilumin]]. This routine is supported for purposes of
   !> backward compatibility only.
   !>
   !> The saved variables of SPICELIB's [[illum]] routines (entry points illum),
   !> held in [[pepper_context]] (one per thread).
   type, public :: illum_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(illum_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(illum_maxl) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsc = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      logical :: first = .true.  !! first call: the pool counters are not yet initialized
   end type illum_state

   ! ------------------------------------------------------------------ ILLUMF

   integer(ip), parameter :: illumf_maxl = 36
   integer(ip), parameter :: illumf_frnmln = 32

   !> Compute the illumination angles---phase, incidence, and
   !> emission---at a specified point on a target body. Return logical
   !> flags indicating whether the surface point is visible from
   !> the observer's position and whether the surface point is
   !> illuminated.
   !>
   !> The saved variables of SPICELIB's [[illumf]] routines (entry points illumf),
   !> held in [[pepper_context]] (one per thread).
   type, public :: illumf_state
      logical :: first = .true.  !! first call: counters, correction and method not yet set up
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      character(mthlen) :: prvmth = ' '  !! method string of the previous call (blank after a parse error)
      integer(ip) :: shape = 0  !! target shape parsed from the saved method (ellipsoid or DSK)
      integer(ip), allocatable :: srflst (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) surface ID list parsed from the saved method
      logical :: uselt = .false.  !! the saved correction includes light time
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(illumf_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(illumf_maxl) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsc = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(illumf_frnmln) :: svfref = ' '  !! name of the body-fixed frame (FIXREF) looked up last ([[zznamfrm]])
      integer(ip) :: svrefc = 0  !! ID code of the body-fixed frame looked up last
      integer(ip) :: svctr4 ( ctrsiz ) = 0  !! saved surface name/ID item declarations.
   end type illumf_state

   ! ------------------------------------------------------------------ ILLUMG

   integer(ip), parameter :: illumg_lnsize = 80
   integer(ip), parameter :: illumg_maxl = 36
   integer(ip), parameter :: illumg_frnmln = 32

   !> Find the illumination angles (phase, incidence, and
   !> emission) at a specified surface point of a target body.
   !>
   !> The saved variables of SPICELIB's [[illumg]] routines (entry points illumg),
   !> held in [[pepper_context]] (one per thread).
   type, public :: illumg_state
      logical :: first = .true.  !! first call: counters, correction and method not yet set up
      integer(ip) :: nsurf = 0  !! number of surface IDs in the saved method's surface list
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      character(illumg_lnsize) :: prvmth = ' '  !! method string of the previous call (blank after a parse error)
      integer(ip) :: shape = 0  !! target shape parsed from the saved method (ellipsoid or DSK)
      integer(ip), allocatable :: srflst (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) surface ID list parsed from the saved method
      logical :: uselt = .false.  !! the saved correction includes light time
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(illumg_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(illumg_maxl) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsc = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(illumg_frnmln) :: svfref = ' '  !! name of the body-fixed frame (FIXREF) looked up last ([[zznamfrm]])
      integer(ip) :: svrefc = 0  !! ID code of the body-fixed frame looked up last
      integer(ip) :: svctr4 ( ctrsiz ) = 0  !! saved surface name/ID item declarations.
   end type illumg_state

   ! ------------------------------------------------------------------ INCNSG

   !> Compute the points of intersection of a specified nappe of a cone
   !> and a line segment.
   !>
   !> The saved variables of SPICELIB's [[incnsg]] routines (entry points incnsg),
   !> held in [[pepper_context]] (one per thread).
   type, public :: incnsg_state
      real(wp) :: origin ( 3 ) = 0.0_wp  !! the zero vector
      real(wp) :: z ( 3 ) = [ 0.0_wp, 0.0_wp, 1.0_wp ]  !! the +Z unit vector
   end type incnsg_state

   ! ------------------------------------------------------------------ JUL2GR

   !> Convert Year Month and Day on the Julian Calendar
   !> to the Gregorian Calendar
   !>
   !> The saved variables of SPICELIB's [[jul2gr]] routines (entry points jul2gr,
   !> gr2jul), held in [[pepper_context]] (one per thread).
   type, public :: jul2gr_state
      logical :: first = .true.  !! the first call has not set up the constants yet
      integer(ip) :: rdayg = 0  !! Gregorian day number of the reference day Oct 15, 1582
      integer(ip) :: rdayj = 0  !! Julian day number of the reference day Oct 5, 1582 (the same day)
      integer(ip) :: offstj = 0  !! offset from Gregorian to Julian calendar day numbers (RDAYJ - RDAYG)
      integer(ip) :: offstg = 0  !! offset from Julian to Gregorian calendar day numbers (RDAYG - RDAYJ)
   end type jul2gr_state

   ! ------------------------------------------------------------------ KEEPER

   integer(ip), parameter :: keeper_wdsize = 32
   integer(ip), parameter :: nknown = 3

   !> This routine is an umbrella for a collection of entry points
   !> that manage the loading and unloading of SPICE kernels from
   !> an application program.
   !>
   !> The saved variables of SPICELIB's KEEPER routines (entry points furnsh,
   !> ktotal, kdata, kinfo, kclear, unload), held in [[pepper_instance]].
   !>
   !> PEPPER: SPICELIB's table was four parallel arrays of MAXFIL (5300)
   !> entries, with names of at most FILSIZ (255) characters; it is an
   !> array of these records that grows, with names of any length.
   type, public :: keeper_kernel
      character(:), allocatable :: name  !! the file name (as given to FURNSH or in KERNELS_TO_LOAD)
      character(typlen) :: type = ' '  !! SPK, CK, PCK, DSK, EK, TEXT or META
      integer(ip) :: handle = 0  !! handle of the kernel (0 for a text kernel)
      integer(ip) :: source = 0  !! index of the meta-kernel it was loaded from (0: loaded directly)
      !> PEPPER: the loader that read the file ([[spice_add_loader]]), or
      !> blank for SPICE's own kernels and programmatic kernels
      !> ([[spice_load_kernel]]).
      character(:), allocatable :: loader
      !> PEPPER: the description of a kernel built in memory (the builder's
      !> [[kb_set_format]]); blank for SPICE's own kernels.
      character(:), allocatable :: format
      !> Source text when loaded from memory ([[spice_furnsh_memory]]), so
      !> [[unload]] can reload remaining in-memory text kernels without a file.
      character(:), allocatable :: memory_text
   end type keeper_kernel

   !> The kernels loaded through FURNSH, in load order.
   type, public :: keeper_state
      integer(ip) :: loaded = 0  !! number of kernels loaded
      type(keeper_kernel), allocatable :: kernels(:)  !! the kernels (1 to loaded are used)
      character(keeper_wdsize) :: known ( nknown ) = ' '  !! the meta-kernel variables (KERNELS_TO_LOAD,
                                                          !! PATH_SYMBOLS, PATH_VALUES) watched by FURNSH
      logical :: first = .true.  !! the watcher has not been set yet
   contains
      procedure :: add => keeper_add
      procedure :: remove => keeper_remove
      procedure :: clear => keeper_clear
   end type keeper_state

   ! ------------------------------------------------------------------ LATSRF

   integer(ip), parameter :: latsrf_bdnmln = 36
   integer(ip), parameter :: latsrf_frnmln = 32

   !> Map array of planetocentric longitude/latitude coordinate pairs
   !> to surface points on a specified target body.
   !>
   !> The saved variables of SPICELIB's [[latsrf]] routines (entry points latsrf),
   !> held in [[pepper_context]] (one per thread).
   type, public :: latsrf_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(latsrf_bdnmln) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(latsrf_frnmln) :: svfref = ' '  !! name of the body-fixed frame looked up last ([[zznamfrm]])
      integer(ip) :: svfxfc = 0  !! ID code of the body-fixed frame looked up last
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! saved target radius values.
      integer(ip) :: svprvt = 0  !! ID code of the target whose radii were looked up last
      real(wp) :: svradi ( 3 ) = 0.0_wp  !! radii of the target body (from the kernel pool, [[zzbodvcd]])
      integer(ip) :: nsurf = 0  !! number of surface IDs in the previous METHOD string
      logical :: first = .true.  !! first call: counters not yet initialized and no METHOD parsed yet
      character(mthlen) :: prvmth = ' '  !! METHOD string of the previous call
      integer(ip) :: shape = 0  !! target shape code parsed from the previous METHOD (ellipsoid or DSK)
      integer(ip) :: srfctr ( ctrsiz ) = 0  !! surface name-ID mapping state counter ([[zzsrftrk]])
      integer(ip), allocatable :: srflst (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) surface ID list parsed from the previous METHOD string
   end type latsrf_state

   ! ------------------------------------------------------------------ LIMBPT

   integer(ip), parameter :: limbpt_maxl = 36
   integer(ip), parameter :: limbpt_frnmln = 32

   !> Find limb points on a target body. The limb is the set of points
   !> of tangency on the target of rays emanating from the observer.
   !> The caller specifies half-planes bounded by the observer-target
   !> center vector in which to search for limb points.
   !>
   !> The saved variables of SPICELIB's [[limbpt]] routines (entry points limbpt),
   !> held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization (1168 bytes): see [[init_limbpt_state]].
   type, public :: limbpt_state
      logical :: first  !! first call: counters, correction, method and locus not yet set up
      integer(ip) :: lmbtyp  !! limb type parsed from the saved method (tangent or guided)
      integer(ip) :: loccde  !! aberration correction locus code parsed from the saved CORLOC
      integer(ip) :: nsurf  !! number of surface IDs in the saved method's surface list
      character(corlen) :: prvcor  !! aberration correction string of the previous call
      character(acllen) :: prvloc  !! aberration correction locus string (CORLOC) of the previous call
      character(mthlen) :: prvmth  !! method string of the previous call (blank after a parse error)
      integer(ip) :: prvtrg  !! target ID code of the previous call (the radii counter is reset when it changes)
      integer(ip) :: shape  !! target shape parsed from the saved method (ellipsoid or DSK)
      integer(ip), allocatable :: srflst (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) surface ID list parsed from the saved method
      integer(ip) :: svnrad  !! number of target radii returned by the last [[zzbodvcd]] lookup
      logical :: usecn  !! the saved correction uses converged Newtonian light time
      logical :: uselt  !! the saved correction includes light time
      logical :: usestl  !! the saved correction includes stellar aberration
      logical :: xmit  !! the saved correction is for transmitted (rather than received) radiation
      integer(ip) :: svctr1 ( ctrsiz )  !! saved name/ID item declarations.
      character(limbpt_maxl) :: svtarg  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde  !! ID code of the target looked up last
      logical :: svfnd1  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz )  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(limbpt_maxl) :: svobsr  !! name of the observer looked up last
      integer(ip) :: svobsc  !! ID code of the observer looked up last
      logical :: svfnd2  !! found flag of the last observer name lookup
      integer(ip) :: svctr3 ( ctrsiz )  !! saved frame name/ID item declarations.
      character(limbpt_frnmln) :: svfref  !! name of the body-fixed frame (FIXREF) looked up last ([[zznamfrm]])
      integer(ip) :: svfxfc  !! ID code of the body-fixed frame looked up last
      integer(ip) :: svctr4 ( ctrsiz )  !! saved surface name/ID item declarations.
      integer(ip) :: svctr5 ( ctrsiz )  !! saved target radii declarations.
      real(wp) :: svradi ( 3 )  !! target radii (km) returned by the last [[zzbodvcd]] lookup
   end type limbpt_state

   ! ------------------------------------------------------------------ LSPCN

   integer(ip), parameter :: lspcn_maxl = 36

   !> Compute L_s, the planetocentric longitude of the sun, as seen
   !> from a specified body.
   !>
   !> The saved variables of SPICELIB's [[lspcn]] routines (entry points lspcn),
   !> held in [[pepper_context]] (one per thread).
   type, public :: lspcn_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(lspcn_maxl) :: svbody = ' '  !! name of the body looked up last ([[zzbods2c]])
      integer(ip) :: svidcd = 0  !! ID code of the body looked up last
      logical :: svfnd1 = .false.  !! found flag of the last body name lookup
      logical :: first = .true.  !! first call: the pool counter is not yet initialized
   end type lspcn_state

   ! ------------------------------------------------------------------ OSCELT

   !> Determine the set of osculating conic orbital elements that
   !> corresponds to the state (position, velocity) of a body at
   !> some epoch.
   !>
   !> The saved variables of SPICELIB's [[oscelt]] routines (entry points oscelt),
   !> held in [[pepper_context]] (one per thread).
   type, public :: oscelt_state
      real(wp) :: zvec ( 3 ) = [ 0.0_wp, 0.0_wp, 1.0_wp ]  !! the +Z unit vector
   end type oscelt_state

   ! ------------------------------------------------------------------ PCKBSR

   !> Load and unload files for use by the readers. Buffer segments
   !> for readers.
   !>
   !> SPICELIB's PCKBSR (entry points pcklof, pckuof, pcksfs): the loaded
   !> binary PCK files and the index of their segments by body, held in
   !> [[pepper_instance]].
   !>
   !> PEPPER: the segments are indexed when a file is loaded (SPICELIB:
   !> found per body at its first query, in per-context tables of 20 bodies
   !> and 5000 segments).
   type, public :: pckbsr_state
      type(segment_index) :: index  !! the segments of the loaded files, by body
   end type pckbsr_state

   !> The per-context state of PCKBSR: the segment returned last for each
   !> body.
   type, public :: pckbsr_context_state
      type(segment_reuse) :: reuse  !! the segment returned last for each body, and its re-use interval
   end type pckbsr_context_state

   ! ------------------------------------------------------------------ PGRREC

   integer(ip), parameter :: pgrrec_maxl = 36

   !> Convert planetographic coordinates to rectangular coordinates.
   !>
   !> The saved variables of SPICELIB's [[pgrrec]] routines (entry points pgrrec),
   !> held in [[pepper_context]] (one per thread).
   type, public :: pgrrec_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(pgrrec_maxl) :: svbody = ' '  !! name of the body looked up last ([[zzbods2c]])
      integer(ip) :: svbdid = 0  !! ID code of the body looked up last
      logical :: svfnd1 = .false.  !! found flag of the last body name lookup
      logical :: first = .true.  !! first call: the pool counter is not yet initialized
   end type pgrrec_state

   ! ------------------------------------------------------------------ PHASEQ

   integer(ip), parameter :: phaseq_maxl = 36

   !> Compute the apparent phase angle for a target, observer,
   !> illuminator set of ephemeris objects.
   !>
   !> The saved variables of SPICELIB's [[phaseq]] routines (entry points phaseq),
   !> held in [[pepper_context]] (one per thread).
   type, public :: phaseq_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(phaseq_maxl) :: svtarg = ' '  !! saved target name for the zzbods2c lookup
      integer(ip) :: svtgid = 0  !! saved target ID code for the zzbods2c lookup
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the illumination source
      character(phaseq_maxl) :: svilmn = ' '  !! saved illumination source name for the zzbods2c lookup
      integer(ip) :: svicde = 0  !! saved illumination source ID code for the zzbods2c lookup
      logical :: svfnd2 = .false.  !! found flag of the last illumination source name lookup
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(phaseq_maxl) :: svobsr = ' '  !! saved observer name for the zzbods2c lookup
      integer(ip) :: svobsn = 0  !! saved observer ID code for the zzbods2c lookup
      logical :: svfnd3 = .false.  !! found flag of the last observer name lookup
      logical :: first = .true.  !! first call: pool counters not yet initialized
   end type phaseq_state

   ! ------------------------------------------------------------------ POOL

   integer(ip), parameter, public :: pool_none = 0  !! a pool variable without values
   integer(ip), parameter, public :: pool_numeric = 1  !! a numeric pool variable
   integer(ip), parameter, public :: pool_character = 2  !! a character pool variable

   !> One assignment in effect of a kernel pool variable (PEPPER): where it
   !> came from, and the values it gave.
   type, public :: pool_assignment
      integer(ip) :: source = 0  !! the source (index in the pool's [[pool_state]]%sources)
      integer(ip) :: line = 0  !! the line where the assignment begins (0: none, e.g. PDPOOL)
      integer(ip) :: first = 1  !! the index of the first value it gave
   end type pool_assignment

   !> A kernel pool variable: its name and values, of any length and number.
   !>
   !> PEPPER: with the assignments in effect, in order: one after `=` (which
   !> replaces the values), one more for each `+=` (which appends).
   type, public :: pool_variable
      character(:), allocatable :: name  !! the name (unallocated: a free slot)
      integer(ip) :: kind = pool_none  !! pool_none, pool_numeric or pool_character
      integer(ip) :: n = 0  !! number of values
      real(wp), allocatable :: dvals(:)  !! the numeric values (1 to n)
      type(string), allocatable :: cvals(:)  !! the character values (1 to n)
      type(pool_assignment), allocatable :: from(:)  !! the assignments in effect (1 to nfrom)
      integer(ip) :: nfrom = 0  !! number of assignments in effect
   end type pool_variable

   !> Maintain a pool of variables read from SPICE ASCII kernel files.
   !>
   !> The saved variables of SPICELIB's POOL routines (entry points clpool,
   !> ldpool, rtpool, expool, wrpool, swpool, cvpool, gcpool, gdpool, gipool,
   !> dtpool, pcpool, pdpool, pipool, lmpool, szpool, dvpool, gnpool, dwpool,
   !> zzvupool, zzpctrck), held in [[pepper_instance]].
   !>
   !> PEPPER: SPICELIB kept the names in a hash table of MAXVAR (26003)
   !> entries and the values in linked lists of MAXVAL (400000) numbers and
   !> MAXLIN (15000) strings of MAXCHR (80) characters (about 9 MB). The
   !> variables are records in slots that grow, found by name through a
   !> hash map; names and string values have any length. The slots keep the
   !> order in which the variables were created (a slot freed by DVPOOL is
   !> reused), which is the order of [[gnpool]] and [[wrpool]].
   type, public :: pool_state
      type(string_map) :: index  !! slot of each variable, by name
      type(pool_variable), allocatable :: vars(:)  !! the variables (slots 1 to nslots)
      integer(ip) :: nslots = 0  !! number of slots used (some may be free)
      integer(ip), allocatable :: free(:)  !! the free slots (1 to nfree)
      integer(ip) :: nfree = 0  !! number of free slots
      logical :: first = .true.  !! the pool has not been initialized yet
      integer(ip) :: subctr ( ctrsiz ) = 0  !! pool state counter (changes when the pool changes)
      character(mrklen) :: begdat = ' '  !! the begin-data marker of text kernels
      character(mrklen) :: begtxt = ' '  !! the begin-text marker of text kernels
      ! PEPPER: the sources of the assignments ([[pool_note_source]])
      type(string_map) :: source_index  !! index of each source, by name
      type(string), allocatable :: sources(:)  !! the sources (1 to nsources): kernel files, or tags such as (PDPOOL)
      integer(ip) :: nsources = 0  !! number of sources
      !> The source that PCPOOL, PDPOOL and PIPOOL record, if set (by the
      !> programmatic frames); otherwise their own name, e.g. (PDPOOL).
      character(:), allocatable :: setter
   contains
      procedure :: find => pool_find
      procedure :: create => pool_create
      procedure :: remove => pool_remove
      procedure :: clear_values => pool_clear_values
      procedure :: add_number => pool_add_number
      procedure :: add_string => pool_add_string
      procedure :: clear => pool_clear
      procedure :: note_source => pool_note_source
   end type pool_state

   ! ------------------------------------------------------------------ PROP2B

   integer(ip), parameter :: prop2b_bufsiz = 3

   !> Compute the state of a massless body at time t_0 + DT by applying
   !> the two-body force model to a given central mass and a given body
   !> state at time t_0.
   !>
   !> The saved variables of SPICELIB's [[prop2b]] routines (entry points prop2b),
   !> held in [[pepper_context]] (one per thread).
   type, public :: prop2b_state
      real(wp) :: lower = 0.0_wp  !! lower end of the bracket of the Kepler equation root (a work variable; not used between calls)
      real(wp) :: upper = 0.0_wp  !! upper end of the bracket of the Kepler equation root (a work variable; not used between calls)
      real(wp) :: vel ( 3 ) = 0.0_wp  !! initial velocity (from pvinit) (a work array; not used between calls)
      real(wp) :: savepv ( 6, prop2b_bufsiz ) = 0.0_wp  !! initial states of the last calls, whose derived quantities (below) are kept for reuse
      real(wp) :: savegm (    prop2b_bufsiz ) = 0.0_wp  !! gm of each saved initial state
      real(wp) :: sbound (    prop2b_bufsiz ) = 0.0_wp  !! bound on the magnitude of x (avoids overflow) for each saved initial state
      real(wp) :: sb2rv (    prop2b_bufsiz ) = 0.0_wp  !! b**2 * (r dot v) for each saved initial state (b = sqrt(q/gm))
      real(wp) :: sbq (    prop2b_bufsiz ) = 0.0_wp  !! b*q for each saved initial state (q = periapsis distance)
      real(wp) :: sbr0 (    prop2b_bufsiz ) = 0.0_wp  !! b*r0 for each saved initial state (r0 = initial distance)
      real(wp) :: sf (    prop2b_bufsiz ) = 0.0_wp  !! 1 - e for each saved initial state (e = eccentricity)
      real(wp) :: sqovr0 (    prop2b_bufsiz ) = 0.0_wp  !! q/r0 for each saved initial state
      integer(ip) :: newest (    prop2b_bufsiz ) = [ 1, 2, 3 ]  !! saved-state slots from most to least recently used
      integer(ip) :: nsaved = 0  !! number of saved initial states
   end type prop2b_state

   ! ------------------------------------------------------------------ PXFORM

   integer(ip), parameter :: pxform_frnmln = 32

   !> Return the matrix that transforms position vectors from one
   !> specified frame to another at a specified epoch.
   !>
   !> The saved variables of SPICELIB's [[pxform]] routines (entry points pxform),
   !> held in [[pepper_context]] (one per thread).
   type, public :: pxform_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! pool counter of the frame name lookup ([[zznamfrm]]) of the "from" frame
      character(pxform_frnmln) :: svfrom = ' '  !! name of the "from" frame looked up last
      integer(ip) :: svfcod = 0  !! ID code of the "from" frame looked up last
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! kernel pool state counter for the "to" frame name lookup
      character(pxform_frnmln) :: svto = ' '  !! name of the "to" frame looked up last
      integer(ip) :: svtcde = 0  !! ID code of the "to" frame looked up last
      logical :: first = .true.  !! true until the pool state counters are initialized
   end type pxform_state

   ! ------------------------------------------------------------------ PXFRM2

   integer(ip), parameter :: pxfrm2_frnmln = 32

   !> Return the 3x3 matrix that transforms position vectors from one
   !> specified frame at a specified epoch to another specified
   !> frame at another specified epoch.
   !>
   !> The saved variables of SPICELIB's [[pxfrm2]] routines (entry points pxfrm2),
   !> held in [[pepper_context]] (one per thread).
   type, public :: pxfrm2_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(pxfrm2_frnmln) :: svfrom = ' '  !! name of the "from" frame looked up last
      integer(ip) :: svfcod = 0  !! ID code of the "from" frame looked up last
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the frame name lookup ([[zznamfrm]]) of the "to" frame
      character(pxfrm2_frnmln) :: svto = ' '  !! name of the "to" frame looked up last
      integer(ip) :: svtcde = 0  !! ID code of the "to" frame looked up last
      logical :: first = .true.  !! first call: the pool counters are not yet initialized
   end type pxfrm2_state

   ! ------------------------------------------------------------------ RDKER

   !> Open and read the contents of a SPICE ASCII kernel file.
   !>
   !> The saved variables of SPICELIB's RDKER routines (entry points rdknew,
   !> rdkdat, rdklin), held in [[pepper_instance]].
   type, public :: rdker_state
      character(:), allocatable :: file  !! the text kernel being read (PEPPER: any length; SPICELIB's
                                          !! held FILSIZ, 255, characters)
      integer(ip) :: linnum = 0  !! number of the last line read
      integer(ip) :: status = 0  !! reading text, data, or at the end of the file
      logical :: frstim = .true.  !! no kernel has been opened yet
      character(mrklen) :: begdat = ' '  !! the begin-data marker
      character(mrklen) :: begtxt = ' '  !! the begin-text marker
   end type rdker_state

   ! ------------------------------------------------------------------ RDTEXT

   integer(ip), parameter :: rdtext_maxlen = 255
   integer(ip), parameter :: maxopn = 96

   !> Read the next line of text from a text file.
   !>
   !> The saved variables of SPICELIB's [[rdtext]] routines (entry points rdtext,
   !> cltext), held in [[pepper_instance]].
   type, public :: rdtext_state
      integer(ip) :: units ( maxopn ) = 0  !! units of the files open for reading
      !> PEPPER: the files are read as streams of bytes, split into lines by
      !> [[rdtext]] itself: the position of the next byte, and the size
      integer(int64) :: pos ( maxopn ) = 1
      integer(int64) :: fsize ( maxopn ) = 0  !! the size of each file (bytes)
      integer(ip) :: n = 0  !! number of files open
      character(:), allocatable :: lstfil  !! the file read last (PEPPER: any length; SPICELIB's
                                            !! held 255 characters)
      integer(ip) :: lstunt = 0  !! the unit of the file read last
      integer(ip) :: index = 0  !! index in units of the file read last
   end type rdtext_state

   ! ------------------------------------------------------------------ RECPGR

   integer(ip), parameter :: recpgr_maxl = 36

   !> Convert rectangular coordinates to planetographic coordinates.
   !>
   !> The saved variables of SPICELIB's [[recpgr]] routines (entry points recpgr),
   !> held in [[pepper_context]] (one per thread).
   type, public :: recpgr_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(recpgr_maxl) :: svbody = ' '  !! name of the body looked up last ([[zzbods2c]])
      integer(ip) :: svbdid = 0  !! ID code of the body looked up last
      logical :: svfnd1 = .false.  !! found flag of the last body name lookup
      logical :: first = .true.  !! first call: the pool counter is not yet initialized
   end type recpgr_state

   ! ------------------------------------------------------------------ REPML

   integer(ip), parameter :: ncase = 3

   !> Replace a marker with the text representation of a logical value.
   !>
   !> The saved variables of SPICELIB's [[repml]] routines (entry points repml),
   !> held in [[pepper_context]] (one per thread).
   type, public :: repml_state
      character(1) :: casstr ( ncase ) = [ character(1) :: 'U', 'L', 'C' ]  !! constant list of the recognized case flags: 'U', 'L', 'C'
   end type repml_state

   ! ------------------------------------------------------------------ ROTGET

   !> Find the rotation from a user specified frame to another frame at
   !> a user specified epoch.
   !>
   !> The saved variables of SPICELIB's [[rotget]] routines (entry points rotget),
   !> held in [[pepper_context]] (one per thread).
   type, public :: rotget_state
      character(6) :: versn = '5.0.0'  !! [[rotget]] version string, used in error messages
   end type rotget_state

   ! ------------------------------------------------------------------ SC01

   integer(ip), parameter :: initsc = 0

   !> Perform time conversions between different representations of
   !> type 1 spacecraft clock.
   !>
   !> The saved variables of SPICELIB's SC01 routines (entry points sctk01,
   !> scfm01, scte01, scet01, scec01, scty01, scpr01), held in [[pepper_context]]
   !> (one per thread).
   !>
   !> No default initialization (2566632 bytes): see [[init_sc01_state]].
   type, public :: sc01_state
      ! PEPPER: the type 1 database grows. SPICELIB's held MXNCLK (100)
      ! clocks in a hash table (emptied when full) and DBUFSZ numbers in
      ! all, with at most MXCOEF (100000) coefficients and MXPART (9999)
      ! partitions per clock (SPICE(TOOMANYCOEFFS), TOOMANYPARTITIONS).
      type(int_map) :: clkmap  !! index in scbase of each clock in the database
      integer(ip) :: nclk = 0  !! number of clocks in the database
      integer(ip), allocatable :: scbase ( : )  !! base index in intbuf of the integer data of each clock in clklst
      character(dplen) :: cmp ( mxnfld )  !! work: string value of each field of an SCLK string (parsed or formatted)
      character(errlen) :: error  !! work: error message from parsing an SCLK string field
      real(wp) :: cmptks ( mxnfld )  !! work: number of ticks per unit of each SCLK field
      real(wp) :: cmpval ( mxnfld )  !! work: numeric value of each SCLK field
      real(wp), allocatable :: dpbuff ( : )  !! double precision buffer of the type 1 database (coefficients, partitions, moduli, offsets)
      real(wp) :: partim  !! work: time in the clock's parallel time system (TDB or TDT)
      integer(ip) :: cmpwid ( mxnfld )  !! work: width (number of digits) of each SCLK field in an output string
      integer(ip) :: cofbas  !! base index in dpbuff of the coefficients of the previous clock (prvsc)
      integer(ip) :: delcde  !! delimiter code of the previous clock (prvsc)
      integer(ip) :: dpfree  !! index of the first free element of dpbuff
      integer(ip) :: end  !! work: index of the last character before the decimal point of a formatted field
      integer(ip) :: endbas  !! base index in dpbuff of the partition end times of the previous clock (prvsc)
      integer(ip) :: ifree  !! index of the first free element of intbuf
      integer(ip), allocatable :: intbuf ( : )  !! integer buffer of the type 1 database (nivals values per clock, see zzsc01.inc)
      integer(ip) :: length ( mxnfld )  !! work: length of the string value of each SCLK field
      integer(ip) :: lower  !! work: index of the coefficient record bracketing the input time (binary search)
      integer(ip) :: modbas  !! base index in dpbuff of the field moduli of the previous clock (prvsc)
      integer(ip) :: ncoeff  !! number of coefficients (3 x number of records) of the previous clock (prvsc)
      integer(ip) :: nfield  !! number of fields of the previous clock (prvsc)
      integer(ip) :: npart  !! number of partitions of the previous clock (prvsc)
      integer(ip) :: offbas  !! base index in dpbuff of the field offsets of the previous clock (prvsc)
      integer(ip) :: polctr ( ctrsiz )  !! kernel pool state counter of the type 1 database (to detect pool updates)
      integer(ip) :: prvsc  !! SCLK ID of the clock seen on the previous successful call (0 if none)
      integer(ip) :: strbas  !! base index in dpbuff of the partition start times of the previous clock (prvsc)
      integer(ip) :: timsys  !! parallel time system code (TDB or TDT) of the previous clock (prvsc)
      logical :: pass1  !! the database must be (re)initialized on the next call (first call or after an error)
   end type sc01_state

   ! ------------------------------------------------------------------ SCDECD

   ! ------------------------------------------------------------------ SCENCD

   ! ------------------------------------------------------------------ SCPS01

   integer(ip), parameter :: scps01_namlen = 60
   integer(ip), parameter :: scps01_nflidx = 1
   integer(ip), parameter :: offidx = scps01_nflidx + 1
   integer(ip), parameter :: modidx = offidx + 1
   integer(ip), parameter :: nkitem = modidx

   !> Convert a character representation of a type 1 spacecraft clock
   !> count to ticks.
   !>
   !> The saved variables of SPICELIB's [[scps01]] routines (entry points scps01),
   !> held in [[pepper_context]] (one per thread).
   type, public :: scps01_state
      character(errlen) :: strerr = ' '  !! work: error message from parsing a clock string field ([[nparsd]])
      !> constant kernel variable names: SCLK01_N_FIELDS, SCLK01_OFFSETS, SCLK01_MODULI
      character(scps01_namlen) :: namlst ( nkitem ) = [ character(scps01_namlen) :: 'SCLK01_N_FIELDS', &
         'SCLK01_OFFSETS', 'SCLK01_MODULI' ]
      real(wp) :: cmptks ( mxnfld ) = 0.0_wp  !! work: number of ticks per unit of each clock field
      real(wp) :: cmpval ( mxnfld ) = 0.0_wp  !! work: values of the clock string fields, offset-corrected
   end type scps01_state

   ! ------------------------------------------------------------------ SGMETA

   !> Obtain the value of a specified generic segment meta data item.
   !>
   !> The saved variables of SPICELIB's [[sgmeta]] routines (entry points sgmeta),
   !> held in [[pepper_context]] (one per thread).
   type, public :: sgmeta_state
      integer(ip) :: lstbeg = -1  !! begin address of the segment whose meta data are buffered
      integer(ip) :: lsthan = 0  !! handle of the file whose descriptor layout is buffered
      integer(ip) :: meta ( mxmeta ) = 0  !! buffered meta data of the segment (base addresses made absolute)
      integer(ip) :: metasz = 0  !! number of meta data items of the buffered segment
      logical :: nieven = .false.  !! the number of integer descriptor components of file lsthan is even
      integer(ip) :: ioffst = 0  !! index in the descriptor of the DP word holding the segment begin and end addresses
   end type sgmeta_state

   ! ------------------------------------------------------------------ SGSEQW

   integer(ip), parameter :: sgseqw_ftsize = 20

   !> This is the umbrella routine for managing the sequential writing
   !> of generic segments to DAF files. It should never be called
   !> directly, it provides the mechanism whereby data are shared by
   !> its entry points.
   !>
   !> The saved variables of SPICELIB's SGSEQW routines (entry points sgbwfs,
   !> sgbwvs, sgwfpk, sgwvpk, sgwes), held in [[pepper_context]] (one per thread).
   type, public :: sgseqw_state
      logical :: explct = .false.  !! reference values of the current segment are explicit (not implicit)
      integer(ip) :: index = 0  !! file table index of the segment being written to the last handle used
      integer(ip) :: numfxd = 0  !! number of fixed size packet segments currently being written
      integer(ip) :: numvar = 0  !! number of variable size packet segments currently being written
      integer(ip) :: lsthan = 0  !! handle of the DAF used in the last call
      logical :: fxdseg = .false.  !! the current segment has fixed (rather than variable) size packets
      real(wp) :: ftrefs ( 2, sgseqw_ftsize ) = 0.0_wp  !! file table declarations. The file table is used to keep track of the vital statistics for each of the generic segments being written.
      integer(ip) :: ftbadr ( sgseqw_ftsize ) = 0  !! file table: DAF begin address of each segment
      integer(ip) :: fthan ( sgseqw_ftsize ) = 0  !! file table: handle of the DAF to which each segment is written
      integer(ip) :: ftmxsz ( sgseqw_ftsize ) = 0  !! file table: maximum packet size written so far (variable size segments)
      integer(ip) :: ftncon ( sgseqw_ftsize ) = 0  !! file table: number of constant values of each segment
      integer(ip) :: ftnpkt ( sgseqw_ftsize ) = 0  !! file table: number of packets written so far to each segment
      integer(ip) :: ftnref ( sgseqw_ftsize ) = 0  !! file table: number of reference values written so far to each segment
      integer(ip) :: ftnres ( sgseqw_ftsize ) = 0  !! file table: number of reserved values of each segment (always 0)
      integer(ip) :: ftityp ( sgseqw_ftsize ) = 0  !! file table: reference value index type of each segment
      logical :: ftexpl ( sgseqw_ftsize ) = .false.  !! file table: each segment has explicit reference values
      integer(ip) :: ftoff ( sgseqw_ftsize ) = 0  !! file table: packet offset (PKTOFF) of each segment
      integer(ip) :: ftpksz ( sgseqw_ftsize ) = 0  !! file table: packet size of each fixed size segment
      logical :: ftfixd ( sgseqw_ftsize ) = .false.  !! file table: each segment has fixed size packets
      integer(ip) :: nft = 0  !! number of segments in the file table
   end type sgseqw_state

   ! ------------------------------------------------------------------ SINCPT

   integer(ip), parameter :: sincpt_maxl = 36
   integer(ip), parameter :: sincpt_frnmln = 32

   !> Compute, for a given observer and a ray emanating from the
   !> observer, the surface intercept of the ray on a target body at
   !> a specified epoch, optionally corrected for light time and
   !> stellar aberration.
   !>
   !> The saved variables of SPICELIB's [[sincpt]] routines (entry points sincpt),
   !> held in [[pepper_context]] (one per thread).
   type, public :: sincpt_state
      logical :: first = .true.  !! first call: pool counters not initialized, correction and method not yet parsed
      integer(ip) :: nsurf = 0  !! number of surface IDs in the surface list parsed from the saved method
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      character(mthlen) :: prvmth = ' '  !! method string of the previous call
      integer(ip) :: shape = 0  !! target shape parsed from the saved method (ellipsoid or DSK)
      integer(ip), allocatable :: srflst (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) surface ID list parsed from the saved method
      logical :: usecn = .false.  !! the saved correction uses converged Newtonian light time (CN)
      logical :: uselt = .false.  !! the saved correction uses light time (LT or CN)
      logical :: usestl = .false.  !! the saved correction includes stellar aberration (+S)
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(sincpt_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(sincpt_maxl) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsc = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(sincpt_frnmln) :: svfref = ' '  !! name of the body-fixed frame looked up last ([[zznamfrm]])
      integer(ip) :: svfxfc = 0  !! ID code of the body-fixed frame looked up last
      integer(ip) :: svctr4 ( ctrsiz ) = 0  !! pool counter of the frame name lookup ([[zznamfrm]]) of the ray frame dref
      character(sincpt_frnmln) :: svdref = ' '  !! name of the ray frame dref looked up last
      integer(ip) :: svdfrc = 0  !! ID code of the ray frame dref looked up last
      integer(ip) :: svctr5 ( ctrsiz ) = 0  !! saved surface name/ID item declarations.
   end type sincpt_state

   ! ------------------------------------------------------------------ SPCRFL

   integer(ip), parameter :: maxcpr = 1000

   !> Read the first line of text from the comment area
   !> of a binary SPK or CK file.
   !>
   !> The saved variables of SPICELIB's [[spcrfl]] routines (entry points spcrfl,
   !> spcrnl), held in [[pepper_context]] (one per thread).
   type, public :: spcrfl_state
      character(1) :: eot = ' '  !! end-of-transmission character (CHAR(4)) that ends the comment area
      character(1) :: null = ' '  !! null character (CHAR(0)) that ends each comment line
      character(maxcpr) :: record = ' '  !! comment area record currently being read
      integer(ip) :: eol = 0  !! position of the end of the last line read within RECORD
      integer(ip) :: fward = 0  !! forward pointer of the file record (first summary record); the comments precede it
      integer(ip) :: hanbuf = 0  !! handle of the file whose comments are being read
      integer(ip) :: iostat = 0  !! work: IOSTAT of the last record read
      integer(ip) :: rec = 0  !! number of the comment area record in RECORD
      logical :: called = .false.  !! [[spcrfl]] has been called ([[spcrnl]] may continue reading)
      logical :: eocsav = .false.  !! the end of the comments was reached by the last SPCRFL/SPCRNL call
   end type spcrfl_state

   ! ------------------------------------------------------------------ SPKACS

   !> Return the state (position and velocity) of a target body
   !> relative to an observer, optionally corrected for light time
   !> and stellar aberration, expressed relative to an inertial
   !> reference frame.
   !>
   !> The saved variables of SPICELIB's [[spkacs]] routines (entry points spkacs),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkacs_state
      logical :: first = .true.  !! first call: no aberration correction has been parsed yet
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: usestl = .false.  !! the saved correction includes stellar aberration
   end type spkacs_state

   ! ------------------------------------------------------------------ SPKAPO

   integer(ip), parameter :: spkapo_corlen = 5

   !> Return the position of a target body relative to an observer,
   !> optionally corrected for light time and stellar aberration.
   !>
   !> The saved variables of SPICELIB's [[spkapo]] routines (entry points spkapo),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkapo_state
      logical :: first = .true.  !! first call: no aberration correction has been parsed yet
      !> list of recognized aberration correction flags ('NONE', 'LT', 'LT+S', ...)
      character(spkapo_corlen) :: flags ( nflags ) = [ character(spkapo_corlen) :: 'NONE', 'LT', 'LT+S', 'CN', &
         'CN+S', 'XLT', 'XLT+S', 'XCN', 'XCN+S' ]
      character(spkapo_corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: usecn = .false.  !! the saved correction uses converged Newtonian light time
      logical :: usestl = .false.  !! the saved correction includes stellar aberration
      logical :: uselt = .false.  !! the saved correction uses (single-iteration) light time
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
   end type spkapo_state

   ! ------------------------------------------------------------------ SPKAPP

   integer(ip), parameter :: spkapp_corlen = 5

   !> **Deprecated:** This routine has been superseded by the SPICELIB
   !> routine [[spkaps]]. This routine is supported for purposes of
   !> backward compatibility only.
   !>
   !> The saved variables of SPICELIB's [[spkapp]] routines (entry points spkapp),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkapp_state
      logical :: first = .true.  !! first call: no aberration correction has been parsed yet
      !> list of recognized aberration correction flags ('NONE', 'LT', 'LT+S', ...)
      character(spkapp_corlen) :: flags ( nflags ) = [ character(spkapp_corlen) :: 'NONE', 'LT', 'LT+S', 'CN', &
         'CN+S', 'XLT', 'XLT+S', 'XCN', 'XCN+S' ]
      character(spkapp_corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: usecn = .false.  !! the saved correction uses converged Newtonian light time
      logical :: usestl = .false.  !! the saved correction includes stellar aberration
      logical :: uselt = .false.  !! the saved correction uses (single-iteration) light time
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
   end type spkapp_state

   ! ------------------------------------------------------------------ SPKAPS

   !> Return the state (position and velocity) of a target body
   !> relative to an observer specified by its state and
   !> acceleration relative to the solar system barycenter. The
   !> returned state may be optionally corrected for light time
   !> and stellar aberration. All input and output vectors are
   !> expressed relative to an inertial reference frame.
   !>
   !> The saved variables of SPICELIB's [[spkaps]] routines (entry points spkaps),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkaps_state
      logical :: first = .true.  !! first call: no aberration correction has been parsed yet
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: usestl = .false.  !! the saved correction includes stellar aberration
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
   end type spkaps_state

   ! ------------------------------------------------------------------ SPKBSR

   !> Load and unload files for use by the readers. Buffer segments
   !> for readers.
   !>
   !> SPICELIB's SPKBSR (entry points spklef, spkuef, spksfs): the loaded
   !> SPK files and the index of their segments by body, held in
   !> [[pepper_instance]].
   !>
   !> PEPPER: the segments are indexed when a file is loaded (SPICELIB:
   !> found per body at its first query, reading the summaries of every
   !> loaded file, in per-context tables of 10000 bodies and 100000
   !> segments, 10.5 MB per context).
   type, public :: spkbsr_state
      type(segment_index) :: index  !! the segments of the loaded files, by body
   end type spkbsr_state

   !> The per-context state of SPKBSR: the segment returned last for each
   !> body.
   type, public :: spkbsr_context_state
      type(segment_reuse) :: reuse  !! the segment returned last for each body, and its re-use interval
   end type spkbsr_context_state

   ! ------------------------------------------------------------------ SPKCVO

   integer(ip), parameter :: spkcvo_maxl = 36
   integer(ip), parameter :: spkcvo_frnmln = 32

   !> Return the state of a specified target relative to an "observer,"
   !> where the observer has constant velocity in a specified reference
   !> frame. The observer's state is provided by the calling program
   !> rather than by loaded SPK files.
   !>
   !> The saved variables of SPICELIB's [[spkcvo]] routines (entry points spkcvo),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkcvo_state
      character(evlfln) :: evlflg ( nevflg ) = [ character(evlfln) :: 'OBSERVER', 'TARGET', 'CENTER' ]  !! recognized frame evaluation loci (OBSERVER, TARGET, CENTER)
      integer(ip) :: j2code = 0  !! inertial frame code ([[irfnum]]) of J2000
      logical :: first = .true.  !! first call: counters, correction flags and J2000 code not yet set up
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: uselt = .false.  !! the saved correction includes light time correction
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(spkcvo_maxl) :: svoctr = ' '  !! name of the observer's center of motion looked up last ([[zzbods2c]])
      integer(ip) :: svccde = 0  !! ID code of the observer's center of motion looked up last
      logical :: svfnd1 = .false.  !! found flag of the last observer center name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the target
      character(spkcvo_maxl) :: svtarg = ' '  !! name of the target looked up last
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd2 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(spkcvo_frnmln) :: svoref = ' '  !! name of the output frame looked up last ([[zznamfrm]])
      integer(ip) :: svorfc = 0  !! ID code of the output frame looked up last
   end type spkcvo_state

   ! ------------------------------------------------------------------ SPKCVT

   integer(ip), parameter :: spkcvt_maxl = 36
   integer(ip), parameter :: spkcvt_frnmln = 32

   !> Return the state, relative to a specified observer, of a target
   !> having constant velocity in a specified reference frame. The
   !> target's state is provided by the calling program rather than by
   !> loaded SPK files.
   !>
   !> The saved variables of SPICELIB's [[spkcvt]] routines (entry points spkcvt),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkcvt_state
      character(evlfln) :: evlflg ( nevflg ) = [ character(evlfln) :: 'OBSERVER', 'TARGET', 'CENTER' ]  !! recognized frame evaluation loci (OBSERVER, TARGET, CENTER)
      integer(ip) :: j2code = 0  !! inertial frame code ([[irfnum]]) of J2000
      logical :: first = .true.  !! first call: counters, correction flags and J2000 code not yet set up
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: uselt = .false.  !! the saved correction includes light time correction
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(spkcvt_maxl) :: svtctr = ' '  !! name of the target's center of motion looked up last ([[zzbods2c]])
      integer(ip) :: svccde = 0  !! ID code of the target's center of motion looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target center name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(spkcvt_maxl) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsc = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(spkcvt_frnmln) :: svoref = ' '  !! name of the output frame looked up last ([[zznamfrm]])
      integer(ip) :: svorfc = 0  !! ID code of the output frame looked up last
   end type spkcvt_state

   ! ------------------------------------------------------------------ SPKEZ

   integer(ip), parameter :: spkez_frnmln = 32

   !> Return the state (position and velocity) of a target body
   !> relative to an observing body, optionally corrected for light
   !> time (planetary aberration) and stellar aberration.
   !>
   !> The saved variables of SPICELIB's [[spkez]] routines (entry points spkez),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkez_state
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      integer(ip) :: fj2000 = 0  !! frame ID code of J2000
      logical :: usegeo = .false.  !! the saved correction is geometric (no aberration correction)
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(spkez_frnmln) :: svref = ' '  !! saved output frame name for the zznamfrm lookup
      integer(ip) :: svreqf = 0  !! saved output frame ID code for the zznamfrm lookup
      logical :: first = .true.  !! first call: counter, correction flags and J2000 code not yet set up
   end type spkez_state

   ! ------------------------------------------------------------------ SPKEZP

   integer(ip), parameter :: spkezp_frnmln = 32

   !> Return the position of a target body relative to an observing
   !> body, optionally corrected for light time (planetary aberration)
   !> and stellar aberration.
   !>
   !> The saved variables of SPICELIB's [[spkezp]] routines (entry points spkezp),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkezp_state
      integer(ip) :: fj2000 = 0  !! frame ID code of J2000
      logical :: first = .true.  !! first call: J2000 code and counter not yet set up
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(spkezp_frnmln) :: svref = ' '  !! saved output frame name for the zznamfrm lookup
      integer(ip) :: svreqf = 0  !! saved output frame ID code for the zznamfrm lookup
   end type spkezp_state

   ! ------------------------------------------------------------------ SPKEZR

   integer(ip), parameter :: spkezr_maxl = 36

   !> Return the state (position and velocity) of a target body
   !> relative to an observing body, optionally corrected for light
   !> time (planetary aberration) and stellar aberration.
   !>
   !> The saved variables of SPICELIB's [[spkezr]] routines (entry points spkezr),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkezr_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(spkezr_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtgid = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(spkezr_maxl) :: svobsn = ' '  !! name of the observer looked up last
      integer(ip) :: svobsi = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      logical :: first = .true.  !! first call: the pool counters are not yet initialized
   end type spkezr_state

   ! ------------------------------------------------------------------ SPKGEO

   integer(ip), parameter :: spkgeo_frnmln = 32

   !> Compute the geometric state (position and velocity) of a target
   !> body relative to an observing body.
   !>
   !> The saved variables of SPICELIB's [[spkgeo]] routines (entry points spkgeo),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkgeo_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(spkgeo_frnmln) :: svref = ' '  !! saved reference frame name for the zznamfrm lookup
      integer(ip) :: svrefi = 0  !! saved reference frame ID code for the zznamfrm lookup
      logical :: first = .true.  !! first call: the frame name counter is not yet initialized
   end type spkgeo_state

   ! ------------------------------------------------------------------ SPKGPS

   integer(ip), parameter :: spkgps_frnmln = 32

   !> Compute the geometric position of a target body relative to an
   !> observing body.
   !>
   !> The saved variables of SPICELIB's [[spkgps]] routines (entry points spkgps),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkgps_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(spkgps_frnmln) :: svref = ' '  !! saved reference frame name for the zznamfrm lookup
      integer(ip) :: svrefi = 0  !! saved reference frame ID code for the zznamfrm lookup
      logical :: first = .true.  !! first call: the frame name counter is not yet initialized
   end type spkgps_state

   ! ------------------------------------------------------------------ SPKLTC

   !> Return the state (position and velocity) of a target body
   !> relative to an observer, optionally corrected for light time,
   !> expressed relative to an inertial reference frame.
   !>
   !> The saved variables of SPICELIB's [[spkltc]] routines (entry points spkltc),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkltc_state
      logical :: pass1 = .true.  !! first call: no aberration correction has been parsed yet
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: usecn = .false.  !! the saved correction uses converged Newtonian light time
      logical :: uselt = .false.  !! the saved correction includes light time (normal or converged Newtonian)
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
   end type spkltc_state

   ! ------------------------------------------------------------------ SPKPOS

   integer(ip), parameter :: spkpos_maxl = 36

   !> Return the position of a target body relative to an observing
   !> body, optionally corrected for light time (planetary aberration)
   !> and stellar aberration.
   !>
   !> The saved variables of SPICELIB's [[spkpos]] routines (entry points spkpos),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkpos_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(spkpos_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtgid = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(spkpos_maxl) :: svobsn = ' '  !! name of the observer looked up last
      integer(ip) :: svobsi = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      logical :: first = .true.  !! first call: the pool counters are not yet initialized
   end type spkpos_state

   ! ------------------------------------------------------------------ SPKPV

   integer(ip), parameter :: spkpv_frnmln = 32

   !> Return the state (position and velocity) of a target body
   !> relative to some center of motion in a specified frame.
   !>
   !> The saved variables of SPICELIB's [[spkpv]] routines (entry points spkpv),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkpv_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(spkpv_frnmln) :: svref = ' '  !! name of the reference frame looked up last ([[zznamfrm]])
      integer(ip) :: svirfr = 0  !! ID code of the reference frame looked up last
      logical :: first = .true.  !! first call: the pool counter is not yet initialized
   end type spkpv_state

   ! ------------------------------------------------------------------ SPKR19

   !> Read a single SPK data record from a segment of type 19
   !> (ESOC/DDID Piecewise Interpolation).
   !>
   !> The saved variables of SPICELIB's [[spkr19]] routines (entry points spkr19),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spkr19_state
      logical :: pass1 = .true.  !! this is the first call (the record size check has not been done yet)
      integer(ip) :: svbeg = -1  !! begin address of the segment used last
      real(wp) :: svbtim = 0.0_wp  !! start time of the mini-segment interval used last
      real(wp) :: svetim = -1.0_wp  !! end time of the mini-segment interval used last
      integer(ip) :: svhan = 0  !! handle of the file of the segment used last
      logical :: svlast = .false.  !! interval selection flag of the segment used last (select the last applicable interval)
      integer(ip) :: svmiix = -1  !! index of the mini-segment (interval) used last
      integer(ip) :: svminb = -1  !! begin address of the mini-segment used last
      integer(ip) :: svn = -1  !! number of mini-segments in the segment used last
      integer(ip) :: svnpkt = -1  !! number of packets in the mini-segment used last
      logical :: svok = .false.  !! the last call succeeded (the saved values are valid only if true)
      integer(ip) :: svpkdb = -1  !! base address of the packet directory of the mini-segment used last
      integer(ip) :: svpknd = -1  !! number of packet directory entries of the mini-segment used last
      integer(ip) :: svpksz = -1  !! packet size of the mini-segment used last
      integer(ip) :: svstyp = -1  !! subtype of the mini-segment used last
      integer(ip) :: svwnsz = -1  !! interpolation window size of the mini-segment used last
   end type spkr19_state

   ! ------------------------------------------------------------------ SPKS19

   !> Extract a subset of the data in an SPK segment of type 19
   !> into a new segment.
   !>
   !> The saved variables of SPICELIB's [[spks19]] routines (entry points spks19),
   !> held in [[pepper_context]] (one per thread).
   type, public :: spks19_state
      real(wp) :: iv1beg = 0.0_wp  !! work: start time of the first output interpolation interval
      real(wp) :: iv1end = 0.0_wp  !! work: end time of the first output interpolation interval
      real(wp) :: ivfbeg = 0.0_wp  !! work: start time of the final output interpolation interval
      real(wp) :: ivfend = 0.0_wp  !! work: end time of the final output interpolation interval
      integer(ip) :: minfsz = 0  !! work: size in DAF addresses of the final output mini-segment
      integer(ip) :: shift = 0  !! work: shift of the middle group mini-segment pointers
   end type spks19_state

   ! ------------------------------------------------------------------ SRFNRM

   integer(ip), parameter :: srfnrm_bdnmln = 36
   integer(ip), parameter :: srfnrm_frnmln = 32

   !> Map array of surface points on a specified target body to
   !> the corresponding unit length outward surface normal vectors.
   !>
   !> The saved variables of SPICELIB's [[srfnrm]] routines (entry points srfnrm),
   !> held in [[pepper_context]] (one per thread).
   type, public :: srfnrm_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(srfnrm_bdnmln) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(srfnrm_frnmln) :: svfref = ' '  !! name of the body-fixed frame looked up last ([[zznamfrm]])
      integer(ip) :: svfxfc = 0  !! ID code of the body-fixed frame looked up last
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! saved target radius values.
      integer(ip) :: svprvt = 0  !! ID code of the target whose radii were looked up last
      real(wp) :: svradi ( 3 ) = 0.0_wp  !! radii of the target body (from the kernel pool, [[zzbodvcd]])
      integer(ip) :: nsurf = 0  !! number of surface IDs in the previous METHOD string
      logical :: first = .true.  !! first call: counters not yet initialized and no METHOD parsed yet
      character(mthlen) :: prvmth = ' '  !! METHOD string of the previous call
      integer(ip) :: shape = 0  !! target shape code parsed from the previous METHOD (ellipsoid or DSK)
      integer(ip) :: srfctr ( ctrsiz ) = 0  !! surface name-ID mapping state counter ([[zzsrftrk]])
      integer(ip), allocatable :: srflst (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) surface ID list parsed from the previous METHOD string
   end type srfnrm_state

   ! ------------------------------------------------------------------ SRFXPT

   integer(ip), parameter :: srfxpt_maxl = 36
   integer(ip), parameter :: srfxpt_frnmln = 32

   !> **Deprecated:** This routine has been superseded by the SPICELIB
   !> routine [[sincpt]]. This routine is supported for purposes of
   !> backward compatibility only.
   !>
   !> The saved variables of SPICELIB's [[srfxpt]] routines (entry points srfxpt),
   !> held in [[pepper_context]] (one per thread).
   type, public :: srfxpt_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(srfxpt_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(srfxpt_maxl) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsc = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      logical :: first = .true.  !! first call: the pool counters are not yet initialized
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(srfxpt_frnmln) :: svdref = ' '  !! name of the ray frame dref looked up last ([[zznamfrm]])
      integer(ip) :: svrefc = 0  !! ID code of the ray frame dref looked up last
   end type srfxpt_state

   ! ------------------------------------------------------------------ STCC01

   !> Check whether a file is a type 1 star catalog and return the
   !> catalog's table name if it is.
   !>
   !> The saved variables of SPICELIB's [[stcc01]] routines (entry points stcc01),
   !> held in [[pepper_context]] (one per thread).
   type, public :: stcc01_state
      character(4) :: dtypes ( mxclsg ) = ' '  !! work: data type of each column of the current segment (from [[ekssum]])
      character(cnamsz) :: cnames ( mxclsg ) = ' '  !! work: names of the columns of the current segment
      character(tnamsz) :: tmptnm = ' '  !! work: table name of the current segment
      character(tnamsz) :: tnmprv = ' '  !! work: table name of the previous segment
      integer(ip) :: ncols = 0  !! work: number of columns in the current segment
      logical :: nullok ( mxclsg ) = .false.  !! work: whether each column of the current segment allows null values
   end type stcc01_state

   ! ------------------------------------------------------------------ STMP03

   ! (SPICELIB: 10 for Intel Fortran on Windows, 11 elsewhere)
   integer(ip), parameter :: stmp03_trunc = 11
   integer(ip), parameter :: npairs = 2*stmp03_trunc - 2

   !> Compute the values of the Stumpff functions C_0 through C_3 at
   !> a specified point.
   !>
   !> The saved variables of SPICELIB's [[stmp03]] routines (entry points stmp03),
   !> held in [[pepper_context]] (one per thread). Generated by
   !> tools/modularize_ctx.py from the saved variables of stmp03.F90.
   type, public :: stmp03_state
      logical :: first = .true.  !! this is the first call (pairs and lbound are not computed yet)
      real(wp) :: lbound = 0.0_wp  !! lower bound of x (below it cosh overflows)
      real(wp) :: pairs ( npairs ) = 0.0_wp  !! reciprocal pairs 1/(i*(i+1)) of the truncated Taylor series
   end type stmp03_state

   ! ------------------------------------------------------------------ STR2ET

   integer(ip), parameter :: str2et_smwdsz = 16

   !> Convert a string representing an epoch to a double precision
   !> value representing the number of TDB seconds past the J2000
   !> epoch corresponding to the input epoch.
   !>
   !> The saved variables of SPICELIB's [[str2et]] routines (entry points str2et),
   !> held in [[pepper_context]] (one per thread).
   type, public :: str2et_state
      character(str2et_smwdsz) :: calndr = ' '  !! scratch: the default calendar, from [[timdef]]
      character(str2et_smwdsz) :: defsys = 'UTC'  !! scratch: the default time system, from [[timdef]]
      character(str2et_smwdsz) :: defzon = ' '  !! scratch: the default time zone, from [[timdef]]
      !> names of the months, for error messages
      character(str2et_smwdsz) :: mname ( 12 ) = [ character(str2et_smwdsz) :: 'January', 'February', 'March', &
         'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December' ]
      real(wp) :: hoff = 0.0_wp  !! scratch: hour offset of the time zone of the string
      real(wp) :: mdy ( 2 ) = 0.0_wp  !! scratch: last days of June and December (months that can have leapseconds)
      real(wp) :: moff = 0.0_wp  !! scratch: minute offset of the time zone of the string
      real(wp) :: mon ( 2 ) = 0.0_wp  !! scratch: the months (June, December) that can have leapseconds
      integer(ip) :: mm = 0  !! scratch: index of the month in the time vector
   end type str2et_state

   ! ------------------------------------------------------------------ SUBPNT

   integer(ip), parameter :: subpnt_maxl = 36
   integer(ip), parameter :: subpnt_frnmln = 32

   !> Compute the rectangular coordinates of the sub-observer point on
   !> a target body at a specified epoch, optionally corrected for
   !> light time and stellar aberration.
   !>
   !> The saved variables of SPICELIB's [[subpnt]] routines (entry points subpnt),
   !> held in [[pepper_context]] (one per thread).
   type, public :: subpnt_state
      logical :: first = .true.  !! first call: counters not yet initialized and no correction/method parsed yet
      logical :: near = .true.  !! the previous METHOD selects the near point (rather than the intercept) sub-point
      integer(ip) :: nsurf = 0  !! number of surface IDs in the previous METHOD string
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      character(mthlen) :: prvmth = ' '  !! METHOD string of the previous call
      integer(ip) :: shape = -1  !! target shape code parsed from the previous METHOD (ellipsoid or DSK)
      integer(ip), allocatable :: srflst (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) surface ID list parsed from the previous METHOD string
      logical :: usecn = .false.  !! the saved correction uses converged Newtonian light time
      logical :: uselt = .false.  !! the saved correction uses light time
      logical :: usestl = .false.  !! the saved correction uses stellar aberration
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(subpnt_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(subpnt_maxl) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsc = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(subpnt_frnmln) :: svfref = ' '  !! name of the body-fixed frame looked up last ([[zznamfrm]])
      integer(ip) :: svrefc = 0  !! ID code of the body-fixed frame looked up last
      integer(ip) :: svctr4 ( ctrsiz ) = 0  !! saved surface name/ID item declarations.
   end type subpnt_state

   ! ------------------------------------------------------------------ SUBPT

   integer(ip), parameter :: subpt_maxl = 36

   !> **Deprecated:** This routine has been superseded by the SPICELIB
   !> routine [[subpnt]]. This routine is supported for purposes of
   !> backward compatibility only.
   !>
   !> The saved variables of SPICELIB's [[subpt]] routines (entry points subpt),
   !> held in [[pepper_context]] (one per thread).
   type, public :: subpt_state
      real(wp) :: origin ( 3 ) = 0.0_wp  !! the origin (zero vector), constant
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(subpt_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(subpt_maxl) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsc = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      logical :: first = .true.  !! first call: the pool counters are not yet initialized
   end type subpt_state

   ! ------------------------------------------------------------------ SUBSLR

   integer(ip), parameter :: subslr_maxl = 36
   integer(ip), parameter :: subslr_frnmln = 32

   !> Compute the rectangular coordinates of the sub-solar point on
   !> a target body at a specified epoch, optionally corrected for
   !> light time and stellar aberration.
   !>
   !> The saved variables of SPICELIB's [[subslr]] routines (entry points subslr),
   !> held in [[pepper_context]] (one per thread).
   type, public :: subslr_state
      logical :: first = .true.  !! first call: pool counters not initialized, correction and method not yet parsed
      logical :: near = .true.  !! the saved method's sub-point type is near point/nadir (not intercept)
      integer(ip) :: nsurf = 0  !! number of surface IDs in the surface list parsed from the saved method
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      character(mthlen) :: prvmth = ' '  !! method string of the previous call
      integer(ip) :: shape = ellshp  !! target shape parsed from the saved method (ellipsoid or DSK)
      integer(ip), allocatable :: srflst (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) surface ID list parsed from the saved method
      logical :: usecn = .false.  !! the saved correction uses converged Newtonian light time (CN)
      logical :: uselt = .false.  !! the saved correction uses light time (LT or CN)
      logical :: usestl = .false.  !! the saved correction includes stellar aberration (+S)
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(subslr_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(subslr_maxl) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsc = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(subslr_frnmln) :: svfref = ' '  !! name of the body-fixed frame looked up last ([[zznamfrm]])
      integer(ip) :: svrefc = 0  !! ID code of the body-fixed frame looked up last
      integer(ip) :: svctr4 ( ctrsiz ) = 0  !! saved surface name/ID item declarations.
   end type subslr_state

   ! ------------------------------------------------------------------ SUBSOL

   integer(ip), parameter :: subsol_maxl = 36

   !> **Deprecated:** This routine has been superseded by the SPICELIB
   !> routine [[subslr]]. This routine is supported for purposes of
   !> backward compatibility only.
   !>
   !> The saved variables of SPICELIB's [[subsol]] routines (entry points subsol),
   !> held in [[pepper_context]] (one per thread).
   type, public :: subsol_state
      real(wp) :: origin ( 3 ) = 0.0_wp  !! the zero vector
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(subsol_maxl) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(subsol_maxl) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsc = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      logical :: first = .true.  !! first call: the pool counters are not yet initialized
   end type subsol_state

   ! ------------------------------------------------------------------ SURFPT

   integer(ip), parameter :: msgdim = 7
   integer(ip), parameter :: msglen = 32

   !> Determine the intersection of a line-of-sight vector with the
   !> surface of an ellipsoid.
   !>
   !> The saved variables of SPICELIB's [[surfpt]] routines (entry points surfpt),
   !> held in [[pepper_context]] (one per thread).
   type, public :: surfpt_state
      !> error messages for each combination of nonpositive axes, constant
      character(msglen) :: mssg ( msgdim ) = [ character(msglen) :: 'Axis A was nonpositive.', &
         'Axis B was nonpositive.', 'Axes A and B were nonpositive.', 'Axis C was nonpositive.', &
         'Axes A and C were nonpositive.', 'Axes B and C were nonpositive.', &
         'All three axes were nonpositive.' ]
   end type surfpt_state

   ! ------------------------------------------------------------------ SXFORM

   integer(ip), parameter :: sxform_frnmln = 32

   !> Return the state transformation matrix from one frame to
   !> another at a specified epoch.
   !>
   !> The saved variables of SPICELIB's [[sxform]] routines (entry points sxform),
   !> held in [[pepper_context]] (one per thread).
   type, public :: sxform_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! pool counter of the frame name lookup ([[zznamfrm]]) of the "from" frame
      character(sxform_frnmln) :: svfrom = ' '  !! name of the "from" frame looked up last
      integer(ip) :: svfcod = 0  !! ID code of the "from" frame looked up last
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! kernel pool state counter for the "to" frame name lookup
      character(sxform_frnmln) :: svto = ' '  !! name of the "to" frame looked up last
      integer(ip) :: svtcde = 0  !! ID code of the "to" frame looked up last
      logical :: first = .true.  !! true until the pool state counters are initialized
   end type sxform_state

   ! ------------------------------------------------------------------ TANGPT

   integer(ip), parameter :: tangpt_bdnmln = 36
   integer(ip), parameter :: tangpt_frnmln = 32
   integer(ip), parameter :: tangpt_loclen = 15

   !> Compute, for a given observer, ray emanating from the observer,
   !> and target, the "tangent point": the point on the ray nearest
   !> to the target's surface. Also compute the point on the target's
   !> surface nearest to the tangent point.
   !>
   !> The saved variables of SPICELIB's [[tangpt]] routines (entry points tangpt),
   !> held in [[pepper_context]] (one per thread).
   type, public :: tangpt_state
      logical :: first = .true.  !! first call: counters, correction and locus not yet set up
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      character(tangpt_loclen) :: prvloc = ' '  !! aberration correction locus string (CORLOC) of the previous call
      integer(ip) :: prvtcd = 0  !! saved body name/ID item declarations.
      logical :: tanloc = .false.  !! the saved locus is TANGENT POINT (false: SURFACE POINT)
      logical :: usecn = .false.  !! the saved correction uses converged Newtonian light time
      logical :: uselt = .false.  !! the saved correction includes light time
      logical :: usestl = .false.  !! the saved correction includes stellar aberration
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the target
      character(tangpt_bdnmln) :: svtarg = ' '  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde = 0  !! ID code of the target looked up last
      logical :: svfnd1 = .false.  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz ) = 0  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(tangpt_bdnmln) :: svobsr = ' '  !! name of the observer looked up last
      integer(ip) :: svobsc = 0  !! ID code of the observer looked up last
      logical :: svfnd2 = .false.  !! found flag of the last observer name lookup
      integer(ip) :: svctr3 ( ctrsiz ) = 0  !! saved frame name/ID item declarations.
      character(tangpt_frnmln) :: svfref = ' '  !! name of the body-fixed frame (FIXREF) looked up last ([[zznamfrm]])
      integer(ip) :: svfxfc = 0  !! ID code of the body-fixed frame looked up last
      integer(ip) :: svctr4 ( ctrsiz ) = 0  !! pool counter of the frame name lookup ([[zznamfrm]]) of the ray frame (DREF)
      character(tangpt_frnmln) :: svdref = ' '  !! name of the ray direction frame (DREF) looked up last
      integer(ip) :: svdfrc = 0  !! ID code of the ray direction frame looked up last
      integer(ip) :: svctr5 ( ctrsiz ) = 0  !! saved target radii declarations.
      real(wp) :: svradi ( 3 ) = 0.0_wp  !! target radii (km) returned by the last [[zzbodvcd]] lookup
      integer(ip) :: svnrad = 0  !! number of target radii returned by the last [[zzbodvcd]] lookup
   end type tangpt_state

   ! ------------------------------------------------------------------ TCHECK

   !> Determine whether the components of a time vector are in the
   !> "usual" range for the components, if component checking is
   !> enabled.
   !>
   !> The saved variables of SPICELIB's [[tcheck]] routines (entry points tcheck,
   !> tparch, tchckd), held in [[pepper_instance]].
   type, public :: tcheck_state
      logical :: dochck = .false.  !! check the components of parsed times (set by [[tparch]])
   end type tcheck_state

   ! ------------------------------------------------------------------ TERMPT

   integer(ip), parameter :: termpt_maxl = 36
   integer(ip), parameter :: termpt_frnmln = 32

   !> Find terminator points on a target body. The caller specifies
   !> half-planes, bounded by the illumination source center-target
   !> center vector, in which to search for terminator points.
   !>
   !> The saved variables of SPICELIB's [[termpt]] routines (entry points termpt),
   !> held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization (1272 bytes): see [[init_termpt_state]].
   type, public :: termpt_state
      logical :: first  !! first call: counters not yet initialized and no correction/method parsed yet
      integer(ip) :: loccde  !! work: code of the aberration correction locus (CENTER or ELLIPSOID TERMINATOR)
      real(wp) :: maxrad  !! work: maximum radius of the target DSK surfaces (DSK shape only)
      integer(ip) :: nrad  !! work: number of radii returned by the last [[zzbodvcd]] lookup
      integer(ip) :: nsurf  !! number of surface IDs in the previous METHOD string
      character(corlen) :: prvcor  !! aberration correction string of the previous call
      integer(ip) :: prvilu  !! ID code of the illumination source whose radii were looked up last (0 after a failure)
      character(acllen) :: prvloc  !! aberration correction locus string of the previous call
      character(mthlen) :: prvmth  !! METHOD string of the previous call
      integer(ip) :: prvtrg  !! ID code of the target whose radii were looked up last
      integer(ip) :: shadow  !! shadow type parsed from the previous METHOD (UMBRAL or PENUMBRAL code)
      integer(ip) :: shape  !! target shape code parsed from the previous METHOD (ellipsoid or DSK)
      integer(ip), allocatable :: srflst (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) surface ID list parsed from the previous METHOD string
      integer(ip) :: svlcod  !! aberration correction locus code parsed from the previous CORLOC
      integer(ip) :: trmtyp  !! terminator point definition parsed from the previous METHOD (TANGENT or GUIDED)
      logical :: uflag  !! the previous METHOD requested the umbral terminator
      logical :: usecn  !! the saved correction uses converged Newtonian light time
      logical :: uselt  !! the saved correction uses light time
      logical :: usestl  !! the saved correction uses stellar aberration
      integer(ip) :: svctr1 ( ctrsiz )  !! saved name/ID item declarations.
      character(termpt_maxl) :: svtarg  !! name of the target looked up last ([[zzbods2c]])
      integer(ip) :: svtcde  !! ID code of the target looked up last
      logical :: svfnd1  !! found flag of the last target name lookup
      integer(ip) :: svctr2 ( ctrsiz )  !! pool counter of the body name lookup ([[zzbods2c]]) of the observer
      character(termpt_maxl) :: svobsr  !! name of the observer looked up last
      integer(ip) :: svobsc  !! ID code of the observer looked up last
      logical :: svfnd2  !! found flag of the last observer name lookup
      integer(ip) :: svctr3 ( ctrsiz )  !! pool counter of the body name lookup ([[zzbods2c]]) of the illumination source
      character(termpt_maxl) :: svilum  !! name of the illumination source looked up last
      integer(ip) :: svicde  !! ID code of the illumination source looked up last
      logical :: svfnd3  !! found flag of the last illumination source name lookup
      integer(ip) :: svctr4 ( ctrsiz )  !! saved frame name/ID item declarations.
      character(termpt_frnmln) :: svfref  !! name of the body-fixed frame looked up last ([[zznamfrm]])
      integer(ip) :: svfxfc  !! ID code of the body-fixed frame looked up last
      integer(ip) :: svctr5 ( ctrsiz )  !! saved surface name/ID item declarations.
      integer(ip) :: svctr6 ( ctrsiz )  !! saved target radii declarations.
      real(wp) :: svtrad ( 3 )  !! radii of the target body (from the kernel pool, [[zzbodvcd]])
      integer(ip) :: svctr7 ( ctrsiz )  !! pool counter of the illumination source radii lookup ([[zzbodvcd]])
      real(wp) :: svsrad ( 3 )  !! radii of the illumination source (from the kernel pool, [[zzbodvcd]])
   end type termpt_state

   ! ------------------------------------------------------------------ TEXPYR

   !> Expand an abbreviated year to a full year specification.
   !>
   !> The saved variables of SPICELIB's [[texpyr]] routines (entry points texpyr,
   !> tsetyr), held in [[pepper_instance]].
   type, public :: texpyr_state
      integer(ip) :: centry = 1900  !! the century assumed for two-digit years
      integer(ip) :: lbound = 1969  !! the lower bound of the 100-year window for two-digit years
   end type texpyr_state

   ! ------------------------------------------------------------------ TIMDEF

   integer(ip), parameter :: timdef_wdsize = 16
   integer(ip), parameter :: nzones = 8

   !> Set and retrieve the defaults associated with calendar
   !> input strings.
   !>
   !> The saved variables of SPICELIB's [[timdef]] routines (entry points timdef),
   !> held in [[pepper_instance]].
   type, public :: timdef_state
      character(timdef_wdsize) :: defzon = ' '  !! setting: the default time zone, as UTC+HR[:MN] (blank when a system is set)
      character(timdef_wdsize) :: defsys = 'UTC'  !! setting: the default time system (UTC, TDB, TDT or TT; blank when a zone is set)
      character(timdef_wdsize) :: defcal = 'GREGORIAN'  !! setting: the default calendar (GREGORIAN, JULIAN or MIXED)
      !> names of the U.S. time zones that [[timdef]] translates to UTC offsets
      character(timdef_wdsize) :: zones ( nzones ) = [ character(timdef_wdsize) :: 'EST', 'EDT', 'CST', 'CDT', &
         'MST', 'MDT', 'PST', 'PDT' ]
   end type timdef_state

   ! ------------------------------------------------------------------ TIMOUT

   integer(ip), parameter :: currnt = 1
   integer(ip), parameter :: begin = currnt
   integer(ip), parameter :: noname = currnt  + 1
   integer(ip), parameter :: year = noname + 1
   integer(ip), parameter :: yr = year   + 1
   integer(ip), parameter :: umon = yr     + 1
   integer(ip), parameter :: mmon = umon   + 1
   integer(ip), parameter :: lmon = mmon   + 1
   integer(ip), parameter :: umnth = lmon   + 1
   integer(ip), parameter :: mmnth = umnth  + 1
   integer(ip), parameter :: lmnth = mmnth  + 1
   integer(ip), parameter :: month = lmnth  + 1
   integer(ip), parameter :: doy = month  + 1
   integer(ip), parameter :: uwkd = doy    + 1
   integer(ip), parameter :: mwkd = uwkd   + 1
   integer(ip), parameter :: lwkd = mwkd   + 1
   integer(ip), parameter :: uweekd = lwkd   + 1
   integer(ip), parameter :: mweekd = uweekd + 1
   integer(ip), parameter :: lweekd = mweekd + 1
   integer(ip), parameter :: day = lweekd + 1
   integer(ip), parameter :: minute = day    + 1
   integer(ip), parameter :: hour = minute + 1
   integer(ip), parameter :: sec = hour   + 1
   integer(ip), parameter :: point = sec    + 1
   integer(ip), parameter :: place = point  + 1
   integer(ip), parameter :: julian = place  + 1
   integer(ip), parameter :: utc = julian + 1
   integer(ip), parameter :: timout_tdb = utc    + 1
   integer(ip), parameter :: timout_tdt = timout_tdb    + 1
   integer(ip), parameter :: tt = timout_tdt    + 1
   integer(ip), parameter :: sp2000 = tt     + 1
   integer(ip), parameter :: sp1950 = sp2000 + 1
   integer(ip), parameter :: round = sp1950 + 1
   integer(ip), parameter :: timout_trunc = round  + 1
   integer(ip), parameter :: uera = timout_trunc  + 1
   integer(ip), parameter :: lera = uera   + 1
   integer(ip), parameter :: uerax = lera + 1
   integer(ip), parameter :: lerax = uerax + 1
   integer(ip), parameter :: uampm = lerax   + 1
   integer(ip), parameter :: lampm = uampm  + 1
   integer(ip), parameter :: utcp = lampm  + 1
   integer(ip), parameter :: utcm = utcp   + 1
   integer(ip), parameter :: jcal = utcm   + 1
   integer(ip), parameter :: gcal = jcal   + 1
   integer(ip), parameter :: mcal = gcal   + 1
   integer(ip), parameter :: timsys = mcal   + 1
   integer(ip), parameter :: calndr = timsys + 1
   integer(ip), parameter :: ampm = calndr + 1
   integer(ip), parameter :: mon = ampm   + 1
   integer(ip), parameter :: wkday = mon    + 1
   integer(ip), parameter :: era = wkday  + 1
   integer(ip), parameter :: noon = era    + 1
   integer(ip), parameter :: rlyear = noon   + 1
   integer(ip), parameter :: finish = rlyear
   integer(ip), parameter :: timout_ymd = 1
   integer(ip), parameter :: contin = timout_ymd + 1
   integer(ip), parameter :: maxmrk = 43
   integer(ip), parameter :: timout_maxlen = 256
   integer(ip), parameter :: mrksiz = 8
   integer(ip), parameter :: timout_loclen = 32

   !> Convert an input epoch represented in TDB seconds past the TDB
   !> epoch of J2000 to a character string formatted to the
   !> specifications of a user's format picture.
   !>
   !> The saved variables of SPICELIB's [[timout]] routines (entry points timout),
   !> held in [[pepper_context]] (one per thread).
   type, public :: timout_state
      logical :: first = .true.  !! first call: the format mark tables are not yet set up
      character(mrksiz) :: marks (     maxmrk ) = ' '  !! the next set of variables holds the marks and auxiliary arrays used for scanning the format string.
      integer(ip) :: class ( 0 : maxmrk ) = 0  !! component class of each format mark (0: no mark)
      integer(ip) :: mrklen (     maxmrk ) = 0  !! lengths of the format marks
      integer(ip) :: pntrs (     100    ) = 0  !! scanning pointers of the format marks (from [[scanpr]])
      integer(ip) :: dump (     11     ) = 0  !! mark IDs of the time system/rounding/calendar tokens removed from the format
      integer(ip) :: ndump = 0  !! number of mark IDs in DUMP
      integer(ip) :: timfmt = 0  !! work: kind of time vector (YMD or CONTIN) of the current component
      integer(ip) :: numtyp = 0  !! work: component type of the last numeric field
      character(32) :: orignl ( begin : finish ) = ' '  !! the variables below are used to hold, base formats, values of time vector components, adjustments to use when rounding, the lengths of the format pictures and whether or not various components have already been computed.
      real(wp) :: values ( begin : finish ) = 0.0_wp  !! work: value of each time component at the epoch
      real(wp) :: pad ( begin : finish ) = 0.0_wp  !! amount added to the epoch to round each component
      integer(ip) :: length ( begin : finish ) = 0  !! default output width of each component
      integer(ip) :: id ( begin : finish ) = 0  !! mark index of each component in MARKS
      logical :: have ( begin : finish ) = .false.  !! work: each component value has been computed for this epoch
      integer(ip) :: compnt ( 8, contin ) = 0  !! the array below contains the indexes of the various values associated with the three different times of time vectors that we will be using YMD, YD, CONTIN.
      character(timout_maxlen) :: string = ' '  !! work: output string being built
      character(timout_loclen) :: fmt = ' '  !! work: numeric format picture of the current component
      integer(ip) :: width = 0  !! work: width of the current numeric field
      character(16) :: intyp = ' '  !! work: time vector type ([[ttrans]]) of the current component
      character(16) :: tsys = ' '  !! work: default time system ([[timdef]])
      character(16) :: cal = ' '  !! work: default calendar ([[timdef]])
      character(32) :: zon = ' '  !! work: default time zone ([[timdef]])
      real(wp) :: ntvec ( 10 ) = 0.0_wp  !! work: time vector of the next unit of the current component
      real(wp) :: ptvec ( 10 ) = 0.0_wp  !! work: time vector of the start of the current component unit
      real(wp) :: tvec ( 10 ) = 0.0_wp  !! work: time vector of the epoch
      real(wp) :: value = 0.0_wp  !! work: value of the current numeric component
      integer(ip) :: mylen = 0  !! work: length of the current month or weekday name
   end type timout_state

   ! ------------------------------------------------------------------ TISBOD

   integer(ip), parameter :: mxpoly = 3
   integer(ip), parameter :: maxbod = 157

   !> The nutation/precession coefficients of a body buffered by [[tisbod]].
   !> PEPPER: arrays of the body's sizes (SPICELIB: MAXANG, 200, per item
   !> and body, about 1.5 MB per context).
   type, public :: tisbod_angles
      real(wp), allocatable :: t(:)  !! the angle (phase) coefficients (NUT_PREC_ANGLES)
      real(wp), allocatable :: a(:)  !! the RA coefficients (NUT_PREC_RA)
      real(wp), allocatable :: d(:)  !! the DEC coefficients (NUT_PREC_DEC)
      real(wp), allocatable :: w(:)  !! the prime meridian coefficients (NUT_PREC_PM)
   end type tisbod_angles

   !> Return a 6x6 matrix that transforms states in inertial
   !> coordinates to states in body-equator-and-prime-meridian
   !> coordinates.
   !>
   !> The saved variables of SPICELIB's [[tisbod]] routines (entry points tisbod),
   !> held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization (1783072 bytes): see [[init_tisbod_state]].
   type, public :: tisbod_state
      real(wp) :: d  !! seconds per day
      real(wp) :: t  !! seconds per Julian century
      integer(ip) :: j2code  !! frame ID code of J2000
      logical :: first  !! true until the first-call initialization is done
      integer(ip) :: pulctr ( ctrsiz )  !! POOL state counter.
      integer(ip) :: bidlst (             maxbod )  !! body ID hash: collision list heads
      integer(ip) :: bidpol ( lbpool :    maxbod )  !! body ID hash: collision list pool
      integer(ip) :: bidids (             maxbod )  !! body ID hash: ID codes of the buffered bodies
      real(wp) :: bdcoef ( 0:mxpoly-1, maxbod )  !! buffered DEC polynomial coefficients of each body
      real(wp) :: bpckep (             maxbod )  !! buffered PCK reference epoch (JED) of each body
      real(wp) :: brcoef ( 0:mxpoly-1, maxbod )  !! buffered RA polynomial coefficients of each body
      real(wp) :: bwcoef ( 0:mxpoly-1, maxbod )  !! buffered prime meridian (W) polynomial coefficients of each body
      integer(ip) :: bna (             maxbod )  !! buffered count of RA nutation precession coefficients of each body
      integer(ip) :: bnd (             maxbod )  !! buffered count of DEC nutation precession coefficients of each body
      integer(ip) :: bnphas (             maxbod )  !! buffered number of nutation precession angles of each body
      integer(ip) :: bnphco (             maxbod )  !! buffered number of coefficients per nutation precession angle of each body
      integer(ip) :: bnw (             maxbod )  !! buffered count of W nutation precession coefficients of each body
      type(tisbod_angles) :: bang ( maxbod )  !! buffered nutation precession coefficients of each body
      integer(ip) :: bpcref (             maxbod )  !! buffered PCK reference frame ID of each body
   end type tisbod_state

   ! ------------------------------------------------------------------ TKFRAM

   integer(ip), parameter :: tkfram_bufsiz = 200
   integer(ip), parameter :: tkfram_wdsize = 32
   integer(ip), parameter :: nitems = 14
   integer(ip), parameter :: nvars = 14
   integer(ip), parameter :: ndps = 9
   integer(ip), parameter :: nints = 1

   !> Find the position rotation matrix from a Text Kernel (TK) frame
   !> with the specified frame class ID to its base frame.
   !>
   !> The saved variables of SPICELIB's [[tkfram]] routines (entry points tkfram),
   !> held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization (18592 bytes): see [[init_tkfram_state]].
   type, public :: tkfram_state
      character(tkfram_wdsize) :: oldagt  !! work: watcher agent name of the frame dropped from the full buffer
      real(wp) :: buffd ( ndps, tkfram_bufsiz )  !! buffered rotation matrix (column order) of each frame
      integer(ip) :: at  !! buffer index of the frame looked up last
      integer(ip) :: buffi ( nints,     tkfram_bufsiz )  !! buffered ID of the frame each buffered rotation is relative to
      integer(ip) :: idents ( 1,         tkfram_bufsiz )  !! frame ID codes of the buffered frames
      integer(ip) :: idnt ( 1 )  !! work: frame ID to look up in the buffer
      integer(ip) :: pool ( 2, lbpool: tkfram_bufsiz )  !! linked list pool of the buffer (in order of use)
      logical :: first  !! true until the buffer pool is initialized
   end type tkfram_state

   ! ------------------------------------------------------------------ TTRANS

   integer(ip), parameter :: ttrans_maxvar = 1
   integer(ip), parameter :: daysec = 1
   integer(ip), parameter :: dayp2 = daysec + 1
   integer(ip), parameter :: et = dayp2 + 1
   integer(ip), parameter :: frml = et     + 1
   integer(ip), parameter :: jdtdb = frml + 1
   integer(ip), parameter :: jdtdt = jdtdb  + 1
   integer(ip), parameter :: jdutc = jdtdt  + 1
   integer(ip), parameter :: jed = jdutc  + 1
   integer(ip), parameter :: ttai = jed    + 1
   integer(ip), parameter :: ttrans_tdb = ttai   + 1
   integer(ip), parameter :: ttrans_tdt = ttrans_tdb    + 1
   integer(ip), parameter :: yd = ttrans_tdt    + 1
   integer(ip), parameter :: ydd = yd     + 1
   integer(ip), parameter :: yddf = ydd    + 1
   integer(ip), parameter :: ydf = yddf   + 1
   integer(ip), parameter :: ttrans_ymd = ydf    + 1
   integer(ip), parameter :: ymdf = ttrans_ymd    + 1
   integer(ip), parameter :: ymwd = ymdf   + 1
   integer(ip), parameter :: ymwdf = ymwd   + 1
   integer(ip), parameter :: ywd = ymwdf  + 1
   integer(ip), parameter :: ywdf = ywd    + 1
   integer(ip), parameter :: nrec = ywdf

   !> Transform a time vector from one representation and system
   !> to another.
   !>
   !> The saved variables of SPICELIB's [[ttrans]] routines (entry points ttrans),
   !> held in [[pepper_context]] (one per thread).
   type, public :: ttrans_state
      character(typlen) :: unifrm (lbcell : nrec ) = ' '  !! set of the uniform time systems, which [[unitim]] converts
      character(typlen) :: recog ( nrec ) = ' '  !! sorted names of the recognized time vector representations
      integer(ip) :: parsed ( nrec ) = 0  !! integer code of each recognized representation (parallel to RECOG)
      logical :: needy ( nrec ) = .false.  !! the representation has a year (parallel to RECOG)
      logical :: forml ( nrec ) = .false.  !! the representation is a formal (not UTC-based) time system (parallel to RECOG)
      character(32) :: vars ( ttrans_maxvar ) = ' '  !! name of the watched kernel pool variable (DELTET/DELTA_AT)
      real(wp) :: halfd = 0.0_wp  !! number of seconds in half a day
      real(wp) :: jd1101 = 0.0_wp  !! Julian date UTC of Jan 1, 1 AD (computed on the first call)
      real(wp) :: secspd = 0.0_wp  !! number of seconds in a day
      real(wp), allocatable :: taitab ( : )  !! TAI at the starts of the UTC days around each leapsecond (from DELTET/DELTA_AT); grows (SPICELIB: 140 leapseconds)
      real(wp) :: tsecs = 0.0_wp  !! scratch: seconds into the day, then into the minute, of the time being converted
      integer(ip), allocatable :: daytab ( : )  !! day numbers past 1 Jan 1 AD of the days in TAITAB
      integer(ip) :: dn2000 = 0  !! day number past 1 Jan 1 AD of Jan 1, 2000 (computed on the first call)
      integer(ip) :: dofyr = 0  !! scratch: day of year of the time being converted
      integer(ip) :: nref = 0  !! number of entries in TAITAB and DAYTAB
      integer(ip) :: sunday = 0  !! day number past 1 Jan 1 AD of Sunday, Jan 6, 1991 (computed on the first call)
      integer(ip) :: usrctr ( ctrsiz ) = 0  !! counter of the kernel pool's state when the leapseconds were read
      logical :: first = .true.  !! the first call has not set up the tables yet
      logical :: nodata = .true.  !! the leapseconds have not been read (successfully) from the kernel pool
   end type ttrans_state

   ! ------------------------------------------------------------------ UNITIM

   integer(ip), parameter :: needed = 4
   integer(ip), parameter :: lngvar = 16
   integer(ip), parameter :: mislen = 20
   integer(ip), parameter :: ntdt = 5
   integer(ip), parameter :: ntdb = 4
   integer(ip), parameter :: unitim_nrecog = ntdb + ntdt

   !> Transform time from one uniform scale to another. The uniform
   !> time scales are TAI, GPS, TT, TDT, TDB, ET, JED, JDTDB, JDTDT.
   !>
   !> The saved variables of SPICELIB's [[unitim]] routines (entry points unitim),
   !> held in [[pepper_context]] (one per thread).
   type, public :: unitim_state
      character(1) :: bslash = ' '  !! the backslash character
      real(wp) :: dta = 0.0_wp  !! TDT - TAI offset, in seconds (DELTET/DELTA_T_A from the kernel pool)
      real(wp) :: eb = 0.0_wp  !! eccentricity of the heliocentric orbit of the Earth-Moon barycenter (DELTET/EB)
      logical :: first = .true.  !! the first call has not set up the tables yet
      real(wp) :: jd2000 = 0.0_wp  !! Julian date of J2000
      real(wp) :: k = 0.0_wp  !! amplitude of the TDB - TDT periodic term, in seconds (DELTET/K)
      real(wp) :: m ( 0:1 ) = 0.0_wp  !! mean anomaly of the Earth-Moon barycenter at J2000 and its rate (DELTET/M)
      !> names of the needed kernel pool variables, for the missing-variable message
      character(mislen) :: missed ( needed ) = [ character(mislen) :: 'DELTET/DELTA_T_A, #', 'DELTET/K, #', &
         'DELTET/EB, #', 'DELTET/M, #' ]
      logical :: nodata = .true.  !! the [[deltet]] variables have not been read (successfully) from the kernel pool
      character(typlen) :: recog ( lbcell : unitim_nrecog    ) = ' '  !! set of all the recognized time types (union of TYPTDB and TYPTDT)
      real(wp) :: secspd = 0.0_wp  !! number of seconds in a day
      character(typlen) :: typtdb ( lbcell : ntdb      ) = ' '  !! set of the time types based on TDB (ET, JDTDB, JED, TDB)
      character(typlen) :: typtdt ( lbcell : ntdt      ) = ' '  !! set of the time types based on TDT (GPS, JDTDT, TAI, TDT, TT)
      !> names of the watched kernel pool variables (DELTET/...)
      character(lngvar) :: vars ( needed ) = [ character(lngvar) :: 'DELTET/DELTA_T_A', 'DELTET/K', &
         'DELTET/EB', 'DELTET/M' ]
      integer(ip) :: usrctr ( ctrsiz ) = 0  !! counter of the kernel pool's state when the [[deltet]] variables were read
   end type unitim_state

   ! ------------------------------------------------------------------ WRENCC

   !> Encode and write characters to a text file.
   !>
   !> The saved variables of SPICELIB's [[wrencc]] routines (entry points wrencc),
   !> held in [[pepper_context]] (one per thread).
   type, public :: wrencc_state
      integer(ip) :: intesc = 0  !! character code of the escape character
      integer(ip) :: intfpc = 0  !! character code of the first printable character
      integer(ip) :: intlpc = 0  !! character code of the last printable character
      integer(ip) :: intquo = 0  !! character code of the quote character
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
   end type wrencc_state

   ! ------------------------------------------------------------------ XFMSTA

   integer(ip), parameter :: xfmsta_maxl = 36

   !> Transform a state between coordinate systems.
   !>
   !> The saved variables of SPICELIB's [[xfmsta]] routines (entry points xfmsta),
   !> held in [[pepper_context]] (one per thread).
   type, public :: xfmsta_state
      integer(ip) :: svctr1 ( ctrsiz ) = 0  !! saved name/ID item declarations.
      character(xfmsta_maxl) :: svbody = ' '  !! name of the body looked up last ([[zzbods2c]])
      integer(ip) :: svbdid = 0  !! ID code of the body looked up last
      logical :: svfnd1 = .false.  !! found flag of the last body name lookup
      logical :: first = .true.  !! first call: the pool counter is not yet initialized
   end type xfmsta_state

   ! ------------------------------------------------------------------ ZZBODBLT

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZBODBLT routines (entry points
   !> zzbodget, zzbodlst), held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization (52596 bytes): see [[init_zzbodblt_state]].
   type, public :: zzbodblt_state
      character(maxl) :: bltnam ( nperm )  !! names of the built-in bodies
      character(maxl) :: bltnor ( nperm )  !! the names, normalized
      integer(ip) :: bltcod ( nperm )  !! NAIF ID codes of the built-in bodies
      logical :: first  !! the tables have not been set up yet (they are set up on the first call)
   end type zzbodblt_state

   ! ------------------------------------------------------------------ ZZBODTRN

   !> Body names and codes: those from the kernel pool (NAIF_BODY_NAME,
   !> NAIF_BODY_CODE, as [[zzbodker]] reads them), or the built-in and BODDEF
   !> definitions.
   !>
   !> PEPPER: SPICELIB kept them in arrays and hash tables of NROOM (14983)
   !> and MAXE (853) entries, with names of at most MAXL (36) characters,
   !> and signaled SPICE(KERVARTOOBIG) or SPICE(TOOMANYPAIRS) for more;
   !> these grow, and names have any length.
   !> As in SPICELIB, the last assignment of a name counts, and a code maps
   !> to the last assignment whose name is not assigned again later.
   type, public :: body_names
      type(string), allocatable :: names(:)  !! the names, as in the kernel pool
      type(string), allocatable :: nornam(:)  !! the names, normalized (uppercase, single blanks)
      integer(ip), allocatable :: codes(:)  !! the codes
      integer(ip) :: nvals = 0  !! number of names and codes
      logical :: extker = .false.  !! the kernel pool defines names
      type(string_map) :: name_map  !! the index of each normalized name
      type(int_map) :: code_map  !! the index of each code
   contains
      procedure :: clear => body_names_clear
      procedure :: append => body_names_append
      procedure :: remove => body_names_remove
      procedure :: index => body_names_index
      procedure :: index_last => body_names_index_last
      procedure :: name_index => body_names_name_index
      procedure :: code_index => body_names_code_index
   end type body_names

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZBODTRN routines (entry points
   !> zzbodn2c, zzbodc2n, zzboddef, zzbodkik, zzbodrst, zzbctrck), held in
   !> [[pepper_context]] (one per thread).
   !>
   !> No default initialization (1677792 bytes): see [[init_zzbodtrn_state]].
   type, public :: zzbodtrn_state
      type(body_names) :: def  !! the built-in and BODDEF-defined names and codes
      type(body_names) :: ker  !! the names from the kernel pool (NAIF_BODY_NAME/CODE)
      integer(ip) :: subctr ( ctrsiz )  !! body name state counter (changes when the name tables
                                        !! change)
      integer(ip) :: pulctr ( ctrsiz )  !! the pool state counter when the kernel pool tables were
                                        !! built
      logical :: bodchg  !! the defined names changed
      logical :: nodata  !! the kernel pool tables have not been built yet
      integer(ip) :: i  !! work variable (saved in SPICELIB; not shown to carry a value between
                        !! calls)
      logical :: first  !! the tables have not been set up yet (they are set up on the first call)
      integer(ip) :: boddef_epoch  !! the instance's boddef_epoch these tables include
      integer(ip) :: boddef_count  !! the number of the instance's definitions these tables include
   end type zzbodtrn_state

   ! ------------------------------------------------------------------ ZZBQUAD

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzbquad]] routines (entry points
   !> zzbquad), held in [[pepper_context]] (one per thread).
   type, public :: zzbquad_state
      real(wp) :: big = 0.0_wp  !! bound on the magnitude of the coefficients: sqrt(dpmax)/100, set on the first call
      logical :: first = .true.  !! first call: big not yet computed
   end type zzbquad_state

   ! ------------------------------------------------------------------ ZZCNQUAD

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzcnquad]] routines (entry points
   !> zzcnquad), held in [[pepper_context]] (one per thread).
   type, public :: zzcnquad_state
      logical :: first = .true.  !! first call: INVUB not yet computed
      real(wp) :: invub = -1.0_wp  !! upper bound on the magnitude of root inverses, sqrt(DPMAX)/200
   end type zzcnquad_state

   ! ------------------------------------------------------------------ ZZCVSTAT

   integer(ip), parameter :: zzcvstat_frnmln = 32

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZCVSTAT routines (entry points
   !> zzcvxsta, zzcvssta), held in [[pepper_context]] (one per thread).
   type, public :: zzcvstat_state
      integer(ip) :: svctr = 0  !! ID code of the center of motion of the stored constant-velocity state
      real(wp) :: svet = 0.0_wp  !! epoch of the stored constant-velocity state
      character(:), allocatable :: svref  !! reference frame name of the stored constant-velocity state
      real(wp) :: svstat ( 6 ) = 0.0_wp  !! constant-velocity state stored by [[zzcvssta]]
   end type zzcvstat_state

   ! ------------------------------------------------------------------ ZZDAFGDR

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzdafgdr]] routines (entry points
   !> zzdafgdr), held in [[pepper_context]] (one per thread).
   type, public :: zzdafgdr_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      integer(ip) :: natbff = 0  !! code of the native binary file format
      character(strsiz) :: strbff ( numbff ) = ' '  !! names of the binary file formats
   end type zzdafgdr_state

   ! ------------------------------------------------------------------ ZZDAFGFR

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzdafgfr]] routines (entry points
   !> zzdafgfr), held in [[pepper_context]] (one per thread).
   type, public :: zzdafgfr_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      integer(ip) :: natbff = 0  !! code of the native binary file format
      character(strsiz) :: strbff ( numbff ) = ' '  !! names of the binary file formats
   end type zzdafgfr_state

   ! ------------------------------------------------------------------ ZZDAFGSR

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzdafgsr]] routines (entry points
   !> zzdafgsr), held in [[pepper_context]] (one per thread).
   type, public :: zzdafgsr_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      integer(ip) :: natbff = 0  !! code of the native binary file format
      character(strsiz) :: strbff ( numbff ) = ' '  !! names of the binary file formats
   end type zzdafgsr_state

   ! ------------------------------------------------------------------ ZZDAFNFR

   integer(ip), parameter :: zzdafnfr_preftp = 603

   !> Write the file record to a new DAF file.
   !>
   !> The saved variables of SPICELIB's [[zzdafnfr]] routines (entry points
   !> zzdafnfr), held in [[pepper_context]] (one per thread).
   type, public :: zzdafnfr_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      character(sizftp) :: ftpstr = ' '  !! the FTP validation string
      character(zzdafnfr_preftp) :: prenul = ' '  !! the bytes before the FTP validation string (nulls)
      character(pstftp) :: pstnul = ' '  !! the bytes after the FTP validation string (nulls)
   end type zzdafnfr_state

   ! ------------------------------------------------------------------ ZZDASGRD

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzdasgrd]] routines (entry points
   !> zzdasgrd), held in [[pepper_context]] (one per thread).
   type, public :: zzdasgrd_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      integer(ip) :: intbff = 0  !! code of the binary file format of the file read last
      integer(ip) :: natbff = 0  !! code of the native binary file format
   end type zzdasgrd_state

   ! ------------------------------------------------------------------ ZZDASGRI

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzdasgri]] routines (entry points
   !> zzdasgri), held in [[pepper_context]] (one per thread).
   type, public :: zzdasgri_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      integer(ip) :: intbff = 0  !! code of the binary file format of the file read last
      integer(ip) :: natbff = 0  !! code of the native binary file format
   end type zzdasgri_state

   ! ------------------------------------------------------------------ ZZDASNFR

   integer(ip), parameter :: zzdasnfr_preftp = 607

   !> Write the file record to a new DAS file.
   !>
   !> The saved variables of SPICELIB's [[zzdasnfr]] routines (entry points
   !> zzdasnfr), held in [[pepper_context]] (one per thread).
   type, public :: zzdasnfr_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      character(sizftp) :: ftpstr = ' '  !! the FTP validation string
      character(zzdasnfr_preftp) :: prenul = ' '  !! the bytes before the FTP validation string (nulls)
      character(pstftp) :: pstnul = ' '  !! the bytes after the FTP validation string (nulls)
   end type zzdasnfr_state

   ! ------------------------------------------------------------------ ZZDASRFR

   !> Return the contents of the file record of a specified DAS file.
   !>
   !> The saved variables of SPICELIB's [[zzdasrfr]] routines (entry points
   !> zzdasrfr), held in [[pepper_context]] (one per thread).
   type, public :: zzdasrfr_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      integer(ip) :: natbff = -1  !! code of the native binary file format
   end type zzdasrfr_state

   ! ------------------------------------------------------------------ ZZDDHGSD

   integer(ip), parameter :: locsiz = 8
   integer(ip), parameter :: numcls = 3

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzddhgsd]] routines (entry points
   !> zzddhgsd), held in [[pepper_context]] (one per thread).
   type, public :: zzddhgsd_state
      character(locsiz) :: clslst ( numcls ) = [ character(locsiz) :: 'METHOD', 'ARCH', 'BFF' ]  !! names of the classes of strings (METHOD, ARCH, BFF)
      character(strsiz) :: stramh ( numamh ) = [ character(strsiz) :: 'READ', 'WRITE', 'SCRATCH', 'NEW' ]  !! names of the access methods
      character(strsiz) :: strarc ( numarc ) = [ character(strsiz) :: 'DAF', 'DAS' ]  !! names of the architectures
      !> names of the binary file formats
      character(strsiz) :: strbff ( numbff ) = [ character(strsiz) :: 'BIG-IEEE', 'LTL-IEEE', 'VAX-GFLT', &
         'VAX-DFLT' ]
   end type zzddhgsd_state

   ! ------------------------------------------------------------------ ZZDDHMAN

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZDDHMAN routines (entry points
   !> zzddhopn, zzddhcls, zzddhhlu, zzddhunl, zzddhnfo, zzddhisn, zzddhfnh,
   !> zzddhluh), held in [[pepper_instance]] (and, for some variables, in
   !> [[pepper_context]]).
   !>
   !> PEPPER: SPICELIB's file table was eight parallel arrays of FTSIZE
   !> (5000) entries, with names of at most FILEN (255) characters; it is
   !> an array of these records that grows as files are loaded, with names
   !> of any length. (SPICELIB's FTABS and FTRTM are abs(handle) and
   !> len(name).)
   type, public :: ddh_file
      integer(ip) :: handle = 0  !! the handle (negative for files open for writing)
      integer(ip) :: method = 0  !! access method (READ, WRITE, SCRTCH, NEW)
      integer(ip) :: arch = 0  !! architecture (DAF, DAS)
      integer(ip) :: bff = 0  !! binary file format
      real(wp) :: mnm = 0.0_wp  !! unique number (from the file's attributes)
      character(:), allocatable :: name  !! the file name
      !> The whole file, when it is loaded in memory (FURNSH's `in_memory`):
      !> DAF word address `a` is `image(a)`. Written when the file is loaded,
      !> read by all contexts, freed with the entry.
      real(wp), allocatable :: image(:)
      !> PEPPER: a DAF built in memory ([[spice_load_kernel]], or a kernel
      !> loader's): `image` is the whole file and there is no file behind
      !> it, so the file is never opened, inquired or read by its name.
      logical :: synthetic = .false.
      !> Raw 1024-byte records when loaded directly from a byte buffer in
      !> memory ([[spice_furnsh_memory]]), with no OS file.
      character(len=1024), allocatable :: raw_records(:)
   end type ddh_file

   !> The file table: the files in increasing order of abs(handle) (the
   !> order in which they were opened), so that a handle is found by
   !> binary search.
   type, public :: zzddhman_state
      integer(ip) :: nft = 0  !! number of files in the file table
      type(ddh_file), allocatable :: files(:)  !! the files (1 to nft are used)
      integer(ip) :: next = 0  !! the next handle number (handles are never reused)
      ! PEPPER: one load (ZZLDKER) checks the same file twice, in GETFAT and
      ! in ZZDDHOPN, and GETFAT opens it again to read its ID word; each
      ! check (ZZDDHF2H) opens the file and reads two records. While
      ! reuse_f2h is set (by FURNSH around ZZLDKER), ZZDDHF2H keeps its
      ! result for a file that is not in the table, with the start of its
      ! first record, until the table changes.
      integer(ip) :: changes = 0  !! changes of the table (add, remove)
      logical :: reuse_f2h = .false.  !! [[zzddhf2h]] may keep and reuse its result
      !> PEPPER: names are files only: a DAF built in memory is not found
      !> by its name (GETFAT, ZZDDHOPN). Set by ZZLDKER while it loads a
      !> file from disk, so that a file is read even if a kernel built in
      !> memory has its name (a loader's file loaded again, or an SPK
      !> written from a kernel under the kernel's name).
      logical :: files_only = .false.
      character(:), allocatable :: f2h_name  !! the file of the kept result
      integer(ip) :: f2h_changes = -1  !! the table's changes when it was kept
      real(wp) :: f2h_mnm = 0.0_wp  !! the file's unique number
      character(12) :: f2h_word = ' '  !! the first 12 characters of its first record
      logical :: f2h_word_ok = .false.  !! the first record could be read (direct access)
   contains
      procedure :: find => ddh_find
      procedure :: add => ddh_add
      procedure :: remove => ddh_remove
      procedure :: kept_f2h => ddh_kept_f2h
   end type zzddhman_state

   !> The saved variables of ZZDDHMAN that are per context.
   !>
   !> PEPPER: the unit table has the instance's [[max_open_files]] entries
   !> (SPICELIB: UTSIZE, 23), allocated when the context first opens a
   !> file.
   type, public :: zzddhman_context_state
      logical :: first = .true.  !! the tables of names have not been built yet
      logical :: opnfst = .true.  !! no file has been opened yet (platform check)
      character(strsiz) :: stramh ( numamh ) = ' '  !! names of the access methods
      character(strsiz) :: strarc ( numarc ) = ' '  !! names of the architectures
      character(strsiz) :: strbff ( numbff ) = ' '  !! names of the binary file formats
      integer(ip) :: natbff = 0  !! the native binary file format
      integer(ip) :: supbff ( numbff ) = 0  !! the supported binary file formats
      integer(ip) :: numsup = 0  !! number of supported binary file formats
      integer(ip) :: nut = 0  !! number of entries in the unit table
      integer(ip), allocatable :: utcst ( : )  !! cost (time of the last request) of each unit
      integer(ip), allocatable :: uthan ( : )  !! handle of the file connected to each unit
      logical, allocatable :: utlck ( : )  !! the unit is locked to its file
      integer(ip), allocatable :: utlun ( : )  !! the units
      integer(ip) :: reqcnt = 0  !! request counter (for the costs)
      integer(ip) :: closegen = 0  !! the instance's close_generation the unit table reflects
   end type zzddhman_context_state

   ! ------------------------------------------------------------------ ZZDDHMNM

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzddhmnm]] routines (entry points
   !> zzddhmnm), held in [[pepper_context]] (one per thread).
   type, public :: zzddhmnm_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      integer(ip) :: natbff = 0  !! code of the native binary file format
      integer(ip) :: supbff ( numbff ) = 0  !! codes of the supported binary file formats
      integer(ip) :: numsup = 0  !! number of supported binary file formats
   end type zzddhmnm_state

   ! ------------------------------------------------------------------ ZZDDHNFC

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzddhnfc]] routines (entry points
   !> zzddhnfc), held in [[pepper_context]] (one per thread).
   type, public :: zzddhnfc_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      integer(ip) :: savbff = 0  !! code of the native binary file format
      character(strsiz) :: strbff ( numbff ) = ' '  !! names of the binary file formats
   end type zzddhnfc_state

   ! ------------------------------------------------------------------ ZZDDHPPF

   integer(ip), parameter :: nullid = numbff+1

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzddhppf]] routines (entry points
   !> zzddhppf), held in [[pepper_context]] (one per thread). Generated by
   !> tools/modularize_ctx.py from the saved variables of zzddhppf.F90.
   type, public :: zzddhppf_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      character(sizdlm) :: ftpdlm = ' '  !! delimiter of the FTP validation string
      character(sizend) :: ftplft = ' '  !! left bracket of the FTP validation string
      character(sizstr) :: ftpmem = ' '  !! the FTP validation string without its brackets
      character(sizend) :: ftprgt = ' '  !! right bracket of the FTP validation string
      character(1) :: null = ' '  !! the null character
      character(strsiz) :: strarc ( numarc ) = ' '  !! names of the architectures
      character(strsiz) :: strbff ( nullid ) = ' '  !! names of the binary file formats
   end type zzddhppf_state

   ! ------------------------------------------------------------------ ZZDIV

   !> Safely calculate the value NUMR/DENOM, avoiding the possibility
   !> of floating point exceptions (FPE), due to numeric underflow,
   !> numeric overflow, or divide-by-zero.
   !>
   !> The saved variables of SPICELIB's [[zzdiv]] routines (entry points zzdiv),
   !> held in [[pepper_context]] (one per thread).
   type, public :: zzdiv_state
      real(wp) :: expnt = 0.0_wp  !! the bounds on the potential result of the calculation.
      logical :: first = .true.  !! first entry flag.
   end type zzdiv_state

   ! ------------------------------------------------------------------ ZZDNPT

   !> Compute the velocity of an ellipsoid surface point nearest to a
   !> specified position.
   !>
   !> The saved variables of SPICELIB's [[zzdnpt]] routines (entry points zzdnpt),
   !> held in [[pepper_context]] (one per thread).
   type, public :: zzdnpt_state
      !> diag(1/a**2, 1/b**2, 1/c**2): maps a surface point to (half) the ellipsoid gradient
      real(wp) :: gradm ( 3, 3 ) = reshape ( [ 1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, &
         1.0_wp ], [ 3, 3 ] )
      !> inverse of (identity + l*gradm), diagonal (off-diagonal zeros set once)
      real(wp) :: m ( 3, 3 ) = reshape ( [ 1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, &
         1.0_wp ], [ 3, 3 ] )
   end type zzdnpt_state

   ! ------------------------------------------------------------------ ZZDSKBBL

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzdskbbl]] routines (entry points
   !> zzdskbbl), held in [[pepper_context]] (one per thread).
   type, public :: zzdskbbl_state
      integer(ip) :: ctr ( ctrsiz ) = 0  !! DSK state change counter ([[zzdskchk]])
      logical :: first = .true.  !! first call: the counter is not yet initialized
      integer(ip) :: prvbod = 0  !! body ID code of the previous successful segment search
   end type zzdskbbl_state

   ! ------------------------------------------------------------------ ZZDSKBSR

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> SPICELIB's ZZDSKBSR (entry points zzdsklsf, zzdskusf, zzdskbss,
   !> zzdsksns, zzdskchk): the loaded DSK files and the index of their
   !> segments by central body, held in [[pepper_instance]].
   !>
   !> PEPPER: the segments are indexed when a file is loaded (SPICELIB:
   !> found per body at its first search, in per-context tables of 100
   !> bodies and 10000 segments, 2.4 MB per context; a body with more
   !> segments than the table signaled SPICE(BUFFEROVERFLOW)).
   type, public :: zzdskbsr_state
      type(segment_index) :: index  !! the segments of the loaded files, by central body
      integer(ip) :: dskctr ( ctrsiz ) = 0  !! counter of changes of the loaded DSK set (checked by [[zzdskchk]])
      logical :: pass1 = .true.  !! the counter has not been set up yet
   end type zzdskbsr_state

   !> The per-context state of ZZDSKBSR: the search in progress ([[zzdskbss]],
   !> [[zzdsksns]]).
   type, public :: zzdskbsr_context_state
      logical :: active = .false.  !! a search was begun by [[zzdskbss]] (and no file was loaded or unloaded since)
      integer(ip) :: epoch = -1  !! the instance's segment_epoch at [[zzdskbss]]
      integer(ip) :: list = 0  !! the body's segment list in the index
      integer(ip) :: pos = 0  !! the position in the list to continue from (searched downwards)
   end type zzdskbsr_context_state

   ! ------------------------------------------------------------------ ZZDSKSBF

   integer(ip), parameter :: zzdsksbf_btsize = 10

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZDSKSBF routines (entry points
   !> zzsbfxr, zzsbfxri, zzsbfnrm), held in [[pepper_context]] (one per thread).
   !>
   !> PEPPER: the segment table is allocatable; it grows to hold the
   !> segments of the bodies in the body table (SPICELIB: 10000 segments,
   !> 2.8 MB per context, and SPICE(SEGMENTTABLEFULL) for a body with
   !> more).
   !>
   !> No default initialization: see [[init_zzdsksbf_state]].
   type, public :: zzdsksbf_state
      integer(ip) :: bsrctr ( ctrsiz )  !! DSK BSR loaded kernel state counter when the tables were last validated
      integer(ip) :: btbody ( zzdsksbf_btsize )  !! body table: body ID codes
      integer(ip) :: btnbod  !! BTSTSZ  is an array of segment table entry counts. The Ith element of BTSTSZ is the number of entries in the segment table for the Ith body.
      integer(ip) :: btsegp ( zzdsksbf_btsize )  !! body table: start index of each body's segment list in the segment table
      integer(ip) :: btstsz ( zzdsksbf_btsize )  !! body table: number of segment table entries of each body
      logical :: first  !! first call: BSRCTR not yet initialized
      real(wp), allocatable :: stctr ( :, : )  !! segment table: outer bounding sphere centers (km, segment frame)
      integer(ip), allocatable :: stdlad ( :, : )  !! segment table: DLA descriptors
      real(wp), allocatable :: stdscr ( :, : )  !! segment table: DSK descriptors
      integer(ip) :: stfree  !! segment table: index of the first free entry
      integer(ip), allocatable :: sthan ( : )  !! segment table variables
      real(wp), allocatable :: stoff ( :, : )  !! segment table: offsets of segment frame centers from the central body (km)
      real(wp), allocatable :: strad ( : )  !! segment table: outer bounding sphere radii (km)
   end type zzdsksbf_state

   ! ------------------------------------------------------------------ ZZDSKSEL

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZDSKSEL routines (entry points
   !> zzdsksbd, zzdskbdc, zzdsknot, zzdsksit, zzdskcit, zzdskusc, zzdskumc,
   !> zzdskmsc, zzdskmmc, zzdsksrc, zzdskmrc), held in [[pepper_context]] (one per
   !> thread).
   type, public :: zzdsksel_state
      integer(ip) :: savbid = 0  !! saved body ID code (surface ID code when set by ZZDSKMSC/ZZDSKSRC) to match segments against
      integer(ip) :: savcls = 0  !! saved DSK data class to match segments against
      real(wp) :: savco1 = 0.0_wp  !! saved first coordinate of the point to match against segment coverage
      real(wp) :: savco2 = 0.0_wp  !! saved second coordinate of the point to match against segment coverage
      real(wp) :: savet = 0.0_wp  !! saved epoch (TDB seconds past J2000) to match against segment time coverage
      integer(ip) :: savfid = 0  !! saved frame ID code of the input coordinates or position
      integer(ip) :: savnsf = 0  !! number of surface IDs in the saved surface list
      real(wp) :: savpar ( nsypar ) = 0.0_wp  !! saved coordinate system parameters of the input coordinates
      real(wp) :: savpos ( 3 ) = 0.0_wp  !! saved body-centered position whose lat/lon must lie within the segment bounds
      integer(ip), allocatable :: savsrf (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) saved surface ID list (sorted) to match segments against
      integer(ip) :: savsys = 0  !! saved coordinate system code of the input coordinates
      integer(ip) :: savtrg = 0  !! saved target body ID code to match segments against
   end type zzdsksel_state

   ! ------------------------------------------------------------------ ZZDSKSPH

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzdsksph]] routines (entry points
   !> zzdsksph), held in [[pepper_context]] (one per thread).
   type, public :: zzdsksph_state
      integer(ip) :: ctr ( ctrsiz ) = -1  !! DSK state change counter ([[zzdskchk]])
      logical :: first = .true.  !! first call: the counter is not yet initialized and nothing is saved
      integer(ip) :: prvbod = 0  !! body ID code of the previous call
      integer(ip), allocatable :: prvlst (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) surface ID list of the previous call
      integer(ip) :: prvnls = -1  !! number of surface IDs of the previous call (-1 if the saved radii are invalid)
      real(wp) :: svmaxr = -1.0_wp  !! maximum radius of the bounding spheres for the saved body and surfaces
      real(wp) :: svminr = -1.0_wp  !! minimum radius of the bounding spheres for the saved body and surfaces
   end type zzdsksph_state

   ! ------------------------------------------------------------------ ZZDYNFRM

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzdynfrm]] routines (entry points
   !> zzdynfrm), held in [[pepper_context]] (one per thread).
   type, public :: zzdynfrm_state
      character(1) :: axes ( 3 ) = [ character(1) :: kvx, kvy, kvz ]  !! axis names 'X', 'Y', 'Z' used to decode an axis kernel variable
      integer(ip) :: earth = 0  !! NAIF ID code of the Earth (Earth-centered check for precession/nutation frames)
      logical :: first = .true.  !! first-call setup (J2000 and Earth codes, kernel variable names) not yet done
      integer(ip) :: j2000 = 0  !! frame ID code of J2000
      character(kvnmln) :: itmabc ( 2 ) = ' '  !! kernel variable names PRI_ABCORR and SEC_ABCORR
      character(kvnmln) :: itmaxe ( 2 ) = ' '  !! kernel variable names PRI_AXIS and SEC_AXIS
      character(kvnmln) :: itmcof ( 3 ) = [ character(kvnmln) :: kweac1, kweac2, kweac3 ]  !! kernel variable names ANGLE_1_COEFFS, ANGLE_2_COEFFS, ANGLE_3_COEFFS (Euler frames)
      character(kvnmln) :: itmdec ( 2 ) = ' '  !! kernel variable names PRI_DEC and SEC_DEC
      character(kvnmln) :: itmfrm ( 2 ) = ' '  !! kernel variable names PRI_FRAME and SEC_FRAME
      character(kvnmln) :: itmlat ( 2 ) = ' '  !! kernel variable names PRI_LATITUDE and SEC_LATITUDE
      character(kvnmln) :: itmlon ( 2 ) = ' '  !! kernel variable names PRI_LONGITUDE and SEC_LONGITUDE
      character(kvnmln) :: itmobs ( 2 ) = ' '  !! kernel variable names PRI_OBSERVER and SEC_OBSERVER
      character(kvnmln) :: itmra ( 2 ) = ' '  !! kernel variable names PRI_RA and SEC_RA
      character(kvnmln) :: itmsep = kwatol  !! kernel variable name ANGLE_SEP_TOL (minimum angular separation of the defining vectors)
      character(kvnmln) :: itmspc ( 2 ) = ' '  !! kernel variable names PRI_SPEC and SEC_SPEC
      character(kvnmln) :: itmtrg ( 2 ) = ' '  !! kernel variable names PRI_TARGET and SEC_TARGET
      character(kvnmln) :: itmunt ( 2 ) = ' '  !! kernel variable names PRI_UNITS and SEC_UNITS
      character(kvnmln) :: itmvdf ( 2 ) = ' '  !! kernel variable names PRI_VECTOR_DEF and SEC_VECTOR_DEF
      character(kvnmln) :: itmvec ( 2 ) = ' '  !! kernel variable names PRI_VECTOR and SEC_VECTOR
   end type zzdynfrm_state

   ! ------------------------------------------------------------------ ZZDYNROT

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzdynrot]] routines (entry points
   !> zzdynrot), held in [[pepper_context]] (one per thread).
   type, public :: zzdynrot_state
      character(1) :: axes ( 3 ) = [ character(1) :: kvx, kvy, kvz ]  !! axis names 'X', 'Y', 'Z' used to decode an axis kernel variable
      integer(ip) :: earth = 0  !! NAIF ID code of the Earth (Earth-centered check for precession/nutation frames)
      logical :: first = .true.  !! first-call setup (J2000 and Earth codes, kernel variable names) not yet done
      integer(ip) :: j2000 = 0  !! frame ID code of J2000
      character(kvnmln) :: itmabc ( 2 ) = ' '  !! kernel variable names PRI_ABCORR and SEC_ABCORR
      character(kvnmln) :: itmaxe ( 2 ) = ' '  !! kernel variable names PRI_AXIS and SEC_AXIS
      character(kvnmln) :: itmcof ( 3 ) = [ character(kvnmln) :: kweac1, kweac2, kweac3 ]  !! kernel variable names ANGLE_1_COEFFS, ANGLE_2_COEFFS, ANGLE_3_COEFFS (Euler frames)
      character(kvnmln) :: itmdec ( 2 ) = ' '  !! kernel variable names PRI_DEC and SEC_DEC
      character(kvnmln) :: itmfrm ( 2 ) = ' '  !! kernel variable names PRI_FRAME and SEC_FRAME
      character(kvnmln) :: itmlat ( 2 ) = ' '  !! kernel variable names PRI_LATITUDE and SEC_LATITUDE
      character(kvnmln) :: itmlon ( 2 ) = ' '  !! kernel variable names PRI_LONGITUDE and SEC_LONGITUDE
      character(kvnmln) :: itmobs ( 2 ) = ' '  !! kernel variable names PRI_OBSERVER and SEC_OBSERVER
      character(kvnmln) :: itmra ( 2 ) = ' '  !! kernel variable names PRI_RA and SEC_RA
      character(kvnmln) :: itmsep = kwatol  !! kernel variable name ANGLE_SEP_TOL (minimum angular separation of the defining vectors)
      character(kvnmln) :: itmspc ( 2 ) = ' '  !! kernel variable names PRI_SPEC and SEC_SPEC
      character(kvnmln) :: itmtrg ( 2 ) = ' '  !! kernel variable names PRI_TARGET and SEC_TARGET
      character(kvnmln) :: itmunt ( 2 ) = ' '  !! kernel variable names PRI_UNITS and SEC_UNITS
      character(kvnmln) :: itmvdf ( 2 ) = ' '  !! kernel variable names PRI_VECTOR_DEF and SEC_VECTOR_DEF
      character(kvnmln) :: itmvec ( 2 ) = ' '  !! kernel variable names PRI_VECTOR and SEC_VECTOR
   end type zzdynrot_state

   ! ------------------------------------------------------------------ ZZEKAD06

   integer(ip), parameter :: maxpad = 100

   !> Add a column entry to a specified record in a class 6 column.
   !> The entries of class 6 columns are arrays of character string
   !> values.
   !>
   !> The saved variables of SPICELIB's [[zzekad06]] routines (entry points
   !> zzekad06), held in [[pepper_context]] (one per thread).
   type, public :: zzekad06_state
      character(maxpad) :: padbuf = ' '  !! blanks used to pad entry elements shorter than the column string length
      integer(ip) :: datptr = 0  !! work: DAS character address where the next character of the entry goes
      integer(ip) :: p = 0  !! work: number of the character data page being written
      integer(ip) :: padlen = 0  !! work: number of trailing blanks that pad each element to the column string length
      logical :: first = .true.  !! padbuf has not been blanked yet
   end type zzekad06_state

   ! ------------------------------------------------------------------ ZZEKJTST

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZEKJTST routines (entry points
   !> zzekjprp, zzekjnxt), held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization: see [[init_zzekjtst_state]]. PEPPER: the arrays
   !> of the join constraints grow to their number in [[zzekjprp]] (SPICELIB:
   !> MXJCON = 100).
   type, public :: zzekjtst_state
      integer(ip) :: case  !! join strategy chosen by zzekjprp: equi-join, non-equi-join, no key (test all), or empty
      integer(ip) :: cnstr  !! index of the key join constraint used to order the candidate row vectors
      integer(ip) :: lbase  !! scratch-area base of the narrow join row set built for the key constraint LHS column
      integer(ip) :: lcur  !! current LHS order-vector index within the run of equal keys (equi-join case)
      integer(ip), allocatable :: ldscrs ( :, : )  !! column descriptor of the LHS column of each active join constraint
      integer(ip) :: lelt  !! element index of the key constraint LHS column entry
      integer(ip), allocatable :: lelts ( : )  !! element index of the LHS column of each constraint (never set, so always 0)
      integer(ip), allocatable :: lhans ( : )  !! handle of the EK containing the LHS column of each active join constraint
      logical, allocatable :: locact ( : )  !! which join constraints to test, with the key constraint deactivated
      integer(ip) :: lovbas  !! scratch-area base of the order vector that sorts the LHS key column
      integer(ip) :: lptr  !! current LHS order-vector index (start of the equal-key run in the equi-join case)
      integer(ip), allocatable :: lrows ( : )  !! work: LHS row pointer of each active constraint, from the candidate row vector
      integer(ip), allocatable :: lsdsc ( :, : )  !! segment descriptor of the LHS column of each active join constraint
      integer(ip) :: rbase  !! scratch-area base of the narrow join row set built for the key constraint RHS column
      integer(ip), allocatable :: rdscrs ( :, : )  !! column descriptor of the RHS column of each active join constraint
      integer(ip) :: relt  !! element index of the key constraint RHS column entry
      integer(ip), allocatable :: relts ( : )  !! element index of the RHS column of each constraint (never set, so always 0)
      integer(ip), allocatable :: rhans ( : )  !! handle of the EK containing the RHS column of each active join constraint
      integer(ip) :: rovbas  !! scratch-area base of the order vector that sorts the RHS key column
      integer(ip) :: rptr  !! current RHS order-vector index
      integer(ip), allocatable :: rrows ( : )  !! work: RHS row pointer of each active constraint, from the candidate row vector
      integer(ip), allocatable :: rsdsc ( :, : )  !! segment descriptor of the RHS column of each active join constraint
      integer(ip) :: svbas1  !! base of the join row set holding the key LHS table (the first set if no key)
      integer(ip) :: svbas2  !! base of the join row set holding the key RHS table (the second set if no key)
      integer(ip), allocatable :: svcp1 ( : )  !! cross-product index of the LHS table of each join constraint
      integer(ip), allocatable :: svcp2 ( : )  !! cross-product index of the RHS table of each join constraint
      integer(ip), allocatable :: svops ( : )  !! relational operator code of each join constraint
      integer(ip) :: svncon  !! number of join constraints
      integer(ip) :: svnr1  !! number of considered row vectors in the svbas1 join row set
      integer(ip) :: svnr2  !! number of considered row vectors in the svbas2 join row set
      integer(ip) :: svnt1  !! number of tables in the svbas1 join row set
      integer(ip) :: svnt2  !! number of tables in the svbas2 join row set
      integer(ip) :: svrb1  !! base-relative address of the considered row vectors in the svbas1 join row set
      integer(ip) :: svrb2  !! base-relative address of the considered row vectors in the svbas2 join row set
      logical :: done  !! all candidate row vector pairs have been tested
      logical :: lsmall  !! the non-equi-join key constraint requires LHS < or <= RHS
   end type zzekjtst_state

   ! ------------------------------------------------------------------ ZZEKPAGE

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZEKPAGE routines (entry points
   !> zzekpgin, zzekpgan, zzekpgal, zzekpgfr, zzekpgrc, zzekpgrd, zzekpgri,
   !> zzekpgwc, zzekpgwd, zzekpgwi, zzekpgbs, zzekpgpg, zzekpgst), held in
   !> [[pepper_context]] (one per thread).
   type, public :: zzekpage_state
      character(pgsizc) :: cfill = ' '  !! note:  the integer fill buffer should be as large as the maximum of the integer page size and the metadata area size.
      real(wp) :: dfill ( pgsizd ) = 0.0_wp  !! zero-filled d.p. page used to append new d.p. pages to the file
      integer(ip) :: ifill ( pgsizi ) = 0  !! zero-filled integer page used to append integer pages and the metadata area
   end type zzekpage_state

   ! ------------------------------------------------------------------ ZZEKPDEC

   integer(ip), parameter :: zzekpdec_nattr = 5
   integer(ip), parameter :: toklen = 32
   integer(ip), parameter :: nrkey = 1

   !> Parse a declaration of a new EK column.
   !>
   !> The saved variables of SPICELIB's [[zzekpdec]] routines (entry points
   !> zzekpdec), held in [[pepper_context]] (one per thread).
   type, public :: zzekpdec_state
      !> recognized column attribute keywords (DATATYPE, SIZE, INDEXED, ...)
      character(toklen) :: attkey ( zzekpdec_nattr ) = [ character(toklen) :: 'DATATYPE', 'SIZE', 'INDEXED', &
         'NULLS_OK', 'FIXED_COUNT' ]
      integer(ip) :: reqkey ( nrkey  ) = 1  !! indices in attkey of the required attributes (DATATYPE)
   end type zzekpdec_state

   ! ------------------------------------------------------------------ ZZEKREQI

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzekreqi]] routines (entry points
   !> zzekreqi), held in [[pepper_context]] (one per thread).
   type, public :: zzekreqi_state
   end type zzekreqi_state

   ! ------------------------------------------------------------------ ZZEKSCA

   integer(ip), parameter :: memsiz = 2500000

   !> Manage the EK scratch area.
   !>
   !> The saved variables of SPICELIB's ZZEKSCA routines (entry points
   !> zzekstop, zzekspsh, zzekspop, zzeksdec, zzeksupd, zzeksrd, zzekscln),
   !> held in [[pepper_context]] (one per thread).
   !>
   !> PEPPER: the in-memory part grows as the stack does, up to MEMSIZ
   !> integers (SPICELIB: 10 MB allocated by the first EK query of a
   !> context), and the scratch DAS beyond it is opened when the stack
   !> first goes past MEMSIZ (SPICELIB: by the first push).
   type, public :: zzeksca_state
      integer(ip) :: scrhan = 0  !! handle of the scratch DAS file holding the stack beyond the in-memory part
      integer(ip), allocatable :: scrtch ( : )  !! in-memory part of the scratch stack (elements 1 to min(t, memsiz)); grows to memsiz
      integer(ip) :: t = 0  !! top of the scratch stack: number of integers in the scratch area
      logical :: first = .true.  !! the scratch DAS is not open (set again by zzekscln after it is closed)
   end type zzeksca_state

   ! ------------------------------------------------------------------ ZZEKSCAN

   !> Scan tokens in an EK query.
   !>
   !> The saved variables of SPICELIB's [[zzekscan]] routines (entry points
   !> zzekscan), held in [[pepper_context]] (one per thread).
   type, public :: zzekscan_state
      integer(ip) :: idspec ( lbcell : mxspec ) = 0  !! identifier character spec for lxidnt (built by lxcsid on the first call)
      logical :: pass1 = .true.  !! idspec has not been built yet
   end type zzekscan_state

   ! ------------------------------------------------------------------ ZZEKSEMC

   integer(ip), parameter :: zzeksemc_short = 32

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzeksemc]] routines (entry points
   !> zzeksemc), held in [[pepper_context]] (one per thread).
   type, public :: zzeksemc_state
      !> names of the EK data types, indexed by type code (for error messages)
      character(zzeksemc_short) :: typstr ( 4 ) = [ character(zzeksemc_short) :: 'CHARACTER', &
         'DOUBLE PRECISION', 'INTEGER', 'TIME' ]
   end type zzeksemc_state

   ! ------------------------------------------------------------------ ZZEKTRIT

   !> Initialize an EK tree, returning the root of the tree.
   !>
   !> The saved variables of SPICELIB's [[zzektrit]] routines (entry points
   !> zzektrit), held in [[pepper_context]] (one per thread).
   type, public :: zzektrit_state
      integer(ip) :: page ( pgsizi ) = 0  !! work: integer page image of the new tree root node, written to the file
   end type zzektrit_state

   ! ------------------------------------------------------------------ ZZEKTRLK

   !> Locate a specified key.  Return metadata describing the node
   !> containing the key.
   !>
   !> The saved variables of SPICELIB's [[zzektrlk]] routines (entry points
   !> zzektrlk), held in [[pepper_context]] (one per thread).
   type, public :: zzektrlk_state
      integer(ip) :: oldhan = 0  !! handle of the EK of the last lookup
      integer(ip) :: oldidx = 0  !! index within its node of the last key found
      integer(ip) :: oldlvl = 0  !! tree level of the node containing the last key found
      integer(ip) :: oldmax = 0  !! number of keys in the node containing the last key found
      integer(ip) :: oldnod = 0  !! number of the node containing the last key found
      integer(ip) :: oldnof = 0  !! key offset of the node containing the last key found
      integer(ip) :: oldtre = 0  !! root node of the tree of the last lookup
      integer(ip) :: oldkey = 0  !! last key found
      integer(ip) :: oldval = 0  !! value of the last key found
      integer(ip) :: page ( pgsizi ) = 0  !! last node page read (the node of the last key found, after a successful lookup)
      logical :: first = .true.  !! no lookup done yet (the file access method is not known)
      logical :: leaf = .false.  !! the node containing the last key found is a leaf
      logical :: rdonly = .false.  !! the file of the last lookup is open read-only, so the cached node can be trusted
   end type zzektrlk_state

   ! ------------------------------------------------------------------ ZZEKVADR

   !> Compute the EK scratch area base address of the row vector having
   !> a specified index, given a union of EK join row sets and a row
   !> vector index. Also return the base address of the row
   !> vector's parent segment vector.
   !>
   !> The saved variables of SPICELIB's ZZEKVADR routines (entry points
   !> zzekvset, zzekvcal), held in [[pepper_context]] (one per thread).
   type, public :: zzekvadr_state
      integer(ip), allocatable :: begidx ( : )  !! index of the first row vector of each join row set in the union
      integer(ip) :: j = 0  !! work: table or row count read from a join row set
      integer(ip) :: maxrwv = 0  !! largest valid row vector index of the union (computed one too large by zzekvset)
      integer(ip) :: ntabs = 0  !! number of tables in each join row set of the union
      integer(ip), allocatable :: rbas ( : )  !! scratch-area base address of the row vectors of each join row set
      integer(ip), allocatable :: svbas ( : )  !! scratch-area base address of each join row set
      integer(ip) :: svnjrs = 0  !! number of join row sets in the union
   end type zzekvadr_state

   ! ------------------------------------------------------------------ ZZEKWEQI

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzekweqi]] routines (entry points
   !> zzekweqi), held in [[pepper_context]] (one per thread).
   type, public :: zzekweqi_state
   end type zzekweqi_state

   ! ------------------------------------------------------------------ ZZELVUPY

   !> Determine whether a specified ellipse intersects the pyramid
   !> defined by a polygonal field of view.
   !>
   !> The saved variables of SPICELIB's [[zzelvupy]] routines (entry points
   !> zzelvupy), held in [[pepper_context]] (one per thread).
   !>
   !> See [[init_zzelvupy_state]].
   type, public :: zzelvupy_state
      real(wp) :: origin ( 3 )  !! the origin (zero vector), constant
      real(wp), allocatable :: work ( :, : )  !! work: projections of the FOV boundary vectors onto the FOV plane; grows to the number of vectors (SPICELIB: 10000)
   end type zzelvupy_state

   ! ------------------------------------------------------------------ ZZFTPCHK

   !> Check a character string that may contain the FTP validation
   !> string for FTP based errors.
   !>
   !> The saved variables of SPICELIB's [[zzftpchk]] routines (entry points
   !> zzftpchk), held in [[pepper_context]] (one per thread).
   type, public :: zzftpchk_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      character(sizend) :: lftbkt = ' '  !! left bracket of the FTP validation string
      character(sizstr) :: memstr = ' '  !! the FTP validation string without its brackets
      character(sizend) :: rgtbkt = ' '  !! right bracket of the FTP validation string
   end type zzftpchk_state

   ! ------------------------------------------------------------------ ZZFTPSTR

   !> Retrieve the components of the FTP validation string.
   !>
   !> The saved variables of SPICELIB's [[zzftpstr]] routines (entry points
   !> zzftpstr), held in [[pepper_context]] (one per thread).
   type, public :: zzftpstr_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      character(sizdlm) :: locdlm = ':'  !! delimiter of the pieces of the FTP validation string
      character(sizend) :: loclnd = 'FTPSTR'  !! left bracket of the FTP validation string
      character(sizend) :: locrnd = 'ENDFTP'  !! right bracket of the FTP validation string
      character(sizstr) :: locstr = ' '  !! the FTP validation string
   end type zzftpstr_state

   ! ------------------------------------------------------------------ ZZGETELM

   integer(ip), parameter :: zzgetelm_lnsize = 160
   integer(ip), parameter :: zzgetelm_maxp = 37
   integer(ip), parameter :: minp = -zzgetelm_maxp

   !> Given a the "lines" of a two-line element set, parse the
   !> lines and return the elements in units suitable for use
   !> in SPICE software.
   !>
   !> The saved variables of SPICELIB's [[zzgetelm]] routines (entry points
   !> zzgetelm), held in [[pepper_context]] (one per thread).
   type, public :: zzgetelm_state
      character(zzgetelm_lnsize) :: errprs ( 13 ) = ' '  !! work: parse error messages for each of the 13 TLE terms
      character(zzgetelm_lnsize) :: term ( 13 ) = ' '  !! descriptions (name and character columns) of the 13 TLE terms, set on the first call
      real(wp) :: d2r = 0.0_wp  !! radians per degree, set on the first call
      real(wp) :: pi2 = 0.0_wp  !! 2 pi, set on the first call
      real(wp) :: power ( minp:zzgetelm_maxp ) = 0.0_wp  !! powers of ten 10**minp ... 10**maxp, set on the first call
      real(wp) :: tvec ( 10 ) = 0.0_wp  !! work: epoch time vector (year, day) converted by [[ttrans]] from YD.D to TDB
      logical :: first = .true.  !! first call: constants, powers of ten and term descriptions not yet set
   end type zzgetelm_state

   ! ------------------------------------------------------------------ ZZGFCOQ

   integer(ip), parameter :: zzgfcoq_bdnmln = 36

   !> SPICE private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzgfcoq]] routines (entry points
   !> zzgfcoq), held in [[pepper_context]] (one per thread).
   type, public :: zzgfcoq_state
      character(zzgfcoq_bdnmln) :: ctrnam = ' '  !! name of the body with ID prvctr (center of the planetographic frame)
      logical :: first = .true.  !! first call: target, observer and center names not yet looked up
      character(zzgfcoq_bdnmln) :: obsnam = ' '  !! name of the observer with ID prvobs
      integer(ip) :: prvctr = 0  !! frame center ID code of the previous call
      integer(ip) :: prvobs = 0  !! observer ID code of the previous call
      integer(ip) :: prvtrg = 0  !! target ID code of the previous call
      !> recognized coordinate system names (constant)
      character(crdlen) :: sysnms (    nsys ) = [ character(crdlen) :: recsys, latsys, radsys, sphsys, cylsys, &
         geosys, pgrsys ]
      character(zzgfcoq_bdnmln) :: trgnam = ' '  !! name of the target with ID prvtrg
   end type zzgfcoq_state

   ! ------------------------------------------------------------------ ZZGFCOU

   integer(ip), parameter :: abclen = 20
   integer(ip), parameter :: zzgfcou_frnmln = 32
   integer(ip), parameter :: zzgfcou_bdnmln = 36
   integer(ip), parameter :: syslen = 32
   integer(ip), parameter :: vdflen = 32

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZGFCOU routines (entry points
   !> zzgfcoin, zzgfcog, zzgfcodc, zzgfcoex, zzgfcocg, zzgfcosg, zzgfcocd,
   !> zzgfcosd), held in [[pepper_context]] (one per thread).
   type, public :: zzgfcou_state
      integer(ip) :: svcidx = 0  !! index of the coordinate within its coordinate system
      character(abclen) :: svcorr = ' '  !! aberration correction saved by [[zzgfcoin]]
      character(crdlen) :: svcrd = ' '  !! coordinate name saved by [[zzgfcoin]]
      character(syslen) :: svcsys = ' '  !! coordinate system name saved by [[zzgfcoin]]
      integer(ip) :: svdctr = 0  !! ID code of the center of the ray frame svdref (intercept searches)
      character(:), allocatable :: svdref  !! ray direction frame name (intercept searches; blank otherwise)
      real(wp) :: svdvec ( 3 ) = 0.0_wp  !! ray direction vector (intercept searches; zero otherwise)
      real(wp) :: svf = 0.0_wp  !! flattening coefficient of the frame center body (geodetic/planetographic)
      character(:), allocatable :: svmeth  !! subpoint/intercept computation method saved by [[zzgfcoin]] (PEPPER: any length; SPICELIB: METLEN, 200)
      integer(ip) :: svobs = 0  !! observer ID code saved by [[zzgfcoin]]
      real(wp) :: svradi ( 3 ) = 0.0_wp  !! radii of the frame center body (sub-observer, intercept, geodetic, planetographic)
      character(zzgfcou_bdnmln) :: svrcnm = ' '  !! name of the frame center body (planetographic only)
      integer(ip) :: svrctr = 0  !! ID code of the center of the coordinate frame svref
      real(wp) :: svre = 0.0_wp  !! equatorial radius of the frame center body (geodetic/planetographic)
      character(:), allocatable :: svref  !! coordinate reference frame name saved by [[zzgfcoin]]
      integer(ip) :: svsens = 0  !! planetographic longitude sense: 1 east, -1 west, 0 not planetographic
      integer(ip) :: svtarg = 0  !! target ID code saved by [[zzgfcoin]]
      character(vdflen) :: svvdef = ' '  !! vector definition (position, sub-observer point, surface intercept)
      !> recognized coordinate system names (constant)
      character(syslen) :: sysnms ( nsys ) = [ character(syslen) :: recsys, latsys, radsys, sphsys, cylsys, &
         geosys, pgrsys ]
      real(wp) :: y ( 3 ) = [ 0.0_wp, 1.0_wp, 0.0_wp ]  !! +Y unit vector used to find the planetographic longitude sense (constant)
   end type zzgfcou_state

   ! ------------------------------------------------------------------ ZZGFCSLV

   integer(ip), parameter :: mxpass = 3
   integer(ip), parameter :: maxop = 6

   !> Perform a coordinate search.
   !>
   !> The saved variables of SPICELIB's [[zzgfcslv]] routines (entry points
   !> zzgfcslv), held in [[pepper_context]] (one per thread).
   type, public :: zzgfcslv_state
      !> recognized relational operators (constant)
      character(maxop) :: cnames ( nc ) = [ character(maxop) :: '>', '=', '<', 'ABSMAX', 'ABSMIN', 'LOCMAX', &
         'LOCMIN' ]
      !> progress report prefixes for each pass (constant)
      character(mxbegm) :: rptpre ( mxpass ) = [ character(mxbegm) :: 'Coordinate pass 1 of # ', &
         'Coordinate pass 2 of # ', 'Intercept existence pass 1 of 1' ]
      character(mxendm) :: rptsuf ( mxpass ) = 'done.'  !! progress report suffixes for each pass (constant)
   end type zzgfcslv_state

   ! ------------------------------------------------------------------ ZZGFDIU

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZGFDIU routines (entry points
   !> zzgfdiin, zzgfdidc, zzgfdigq), held in [[pepper_context]] (one per thread).
   type, public :: zzgfdiu_state
      character(corlen) :: svcorr = ' '  !! aberration correction saved by [[zzgfdiin]]
      integer(ip) :: svobs = 0  !! observer ID code saved by [[zzgfdiin]]
      integer(ip) :: svtarg = 0  !! target ID code saved by [[zzgfdiin]]
   end type zzgfdiu_state

   ! ------------------------------------------------------------------ ZZGFFVU

   integer(ip), parameter :: ubpl = 4
   integer(ip), parameter :: zzgffvu_bdnmln = 36
   integer(ip), parameter :: zzgffvu_frnmln = 32

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZGFFVU routines (entry points
   !> zzgffvin, zzgffvst), held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization: see [[init_zzgffvu_state]].
   type, public :: zzgffvu_state
      real(wp) :: svarad  !! angular radius of the FOV (max angle between axis and boundary vectors)
      real(wp), allocatable :: svbnds ( :, : )  !! FOV boundary vectors, from [[getfov]] (second semi-axis added for circles); as many as the instrument has (SPICELIB: MAXVRT)
      character(corlen) :: svcorr  !! aberration correction saved by [[zzgffvin]]
      real(wp) :: svedct ( 3 )  !! center of the ellipsoid representing a circular/elliptical FOV (for [[zzocced]])
      logical :: svetrg  !! the target is an ellipsoid
      real(wp) :: svfaxi ( 3 )  !! FOV axis vector (instrument frame)
      real(wp) :: svfovm ( 3, 3 )  !! rotation from the instrument frame to a frame whose +Z is the FOV axis
      real(wp), allocatable :: svfpol ( :, : )  !! 2D vertices of the FOV polygon in the FOV plane (svfovm frame), the size of SVBNDS
      real(wp) :: svfsmx ( 3, 3 )  !! semi-axis matrix of the ellipsoid representing the FOV (for [[zzocced]])
      real(wp) :: svfvct ( 3 )  !! unit FOV axis vector: center of the FOV plane
      character(:), allocatable :: svifrm  !! instrument frame name, from [[getfov]]
      character(zzgffvu_bdnmln) :: svinam  !! instrument name saved by [[zzgffvin]]
      character(shplen) :: svishp  !! FOV shape (RECTANGLE mapped to POLYGON)
      integer(ip) :: svnvrt  !! number of FOV boundary vectors
      integer(ip) :: svobs  !! observer ID code saved by [[zzgffvin]]
      real(wp) :: svorig ( 3 )  !! origin (zero vector): the observer/instrument position (constant)
      real(wp) :: svplan ( ubpl )  !! FOV plane: normal to the FOV axis at unit distance from the origin
      real(wp) :: svrdir ( 3 )  !! unit ray direction (ray target) saved by [[zzgffvin]]
      real(wp) :: svsemi ( 3, 2 )  !! semi-axis vectors of an elliptical/circular FOV in the FOV plane
      integer(ip) :: svtarg  !! target ID code saved by [[zzgffvin]] (0 for a ray target)
      character(:), allocatable :: svtfrm  !! target body-fixed frame (or ray frame) name saved by [[zzgffvin]]
      character(zzgffvu_bdnmln) :: svtnam  !! target name saved by [[zzgffvin]]
      real(wp) :: svtrad ( 3 )  !! target ellipsoid radii (zero unless the target is an ellipsoid)
      logical :: svuray  !! the target is a ray
      logical :: svustl  !! the aberration correction includes stellar aberration
      real(wp) :: svxmag ( 2 )  !! lengths of the svsemi semi-axis vectors
      logical :: svxmit  !! the aberration correction is for transmission
   end type zzgffvu_state

   ! ------------------------------------------------------------------ ZZGFILU

   integer(ip), parameter :: zzgfilu_bdnmln = 36
   integer(ip), parameter :: zzgfilu_frnmln = 32
   integer(ip), parameter :: zzgfilu_namlen = 50

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZGFILU routines (entry points
   !> zzgfilin, zzgfildc, zzgfilgq), held in [[pepper_context]] (one per thread).
   type, public :: zzgfilu_state
      !> recognized illumination angle names (constant)
      character(zzgfilu_namlen) :: angnms ( 3 ) = [ character(zzgfilu_namlen) :: 'PHASE', 'INCIDENCE', &
         'EMISSION' ]
      integer(ip) :: svaidx = 0  !! index of the angle type in angnms
      character(corlen) :: svcorr = ' '  !! aberration correction saved by [[zzgfilin]]
      character(zzgfilu_bdnmln) :: svinam = ' '  !! illumination source name saved by [[zzgfilin]]
      character(:), allocatable :: svmeth  !! computation method saved by [[zzgfilin]] (PEPPER: any length; SPICELIB: MTHLEN, 200)
      real(wp) :: svnrml ( 3 ) = 0.0_wp  !! outward surface normal at the surface point svsspt
      character(zzgfilu_bdnmln) :: svonam = ' '  !! observer name saved by [[zzgfilin]]
      character(:), allocatable :: svref  !! target body-fixed frame name saved by [[zzgfilin]]
      real(wp) :: svsspt ( 3 ) = 0.0_wp  !! surface point on the target saved by [[zzgfilin]]
      character(zzgfilu_bdnmln) :: svtnam = ' '  !! target name saved by [[zzgfilin]]
   end type zzgfilu_state

   ! ------------------------------------------------------------------ ZZGFLONG

   integer(ip), parameter :: oplen = 6

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzgflong]] routines (entry points
   !> zzgflong), held in [[pepper_context]] (one per thread).
   type, public :: zzgflong_state
      !> recognized relational operators (constant)
      character(oplen) :: ops ( nc ) = [ character(oplen) :: '<', '=', '>', 'LOCMIN', 'ABSMIN', 'LOCMAX', &
         'ABSMAX' ]
      real(wp) :: y ( 3 ) = [ 0.0_wp, 1.0_wp, 0.0_wp ]  !! +Y unit vector used to find the planetographic longitude sense (constant)
   end type zzgflong_state

   ! ------------------------------------------------------------------ ZZGFOCU

   integer(ip), parameter :: zzgfocu_bdnmln = 36
   integer(ip), parameter :: zzgfocu_frnmln = 32

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZGFOCU routines (entry points
   !> zzgfocin, zzgfocst), held in [[pepper_context]] (one per thread).
   type, public :: zzgfocu_state
      integer(ip) :: ncalls = 0  !! number of [[sincpt]] calls made by [[zzgfocst]] (diagnostic count only)
      integer(ip) :: svback = 0  !! back body ID code saved by [[zzgfocin]]
      character(:), allocatable :: svbfrm  !! back body-fixed frame name (blank for a point)
      character(:), allocatable :: svbmth  !! back body shape/method string saved by [[zzgfocin]] (PEPPER: any length; SPICELIB: MTHLEN, 500)
      character(zzgfocu_bdnmln) :: svbnam = ' '  !! back body name saved by [[zzgfocin]]
      real(wp) :: svbrad ( 3 ) = 0.0_wp  !! back body ellipsoid radii (zero for a point)
      character(shplen) :: svbshp = ' '  !! back body shape: point, ellipsoid or DSK
      character(corlen) :: svcorr = ' '  !! aberration correction for [[spkezp]] (stellar aberration removed)
      character(:), allocatable :: svffrm  !! front body-fixed frame name (blank for a point)
      character(:), allocatable :: svfmth  !! front body shape/method string saved by [[zzgfocin]] (PEPPER: any length; SPICELIB: MTHLEN, 500)
      character(zzgfocu_bdnmln) :: svfnam = ' '  !! front body name saved by [[zzgfocin]]
      real(wp) :: svfrad ( 3 ) = 0.0_wp  !! front body ellipsoid radii (zero for a point)
      integer(ip) :: svfrnt = 0  !! front body ID code saved by [[zzgfocin]]
      character(shplen) :: svfshp = ' '  !! front body shape: point, ellipsoid or DSK
      real(wp) :: svmnbr = 0.0_wp  !! minimum radius of the back body (bounding radius for DSK shapes)
      real(wp) :: svmnfr = 0.0_wp  !! minimum radius of the front body (bounding radius for DSK shapes)
      real(wp) :: svmxbr = 0.0_wp  !! maximum radius of the back body (bounding radius for DSK shapes)
      real(wp) :: svmxfr = 0.0_wp  !! maximum radius of the front body (bounding radius for DSK shapes)
      integer(ip) :: svobs = 0  !! observer ID code saved by [[zzgfocin]]
      character(zzgfocu_bdnmln) :: svonam = ' '  !! observer name saved by [[zzgfocin]]
      real(wp) :: svorig ( 3 ) = 0.0_wp  !! origin (zero vector): the observer position (constant)
      character(oclln) :: svtype = ' '  !! occultation type saved by [[zzgfocin]]
      character(oclln) :: svtyps ( noctyp ) = [ character(oclln) :: annulr, any, partl, full ]  !! recognized occultation types (constant)
   end type zzgfocu_state

   ! ------------------------------------------------------------------ ZZGFPAU

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZGFPAU routines (entry points
   !> zzgfpain, zzgfpadc, zzgfpagq, zzgfpax), held in [[pepper_context]] (one per
   !> thread).
   type, public :: zzgfpau_state
      logical :: svablk ( nabcor ) = .false.  !! attribute flags of the aberration correction svabco (from [[zzvalcor]])
      character(corlen) :: svabco = ' '  !! aberration correction saved by [[zzgfpain]]
      integer(ip) :: svtarg = 0  !! target ID code saved by [[zzgfpain]]
      integer(ip) :: svobs = 0  !! observer ID code saved by [[zzgfpain]]
      integer(ip) :: svillm = 0  !! illumination source ID code saved by [[zzgfpain]]
   end type zzgfpau_state

   ! ------------------------------------------------------------------ ZZGFREL

   integer(ip), parameter :: zzgfrel_maxcln = 80

   !> SPICE private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzgfrel]] routines (entry points
   !> zzgfrel), held in [[pepper_context]] (one per thread).
   type, public :: zzgfrel_state
      !> recognized relational operators (constant)
      character(zzgfrel_maxcln) :: cnames ( nc ) = [ character(zzgfrel_maxcln) :: '<', '=', '>', 'LOCMIN', &
         'ABSMIN', 'LOCMAX', 'ABSMAX' ]
      logical :: cstep = .false.  !! constant step size flag passed to [[zzgfsolv]]: always false (constant)
   end type zzgfrel_state

   ! ------------------------------------------------------------------ ZZGFRELX

   integer(ip), parameter :: zzgfrelx_maxcln = 80

   !> SPICE private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzgfrelx]] routines (entry points
   !> zzgfrelx), held in [[pepper_context]] (one per thread).
   type, public :: zzgfrelx_state
      !> recognized relational operators (constant)
      character(zzgfrelx_maxcln) :: cnames ( nc ) = [ character(zzgfrelx_maxcln) :: '<', '=', '>', 'LOCMIN', &
         'ABSMIN', 'LOCMAX', 'ABSMAX' ]
      logical :: cstep = .false.  !! constant step size flag passed to [[zzgfsolvx]]: always false (constant)
   end type zzgfrelx_state

   ! ------------------------------------------------------------------ ZZGFRPWK

   !> The entry points under this routine allows one to easily monitor
   !> the status of job in progress.
   !>
   !> The saved variables of SPICELIB's ZZGFRPWK routines (entry points
   !> zzgftswk, zzgfwkin, zzgfwkad, zzgfwkun, zzgfwkmo), held in
   !> [[pepper_context]] (one per thread).
   type, public :: zzgfrpwk_state
      integer(ip) :: calls = 0  !! [[zzgfwkin]] calls since the last time check
      integer(ip) :: check = 1  !! number of [[zzgfwkin]] calls between system time checks
      real(wp) :: done = 0.0_wp  !! amount of work done so far
      real(wp) :: entire = 0.0_wp  !! total amount of work to be done
      character(mxendm) :: finish = ' '  !! progress report end message
      integer(ip) :: ls = 1  !! nonblank length of the begin message (set by [[zzgftswk]])
      real(wp) :: lstsec = 0.0_wp  !! system time of the last report, seconds past midnight
      character(mxbegm) :: start = ' '  !! progress report begin message
      real(wp) :: step = 0.0_wp  !! minimum seconds between progress reports (0 to 3600)
      real(wp) :: svincr = 0.0_wp  !! last increment passed to [[zzgfwkin]]
      integer(ip) :: svunit = output_unit  !! logical unit the progress report is written to (standard output unless [[zzgfwkun]] sets another)
   end type zzgfrpwk_state

   ! ------------------------------------------------------------------ ZZGFRRU

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZGFRRU routines (entry points
   !> zzgfrrin, zzgfrrdc, zzgfrrgq, zzgfrrx), held in [[pepper_context]] (one per
   !> thread).
   type, public :: zzgfrru_state
      character(corlen) :: svabco = ' '  !! aberration correction saved by [[zzgfrrin]]
      integer(ip) :: svtarg = 0  !! target ID code saved by [[zzgfrrin]]
      character(32) :: svref = ' '  !! reference frame for the states: J2000
      integer(ip) :: svobs = 0  !! observer ID code saved by [[zzgfrrin]]
      real(wp) :: svdt = 0.0_wp  !! time step of the numerical derivative of range rate
   end type zzgfrru_state

   ! ------------------------------------------------------------------ ZZGFSPU

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZGFSPU routines (entry points
   !> zzgfspin, zzgfspdc, zzgfspgq, zzgfspx), held in [[pepper_context]] (one per
   !> thread).
   type, public :: zzgfspu_state
      character(32) :: svabcr = ' '  !! aberration correction saved by [[zzgfspin]]
      integer(ip) :: svbod1 = 0  !! ID code of the first body saved by [[zzgfspin]]
      integer(ip) :: svbod2 = 0  !! ID code of the second body saved by [[zzgfspin]]
      character(:), allocatable :: svref  !! reference frame of the states saved by [[zzgfspin]]
      character(:), allocatable :: svref1  !! body-fixed frame name of the first body saved by [[zzgfspin]]
      character(:), allocatable :: svref2  !! body-fixed frame name of the second body saved by [[zzgfspin]]
      integer(ip) :: svobs = 0  !! observer ID code saved by [[zzgfspin]]
      real(wp) :: svrad1 = 0.0_wp  !! radius of the first body (largest radius for a sphere, 0 for a point)
      real(wp) :: svrad2 = 0.0_wp  !! radius of the second body (largest radius for a sphere, 0 for a point)
      integer(ip) :: svshp1 = 0  !! shape index of the first body (point or sphere)
      integer(ip) :: svshp2 = 0  !! shape index of the second body (point or sphere)
   end type zzgfspu_state

   ! ------------------------------------------------------------------ ZZGFSSIN

   integer(ip), parameter :: zzgfssin_bdnmln = 36

   !> SPICE private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzgfssin]] routines (entry points
   !> zzgfssin), held in [[pepper_context]] (one per thread).
   type, public :: zzgfssin_state
      logical :: first = .true.  !! first call: target and observer names not yet looked up
      character(zzgfssin_bdnmln) :: svobs = ' '  !! name of the observer with ID prvobs
      integer(ip) :: prvobs = 0  !! observer ID code of the previous call
      integer(ip) :: prvtrg = 0  !! target ID code of the previous call
      character(zzgfssin_bdnmln) :: svtarg = ' '  !! name of the target with ID prvtrg
   end type zzgfssin_state

   ! ------------------------------------------------------------------ ZZGFSSOB

   integer(ip), parameter :: zzgfssob_bdnmln = 36

   !> SPICE private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzgfssob]] routines (entry points
   !> zzgfssob), held in [[pepper_context]] (one per thread).
   type, public :: zzgfssob_state
      logical :: first = .true.  !! first call: target and observer names not yet looked up
      character(zzgfssob_bdnmln) :: svobs = ' '  !! name of the observer with ID prvobs
      integer(ip) :: prvobs = 0  !! observer ID code of the previous call
      integer(ip) :: prvtrg = 0  !! target ID code of the previous call
      character(zzgfssob_bdnmln) :: svtarg = ' '  !! name of the target with ID prvtrg
   end type zzgfssob_state

   ! ------------------------------------------------------------------ ZZHOLDD

   !> SPICE private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzholdd]] routines (entry points
   !> zzholdd), held in [[pepper_context]] (one per thread).
   type, public :: zzholdd_state
      real(wp) :: svalue (nid) = 0.0_wp  !! the stored value of each ID ([[gf_ref]], GF_TOL, GF_DT, ...)
      logical :: first (nid) = .false.  !! no value has been put for the ID yet (or it was reset)
      logical :: init = .true.  !! first call: the FIRST flags not yet initialized
   end type zzholdd_state

   ! ------------------------------------------------------------------ ZZINPDT

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzinpdt]] routines (entry points
   !> zzinpdt), held in [[pepper_context]] (one per thread).
   type, public :: zzinpdt_state
      real(wp) :: altbds ( 2, 3 ) = 0.0_wp  !! bounds for the altitude-only test: full lat/lon range, altitudes expanded by the margin
      logical :: first = .true.  !! first call: hpi, pi2 and the lat/lon parts of altbds not yet set
      real(wp) :: hpi = -1.0_wp  !! pi/2, set on the first call
      real(wp) :: pi2 = 0.0_wp  !! 2 pi, set on the first call
   end type zzinpdt_state

   ! ------------------------------------------------------------------ ZZMOBLIQ

   !> Return the mean obliquity of the ecliptic, and its time
   !> derivative, at a specified epoch.
   !>
   !> The saved variables of SPICELIB's [[zzmobliq]] routines (entry points
   !> zzmobliq), held in [[pepper_context]] (one per thread).
   type, public :: zzmobliq_state
      real(wp) :: rad = 0.0_wp  !! radians per degree
      real(wp) :: year = 0.0_wp  !! seconds per Julian year
      real(wp) :: persec = 0.0_wp  !! Julian centuries per second
      logical :: first = .true.  !! true until the constants above have been set
   end type zzmobliq_state

   ! ------------------------------------------------------------------ ZZMULT

   !> Safely calculate the value A*B, avoiding the possibility
   !> of floating point exceptions (FPE) due to numeric underflow
   !> or numeric overflow.
   !>
   !> The saved variables of SPICELIB's [[zzmult]] routines (entry points zzmult),
   !> held in [[pepper_context]] (one per thread).
   type, public :: zzmult_state
      real(wp) :: expnt = 0.0_wp  !! the bounds on the potential result of the calculation.
      logical :: first = .true.  !! first entry flag.
   end type zzmult_state

   ! ------------------------------------------------------------------ ZZNRDDP

   !> This subroutine is a shell for the routines needed by [[dpspce]]
   !> for calculating deep space effects on a vehicle.
   !>
   !> The saved variables of SPICELIB's ZZNRDDP routines (entry points
   !> zzdpinit, zzdpsec, zzdpper), held in [[pepper_context]] (one per thread).
   type, public :: zznrddp_state
      real(wp) :: atime = 0.0_wp  !! resonance integrator: time of the current integration step (min from epoch)
      real(wp) :: cosiq = 0.0_wp  !! cosine of the inclination at epoch
      real(wp) :: delt = 0.0_wp  !! work: step (+/-720 min) of the resonance integrator
      real(wp) :: e3 = 0.0_wp  !! lunar long-period eccentricity coefficient e3
      real(wp) :: ee2 = 0.0_wp  !! lunar long-period eccentricity coefficient ee2
      real(wp) :: omegao = 0.0_wp  !! argument of perigee at epoch (rad)
      real(wp) :: pe0 = 0.0_wp  !! long-period eccentricity perturbation at epoch, subtracted in [[zzdpper]] (never set)
      real(wp) :: pgh0 = 0.0_wp  !! long-period argument of perigee perturbation at epoch, subtracted in [[zzdpper]] (never set)
      real(wp) :: ph0 = 0.0_wp  !! long-period node perturbation at epoch, subtracted in [[zzdpper]] (never set)
      real(wp) :: pinc0 = 0.0_wp  !! long-period inclination perturbation at epoch, subtracted in [[zzdpper]] (never set)
      real(wp) :: pix1 = 0.0_wp  !! pi
      real(wp) :: pix2 = 0.0_wp  !! 2 pi
      real(wp) :: pl0 = 0.0_wp  !! long-period mean longitude perturbation at epoch, subtracted in [[zzdpper]] (never set)
      real(wp) :: preep = 0.0_wp  !! epoch day (days since 1900 Jan 0.5) of the last lunar orbit computation
      real(wp) :: se = 0.0_wp  !! work: secular eccentricity rate of the current (solar or lunar) pass of [[zzdpinit]]
      real(wp) :: se2 = 0.0_wp  !! solar long-period eccentricity coefficient se2
      real(wp) :: se3 = 0.0_wp  !! solar long-period eccentricity coefficient se3
      real(wp) :: sgh = 0.0_wp  !! work: secular argument of perigee rate of the current (solar or lunar) pass of [[zzdpinit]]
      real(wp) :: sgh2 = 0.0_wp  !! solar long-period argument of perigee coefficient sgh2
      real(wp) :: sgh3 = 0.0_wp  !! solar long-period argument of perigee coefficient sgh3
      real(wp) :: sgh4 = 0.0_wp  !! solar long-period argument of perigee coefficient sgh4
      real(wp) :: sh = 0.0_wp  !! work: secular node rate of the current (solar or lunar) pass of [[zzdpinit]]
      real(wp) :: sh2 = 0.0_wp  !! solar long-period node coefficient sh2
      real(wp) :: sh3 = 0.0_wp  !! solar long-period node coefficient sh3
      real(wp) :: si = 0.0_wp  !! work: secular inclination rate of the current (solar or lunar) pass of [[zzdpinit]]
      real(wp) :: si2 = 0.0_wp  !! solar long-period inclination coefficient si2
      real(wp) :: si3 = 0.0_wp  !! solar long-period inclination coefficient si3
      real(wp) :: siniq = 0.0_wp  !! sine of the inclination at epoch
      real(wp) :: sl = 0.0_wp  !! work: secular mean anomaly rate of the current (solar or lunar) pass of [[zzdpinit]]
      real(wp) :: sl2 = 0.0_wp  !! solar long-period mean longitude coefficient sl2
      real(wp) :: sl3 = 0.0_wp  !! solar long-period mean longitude coefficient sl3
      real(wp) :: sl4 = 0.0_wp  !! solar long-period mean longitude coefficient sl4
      real(wp) :: thgr = 0.0_wp  !! Greenwich sidereal angle at epoch (rad)
      real(wp) :: xfact = 0.0_wp  !! resonance term: rate of the resonance angle minus the mean motion at epoch (rad/min)
      real(wp) :: xgh2 = 0.0_wp  !! lunar long-period argument of perigee coefficient xgh2
      real(wp) :: xgh3 = 0.0_wp  !! lunar long-period argument of perigee coefficient xgh3
      real(wp) :: xgh4 = 0.0_wp  !! lunar long-period argument of perigee coefficient xgh4
      real(wp) :: xh2 = 0.0_wp  !! lunar long-period node coefficient xh2
      real(wp) :: xh3 = 0.0_wp  !! lunar long-period node coefficient xh3
      real(wp) :: xi2 = 0.0_wp  !! lunar long-period inclination coefficient xi2
      real(wp) :: xi3 = 0.0_wp  !! lunar long-period inclination coefficient xi3
      real(wp) :: xl2 = 0.0_wp  !! lunar long-period mean longitude coefficient xl2
      real(wp) :: xl3 = 0.0_wp  !! lunar long-period mean longitude coefficient xl3
      real(wp) :: xl4 = 0.0_wp  !! lunar long-period mean longitude coefficient xl4
      real(wp) :: xlamo = 0.0_wp  !! resonance angle lambda at epoch (rad)
      real(wp) :: xli = 0.0_wp  !! resonance integrator: resonance angle lambda at time atime (rad)
      real(wp) :: xni = 0.0_wp  !! resonance integrator: mean motion at time atime (rad/min)
      real(wp) :: xnq = 0.0_wp  !! mean motion at epoch (rad/min, un-Kozai'd)
      real(wp) :: xqncl = 0.0_wp  !! inclination at epoch (rad)
      real(wp) :: zcosgl = 0.0_wp  !! cosine of the lunar argument of perigee angle (epoch preep)
      real(wp) :: zcoshl = 0.0_wp  !! cosine of the right ascension of the lunar node (epoch preep)
      real(wp) :: zcosil = 0.0_wp  !! cosine of the inclination of the lunar orbit to the equator (epoch preep)
      real(wp) :: zmol = 0.0_wp  !! lunar mean anomaly at the epoch day preep (rad)
      real(wp) :: zmos = 0.0_wp  !! solar mean anomaly at the epoch day preep (rad)
      real(wp) :: zsingl = 0.0_wp  !! sine of the lunar argument of perigee angle (epoch preep)
      real(wp) :: zsinhl = 0.0_wp  !! sine of the right ascension of the lunar node (epoch preep)
      real(wp) :: zsinil = 0.0_wp  !! sine of the inclination of the lunar orbit to the equator (epoch preep)
      real(wp) :: del ( 3  ) = 0.0_wp  !! one-day (synchronous) resonance coefficients del1, del2, del3
      real(wp) :: dg ( 10 ) = 0.0_wp  !! half-day resonance coefficients d2201, d2211, d3210, ..., d5433 (in that order)
      real(wp) :: ssx ( 5  ) = 0.0_wp  !! lunar-solar secular rates of the mean anomaly, perigee, node, eccentricity, inclination (/min)
      integer(ip) :: iresfl = 0  !! resonance flag: 1 if the orbit is resonant (one-day or half-day), else 0
      integer(ip) :: isynfl = 0  !! synchronous flag: 1 for one-day (synchronous) resonance, else 0
      logical :: return = .false.  !! unused (name of the SPICELIB RETURN function in the original declarations)
   end type zznrddp_state

   ! ------------------------------------------------------------------ ZZPDCMPL

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzpdcmpl]] routines (entry points
   !> zzpdcmpl), held in [[pepper_context]] (one per thread).
   type, public :: zzpdcmpl_state
      real(wp) :: apex ( 3 ) = 0.0_wp  !! apex of the latitude cone (X and Y are zero; Z is set on each call)
   end type zzpdcmpl_state

   ! ------------------------------------------------------------------ ZZPLATFM

   integer(ip), parameter :: kysize = 64
   integer(ip), parameter :: zzplatfm_wdsize = 32
   integer(ip), parameter :: zzplatfm_nattr = 6

   !> Return platform ID and various attributes of the intended
   !> environment
   !>
   !> The saved variables of SPICELIB's [[zzplatfm]] routines (entry points
   !> zzplatfm), held in [[pepper_context]] (one per thread). Generated by
   !> tools/modularize_ctx.py from the saved variables of zzplatfm.F90.
   type, public :: zzplatfm_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      character(zzplatfm_wdsize) :: attcpy ( 0 : zzplatfm_nattr ) = ' '  !! copy of the attribute names (uppercase)
      character(kysize) :: keyval ( zzplatfm_nattr ) = ' '  !! value of each platform attribute (system, compiler,
                                                            !! format, ...)
   end type zzplatfm_state

   ! ------------------------------------------------------------------ ZZPTPL02

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzptpl02]] routines (entry points
   !> zzptpl02), held in [[pepper_context]] (one per thread).
   type, public :: zzptpl02_state
      integer(ip) :: cgrscl = 0  !! coarse voxel grid scale of the saved segment
      integer(ip) :: corsys = 0  !! coordinate system code of the saved segment, from its DSK descriptor
      real(wp) :: maxr = 0.0_wp  !! radius of the saved segment's bounding box ([[zzsegbox]]), scales the search distance limit
      logical :: pass1 = .true.  !! first call: no segment parameters saved yet
      integer(ip) :: prvhan = 0  !! DAS handle of the segment whose parameters are saved
      integer(ip) :: prvdsc ( dladsz ) = 0  !! DLA descriptor of the segment whose parameters are saved
      integer(ip) :: vgrext ( 3 ) = 0  !! voxel grid extents (voxel counts in x, y, z) of the saved segment
      real(wp) :: voxori ( 3 ) = 0.0_wp  !! voxel grid origin of the saved segment
      real(wp) :: voxsiz = 0.0_wp  !! voxel size of the saved segment
   end type zzptpl02_state

   ! ------------------------------------------------------------------ ZZRTNMAT

   !> Given a vector, return a transformation matrix that maps from the
   !> vector's base reference frame to the RTN
   !> (radial-tangential-normal) frame associated with the vector.
   !>
   !> The saved variables of SPICELIB's [[zzrtnmat]] routines (entry points
   !> zzrtnmat), held in [[pepper_context]] (one per thread).
   type, public :: zzrtnmat_state
      real(wp) :: z ( 3 ) = [ 0.0_wp, 0.0_wp, 1.0_wp ]  !! constant +Z unit vector (0,0,1)
   end type zzrtnmat_state

   ! ------------------------------------------------------------------ ZZRYTLAT

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzrytlat]] routines (entry points
   !> zzrytlat), held in [[pepper_context]] (one per thread).
   type, public :: zzrytlat_state
      real(wp) :: origin ( 3 ) = 0.0_wp  !! constant origin vector (0,0,0), vertex of the latitude cones
      real(wp) :: z ( 3 ) = [ 0.0_wp, 0.0_wp, 1.0_wp ]  !! constant +Z unit vector (0,0,1), axis of the latitude cones
   end type zzrytlat_state

   ! ------------------------------------------------------------------ ZZRYTPDT

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzrytpdt]] routines (entry points
   !> zzrytpdt), held in [[pepper_context]] (one per thread).
   type, public :: zzrytpdt_state
      real(wp) :: z ( 3 ) = [ 0.0_wp, 0.0_wp, 1.0_wp ]  !! the +Z unit vector, constant
   end type zzrytpdt_state

   ! ------------------------------------------------------------------ ZZSCLK

   integer(ip), parameter :: maxids = 10

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzsclk]] routines (entry points zzsclk),
   !> held in [[pepper_context]] (one per thread).
   type, public :: zzsclk_state
      integer(ip) :: known ( lbcell : maxids ) = 0  !! set of SCLK IDs watched in the kernel pool (by agent [[zzsclk]]<id>)
      integer(ip) :: passed ( lbcell : maxids ) = 0  !! set of SCLK IDs known to have all required SCLK kernel variables
      logical :: first = .true.  !! this is the first call (dtsize and the sets are not yet initialized)
      integer(ip) :: dtsize ( 7 ) = 0  !! required multiple of the size of each of the 7 SCLK kernel variables checked
   end type zzsclk_state

   ! ------------------------------------------------------------------ ZZSFXCOR

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzsfxcor]] routines (entry points
   !> zzsfxcor), held in [[pepper_context]] (one per thread).
   type, public :: zzsfxcor_state
      logical :: first = .true.  !! first call: no aberration correction has been parsed yet
      character(corlen) :: loccor = ' '  !! correction string for [[spkezp]] derived from the previous ABCORR (stellar aberration removed)
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
   end type zzsfxcor_state

   ! ------------------------------------------------------------------ ZZSGLATX

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzsglatx]] routines (entry points
   !> zzsglatx), held in [[pepper_context]] (one per thread).
   type, public :: zzsglatx_state
      real(wp) :: z ( 3 ) = [ 0.0_wp, 0.0_wp, 1.0_wp ]  !! the +Z unit vector
   end type zzsglatx_state

   ! ------------------------------------------------------------------ ZZSGP4

   !> Umbrella for the SGP4 initializer and evaluator routines.
   !>
   !> The saved variables of SPICELIB's ZZSGP4 routines (entry points xxsgp4i,
   !> xxsgp4e), held in [[pepper_context]] (one per thread).
   type, public :: zzsgp4_state
      real(wp) :: a = 0.0_wp  !! mean semi-major axis at epoch from the TLE mean motion (earth radii)
      real(wp) :: alta = 0.0_wp  !! apogee altitude at epoch (earth radii)
      real(wp) :: altp = 0.0_wp  !! perigee altitude at epoch (earth radii)
      real(wp) :: argpdot = 0.0_wp  !! secular rate of the argument of perigee (rad/min)
      real(wp) :: argpo = 0.0_wp  !! argument of perigee at epoch (rad), from the TLE
      real(wp) :: atime = 0.0_wp  !! deep-space resonance integrator: time of the current integration step (min)
      real(wp) :: aycof = 0.0_wp  !! J3 long-period coefficient of the axN/ayN term (recomputed by [[xxsgp4e]] for deep space)
      real(wp) :: bstar = 0.0_wp  !! B* drag term (1/earth radii), from the TLE
      real(wp) :: cc1 = 0.0_wp  !! drag coefficient C1 (secular decay of the semi-major axis)
      real(wp) :: cc4 = 0.0_wp  !! drag coefficient C4 (secular decay of the eccentricity)
      real(wp) :: cc5 = 0.0_wp  !! drag coefficient C5 (periodic eccentricity term, near-earth only)
      real(wp) :: con41 = 0.0_wp  !! 3 cos^2(inclination) - 1 at epoch
      real(wp) :: d2 = 0.0_wp  !! drag coefficient D2 (t^2 term of the semi-major axis, near-earth only)
      real(wp) :: d2201 = 0.0_wp  !! deep-space resonance term d2201 (half-day resonance)
      real(wp) :: d2211 = 0.0_wp  !! deep-space resonance term d2211 (half-day resonance)
      real(wp) :: d3 = 0.0_wp  !! drag coefficient D3 (t^3 term of the semi-major axis, near-earth only)
      real(wp) :: d3210 = 0.0_wp  !! deep-space resonance term d3210 (half-day resonance)
      real(wp) :: d3222 = 0.0_wp  !! deep-space resonance term d3222 (half-day resonance)
      real(wp) :: d4 = 0.0_wp  !! drag coefficient D4 (t^4 term of the semi-major axis, near-earth only)
      real(wp) :: d4410 = 0.0_wp  !! deep-space resonance term d4410 (half-day resonance)
      real(wp) :: d4422 = 0.0_wp  !! deep-space resonance term d4422 (half-day resonance)
      real(wp) :: d5220 = 0.0_wp  !! deep-space resonance term d5220 (half-day resonance)
      real(wp) :: d5232 = 0.0_wp  !! deep-space resonance term d5232 (half-day resonance)
      real(wp) :: d5421 = 0.0_wp  !! deep-space resonance term d5421 (half-day resonance)
      real(wp) :: d5433 = 0.0_wp  !! deep-space resonance term d5433 (half-day resonance)
      real(wp) :: dedt = 0.0_wp  !! deep-space secular rate of the eccentricity (lunar-solar, 1/min)
      real(wp) :: del1 = 0.0_wp  !! deep-space resonance term del1 (one-day resonance)
      real(wp) :: del2 = 0.0_wp  !! deep-space resonance term del2 (one-day resonance)
      real(wp) :: del3 = 0.0_wp  !! deep-space resonance term del3 (one-day resonance)
      real(wp) :: delmo = 0.0_wp  !! (1 + eta cos(mean anomaly at epoch))^3, for the drag mean anomaly term
      real(wp) :: didt = 0.0_wp  !! deep-space secular rate of the inclination (lunar-solar, rad/min)
      real(wp) :: dmdt = 0.0_wp  !! deep-space secular rate of the mean anomaly (lunar-solar, rad/min)
      real(wp) :: dnodt = 0.0_wp  !! deep-space secular rate of the right ascension of the node (lunar-solar, rad/min)
      logical :: dodeep = .false.  !! the orbit is deep space (period >= 225 min): apply lunar-solar terms
      real(wp) :: domdt = 0.0_wp  !! deep-space secular rate of the argument of perigee (lunar-solar, rad/min)
      logical :: dosimp = .false.  !! use the simplified drag model (perigee below 220 km, or deep space)
      real(wp) :: e3 = 0.0_wp  !! deep-space lunar long-period eccentricity coefficient e3
      real(wp) :: ecco = 0.0_wp  !! eccentricity at epoch, from the TLE
      real(wp) :: ee2 = 0.0_wp  !! deep-space lunar long-period eccentricity coefficient ee2
      real(wp) :: er = 0.0_wp  !! equatorial radius of the earth (km), from GEOPHS
      real(wp) :: eta = 0.0_wp  !! SGP4 eta: a0 e0 / (a0 - s) at epoch
      real(wp) :: gsto = 0.0_wp  !! Greenwich sidereal time at epoch (rad)
      real(wp) :: inclo = 0.0_wp  !! inclination at epoch (rad), from the TLE
      integer(ip) :: irez = 0  !! deep-space resonance flag: 0 none, 1 one-day, 2 half-day
      real(wp) :: j2 = 0.0_wp  !! J2 zonal harmonic, from GEOPHS
      real(wp) :: j3oj2 = 0.0_wp  !! J3/J2 ratio of the zonal harmonics, from GEOPHS
      real(wp) :: mdot = 0.0_wp  !! secular rate of the mean anomaly (rad/min), including J2 and J4 terms
      real(wp) :: mo = 0.0_wp  !! mean anomaly at epoch (rad), from the TLE
      real(wp) :: no = 0.0_wp  !! mean motion at epoch (rad/min), from the TLE (un-Kozai'd by [[zzinil]])
      real(wp) :: nodedot = 0.0_wp  !! secular rate of the right ascension of the ascending node (rad/min)
      real(wp) :: nodeo = 0.0_wp  !! right ascension of the ascending node at epoch (rad), from the TLE
      real(wp) :: omgcof = 0.0_wp  !! drag coefficient of the argument of perigee correction (B* C3 cos(argpo))
      real(wp) :: peo = 0.0_wp  !! deep-space long-period eccentricity perturbation at epoch
      real(wp) :: pgho = 0.0_wp  !! deep-space long-period argument of perigee perturbation at epoch
      real(wp) :: pho = 0.0_wp  !! deep-space long-period node perturbation at epoch
      real(wp) :: pinco = 0.0_wp  !! deep-space long-period inclination perturbation at epoch
      real(wp) :: plo = 0.0_wp  !! deep-space long-period mean longitude perturbation at epoch
      real(wp) :: se2 = 0.0_wp  !! deep-space solar long-period eccentricity coefficient se2
      real(wp) :: se3 = 0.0_wp  !! deep-space solar long-period eccentricity coefficient se3
      real(wp) :: sgh2 = 0.0_wp  !! deep-space solar long-period argument of perigee coefficient sgh2
      real(wp) :: sgh3 = 0.0_wp  !! deep-space solar long-period argument of perigee coefficient sgh3
      real(wp) :: sgh4 = 0.0_wp  !! deep-space solar long-period argument of perigee coefficient sgh4
      real(wp) :: sh2 = 0.0_wp  !! deep-space solar long-period node coefficient sh2
      real(wp) :: sh3 = 0.0_wp  !! deep-space solar long-period node coefficient sh3
      real(wp) :: si2 = 0.0_wp  !! deep-space solar long-period inclination coefficient si2
      real(wp) :: si3 = 0.0_wp  !! deep-space solar long-period inclination coefficient si3
      real(wp) :: sinmao = 0.0_wp  !! sin(mean anomaly at epoch), for the drag eccentricity term
      real(wp) :: sl2 = 0.0_wp  !! deep-space solar long-period mean longitude coefficient sl2
      real(wp) :: sl3 = 0.0_wp  !! deep-space solar long-period mean longitude coefficient sl3
      real(wp) :: sl4 = 0.0_wp  !! deep-space solar long-period mean longitude coefficient sl4
      integer(ip) :: svmode = 0  !! operation mode (OPMODE of [[xxsgp4i]]: AFSPC or improved), used by [[xxsgp4e]]
      real(wp) :: t2cof = 0.0_wp  !! coefficient of the t^2 term of the mean anomaly, times no (1.5 C1)
      real(wp) :: t3cof = 0.0_wp  !! coefficient of the t^3 term of the mean anomaly, times no (near-earth only)
      real(wp) :: t4cof = 0.0_wp  !! coefficient of the t^4 term of the mean anomaly, times no (near-earth only)
      real(wp) :: t5cof = 0.0_wp  !! coefficient of the t^5 term of the mean anomaly, times no (near-earth only)
      real(wp) :: x1mth2 = 0.0_wp  !! 1 - cos^2(inclination) at epoch
      real(wp) :: x7thm1 = 0.0_wp  !! 7 cos^2(inclination) - 1 at epoch
      real(wp) :: xfact = 0.0_wp  !! deep-space resonance: rate of the resonance angle minus the mean motion
      real(wp) :: xgh2 = 0.0_wp  !! deep-space lunar long-period argument of perigee coefficient xgh2
      real(wp) :: xgh3 = 0.0_wp  !! deep-space lunar long-period argument of perigee coefficient xgh3
      real(wp) :: xgh4 = 0.0_wp  !! deep-space lunar long-period argument of perigee coefficient xgh4
      real(wp) :: xh2 = 0.0_wp  !! deep-space lunar long-period node coefficient xh2
      real(wp) :: xh3 = 0.0_wp  !! deep-space lunar long-period node coefficient xh3
      real(wp) :: xi2 = 0.0_wp  !! deep-space lunar long-period inclination coefficient xi2
      real(wp) :: xi3 = 0.0_wp  !! deep-space lunar long-period inclination coefficient xi3
      real(wp) :: xke = 0.0_wp  !! sqrt(GM) of the earth in earth radii^1.5/min, from GEOPHS
      real(wp) :: xl2 = 0.0_wp  !! deep-space lunar long-period mean longitude coefficient xl2
      real(wp) :: xl3 = 0.0_wp  !! deep-space lunar long-period mean longitude coefficient xl3
      real(wp) :: xl4 = 0.0_wp  !! deep-space lunar long-period mean longitude coefficient xl4
      real(wp) :: xlamo = 0.0_wp  !! deep-space resonance angle (lambda) at epoch (rad)
      real(wp) :: xlcof = 0.0_wp  !! J3 long-period coefficient of the mean longitude (recomputed by [[xxsgp4e]] for deep space)
      real(wp) :: xli = 0.0_wp  !! deep-space resonance integrator: resonance angle at atime (rad)
      real(wp) :: xmcof = 0.0_wp  !! drag coefficient of the mean anomaly correction (-2/3 coef B* / (e eta))
      real(wp) :: xni = 0.0_wp  !! deep-space resonance integrator: mean motion at atime (rad/min)
      real(wp) :: xnodcf = 0.0_wp  !! coefficient of the t^2 drag term of the node (3.5 beta0^2 xhdot1 C1)
      real(wp) :: zmol = 0.0_wp  !! lunar mean anomaly at epoch (rad)
      real(wp) :: zmos = 0.0_wp  !! solar mean anomaly at epoch (rad)
   end type zzsgp4_state

   ! ------------------------------------------------------------------ ZZSINUTL

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZSINUTL routines (entry points
   !> zzsuelin, zzsudski, zzraysfx, zzmaxrad, zzminrad, zzraynp), held in
   !> [[pepper_context]] (one per thread).
   type, public :: zzsinutl_state
      integer(ip) :: savfid = 0  !! body-fixed frame ID code of the target, saved by [[zzsudski]]
      real(wp) :: savmnr = 0.0_wp  !! minimum radius of the target (ellipsoid radii or DSK bounding radius), km
      real(wp) :: savmxr = 0.0_wp  !! maximum radius of the target (ellipsoid radii or DSK bounding radius), km
      integer(ip) :: savnsf = 0  !! number of surface IDs in savsrf, saved by [[zzsudski]]
      real(wp) :: savrad ( 3 ) = 0.0_wp  !! target ellipsoid radii (km), saved by [[zzsuelin]] (zeroed by [[zzsudski]])
      integer(ip), allocatable :: savsrf (:)  !! (PEPPER: grows; SPICELIB: MAXSRF, 100) surface ID list of the DSK target, saved by [[zzsudski]]
      integer(ip) :: savtrg = 0  !! target ID code, saved by [[zzsudski]]
      integer(ip) :: savtyp = 0  !! target shape type: ellipsoid ([[zzsuelin]]) or DSK ([[zzsudski]])
   end type zzsinutl_state

   ! ------------------------------------------------------------------ ZZSPIN

   integer(ip), parameter :: spshpn = 2

   !> Check/initialize the quantities describing an angular separation
   !> between two spherical or point objects.
   !>
   !> The saved variables of SPICELIB's [[zzspin]] routines (entry points zzspin),
   !> held in [[pepper_context]] (one per thread).
   type, public :: zzspin_state
      character(32) :: svshap ( spshpn ) = [ character(32) :: 'POINT', 'SPHERE' ]  !! names of the supported target shapes (POINT, SPHERE), constant
   end type zzspin_state

   ! ------------------------------------------------------------------ ZZSPKAP0

   ! ------------------------------------------------------------------ ZZSPKAP1

   ! ------------------------------------------------------------------ ZZSPKEZ0

   ! ------------------------------------------------------------------ ZZSPKEZ1

   ! ------------------------------------------------------------------ ZZSPKFAO

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzspkfao]] routines (entry points
   !> zzspkfao), held in [[pepper_context]] (one per thread).
   type, public :: zzspkfao_state
      logical :: pass1 = .true.  !! first call: aberration correction flags not yet set up
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: usestl = .false.  !! the saved correction includes stellar aberration correction
   end type zzspkfao_state

   ! ------------------------------------------------------------------ ZZSPKFAP

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzspkfap]] routines (entry points
   !> zzspkfap), held in [[pepper_context]] (one per thread).
   type, public :: zzspkfap_state
      logical :: pass1 = .true.  !! first call: aberration correction flags not yet set up
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: usestl = .false.  !! the saved correction includes stellar aberration correction
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
   end type zzspkfap_state

   ! ------------------------------------------------------------------ ZZSPKFAT

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzspkfat]] routines (entry points
   !> zzspkfat), held in [[pepper_context]] (one per thread).
   type, public :: zzspkfat_state
      logical :: pass1 = .true.  !! first call: aberration correction flags not yet set up
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: usestl = .false.  !! the saved correction includes stellar aberration correction
   end type zzspkfat_state

   ! ------------------------------------------------------------------ ZZSPKFLT

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzspkflt]] routines (entry points
   !> zzspkflt), held in [[pepper_context]] (one per thread).
   type, public :: zzspkflt_state
      logical :: pass1 = .true.  !! first call: aberration correction flags not yet set up
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: usecn = .false.  !! the saved correction uses converged Newtonian light time
      logical :: uselt = .false.  !! the saved correction includes light time correction
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
   end type zzspkflt_state

   ! ------------------------------------------------------------------ ZZSPKFZO

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzspkfzo]] routines (entry points
   !> zzspkfzo), held in [[pepper_context]] (one per thread).
   type, public :: zzspkfzo_state
      integer(ip) :: fj2000 = 0  !! frame ID code of J2000
      logical :: pass1 = .true.  !! first call: correction flags and J2000 code not yet set up
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      logical :: usegeo = .false.  !! the saved correction is geometric (no aberration correction)
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
   end type zzspkfzo_state

   ! ------------------------------------------------------------------ ZZSPKFZT

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzspkfzt]] routines (entry points
   !> zzspkfzt), held in [[pepper_context]] (one per thread).
   type, public :: zzspkfzt_state
      character(corlen) :: prvcor = ' '  !! aberration correction string of the previous call
      real(wp) :: sttctr ( 6    ) = 0.0_wp  !! work: state of the target relative to its center (from TRGSUB)
      integer(ip) :: fj2000 = 0  !! frame ID code of J2000
      integer(ip) :: trgctr = 0  !! work: ID code of the target's center (from TRGSUB)
      logical :: first = .true.  !! first call: correction flags and J2000 code not yet set up
      logical :: usegeo = .false.  !! the saved correction is geometric (no aberration correction)
      logical :: xmit = .false.  !! the saved correction is for transmitted (rather than received) radiation
   end type zzspkfzt_state

   ! ------------------------------------------------------------------ ZZSPKGO0

   ! ------------------------------------------------------------------ ZZSPKGO1

   ! ------------------------------------------------------------------ ZZSPKGP0

   ! ------------------------------------------------------------------ ZZSPKGP1

   ! ------------------------------------------------------------------ ZZSPKPA0

   ! ------------------------------------------------------------------ ZZSPKPA1

   ! ------------------------------------------------------------------ ZZSPKZP0

   ! ------------------------------------------------------------------ ZZSPKZP1

   ! ------------------------------------------------------------------ ZZSRFKER

   character(*), parameter :: kvsfbd = 'NAIF_SURFACE_BODY'
   character(*), parameter :: kvsfcd = 'NAIF_SURFACE_CODE'
   character(*), parameter :: kvsfnm = 'NAIF_SURFACE_NAME'
   integer(ip), parameter :: zzsrfker_kvnmln = 32
   integer(ip), parameter :: nnames = 3

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzsrfker]] routines (entry points
   !> zzsrfker), held in [[pepper_context]] (one per thread).
   type, public :: zzsrfker_state
      logical :: pass1 = .true.  !! first call: the kernel pool watch on the surface variables is not yet set
      !> names of the surface name-ID kernel variables (watched by ZZSRFTRN)
      character(zzsrfker_kvnmln) :: names ( nnames ) = [ character(zzsrfker_kvnmln) :: kvsfbd, kvsfcd, &
         kvsfnm ]
   end type zzsrfker_state

   ! ------------------------------------------------------------------ ZZSRFTRN

   !> Surface names, IDs and bodies from the kernel pool
   !> (NAIF_SURFACE_NAME, NAIF_SURFACE_CODE, NAIF_SURFACE_BODY), as
   !> [[zzsrfker]] reads them.
   !>
   !> PEPPER: SPICELIB kept them in arrays of MXNSRF (2000) entries with
   !> hash tables, names of at most SFNMLN (36) characters, and signaled
   !> SPICE(TOOMANYSURFACES) for more; these grow, and names have any
   !> length. As in SPICELIB (ZZSRFINI), the last assignment of a name for
   !> a body counts, and a surface ID of a body maps to the last assignment
   !> whose name is not assigned again later for that body.
   type, public :: surface_names
      type(string), allocatable :: names(:)  !! the surface names, as in the kernel pool
      type(string), allocatable :: nornam(:)  !! the names, normalized (uppercase, single blanks)
      integer(ip), allocatable :: ids(:)  !! the surface IDs
      integer(ip), allocatable :: bodies(:)  !! the body ID codes
      integer(ip) :: n = 0  !! number of assignments
      logical :: extker = .false.  !! the kernel pool defines surface names
      type(string_map) :: name_map  !! the index of each (normalized name, body)
      type(string_map) :: id_map  !! the index of each (surface ID, body)
   contains
      procedure :: clear => surface_names_clear
      procedure :: index => surface_names_index
      procedure :: name_index => surface_names_name_index
      procedure :: id_index => surface_names_id_index
   end type surface_names

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZSRFTRN routines (entry points
   !> zzsrfn2c, zzsrfc2n, zzsrftrk), held in [[pepper_context]] (one per thread).
   !>
   type, public :: zzsrftrn_state
      type(surface_names) :: srf  !! the surface names from the kernel pool
      integer(ip) :: polctr ( ctrsiz ) = 0  !! the kernel pool state counter of the last check
      integer(ip) :: srfctr ( ctrsiz ) = 0  !! surface mapping state counter (incremented when the tables are rebuilt)
      logical :: pass1 = .true.  !! first call: counters and tables not yet initialized
   end type zzsrftrn_state

   ! ------------------------------------------------------------------ ZZSWFXFM

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzswfxfm]] routines (entry points
   !> zzswfxfm), held in [[pepper_context]] (one per thread).
   !>
   !> No default initialization (448704 bytes): see [[init_zzswfxfm_state]].
   type, public :: zzswfxfm_state
      ! PEPPER: the base frame arrays grow (ZZSWFXFM makes room for a
      ! frame's base frames before buffering it; SPICELIB: MAXBAS, 15000,
      ! and SPICE(TOOMANYBASEFRAMES) for a frame with more). The table of
      ! switch frames is still a cache of MAXFRM (1013) frames, emptied
      ! when full.
      integer(ip) :: basbeg ( maxfrm )  !! start index in baslst of each switch frame's base frame list
      integer(ip) :: bascnt ( maxfrm )  !! number of base frames of each switch frame
      integer(ip), allocatable :: baslst ( : )  !! ID codes of the base frames of the buffered switch frames
      integer(ip), allocatable :: clsids ( : )  !! frame class ID of each base frame in baslst
      integer(ip), allocatable :: clsses ( : )  !! frame class of each base frame in baslst
      logical :: binary ( maxfrm )  !! true for each switch frame whose base frame intervals allow a binary search
      integer(ip) :: fidlst ( maxfrm )  !! switch frame ID hash: ID codes of the buffered switch frames
      integer(ip) :: free  !! index of the first free element of baslst and its parallel arrays
      integer(ip) :: frpool ( lbsngl : maxfrm )  !! switch frame ID hash: collision list pool
      integer(ip) :: hdfram ( maxfrm )  !! switch frame ID hash: collision list heads
      logical :: pass1  !! true until the first-call initialization is done
      integer(ip) :: polctr ( ctrsiz )  !! kernel pool state counter
      integer(ip) :: prvat  !! buffer index of the switch frame from the previous successful call
      integer(ip) :: prvfrm  !! switch frame ID from the previous successful call (0 if none)
      real(wp), allocatable :: starts ( : )  !! start time (TDB) of the interval of each base frame of the switch frames
      real(wp), allocatable :: stops ( : )  !! applicability interval stop time of each base frame in baslst
      logical :: usetim ( maxfrm )  !! true for each switch frame whose base frames have time intervals
      real(wp) :: xident ( 6, 6 )  !! 6x6 identity matrix, set on the first call
   end type zzswfxfm_state

   ! ------------------------------------------------------------------ ZZTANUTL

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due to the
   !> volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZTANUTL routines (entry points
   !> zztanini, zztansta), held in [[pepper_context]] (one per thread).
   type, public :: zztanutl_state
      real(wp) :: svaxis ( 3 ) = 0.0_wp  !! unit vector along the axis (AXIS) saved by [[zztanini]]
      integer(ip) :: svcurv = 0  !! curve type code (limb, umbral or penumbral terminator) saved by [[zztanini]]
      real(wp) :: svet = 0.0_wp  !! evaluation epoch (ET, used for DSK segment selection) saved by [[zztanini]]
      real(wp) :: svirad = 0.0_wp  !! illumination source radius (km) saved by [[zztanini]]
      real(wp) :: svnrml ( 3 ) = 0.0_wp  !! unit normal of the half-plane of AXIS and PLNVEC, saved by [[zztanini]]
      real(wp) :: svvrtx ( 3 ) = 0.0_wp  !! the axis vector as given, used as the ray vertex; saved by [[zztanini]]
   end type zztanutl_state

   ! ------------------------------------------------------------------ ZZTIME

   integer(ip), parameter :: lower = 1
   integer(ip), parameter :: upper = lower  + 1
   integer(ip), parameter :: mixed = upper  + 1
   integer(ip), parameter :: zztime_full = 1
   integer(ip), parameter :: zztime_short = zztime_full   + 1
   integer(ip), parameter :: maxtkn = 64
   integer(ip), parameter :: piclen = maxtkn * 5
   integer(ip), parameter :: zztime_wdsize = 32
   integer(ip), parameter :: zztime_nrecog = 70
   integer(ip), parameter :: zztime_smwdsz = 12

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines. Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZTIME routines (entry points zzcmbt,
   !> zzgrep, zzispt, zzist, zznote, zzremt, zzsubt, zztokns, zzunpck,
   !> zzvalt), held in [[pepper_context]] (one per thread).
   type, public :: zztime_state
      character(maxtkn) :: rep = ' '  !! the token representation of the time string: one letter per token
      integer(ip) :: size = 0  !! number of tokens
      integer(ip) :: begs ( maxtkn ) = 0  !! first character of each token in the string
      integer(ip) :: ends ( maxtkn ) = 0  !! last character of each token in the string
      integer(ip) :: pbegs ( maxtkn ) = 0  !! first character of each token's picture in pictur
      integer(ip) :: pends ( maxtkn ) = 0  !! last character of each token's picture in pictur
      character(piclen) :: pictur = ' '  !! the format picture (as for [[timout]]) built from the tokens
      character(zztime_wdsize) :: names ( 32 : 126 ) = ' '  !! name of each token class (by its letter), for error
                                                            !! messages
      character(piclen) :: picerr = ' '  !! template of the message for a picture that is too long
      character(piclen) :: tknerr = ' '  !! template of the message for too many tokens
      real(wp) :: hms ( 3  ) = 0.0_wp  !! work variable (saved in SPICELIB; not shown to carry a value
                                       !! between calls); hours, minutes and seconds
      integer(ip) :: blank = 0  !! character code of ' '
      integer(ip) :: to = 0  !! work variable (saved in SPICELIB; not shown to carry a value between
                             !! calls)
      integer(ip) :: pto = 0  !! work variable (saved in SPICELIB; not shown to carry a value between
                              !! calls)
      integer(ip) :: case = 0  !! work variable (saved in SPICELIB; not shown to carry a value between
                               !! calls)
      integer(ip) :: mnsize ( zztime_short ) = 0  !! number of characters of month names (full, short)
      integer(ip) :: wksize ( zztime_short ) = 0  !! number of characters of weekday names (full, short)
      integer(ip) :: width ( zztime_nrecog ) = 0  !! length of each recognized substring
      character(zztime_smwdsz) :: recog ( zztime_nrecog ) = ' '  !! the recognized substrings (month and weekday names,
                                                                 !! eras, ...)
      character(zztime_smwdsz) :: mnmrk ( mixed, zztime_short ) = ' '  !! picture markers of month names by case (lower,
                                                                       !! upper, mixed) and length
      character(zztime_smwdsz) :: wkday ( mixed, zztime_short ) = ' '  !! picture markers of weekday names by case and
                                                                       !! length
      character(1) :: class ( zztime_nrecog ) = ' '  !! token class letter of each recognized substring
      integer(ip) :: f ( 32 : 126 ) = 0  !! first recognized substring that starts with each character
      integer(ip) :: l ( 32 : 126 ) = 0  !! last recognized substring that starts with each character
      logical :: ampm = .false.  !! the string has an AM/PM marker
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
   end type zztime_state

   ! ------------------------------------------------------------------ ZZVSTRNG

   integer(ip), parameter :: maxsig = 14
   integer(ip), parameter :: zzvstrng_wdsize = maxsig + 16

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's ZZVSTRNG routines (entry points
   !> zzvststr, zzvsbstr), held in [[pepper_context]] (one per thread).
   type, public :: zzvstrng_state
      character(zzvstrng_wdsize) :: string = ' 0.0000000000000E+00 '  !! the decimal string (from [[dpstr]]) of the number set by [[zzvststr]]
      character(1) :: myfill = ' '  !! the fill character for the digits before the first significant one
      integer(ip) :: code0 = 0  !! the character code of '0'
      integer(ip) :: exp = 0  !! the exponent of the number set by [[zzvststr]]
   end type zzvstrng_state

   ! ------------------------------------------------------------------ ZZWAHR

   !> Calculates nutation angles delta psi and delta epsilon,  and
   !> their rates of change, referred to the ecliptic of date, from
   !> the Wahr series (Table 1,'Proposal to the IAU Working Group
   !> on Nutation', John M. Wahr and Martin L. Smith 1979)
   !>
   !> The saved variables of SPICELIB's [[zzwahr]] routines (entry points zzwahr),
   !> held in [[pepper_context]] (one per thread).
   type, public :: zzwahr_state
      real(wp) :: angle (5) = 0.0_wp  !! fundamental arguments l, l', F, D, Omega at the epoch of the last call (radians)
      real(wp) :: angrt (5) = 0.0_wp  !! rates of the fundamental arguments at the epoch of the last call (radians/day)
      real(wp) :: dtwopi = 0.0_wp  !! two pi
      real(wp) :: factr = 0.0_wp  !! conversion from units of 0.0001 arcseconds to radians (divisor)
      real(wp) :: oneday = 0.0_wp  !! number of seconds in a day
      real(wp) :: radian = 0.0_wp  !! degrees per radian
      real(wp) :: d0 = 0.0_wp  !! D (Moon minus Sun mean longitude): constant term (degrees)
      real(wp) :: d1 = 0.0_wp  !! D: linear coefficient (degrees/day)
      real(wp) :: d2 = 0.0_wp  !! D: quadratic coefficient (degrees/(0.0001 days)**2)
      real(wp) :: d3 = 0.0_wp  !! D: cubic coefficient (degrees/(0.0001 days)**3)
      real(wp) :: f0 = 0.0_wp  !! F (Moon minus Moon node mean longitude): constant term (degrees)
      real(wp) :: f1 = 0.0_wp  !! F: linear coefficient (degrees/day)
      real(wp) :: f2 = 0.0_wp  !! F: quadratic coefficient (degrees/(0.0001 days)**2)
      real(wp) :: f3 = 0.0_wp  !! F: cubic coefficient (degrees/(0.0001 days)**3)
      real(wp) :: l0 = 0.0_wp  !! l (Moon mean anomaly): constant term (degrees)
      real(wp) :: l1 = 0.0_wp  !! l: linear coefficient (degrees/day)
      real(wp) :: l2 = 0.0_wp  !! l: quadratic coefficient (degrees/(0.0001 days)**2)
      real(wp) :: l3 = 0.0_wp  !! l: cubic coefficient (degrees/(0.0001 days)**3)
      real(wp) :: lp0 = 0.0_wp  !! l' (Sun mean anomaly): constant term (degrees)
      real(wp) :: lp1 = 0.0_wp  !! l': linear coefficient (degrees/day)
      real(wp) :: lp2 = 0.0_wp  !! l': quadratic coefficient (degrees/(0.0001 days)**2)
      real(wp) :: lp3 = 0.0_wp  !! l': cubic coefficient (degrees/(0.0001 days)**3)
      real(wp) :: mg0 = 0.0_wp  !! Omega (lunar mean ascending node longitude): constant term (degrees)
      real(wp) :: mg1 = 0.0_wp  !! Omega: linear coefficient (degrees/day)
      real(wp) :: mg2 = 0.0_wp  !! Omega: quadratic coefficient (degrees/(0.0001 days)**2)
      real(wp) :: mg3 = 0.0_wp  !! Omega: cubic coefficient (degrees/(0.0001 days)**3)
      logical :: first = .true.  !! true until the constants above have been computed
   end type zzwahr_state

   ! ------------------------------------------------------------------ ZZXLATED

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzxlated]] routines (entry points
   !> zzxlated), held in [[pepper_context]] (one per thread). Generated by
   !> tools/modularize_ctx.py from the saved variables of zzxlated.F90.
   type, public :: zzxlated_state
      integer(ip) :: bigint = 0  !! largest integer
      integer(ip) :: smlint = 0  !! smallest integer
      character(strsiz) :: strbff ( numbff ) = ' '  !! names of the binary file formats
      integer(ip) :: natbff = 0  !! code of the native binary file format
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
   end type zzxlated_state

   ! ------------------------------------------------------------------ ZZXLATEI

   !> SPICE Private routine intended solely for the support of SPICE
   !> routines.  Users should not call this routine directly due
   !> to the volatile nature of this routine.
   !>
   !> The saved variables of SPICELIB's [[zzxlatei]] routines (entry points
   !> zzxlatei), held in [[pepper_context]] (one per thread). Generated by
   !> tools/modularize_ctx.py from the saved variables of zzxlatei.F90.
   type, public :: zzxlatei_state
      logical :: first = .true.  !! the tables have not been set up yet (they are set up on the first call)
      integer(ip) :: natbff = 0  !! code of the native binary file format
      character(strsiz) :: strbff ( numbff ) = ' '  !! names of the binary file formats
      integer(ip) :: smlint = 0  !! smallest integer
      integer(ip) :: bigint = 0  !! largest integer
   end type zzxlatei_state

contains

   !> Set the initial values of a new [[dafah_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   !> The index of the DAF with a handle in the DAF file table: 0 if no
   !> DAF has this handle.
   pure integer(ip) function daf_find(self, handle) result(i)
      class(dafah_state), intent(in) :: self  !! the file table
      integer(ip), intent(in) :: handle  !! the handle
      integer(ip) :: lo, hi, key, k
      key = abs(handle)
      lo = 1
      hi = self%nft
      i = 0
      do while (lo <= hi)
         k = lo + (hi - lo)/2
         if (abs(self%files(k)%handle) < key) then
            lo = k + 1
         else if (abs(self%files(k)%handle) > key) then
            hi = k - 1
         else
            if (self%files(k)%handle == handle) i = k
            return
         end if
      end do
   end function daf_find

   !> Add a DAF to the DAF file table, in order of abs(handle) (normally
   !> at the end: handles grow).
   pure subroutine daf_add(self, file)
      class(dafah_state), intent(inout) :: self  !! the file table
      type(daf_file), intent(in) :: file  !! the DAF
      type(daf_file), allocatable :: tmp(:)
      integer(ip) :: k
      if (.not. allocated(self%files)) allocate (self%files(8))
      if (self%nft == size(self%files)) then
         allocate (tmp(2*size(self%files)))
         tmp(1:self%nft) = self%files(1:self%nft)
         call move_alloc(tmp, self%files)
      end if
      k = self%nft
      do while (k > 0)
         if (abs(self%files(k)%handle) < abs(file%handle)) exit
         self%files(k + 1) = self%files(k)
         k = k - 1
      end do
      self%files(k + 1) = file
      self%nft = self%nft + 1
   end subroutine daf_add

   !> Remove DAF i from the DAF file table, keeping the order of the
   !> others.
   pure subroutine daf_remove(self, i)
      class(dafah_state), intent(inout) :: self  !! the file table
      integer(ip), intent(in) :: i  !! the index of the DAF
      self%files(i:self%nft - 1) = self%files(i + 1:self%nft)
      self%files(self%nft) = daf_file()
      self%nft = self%nft - 1
   end subroutine daf_remove

   !> Set the initial values of a new [[dafana_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_dafana_state(state)
      type(dafana_state), intent(inout) :: state  !! the state
      state%sthead = nil
      state%stfptr = nil
      call dafana_grow(state)
   end subroutine init_dafana_state

   !> Enlarge the DAFANA state table (to DAFANA_TBSIZE entries if it has
   !> none, else to twice its size), keeping its entries and lists; the
   !> new entries are added to the free list.
   pure subroutine dafana_grow(state)
      type(dafana_state), intent(inout) :: state  !! the state
      integer(ip) :: n, m, i
      n = 0
      if (allocated(state%stpool)) n = size(state%stpool)
      m = max(dafana_tbsize, 2*n)
      call grow(state%stfh, m)
      call grow(state%stifnm, m)
      call grow(state%staddg, m)
      call grow(state%stfrst, m)
      call grow(state%stlast, m)
      call grow(state%stfree, m)
      call grow(state%stbegn, m)
      call grow(state%stlsum, m, maxsum)
      call grow(state%stname, m)
      call grow(state%stpool, m)
      m = size(state%stpool)
      do i = n + 1, m - 1
         state%stpool(i) = i + 1
      end do
      state%stpool(m) = state%stfptr
      state%stfptr = n + 1
   end subroutine dafana_grow

   !> Set the initial values of a new [[dafec_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_dafec_state(state)
      type(dafec_state), intent(inout) :: state  !! the state
      state%crecrd = ' '
      state%eocmrk = ' '
      state%eolmrk = ' '
      state%lsthan = 0
      state%nfiles = 0
      allocate ( state%filchr(8), state%filcnt(8), state%filhan(8), state%lstpos(8), &
                 state%lstrec(8), source=0_ip )
      state%first = .true.
   end subroutine init_dafec_state

   !> Set the initial values of a new [[daffa_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_daffa_state(state)
      type(daffa_state), intent(inout) :: state  !! the state
      state%sthead = nil
      state%stfptr = nil
      call daffa_grow(state)
   end subroutine init_daffa_state

   !> Enlarge the DAFFA state table (to DAFFA_TBSIZE entries if it has
   !> none, else to twice its size), keeping its entries and lists; the
   !> new entries are added to the free list.
   pure subroutine daffa_grow(state)
      type(daffa_state), intent(inout) :: state  !! the state
      integer(ip) :: n, m, i
      n = 0
      if (allocated(state%stpool)) n = size(state%stpool)
      m = max(daffa_tbsize, 2*n)
      call grow(state%stfh, m)
      call grow(state%stprev, m)
      call grow(state%stthis, m)
      call grow(state%stnext, m)
      call grow(state%stnseg, m)
      call grow(state%stcurr, m)
      call grow(state%stnr, m)
      call grow(state%sthvnr, m)
      call grow(state%stsr, m, dprsiz)
      call grow(state%stpool, m)
      m = size(state%stpool)
      do i = n + 1, m - 1
         state%stpool(i) = i + 1
      end do
      state%stpool(m) = state%stfptr
      state%stfptr = n + 1
   end subroutine daffa_grow

   !> Set the initial values of a new [[dafrwd_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_dafrwd_state(state)
      type(dafrwd_state), intent(inout) :: state  !! the state
      state%rbhan = 0
      state%rbrec = 0
      state%rbreq = 0
      state%rbdat = 0.0_wp
      state%rbnbr = 1
      state%nread = 0
      state%nreq = 0
   end subroutine init_dafrwd_state

   !> Set the initial values of a new [[dasec_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_dasec_state(state)
      type(dasec_state), intent(inout) :: state  !! the state
      state%crecrd = ' '
      state%lsthan = 0
      state%nfiles = 0
      allocate ( state%filchr(8), state%filcnt(8), state%filhan(8), state%lstpos(8), &
                 state%lstrec(8), source=0_ip )
      state%first = .true.
   end subroutine init_dasec_state

   !> Set the initial values of a new [[dasfm_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   !> The index of the DAS with a handle in the DAS file table: 0 if no
   !> DAS has this handle.
   pure integer(ip) function das_find(self, handle) result(i)
      class(dasfm_state), intent(in) :: self  !! the file table
      integer(ip), intent(in) :: handle  !! the handle
      integer(ip) :: lo, hi, key, k
      key = abs(handle)
      lo = 1
      hi = self%nft
      i = 0
      do while (lo <= hi)
         k = lo + (hi - lo)/2
         if (abs(self%files(k)%handle) < key) then
            lo = k + 1
         else if (abs(self%files(k)%handle) > key) then
            hi = k - 1
         else
            if (self%files(k)%handle == handle) i = k
            return
         end if
      end do
   end function das_find

   !> Add a DAS to the DAS file table, in order of abs(handle) (normally
   !> at the end: handles grow).
   pure subroutine das_add(self, file)
      class(dasfm_state), intent(inout) :: self  !! the file table
      type(das_file), intent(in) :: file  !! the DAS
      type(das_file), allocatable :: tmp(:)
      integer(ip) :: k
      if (.not. allocated(self%files)) allocate (self%files(8))
      if (self%nft == size(self%files)) then
         allocate (tmp(2*size(self%files)))
         tmp(1:self%nft) = self%files(1:self%nft)
         call move_alloc(tmp, self%files)
      end if
      k = self%nft
      do while (k > 0)
         if (abs(self%files(k)%handle) < abs(file%handle)) exit
         self%files(k + 1) = self%files(k)
         k = k - 1
      end do
      self%files(k + 1) = file
      self%nft = self%nft + 1
   end subroutine das_add

   !> Remove DAS i from the DAS file table, keeping the order of the
   !> others.
   pure subroutine das_remove(self, i)
      class(dasfm_state), intent(inout) :: self  !! the file table
      integer(ip), intent(in) :: i  !! the index of the DAS
      self%files(i:self%nft - 1) = self%files(i + 1:self%nft)
      self%files(self%nft) = das_file()
      self%nft = self%nft - 1
   end subroutine das_remove

   !> Set the initial values of a new [[dasrwr_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_dasrwr_state(state)
      type(dasrwr_state), intent(inout) :: state  !! the state
      state%rcbufc = ' '
      state%rcbufd = 0.0_wp
      state%rcbufi = 0
      state%rnbufc = 0
      state%rnbufd = 0
      state%rnbufi = 0
      state%hnbufc = 0
      state%hnbufd = 0
      state%hnbufi = 0
      state%upbufc = .false.
      state%upbufd = .false.
      state%upbufi = .false.
      state%poolc = 0
      state%poold = 0
      state%pooli = 0
      state%headc = 0
      state%headd = 0
      state%headi = 0
      state%usedc = 0
      state%usedd = 0
      state%usedi = 0
      state%pass1 = .true.
   end subroutine init_dasrwr_state

   !> Set the initial values of a new [[ekqmgr_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_ekqmgr_state(state)
      type(ekqmgr_state), intent(inout) :: state  !! the state
      allocate (state%ftpool(2, lbpool:4), state%fthan(4), source=0_ip)
      allocate (state%tbpool(2, lbpool:8), state%tbstpt(8), state%tbncol(8), state%tbctpt(8), source=0_ip)
      allocate (state%tbnams(8), source=repeat(' ', tnamsz))
      allocate (state%stpool(2, lbpool:16), state%sthan(16), state%stsidx(16), state%stnrow(16), &
                state%stncol(16), state%stdtpt(16), state%stdscs(sdscsz, 16), source=0_ip)
      allocate (state%dtpool(2, lbpool:64), state%dtdscs(cdscsz, 64), source=0_ip)
      allocate (state%ctpool(2, lbpool:64), state%ctclas(64), state%cttyps(64), state%ctlens(64), &
                state%ctsizs(64), source=0_ip)
      allocate (state%ctnams(64), source=repeat(' ', cnamsz))
      allocate (state%ctfixd(64), state%ctindx(64), state%ctnull(64), source=.false.)
      state%fthead = 0
      state%tbhead = 0
      state%first = .true.
   end subroutine init_ekqmgr_state

   !> Set the initial values of a new [[ekqmgr_context_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_ekqmgr_context_state(state)
      type(ekqmgr_context_state), intent(inout) :: state  !! the state
      state%cnams = ' '
      state%cnmset = ' '
      state%colnam = ' '
      state%problm = ' '
      state%tabnam = ' '
      state%cdscrs = 0
      state%ncols = 0
      state%new = 0
      state%nmatch = 0
      state%norder = 0
      state%npcol = 0
      state%nsel = 0
      state%ntab = 0
      state%ordbas = 0
      state%segdsc = 0
      state%stnew = 0
      state%tbcurr = 0
      state%unrows = 0
      state%usize = 0
      state%dosort = .false.
      state%fnd = .false.
      state%presnt = .false.
      state%sorted = .false.
   end subroutine init_ekqmgr_context_state

   !> Set the initial values of a new [[framex_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_framex_state(state)
      type(framex_state), intent(inout) :: state  !! the state
      state%name = ' '
      state%center = 0
      state%centrd = 0
      state%idcode = 0
      state%type = 0
      state%typeid = 0
      state%first = .true.
      state%pulctr = 0
      state%bnmlst = 0
      state%bnmpol = 0
      state%bnmnms = ' '
      state%bnmidx = 0
      state%bidlst = 0
      state%bidpol = 0
      state%bidids = 0
      state%bididx = 0
   end subroutine init_framex_state

   !> Empty the FRAMEX caches of kernel pool frames (the pool changed).
   pure subroutine framex_kreset(self)
      class(framex_state), intent(inout) :: self  !! the state
      call self%knmmap%clear()
      call self%kidmap%clear()
      self%nknm = 0
      self%nkid = 0
   end subroutine framex_kreset

   !> The index (in knmids) of a kernel frame name in the cache: 0 if it is
   !> not there.
   pure integer(ip) function framex_kname_find(self, name) result(item)
      class(framex_state), intent(in) :: self  !! the state
      character(*), intent(in) :: name  !! the frame name
      logical :: found
      call self%knmmap%get(name, item, found)
   end function framex_kname_find

   !> Add a kernel frame name to the cache: its index in knmids (where the
   !> caller stores the frame ID).
   pure subroutine framex_kname_add(self, name, item)
      class(framex_state), intent(inout) :: self  !! the state
      character(*), intent(in) :: name  !! the frame name
      integer(ip), intent(out) :: item  !! its index in knmids
      item = self%kname_find(name)
      if (item > 0) return
      self%nknm = self%nknm + 1
      item = self%nknm
      call grow(self%knmids, item)
      call self%knmmap%set(name, item)
   end subroutine framex_kname_add

   !> The index (in kname, kcent, ...) of a kernel frame ID in the cache: 0
   !> if it is not there.
   pure integer(ip) function framex_kid_find(self, id) result(item)
      class(framex_state), intent(in) :: self  !! the state
      integer(ip), intent(in) :: id  !! the frame ID code
      logical :: found
      call self%kidmap%get(id, item, found)
   end function framex_kid_find

   !> Add a kernel frame ID to the cache: its index in kname, kcent, ...
   !> (where the caller stores the frame's data).
   pure subroutine framex_kid_add(self, id, item)
      class(framex_state), intent(inout) :: self  !! the state
      integer(ip), intent(in) :: id  !! the frame ID code
      integer(ip), intent(out) :: item  !! its index in kname, kcent, ...
      item = self%kid_find(id)
      if (item > 0) return
      self%nkid = self%nkid + 1
      item = self%nkid
      call grow(self%kname, item)
      call grow(self%kcent, item)
      call grow(self%kclass, item)
      call grow(self%kclsid, item)
      call self%kidmap%set(id, item)
   end subroutine framex_kid_add

   !> Add a kernel at the end of the KEEPER table.
   pure subroutine keeper_add(self, kernel)
      class(keeper_state), intent(inout) :: self  !! the table
      type(keeper_kernel), intent(in) :: kernel  !! the kernel
      type(keeper_kernel), allocatable :: tmp(:)
      integer(ip) :: k
      if (.not. allocated(self%kernels)) allocate (self%kernels(16))
      if (self%loaded == size(self%kernels)) then
         allocate (tmp(2*size(self%kernels)))
         do k = 1, self%loaded
            call move_kernel(self%kernels(k), tmp(k))
         end do
         call move_alloc(tmp, self%kernels)
      end if
      self%loaded = self%loaded + 1
      self%kernels(self%loaded) = kernel
   end subroutine keeper_add

   !> Remove kernel i from the KEEPER table, keeping the order of the
   !> others (the sources of the others are not changed).
   pure subroutine keeper_remove(self, i)
      class(keeper_state), intent(inout) :: self  !! the table
      integer(ip), intent(in) :: i  !! the index of the kernel
      integer(ip) :: k
      do k = i + 1, self%loaded
         call move_kernel(self%kernels(k), self%kernels(k - 1))
      end do
      self%kernels(self%loaded) = keeper_kernel()
      self%loaded = self%loaded - 1
   end subroutine keeper_remove

   !> Remove all kernels from the KEEPER table (and release its memory).
   pure subroutine keeper_clear(self)
      class(keeper_state), intent(inout) :: self  !! the table
      self%loaded = 0
      if (allocated(self%kernels)) deallocate (self%kernels)
   end subroutine keeper_clear

   !> Move a KEEPER record (its name without copying).
   pure subroutine move_kernel(from, to)
      type(keeper_kernel), intent(inout) :: from  !! the record moved (its name is deallocated)
      type(keeper_kernel), intent(inout) :: to  !! the record replaced
      to%type = from%type
      to%handle = from%handle
      to%source = from%source
      call move_alloc(from%name, to%name)
      call move_alloc(from%loader, to%loader)
      call move_alloc(from%format, to%format)
      call move_alloc(from%memory_text, to%memory_text)
   end subroutine move_kernel

   !> Set the initial values of a new [[limbpt_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_limbpt_state(state)
      type(limbpt_state), intent(inout) :: state  !! the state
      state%first = .true.
      state%lmbtyp = 0
      state%loccde = 0
      state%nsurf = 0
      state%prvcor = ' '
      state%prvloc = ' '
      state%prvmth = ' '
      state%prvtrg = 0
      state%shape = 0
      state%svnrad = 0
      state%usecn = .false.
      state%uselt = .false.
      state%usestl = .false.
      state%xmit = .false.
      state%svctr1 = 0
      state%svtarg = ' '
      state%svtcde = 0
      state%svfnd1 = .false.
      state%svctr2 = 0
      state%svobsr = ' '
      state%svobsc = 0
      state%svfnd2 = .false.
      state%svctr3 = 0
      state%svfref = ' '
      state%svfxfc = 0
      state%svctr4 = 0
      state%svctr5 = 0
      state%svradi = 0.0_wp
   end subroutine init_limbpt_state

   !> The slot of a kernel pool variable: 0 if there is no such variable.
   pure integer(ip) function pool_find(self, name) result(i)
      class(pool_state), intent(in) :: self  !! the pool
      character(*), intent(in) :: name  !! the name (trailing blanks are not significant)
      logical :: found
      call self%index%get(name, i, found)
   end function pool_find

   !> The slot of a kernel pool variable, created (without values) if it
   !> does not exist.
   pure subroutine pool_create(self, name, i, existed)
      class(pool_state), intent(inout) :: self  !! the pool
      character(*), intent(in) :: name  !! the name (trailing blanks are not significant)
      integer(ip), intent(out) :: i  !! the slot of the variable
      logical, intent(out), optional :: existed  !! the variable existed already
      type(pool_variable), allocatable :: tmp(:)
      integer(ip) :: k, n, existing
      ! the slot a new variable gets, added to the index with one lookup
      n = len_trim(name)
      if (self%nfree > 0) then
         i = self%free(self%nfree)
      else
         i = self%nslots + 1
      end if
      call self%index%insert(name(1:n), i, existing)
      if (present(existed)) existed = existing > 0
      if (existing > 0) then
         i = existing
         return
      end if
      if (self%nfree > 0) then
         self%nfree = self%nfree - 1
      else
         if (.not. allocated(self%vars)) allocate (self%vars(64))
         if (self%nslots == size(self%vars)) then
            allocate (tmp(2*size(self%vars)))
            do k = 1, self%nslots
               call move_variable(self%vars(k), tmp(k))
            end do
            call move_alloc(tmp, self%vars)
         end if
         self%nslots = self%nslots + 1
         i = self%nslots
      end if
      self%vars(i)%name = name(1:n)
      self%vars(i)%kind = pool_none
      self%vars(i)%n = 0
   end subroutine pool_create

   !> Remove the variable in slot i (its slot becomes free).
   pure subroutine pool_remove(self, i)
      class(pool_state), intent(inout) :: self  !! the pool
      integer(ip), intent(in) :: i  !! the slot
      if (.not. allocated(self%vars(i)%name)) return
      call self%index%remove(self%vars(i)%name)
      self%vars(i) = pool_variable()
      call grow(self%free, self%nfree + 1)
      self%nfree = self%nfree + 1
      self%free(self%nfree) = i
   end subroutine pool_remove

   !> Remove the values of the variable in slot i (it stays, without
   !> values or type).
   pure subroutine pool_clear_values(self, i)
      class(pool_state), intent(inout) :: self  !! the pool
      integer(ip), intent(in) :: i  !! the slot
      self%vars(i)%kind = pool_none
      self%vars(i)%n = 0
      self%vars(i)%nfrom = 0
      if (allocated(self%vars(i)%dvals)) deallocate (self%vars(i)%dvals)
      if (allocated(self%vars(i)%cvals)) deallocate (self%vars(i)%cvals)
   end subroutine pool_clear_values

   !> Append a number to the variable in slot i (which becomes numeric).
   pure subroutine pool_add_number(self, i, value)
      class(pool_state), intent(inout) :: self  !! the pool
      integer(ip), intent(in) :: i  !! the slot
      real(wp), intent(in) :: value  !! the value
      associate (v => self%vars(i))
         v%kind = pool_numeric
         call grow(v%dvals, v%n + 1)
         v%n = v%n + 1
         v%dvals(v%n) = value
      end associate
   end subroutine pool_add_number

   !> Append a string to the variable in slot i (which becomes a character
   !> variable). Trailing blanks are not kept.
   pure subroutine pool_add_string(self, i, value)
      class(pool_state), intent(inout) :: self  !! the pool
      integer(ip), intent(in) :: i  !! the slot
      character(*), intent(in) :: value  !! the value
      associate (v => self%vars(i))
         v%kind = pool_character
         call grow(v%cvals, v%n + 1)
         v%n = v%n + 1
         v%cvals(v%n)%s = trim(value)
      end associate
   end subroutine pool_add_string

   !> Remove all variables (and release the memory).
   pure subroutine pool_clear(self)
      class(pool_state), intent(inout) :: self  !! the pool
      integer(ip) :: i
      ! The slots and the index keep their capacity: CLPOOL runs on every
      ! unload of a text kernel, before the others are loaded again.
      call self%index%clear(keep=.true.)
      do i = 1, self%nslots
         if (allocated(self%vars(i)%name)) deallocate (self%vars(i)%name)
         if (allocated(self%vars(i)%dvals)) deallocate (self%vars(i)%dvals)
         if (allocated(self%vars(i)%cvals)) deallocate (self%vars(i)%cvals)
         self%vars(i)%kind = pool_none
         self%vars(i)%n = 0
         self%vars(i)%nfrom = 0
      end do
      self%nslots = 0
      self%nfree = 0
      call self%source_index%clear(keep=.true.)
      self%nsources = 0
   end subroutine pool_clear

   !> Record an assignment of the variable in slot i (PEPPER): call it after
   !> the variable is created and, for `=`, its values cleared, before the
   !> assignment adds its values. The assignment's first value is the
   !> variable's next one.
   pure subroutine pool_note_source(self, i, source, line)
      class(pool_state), intent(inout) :: self  !! the pool
      integer(ip), intent(in) :: i  !! the slot
      character(*), intent(in) :: source  !! the source: a kernel file, or a tag such as (PDPOOL)
      integer(ip), intent(in) :: line  !! the line where the assignment begins (0: none)
      integer(ip) :: id, existing
      ! the source's index, added if new
      call self%source_index%insert(trim(source), self%nsources + 1, existing)
      if (existing > 0) then
         id = existing
      else
         self%nsources = self%nsources + 1
         id = self%nsources
         call grow(self%sources, id)
         self%sources(id)%s = trim(source)
      end if
      associate (v => self%vars(i))
         if (.not. allocated(v%from)) allocate (v%from(2))
         if (v%nfrom == size(v%from)) v%from = [v%from, v%from]
         v%nfrom = v%nfrom + 1
         v%from(v%nfrom) = pool_assignment(source=id, line=line, first=v%n + 1)
      end associate
   end subroutine pool_note_source

   !> Move a pool variable (its name and values without copying).
   pure subroutine move_variable(from, to)
      type(pool_variable), intent(inout) :: from  !! the variable moved (left empty)
      type(pool_variable), intent(inout) :: to  !! the variable replaced
      to%kind = from%kind
      to%n = from%n
      call move_alloc(from%name, to%name)
      call move_alloc(from%dvals, to%dvals)
      call move_alloc(from%cvals, to%cvals)
      to%nfrom = from%nfrom
      call move_alloc(from%from, to%from)
   end subroutine move_variable

   !> Set the initial values of a new [[sc01_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_sc01_state(state)
      type(sc01_state), intent(inout) :: state  !! the state
      allocate (state%scbase(8), state%intbuf(8*nivals), source=0_ip)
      allocate (state%dpbuff(1024), source=0.0_wp)
      state%cmp = ' '
      state%error = ' '
      state%cmptks = 0.0_wp
      state%cmpval = 0.0_wp
      state%partim = 0.0_wp
      state%cmpwid = 0
      state%cofbas = 0
      state%delcde = 0
      state%dpfree = 1
      state%end = 0
      state%endbas = 0
      state%ifree = 1
      state%length = 0
      state%lower = 0
      state%modbas = 0
      state%ncoeff = 0
      state%nfield = 0
      state%npart = 0
      state%offbas = 0
      state%polctr = 0
      state%prvsc = initsc
      state%strbas = 0
      state%timsys = 0
      state%pass1 = .true.
   end subroutine init_sc01_state

   !> Set the initial values of a new [[termpt_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_termpt_state(state)
      type(termpt_state), intent(inout) :: state  !! the state
      state%first = .true.
      state%loccde = 0
      state%maxrad = 0.0_wp
      state%nrad = 0
      state%nsurf = 0
      state%prvcor = ' '
      state%prvilu = 0
      state%prvloc = ' '
      state%prvmth = ' '
      state%prvtrg = 0
      state%shadow = 0
      state%shape = 0
      state%svlcod = 0
      state%trmtyp = 0
      state%uflag = .false.
      state%usecn = .false.
      state%uselt = .false.
      state%usestl = .false.
      state%svctr1 = 0
      state%svtarg = ' '
      state%svtcde = 0
      state%svfnd1 = .false.
      state%svctr2 = 0
      state%svobsr = ' '
      state%svobsc = 0
      state%svfnd2 = .false.
      state%svctr3 = 0
      state%svilum = ' '
      state%svicde = 0
      state%svfnd3 = .false.
      state%svctr4 = 0
      state%svfref = ' '
      state%svfxfc = 0
      state%svctr5 = 0
      state%svctr6 = 0
      state%svtrad = 0.0_wp
      state%svctr7 = 0
      state%svsrad = 0.0_wp
   end subroutine init_termpt_state

   !> Set the initial values of a new [[tisbod_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_tisbod_state(state)
      type(tisbod_state), intent(inout) :: state  !! the state
      state%d = 0.0_wp
      state%t = 0.0_wp
      state%j2code = 0
      state%first = .true.
      state%pulctr = 0
      state%bidlst = 0
      state%bidpol = 0
      state%bidids = 0
      state%bdcoef = 0.0_wp
      state%bpckep = 0.0_wp
      state%brcoef = 0.0_wp
      state%bwcoef = 0.0_wp
      state%bna = 0
      state%bnd = 0
      state%bnphas = 0
      state%bnphco = 0
      state%bnw = 0
      state%bpcref = 0
   end subroutine init_tisbod_state

   !> Set the initial values of a new [[tkfram_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_tkfram_state(state)
      type(tkfram_state), intent(inout) :: state  !! the state
      state%oldagt = ' '
      state%buffd = 0.0_wp
      state%at = 0
      state%buffi = 0
      state%idents = 0
      state%idnt = 0
      state%pool = 0
      state%first = .true.
   end subroutine init_tkfram_state

   !> Set the initial values of a new [[zzbodblt_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_zzbodblt_state(state)
      type(zzbodblt_state), intent(inout) :: state  !! the state
      state%bltnam = ' '
      state%bltnor = ' '
      state%bltcod = 0
      state%first = .true.
   end subroutine init_zzbodblt_state

   !> Set the initial values of a new [[zzbodtrn_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_zzbodtrn_state(state)
      type(zzbodtrn_state), intent(inout) :: state  !! the state
      state%subctr = 0
      state%pulctr = 0
      state%bodchg = .false.
      state%nodata = .true.
      state%i = 0
      state%first = .true.
      state%boddef_epoch = 0
      state%boddef_count = 0
   end subroutine init_zzbodtrn_state

   !> Remove the kernel pool names.
   pure subroutine body_names_clear(self)
      class(body_names), intent(inout) :: self  !! the names
      self%nvals = 0
      self%extker = .false.
      if (allocated(self%names)) deallocate (self%names)
      if (allocated(self%nornam)) deallocate (self%nornam)
      if (allocated(self%codes)) deallocate (self%codes)
      call self%name_map%clear()
      call self%code_map%clear()
   end subroutine body_names_clear

   !> Append a name and code (the maps are not updated: see index).
   pure subroutine body_names_append(self, name, nornam, code)
      class(body_names), intent(inout) :: self  !! the names
      character(*), intent(in) :: name  !! the name
      character(*), intent(in) :: nornam  !! the name, normalized
      integer(ip), intent(in) :: code  !! the code
      self%nvals = self%nvals + 1
      call grow(self%names, self%nvals)
      call grow(self%nornam, self%nvals)
      call grow(self%codes, self%nvals)
      self%names(self%nvals)%s = name
      self%nornam(self%nvals)%s = nornam
      self%codes(self%nvals) = code
   end subroutine body_names_append

   !> Remove the name and code i, keeping the order of the others (the
   !> maps are not updated: see index).
   pure subroutine body_names_remove(self, i)
      class(body_names), intent(inout) :: self  !! the names
      integer(ip), intent(in) :: i  !! the index
      integer(ip) :: k
      do k = i + 1, self%nvals
         call move_alloc(self%names(k)%s, self%names(k - 1)%s)
         call move_alloc(self%nornam(k)%s, self%nornam(k - 1)%s)
         self%codes(k - 1) = self%codes(k)
      end do
      self%nvals = self%nvals - 1
   end subroutine body_names_remove

   !> Build the maps from the names and codes (SPICELIB's [[zzbodini]] order:
   !> from the last assignment to the first, a name is added if it is new,
   !> and then its code if the code is new).
   pure subroutine body_names_index(self)
      class(body_names), intent(inout) :: self  !! the names
      integer(ip) :: i, k
      logical :: found
      call self%name_map%clear()
      call self%code_map%clear()
      do i = self%nvals, 1, -1
         call self%name_map%get(self%nornam(i)%s, k, found)
         if (found) cycle
         call self%name_map%set(self%nornam(i)%s, i)
         call self%code_map%get(self%codes(i), k, found)
         if (.not. found) call self%code_map%set(self%codes(i), i)
      end do
   end subroutine body_names_index

   !> Add the last name and code to the maps, when its name was not
   !> assigned before (the result of index, in O(1)): the last assignment
   !> is the one its name and its code map to.
   pure subroutine body_names_index_last(self)
      class(body_names), intent(inout) :: self  !! the names
      call self%name_map%set(self%nornam(self%nvals)%s, self%nvals)
      call self%code_map%set(self%codes(self%nvals), self%nvals)
   end subroutine body_names_index_last

   !> The index of a normalized name: 0 if it is not assigned.
   pure integer(ip) function body_names_name_index(self, nornam) result(i)
      class(body_names), intent(in) :: self  !! the names
      character(*), intent(in) :: nornam  !! the normalized name
      logical :: found
      call self%name_map%get(nornam, i, found)
   end function body_names_name_index

   !> The index of the assignment a code maps to: 0 if none.
   pure integer(ip) function body_names_code_index(self, code) result(i)
      class(body_names), intent(in) :: self  !! the names
      integer(ip), intent(in) :: code  !! the code
      logical :: found
      call self%code_map%get(code, i, found)
   end function body_names_code_index

   !> Set the initial values of a new [[zzddhman_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   !> The index of the file with a handle in the file table: 0 if no
   !> file has this handle.
   pure integer(ip) function ddh_find(self, handle) result(i)
      class(zzddhman_state), intent(in) :: self  !! the file table
      integer(ip), intent(in) :: handle  !! the handle
      integer(ip) :: lo, hi, key, k
      key = abs(handle)
      lo = 1
      hi = self%nft
      i = 0
      do while (lo <= hi)
         k = lo + (hi - lo)/2
         if (abs(self%files(k)%handle) < key) then
            lo = k + 1
         else if (abs(self%files(k)%handle) > key) then
            hi = k - 1
         else
            if (self%files(k)%handle == handle) i = k
            return
         end if
      end do
   end function ddh_find

   !> Add a file at the end of the file table (its handle must be larger
   !> in absolute value than those in the table).
   pure subroutine ddh_add(self, file)
      class(zzddhman_state), intent(inout) :: self  !! the file table
      type(ddh_file), intent(in) :: file  !! the file
      type(ddh_file), allocatable :: tmp(:)
      integer(ip) :: k
      if (.not. allocated(self%files)) allocate (self%files(8))
      if (self%nft == size(self%files)) then
         allocate (tmp(2*size(self%files)))
         do k = 1, self%nft
            call move_file(self%files(k), tmp(k))
         end do
         call move_alloc(tmp, self%files)
      end if
      self%nft = self%nft + 1
      self%files(self%nft) = file
      self%changes = self%changes + 1
   end subroutine ddh_add

   !> Remove file i from the file table, keeping the order of the others.
   pure subroutine ddh_remove(self, i)
      class(zzddhman_state), intent(inout) :: self  !! the file table
      integer(ip), intent(in) :: i  !! the index of the file
      integer(ip) :: k
      do k = i + 1, self%nft
         call move_file(self%files(k), self%files(k - 1))
      end do
      self%files(self%nft) = ddh_file()
      self%nft = self%nft - 1
      self%changes = self%changes + 1
   end subroutine ddh_remove

   !> True if [[zzddhf2h]] kept its result for the file `name` (not in the
   !> table) during the current load, and the table has not changed since.
   pure logical function ddh_kept_f2h(self, name) result(kept)
      class(zzddhman_state), intent(in) :: self  !! the file table
      character(*), intent(in) :: name  !! the file name
      kept = .false.
      if (.not. self%reuse_f2h .or. .not. allocated(self%f2h_name)) return
      kept = self%f2h_changes == self%changes .and. self%f2h_name == name
   end function ddh_kept_f2h

   !> Move a file record (its name without copying).
   pure subroutine move_file(from, to)
      type(ddh_file), intent(inout) :: from  !! the record moved (its name is deallocated)
      type(ddh_file), intent(inout) :: to  !! the record replaced
      to%handle = from%handle
      to%method = from%method
      to%arch = from%arch
      to%bff = from%bff
      to%mnm = from%mnm
      call move_alloc(from%name, to%name)
      call move_alloc(from%image, to%image)
      to%synthetic = from%synthetic
      call move_alloc(from%raw_records, to%raw_records)
   end subroutine move_file

   !> Set the initial values of a new [[zzdsksbf_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_zzdsksbf_state(state)
      type(zzdsksbf_state), intent(inout) :: state  !! the state
      state%bsrctr = 0
      state%btbody = 0
      state%btnbod = 0
      state%btsegp = 0
      state%btstsz = 0
      state%first = .true.
      state%stfree = 1
   end subroutine init_zzdsksbf_state

   !> Set the initial values of a new [[zzekjtst_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_zzekjtst_state(state)
      type(zzekjtst_state), intent(inout) :: state  !! the state
      state%case = 0
      state%cnstr = 0
      state%lbase = 0
      state%lcur = 0
      state%lelt = 0
      state%lovbas = 0
      state%lptr = 0
      state%rbase = 0
      state%relt = 0
      state%rovbas = 0
      state%rptr = 0
      state%svbas1 = 0
      state%svbas2 = 0
      state%svncon = 0
      state%svnr1 = 0
      state%svnr2 = 0
      state%svnt1 = 0
      state%svnt2 = 0
      state%svrb1 = 0
      state%svrb2 = 0
      state%done = .false.
      state%lsmall = .false.
   end subroutine init_zzekjtst_state

   !> Set the initial values of a new [[zzeksca_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_zzeksca_state(state)
      type(zzeksca_state), intent(inout) :: state  !! the state
      if (.not. allocated(state%scrtch)) allocate (state%scrtch(0))
   end subroutine init_zzeksca_state

   !> Set the initial values of a new [[zzelvupy_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_zzelvupy_state(state)
      type(zzelvupy_state), intent(inout) :: state  !! the state
      state%origin = 0.0_wp
   end subroutine init_zzelvupy_state

   !> Set the initial values of a new [[zzgffvu_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_zzgffvu_state(state)
      type(zzgffvu_state), intent(inout) :: state  !! the state
      state%svarad = 0.0_wp
      state%svcorr = ' '
      state%svedct = 0.0_wp
      state%svetrg = .false.
      state%svfaxi = 0.0_wp
      state%svfovm = 0.0_wp
      state%svfsmx = 0.0_wp
      state%svfvct = 0.0_wp
      state%svifrm = ' '
      state%svinam = ' '
      state%svishp = ' '
      state%svnvrt = 0
      state%svobs = 0
      state%svorig = 0.0_wp
      state%svplan = 0.0_wp
      state%svrdir = 0.0_wp
      state%svsemi = 0.0_wp
      state%svtarg = 0
      state%svtfrm = ' '
      state%svtnam = ' '
      state%svtrad = 0.0_wp
      state%svuray = .false.
      state%svustl = .false.
      state%svxmag = 0.0_wp
      state%svxmit = .false.
      state%svifrm = ' '
      state%svtfrm = ' '
   end subroutine init_zzgffvu_state

   !> Remove the surface names.
   pure subroutine surface_names_clear(self)
      class(surface_names), intent(inout) :: self  !! the names
      self%n = 0
      self%extker = .false.
      if (allocated(self%names)) deallocate (self%names)
      if (allocated(self%nornam)) deallocate (self%nornam)
      if (allocated(self%ids)) deallocate (self%ids)
      if (allocated(self%bodies)) deallocate (self%bodies)
      call self%name_map%clear()
      call self%id_map%clear()
   end subroutine surface_names_clear

   !> The length of id_text's result.
   pure integer function id_text_length(id) result(n)
      integer(ip), intent(in) :: id  !! the ID
      character(12) :: b
      write (b, '(i0)') id
      n = len_trim(b)
   end function id_text_length

   !> The key of a (normalized name or ID, body) pair in the maps.
   pure function surface_key(item, body) result(key)
      character(*), intent(in) :: item  !! the normalized name, or the ID as text
      integer(ip), intent(in) :: body  !! the body ID code
      character(id_text_length(body) + 1 + len_trim(item)) :: key
      key = id_text(body) // achar(0) // trim(item)
   end function surface_key

   !> The ID as text, for surface_key. (Not a deferred-length result:
   !> gfortran keeps that length in a static variable at the call site,
   !> which is not thread-safe; see text_length in [[pepper_instance_module]].)
   pure function id_text(id) result(t)
      integer(ip), intent(in) :: id  !! the ID
      character(id_text_length(id)) :: t
      character(12) :: b
      write (b, '(i0)') id
      t = b
   end function id_text

   !> Build the maps (SPICELIB's ZZSRFINI order: from the last assignment
   !> to the first, a (name, body) is added if it is new, and then its
   !> (ID, body) if that is new).
   pure subroutine surface_names_index(self)
      class(surface_names), intent(inout) :: self  !! the names
      integer(ip) :: i, k
      logical :: found
      call self%name_map%clear()
      call self%id_map%clear()
      do i = self%n, 1, -1
         call self%name_map%get(surface_key(self%nornam(i)%s, self%bodies(i)), k, found)
         if (found) cycle
         call self%name_map%set(surface_key(self%nornam(i)%s, self%bodies(i)), i)
         call self%id_map%get(surface_key(id_text(self%ids(i)), self%bodies(i)), k, found)
         if (.not. found) call self%id_map%set(surface_key(id_text(self%ids(i)), self%bodies(i)), i)
      end do
   end subroutine surface_names_index

   !> The index of a (normalized name, body): 0 if it is not assigned.
   pure integer(ip) function surface_names_name_index(self, nornam, body) result(i)
      class(surface_names), intent(in) :: self  !! the names
      character(*), intent(in) :: nornam  !! the normalized surface name
      integer(ip), intent(in) :: body  !! the body ID code
      logical :: found
      call self%name_map%get(surface_key(nornam, body), i, found)
   end function surface_names_name_index

   !> The index of the assignment a (surface ID, body) maps to: 0 if none.
   pure integer(ip) function surface_names_id_index(self, id, body) result(i)
      class(surface_names), intent(in) :: self  !! the names
      integer(ip), intent(in) :: id  !! the surface ID
      integer(ip), intent(in) :: body  !! the body ID code
      logical :: found
      call self%id_map%get(surface_key(id_text(id), body), i, found)
   end function surface_names_id_index

   !> Set the initial values of a new [[zzswfxfm_state]]: SPICELIB's DATA
   !> values, and zero for the other components.
   pure subroutine init_zzswfxfm_state(state)
      type(zzswfxfm_state), intent(inout) :: state  !! the state
      state%basbeg = 0
      state%bascnt = 0
      allocate (state%baslst(64), state%clsids(64), state%clsses(64), source=0_ip)
      state%binary = .false.
      state%fidlst = 0
      state%free = 1
      state%frpool = 0
      state%hdfram = 0
      state%pass1 = .true.
      state%polctr = 0
      state%prvat = 0
      state%prvfrm = 0
      allocate (state%starts(64), state%stops(64), source=0.0_wp)
      state%usetim = .false.
      state%xident = 0.0_wp
   end subroutine init_zzswfxfm_state

end module pepper_states
