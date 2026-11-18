!> The legacy interface's routines of the subsystem geometry (205 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_geometry

   use pepper_instance_module, only: pepper_context, pepper_zzasryel => zzasryel, &
      pepper_zzbquad => zzbquad, pepper_zzchrlat => zzchrlat, pepper_zzcputim => zzcputim, &
      pepper_zzcxbrut => zzcxbrut, pepper_zzdiv => zzdiv, pepper_zzdskbbl => zzdskbbl, &
      pepper_zzdsksbi => zzdsksbi, pepper_zzdsksbr => zzdsksbr, pepper_zzdsksbd => zzdsksbd, &
      pepper_zzdskbdc => zzdskbdc, pepper_zzdsknot => zzdsknot, pepper_zzdsksit => zzdsksit, &
      pepper_zzdskcit => zzdskcit, pepper_zzdskusc => zzdskusc, pepper_zzdskumc => zzdskumc, &
      pepper_zzdskmsc => zzdskmsc, pepper_zzdskmmc => zzdskmmc, pepper_zzdsksrc => zzdsksrc, &
      pepper_zzdskmrc => zzdskmrc, pepper_zzdsksgr => zzdsksgr, pepper_zzedterm => zzedterm, &
      pepper_zzedtmpt => zzedtmpt, pepper_zzellbds => zzellbds, pepper_zzelnaxx => zzelnaxx, &
      pepper_zzeprcss => zzeprcss, pepper_zzgetelm => zzgetelm, pepper_zzgrav => zzgrav, &
      pepper_zzholdd => zzholdd, pepper_zzhullax => zzhullax, pepper_zzingrd => zzingrd, &
      pepper_zzinrec => zzinrec, pepper_zzinrypl => zzinrypl, pepper_zzlatbox => zzlatbox, &
      pepper_zzlexmet => zzlexmet, pepper_zzmult => zzmult, pepper_zznrmlon => zznrmlon, &
      pepper_zzrecbox => zzrecbox, pepper_zzrtnmat => zzrtnmat, pepper_zzryxsph => zzryxsph, &
      pepper_zzsecprt => zzsecprt, pepper_zzsepq => zzsepq, pepper_zzsfxcor => zzsfxcor, &
      pepper_zzsglatx => zzsglatx, pepper_zzspin => zzspin, pepper_zztanslv => zztanslv, &
      pepper_zztogrid => zztogrid, pepper_zzwind => zzwind, pepper_zzwind2d => zzwind2d, &
      pepper_zzcnquad => zzcnquad, pepper_zzdsksph => zzdsksph, pepper_zzfovaxi => zzfovaxi, &
      pepper_zzinlat0 => zzinlat0, pepper_zzdpinit => zzdpinit, pepper_zzdpsec => zzdpsec, &
      pepper_zzdpper => zzdpper, pepper_zzpdcmpl => zzpdcmpl, pepper_zzpdpltc => zzpdpltc, &
      pepper_zzpdtbox => zzpdtbox, pepper_zzraybox => zzraybox, pepper_zzsrfker => zzsrfker, &
      pepper_zzilusta => zzilusta, pepper_zzinlat => zzinlat, pepper_zzinpdt0 => zzinpdt0, &
      pepper_zzrytrec => zzrytrec, pepper_zzsegbox => zzsegbox, pepper_zzsrfn2c => zzsrfn2c, &
      pepper_zzsrfc2n => zzsrfc2n, pepper_zzsrftrk => zzsrftrk, pepper_zzdsksba => zzdsksba, &
      pepper_zzelvupy => zzelvupy, pepper_zzinpdt => zzinpdt, pepper_zzrytlat => zzrytlat, &
      pepper_zzinvelt => zzinvelt, pepper_zzprsmet => zzprsmet, pepper_zzrytpdt => zzrytpdt, &
      pepper_zzocced => zzocced, pepper_zzptpl02 => zzptpl02, pepper_zzrytelt => zzrytelt, &
      pepper_zzdskbun => zzdskbun, pepper_zzdsksgx => zzdsksgx, pepper_zzdskbux => zzdskbux, &
      pepper_zzsbfxr => zzsbfxr, pepper_zzsbfxri => zzsbfxri, pepper_zzsbfnrm => zzsbfnrm, &
      pepper_zzsuelin => zzsuelin, pepper_zzsudski => zzsudski, pepper_zzraysfx => zzraysfx, &
      pepper_zzmaxrad => zzmaxrad, pepper_zzminrad => zzminrad, pepper_zzraynp => zzraynp, &
      pepper_zztanini => zztanini, pepper_zztansta => zztansta, pepper_zztangnt => zztangnt
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_legacy_callbacks
   use pepper_bounds, only: lbcell, ubpl, ubel

   implicit none

   private
   public :: gfstep, gfsstp, gfrefn, ana, azlcpo, bltfrm, bodmat, conics, delfil, dgeodr, dhfa, &
      diags2, drotat, dxtrct, ev2lin, evsgp4, fetchc, fetchd, fetchi, fn2lun, georec, getfov, &
      hyptof, illum, irftrn, iso2utc, isopen, kplfrm, lparss, lspcn, ltime, lun2fn, lx4sgn, &
      lxidnt, lxdfid, lxcsid, npelpt, oscelt, partof, plnsns, prompt, prsdp, prtenc, prtdec, &
      pxfrm2, rdnbl, repmct, repml, rotvec, scps01, scn2id, scid2n, sphsd, spkpv, srfrec, surfpt, &
      twovxf, uddf, udf, zzasryel, zzbquad, zzchrlat, zzcputim, zzcxbrut, zzdiv, zzdskbbl, &
      zzdsksbi, zzdsksbr, zzdsksbd, zzdskbdc, zzdsknot, zzdsksit, zzdskcit, zzdskusc, zzdskumc, &
      zzdskmsc, zzdskmmc, zzdsksrc, zzdskmrc, zzdsksgr, zzedterm, zzedtmpt, zzellbds, zzelnaxx, &
      zzeprcss, zzgetelm, zzgrav, zzholdd, zzhullax, zzingrd, zzinrec, zzinrypl, zzlatbox, &
      zzlexmet, zzmult, zznrmlon, zzrecbox, zzrtnmat, zzryxsph, zzsecprt, zzsepq, zzsfxcor, &
      zzsglatx, zzspin, zzspkap0, zzspkap1, zztanslv, zztogrid, zzwind, zzwind2d, dpgrdr, drdpgr, &
      dvsep, edterm, getelm, getfvn, lx4dec, oscltx, pgrrec, recpgr, saelgv, scpars, subpt, &
      subsol, surfpv, tostdo, trgsep, uddc, wrkvar, xdda, zzcnquad, zzdsksph, zzfovaxi, zzinlat0, &
      zzdpinit, zzdpsec, zzdpper, zzpdcmpl, zzpdpltc, zzpdtbox, zzraybox, zzsrfker, cgv2el, &
      dpspce, et2lst, incnsg, lx4num, rdkvar, xfmsta, zzilusta, zzinlat, zzinpdt0, zzrytrec, &
      zzsegbox, zzsrfn2c, zzsrfc2n, zzsrftrk, inedpl, pjelpl, srfc2s, srfcss, srfs2c, srfscc, &
      zzdsksba, zzelvupy, zzinpdt, zzrytlat, edlimb, npedln, zzinvelt, zzprsmet, zzrytpdt, dskx02, &
      srfxpt, tangpt, zzocced, zzptpl02, zzrytelt, zzdskbun, zzdsksgx, zzdskbux, zzsbfxr, &
      zzsbfxri, zzsbfnrm, dskxsi, dskxv, illumg, srfnrm, zzsuelin, zzsudski, zzraysfx, zzmaxrad, &
      zzminrad, zzraynp, illumf, ilumin, latsrf, sincpt, subpnt, subslr, zztanini, zztansta, &
      zztangnt, limbpt, termpt

contains

      !> Return the time step set by the most recent call to GFSSTP.
      !>
      !> Legacy interface: calls ctx%gfstep with the legacy context.
      subroutine gfstep ( time, step )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: time  !! Ignored ET value.
      real(wp) :: step  !! Time step to take.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfstep ( time, step )
      end subroutine gfstep

      !> Set the step size to be returned by GFSTEP.
      !>
      !> Legacy interface: calls ctx%gfsstp with the legacy context.
      subroutine gfsstp ( step )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: step  !! Time step to take.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfsstp ( step )
      end subroutine gfsstp

      !> Estimate, using a bisection method, the next abscissa value at
      !> which a state change occurs. This is the default GF refinement
      !> method.
      !>
      !> Legacy interface: calls ctx%gfrefn with the legacy context.
      subroutine gfrefn ( t1, t2, s1, s2, t )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: t1  !! One of two values bracketing a state change.
      real(wp) :: t2  !! The other value that brackets a state change.
      logical :: s1  !! State at T1.
      logical :: s2  !! State at T2.
      real(wp) :: t  !! New value at which to check for transition.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gfrefn ( t1, t2, s1, s2, t )
      end subroutine gfrefn

      !> Return the correct article "a" or "an" used to modify a word
      !> and return it capitalized, lower case, or upper case.
      !>
      !> Legacy interface: calls ctx%ana with the legacy context.
      character(2) function ana ( word, case )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: word  !! is a word that should be modified by "a" or "an".
      character(*) :: case  !! 'U', 'L', or 'C' to specify capitalization of ANA.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ana = ctx%ana ( word, case )
      end function ana

      !> Return the azimuth/elevation coordinates of a specified target
      !> relative to an "observer," where the observer has constant
      !> position in a specified reference frame. The observer's position
      !> is provided by the calling program rather than by loaded SPK
      !> files.
      !>
      !> Legacy interface: calls ctx%azlcpo with the legacy context.
      subroutine azlcpo ( method, target, et, abcorr, azccw, elplsz, obspos, obsctr, obsref, azlsta, lt )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: method  !! Method to obtain the surface normal vector.
      character(*) :: target  !! Name of target ephemeris object.
      real(wp) :: et  !! Observation epoch.
      character(*) :: abcorr  !! Aberration correction.
      logical :: azccw  !! Flag indicating how azimuth is measured.
      logical :: elplsz  !! Flag indicating how elevation is measured.
      real(wp) :: obspos ( 3 )  !! Observer position relative to center of motion.
      character(*) :: obsctr  !! Center of motion of observer.
      character(*) :: obsref  !! Body-fixed, body-centered frame of observer's center.
      real(wp) :: azlsta ( 6 )  !! State of target with respect to observer, in azimuth/elevation coordinates.
      real(wp) :: lt  !! One way light time between target and observer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: obspos is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%azlcpo ( method, target, et, abcorr, azccw, elplsz, (obspos), obsctr, obsref, &
         azlsta, lt )
      end subroutine azlcpo

      !> Return a set containing the frame IDs of all built-in frames of a
      !> specified class.
      !>
      !> Legacy interface: calls ctx%bltfrm with the legacy context.
      subroutine bltfrm ( frmcls, idset )
      use pepper_kinds, only: ip
      use pepper_inc_frmtyp
      use pepper_inc_ninert
      use pepper_inc_nninrt
      implicit none
      integer(ip) :: frmcls  !! Frame class.
      integer(ip) :: idset  ( lbcell : * )  !! Set of ID codes of frames of the specified class.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%bltfrm ( frmcls, idset )
      end subroutine bltfrm

      !> Return the J2000 to body Equator and Prime Meridian coordinate
      !> transformation matrix for a specified body.
      !>
      !> Legacy interface: calls ctx%bodmat with the legacy context.
      subroutine bodmat ( body, et, tipm )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: body  !! ID code of body.
      real(wp) :: et  !! Epoch of transformation.
      real(wp) :: tipm   ( 3,3 )  !! Transformation from Inertial to PM for BODY at ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%bodmat ( body, et, tipm )
      end subroutine bodmat

      !> Determine the state (position, velocity) of an orbiting body
      !> from a set of elliptic, hyperbolic, or parabolic orbital
      !> elements.
      !>
      !> Legacy interface: calls ctx%conics with the legacy context.
      subroutine conics ( elts, et, state )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: elts    ( 8 )  !! Conic elements.
      real(wp) :: et  !! Input time.
      real(wp) :: state   ( 6 )  !! State of orbiting body at ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: elts is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%conics ( (elts), et, state )
      end subroutine conics

      !> Delete a file.
      !>
      !> Legacy interface: calls ctx%delfil with the legacy context.
      subroutine delfil ( filnam )
      use pepper_kinds, only: ip
      character(*) :: filnam  !! The name of a file to be deleted.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%delfil ( filnam )
      end subroutine delfil

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to geodetic coordinates.
      !>
      !> Legacy interface: calls ctx%dgeodr with the legacy context.
      subroutine dgeodr ( x, y, z, re, f, jacobi )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! X-coordinate of point.
      real(wp) :: y  !! Y-coordinate of point.
      real(wp) :: z  !! Z-coordinate of point.
      real(wp) :: re  !! Equatorial radius of the reference spheroid.
      real(wp) :: f  !! Flattening coefficient.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dgeodr ( x, y, z, re, f, jacobi )
      end subroutine dgeodr

      !> Calculate the value of the time derivative of the
      !> half angle of a spherical body given a state vector
      !> STATE and body radius BODYR.
      !>
      !> Legacy interface: calls ctx%dhfa with the legacy context.
      real(wp) function dhfa ( state, bodyr)
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: state (6)  !! SPICE state vector
      real(wp) :: bodyr  !! Radius of body
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      dhfa = ctx%dhfa ( state, bodyr )
      end function dhfa

      !> Diagonalize a symmetric 2x2 matrix.
      !>
      !> Legacy interface: calls ctx%diags2 with the legacy context.
      subroutine diags2 ( symmat, diag, rotate )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: symmat ( 2, 2 )  !! A symmetric 2x2 matrix.
      real(wp) :: diag   ( 2, 2 )  !! A diagonal matrix similar to SYMMAT.
      real(wp) :: rotate ( 2, 2 )  !! A rotation used as the similarity transformation.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: symmat is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%diags2 ( (symmat), diag, rotate )
      end subroutine diags2

      !> Calculate the derivative with respect to the angle of rotation
      !> of a 3x3 coordinate system rotation matrix generated by a
      !> rotation of a specified angle about a specified axis.
      !>
      !> Legacy interface: calls ctx%drotat with the legacy context.
      subroutine drotat ( angle, iaxis, dmout )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: angle  !! Angle of rotation (radians).
      integer(ip) :: iaxis  !! Coordinate axis number (X=1, Y=2, Z=3).
      real(wp) :: dmout  ( 3,3 )  !! Derivative of rotation matrix [ANGLE] w.r.t. angle IAXIS
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%drotat ( angle, iaxis, dmout )
      end subroutine drotat

      !> Locate a keyword and succeeding numeric words within a string.
      !> Parse and store the numeric words. Remove the keyword and
      !> numeric words from the input string.
      !>
      !> Legacy interface: calls ctx%dxtrct with the legacy context.
      subroutine dxtrct ( keywd,   maxwds ,  string, &
                          nfound,  parsed,   values   )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: keywd  !! Keyword used to mark start of a set of numbers.
      integer(ip) :: maxwds  !! Maximum number of numeric words that can be parsed
      character(*) :: string  !! String potentially containing KEYWD and numbers.
      integer(ip) :: nfound  !! Number of numeric words found following the KEYWD.
      integer(ip) :: parsed  !! Number of numeric words translated and returned.
      real(wp) :: values ( * )  !! The double precision values for the numbers.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: keywd is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%dxtrct ( (keywd), maxwds, string, nfound, parsed, values )
      end subroutine dxtrct

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine EVSGP4. *ALL* TLE evaluations should use that routine.
      !> NAIF supports EV2LIN only for backward compatibility.
      !>
      !> Legacy interface: calls ctx%ev2lin with the legacy context.
      subroutine ev2lin ( et, geophs, elems, state )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Epoch in seconds past ephemeris epoch J2000.
      real(wp) :: geophs ( * )  !! Geophysical constants
      real(wp) :: elems  ( * )  !! Two-line element data
      real(wp) :: state  ( * )  !! Evaluated state
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ev2lin ( et, geophs, elems, state )
      end subroutine ev2lin

      !> Evaluate NORAD two-line element data for earth orbiting
      !> spacecraft. This evaluator uses algorithms as described
      !> in Vallado 2006 [4].
      !>
      !> Legacy interface: calls ctx%evsgp4 with the legacy context.
      subroutine evsgp4 ( et, geophs, elems, state )
      use pepper_kinds, only: wp
      use pepper_inc_zzsgp4
      implicit none
      real(wp) :: et  !! Epoch in seconds past ephemeris epoch J2000.
      real(wp) :: geophs (  8 )  !! Geophysical constants
      real(wp) :: elems  ( 10 )  !! Two-line element data
      real(wp) :: state  (  6 )  !! Evaluated state
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: geophs, elems are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%evsgp4 ( et, (geophs), (elems), state )
      end subroutine evsgp4

      !> Return the location within the set array of the NTH element within
      !> the order imposed by the ASCII collating sequence.
      !>
      !> Legacy interface: calls ctx%fetchc with the legacy context.
      integer(ip) function fetchc ( nth, set )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: nth  !! Index of a particular element.
      character(*) :: set      ( lbcell:* )  !! Input set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      fetchc = ctx%fetchc ( nth, set )
      end function fetchc

      !> Return the location within the set array of the NTH element
      !> within the order imposed by the values of the elements.
      !>
      !> Legacy interface: calls ctx%fetchd with the legacy context.
      integer(ip) function fetchd ( nth, set )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nth  !! Index of a particular element.
      real(wp) :: set      ( lbcell:* )  !! Input set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      fetchd = ctx%fetchd ( nth, set )
      end function fetchd

      !> Return the location within the set array of the NTH element
      !> within the order imposed by the values of the elements.
      !>
      !> Legacy interface: calls ctx%fetchi with the legacy context.
      integer(ip) function fetchi ( nth, set )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: nth  !! Index of a particular element.
      integer(ip) :: set      ( lbcell:* )  !! Input set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      fetchi = ctx%fetchi ( nth, set )
      end function fetchi

      !> Map the name of an open file to its associated logical unit.
      !>
      !> Legacy interface: calls ctx%fn2lun with the legacy context.
      subroutine fn2lun ( filnam, lunit )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: filnam  !! Name of the file to be mapped to its logical unit.
      integer(ip) :: lunit  !! The logical unit associated with the filename.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%fn2lun ( filnam, lunit )
      end subroutine fn2lun

      !> Convert geodetic coordinates to rectangular coordinates.
      !>
      !> Legacy interface: calls ctx%georec with the legacy context.
      subroutine georec ( lon, lat, alt,  re, f,  rectan )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: lon  !! Geodetic longitude of point (radians).
      real(wp) :: lat  !! Geodetic latitude  of point (radians).
      real(wp) :: alt  !! Altitude of point above the reference spheroid.
      real(wp) :: re  !! Equatorial radius of the reference spheroid.
      real(wp) :: f  !! Flattening coefficient.
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%georec ( lon, lat, alt, re, f, rectan )
      end subroutine georec

      !> Return the field-of-view (FOV) parameters for a specified
      !> instrument. The instrument is specified by its NAIF ID code.
      !>
      !> Legacy interface: calls ctx%getfov with the legacy context.
      subroutine getfov ( instid, &
                          room, &
                          shape, &
                          frame, &
                          bsight, &
                          n, &
                          bounds  )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: instid  !! NAIF ID of an instrument.
      integer(ip) :: room  !! Maximum number of vectors that can be returned.
      character(*) :: shape  !! Instrument FOV shape.
      character(*) :: frame  !! Name of the frame in which FOV vectors are defined.
      real(wp) :: bsight ( 3    )  !! Boresight vector.
      integer(ip) :: n  !! Number of boundary vectors returned.
      real(wp) :: bounds ( 3, * )  !! FOV boundary vectors.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%getfov ( instid, room, shape, frame, bsight, n, bounds )
      end subroutine getfov

      !> Solve the time of flight equation MA = e sinh(F) - F for the
      !> hyperbolic eccentric anomaly F, given the mean anomaly, MA,
      !> and the eccentricity, e.
      !>
      !> Legacy interface: calls ctx%hyptof with the legacy context.
      subroutine hyptof ( ma, ecc, f )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: ma  !! Mean anomaly at epoch.
      real(wp) :: ecc  !! Eccentricity.
      real(wp) :: f  !! Hyperbolic eccentric anomaly.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%hyptof ( ma, ecc, f )
      end subroutine hyptof

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine ILUMIN. This routine is supported for purposes of
      !> backward compatibility only.
      !>
      !> Legacy interface: calls ctx%illum with the legacy context.
      subroutine illum ( target, et, abcorr, obsrvr, spoint, phase, solar, emissn )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000.
      character(*) :: abcorr  !! Desired aberration correction.
      character(*) :: obsrvr  !! Name of observing body.
      real(wp) :: spoint ( 3 )  !! Body-fixed coordinates of a target surface point.
      real(wp) :: phase  !! Phase angle at the surface point.
      real(wp) :: solar  !! Solar incidence angle at the surface point.
      real(wp) :: emissn  !! Emission angle at the surface point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: spoint is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%illum ( target, et, abcorr, obsrvr, (spoint), phase, solar, emissn )
      end subroutine illum

      !> Return the matrix that transforms vectors from one specified
      !> inertial reference frame to another.
      !>
      !> Legacy interface: calls ctx%irftrn with the legacy context.
      subroutine irftrn ( refa, refb, rotab )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: refa  !! Name of reference frame to transform vectors FROM.
      character(*) :: refb  !! Name of reference frame to transform vectors TO.
      real(wp) :: rotab ( 3, 3 )  !! REFA-to-REFB transformation matrix.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%irftrn ( refa, refb, rotab )
      end subroutine irftrn

      !> **Deprecated:** This routine is deprecated because all high-level
      !> time conversion routines (STR2ET, UTC2ET, TPARSE) were updated
      !> to accept ISO formatted times on input.  This routine is
      !> supported for purposes of backward compatibility only.
      !>
      !> Legacy interface: calls ctx%iso2utc with the legacy context.
      subroutine iso2utc ( tstrng, utcstr, error )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: tstrng  !! String representing a calendar or julian date epoch
      character(*) :: utcstr  !! SPICELIB UTC string corresponding to TSTRNG
      character(*) :: error  !! Error message if something went wrong.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: tstrng is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%iso2utc ( (tstrng), utcstr, error )
      end subroutine iso2utc

      !> Determine whether a named file is currently open.
      !>
      !> Legacy interface: calls ctx%isopen with the legacy context.
      logical function isopen ( file )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: file  !! Name of the file in question.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      isopen = ctx%isopen ( file )
      end function isopen

      !> Return a SPICE set containing the frame IDs of all reference
      !> frames of a given class having specifications in the kernel pool.
      !>
      !> Legacy interface: calls ctx%kplfrm with the legacy context.
      subroutine kplfrm ( frmcls, idset )
      use pepper_kinds, only: ip
      use pepper_inc_frmtyp
      use pepper_inc_ninert
      use pepper_inc_nninrt
      implicit none
      integer(ip) :: frmcls  !! Frame class.
      integer(ip) :: idset  ( lbcell : * )  !! Set of ID codes of frames of the specified class.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%kplfrm ( frmcls, idset )
      end subroutine kplfrm

      !> Parse a list of items delimited by multiple delimiters,
      !> placing the resulting items into a set.
      !>
      !> Legacy interface: calls ctx%lparss with the legacy context.
      subroutine lparss ( list, delims, set )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: list  !! List of items delimited by DELIMS on input.
      character(*) :: delims  !! Single characters which delimit items.
      character(*) :: set  ( lbcell:* )  !! Items in the list, validated, left justified.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: list, delims are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%lparss ( (list), (delims), set )
      end subroutine lparss

      !> Compute L_s, the planetocentric longitude of the sun, as seen
      !> from a specified body.
      !>
      !> Legacy interface: calls ctx%lspcn with the legacy context.
      real(wp) function lspcn ( body, et, abcorr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: body  !! Name of central body.
      real(wp) :: et  !! Epoch in seconds past J2000 TDB.
      character(*) :: abcorr  !! Aberration correction.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      lspcn = ctx%lspcn ( body, et, abcorr )
      end function lspcn

      !> Compute the transmission (or reception) time of a signal at a
      !> specified target, given the reception (or transmission) time at a
      !> specified observer. Also return the elapsed time between
      !> transmission and reception.
      !>
      !> Legacy interface: calls ctx%ltime with the legacy context.
      subroutine ltime ( etobs, obs, dir, targ, ettarg, elapsd )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: etobs  !! Epoch of a signal at some observer.
      integer(ip) :: obs  !! NAIF ID of some observer.
      character(2) :: dir  !! Direction the signal travels ( '->' or '<-' ).
      integer(ip) :: targ  !! NAIF ID of the target object.
      real(wp) :: ettarg  !! Epoch of the signal at the target.
      real(wp) :: elapsd  !! Time between transmit and receipt of the signal.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ltime ( etobs, obs, dir, targ, ettarg, elapsd )
      end subroutine ltime

      !> Map the logical unit of an open file to its associated filename.
      !>
      !> Legacy interface: calls ctx%lun2fn with the legacy context.
      subroutine lun2fn ( lunit, filnam )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: lunit  !! A logical unit to be mapped to a filename.
      character(*) :: filnam  !! Name of the file associated with LUNIT.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lun2fn ( lunit, filnam )
      end subroutine lun2fn

      !> Scan a string from a specified starting position for the
      !> end of a signed integer.
      !>
      !> Legacy interface: calls ctx%lx4sgn with the legacy context.
      subroutine lx4sgn ( string, first, last, nchar )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! any character string
      integer(ip) :: first  !! first character to scan from in STRING
      integer(ip) :: last  !! last character that is part of a signed integer
      integer(ip) :: nchar  !! number of characters in the signed integer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lx4sgn ( string, first, last, nchar )
      end subroutine lx4sgn

      !> Scan an identifier, starting from a specified character
      !> position.
      !>
      !> Legacy interface: calls ctx%lxidnt with the legacy context.
      subroutine lxidnt ( idspec, string, first, last, nchar )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! String to be scanned.
      integer(ip) :: first  !! Character position at which to start scanning.
      integer(ip) :: idspec ( lbcell : * )  !! Identifier character specification.
      integer(ip) :: last  !! Character position of end of token.
      integer(ip) :: nchar  !! Number of characters in token.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lxidnt ( idspec, string, first, last, nchar )
      end subroutine lxidnt

      !> Return the default specification for the characters that may
      !> appear in an identifier.
      !>
      !> Legacy interface: calls ctx%lxdfid with the legacy context.
      subroutine lxdfid ( idspec )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: idspec ( lbcell : * )  !! Identifier character specification.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lxdfid ( idspec )
      end subroutine lxdfid

      !> Set the acceptable characters that may appear in an identifier
      !> token.
      !>
      !> Legacy interface: calls ctx%lxcsid with the legacy context.
      subroutine lxcsid ( hdchrs, tlchrs, idspec )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: hdchrs  !! Allowed head characters for identifiers.
      character(*) :: tlchrs  !! Allowed tail characters for identifiers.
      integer(ip) :: idspec ( lbcell : * )  !! Identifier character specification.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lxcsid ( hdchrs, tlchrs, idspec )
      end subroutine lxcsid

      !> Find the nearest point on an ellipse to a specified point, both
      !> in three-dimensional space, and find the distance between the
      !> ellipse and the point.
      !>
      !> Legacy interface: calls ctx%npelpt with the legacy context.
      subroutine npelpt ( point, ellips, pnear, dist )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: ellips ( ubel )  !! A SPICE ellipse.
      real(wp) :: point  (    3 )  !! Point whose distance to an ellipse is to be found.
      real(wp) :: pnear  (    3 )  !! Nearest point on ellipse to input point.
      real(wp) :: dist  !! Distance of input point to ellipse.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: point, ellips are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%npelpt ( (point), (ellips), pnear, dist )
      end subroutine npelpt

      !> Determine the set of osculating conic orbital elements that
      !> corresponds to the state (position, velocity) of a body at
      !> some epoch.
      !>
      !> Legacy interface: calls ctx%oscelt with the legacy context.
      subroutine oscelt ( state, et, mu, elts )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: state  ( 6 )  !! State of body at epoch of elements.
      real(wp) :: et  !! Epoch of elements.
      real(wp) :: mu  !! Gravitational parameter (GM) of primary body.
      real(wp) :: elts   ( 8 )  !! Equivalent conic elements.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: state is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%oscelt ( (state), et, mu, elts )
      end subroutine oscelt

      !> Solve the time of flight equation MA = D + (D**3) / 3
      !> for the parabolic eccentric anomaly D, given mean anomaly.
      !>
      !> Legacy interface: calls ctx%partof with the legacy context.
      subroutine partof ( ma, d )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: ma  !! Mean anomaly at epoch.
      real(wp) :: d  !! Parabolic eccentric anomaly.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%partof ( ma, d )
      end subroutine partof

      !> Indicate for a specified body whether planetographic and
      !> planetocentric longitude increase in the same sense.
      !>
      !> Legacy interface: calls ctx%plnsns with the legacy context.
      integer(ip) function plnsns ( bodid )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: bodid  !! is the NAIF integer ID code of some solar system object.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      plnsns = ctx%plnsns ( bodid )
      end function plnsns

      !> Prompt a user for keyboard input.
      !>
      !> Legacy interface: calls ctx%prompt with the legacy context.
      subroutine prompt ( dspmsg, buffer )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: dspmsg  !! The prompt to use when asking for input.
      character(*) :: buffer  !! The response typed by a user.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: dspmsg is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%prompt ( (dspmsg), buffer )
      end subroutine prompt

      !> Parse a string as a double precision number, encapsulating error
      !> handling.
      !>
      !> Legacy interface: calls ctx%prsdp with the legacy context.
      subroutine prsdp ( string, dpval )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! String representing a numeric value.
      real(wp) :: dpval  !! D.p. value obtained by parsing STRING.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%prsdp ( string, dpval )
      end subroutine prsdp

      !> Encode a nonnegative integer number into a character string,
      !> portably, using 128 as the base for encoding.
      !>
      !> Legacy interface: calls ctx%prtenc with the legacy context.
      subroutine prtenc ( number, string )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: number  !! Number to be encoded.
      character(*) :: string  !! Encoded string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%prtenc ( number, string )
      end subroutine prtenc

      !> Decode a character string encoded by PRTENC.
      !>
      !> Legacy interface: calls ctx%prtdec with the legacy context.
      subroutine prtdec ( string, number )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: number  !! Decoded number.
      character(*) :: string  !! Encoded character string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%prtdec ( string, number )
      end subroutine prtdec

      !> Return the 3x3 matrix that transforms position vectors from one
      !> specified frame at a specified epoch to another specified
      !> frame at another specified epoch.
      !>
      !> Legacy interface: calls ctx%pxfrm2 with the legacy context.
      subroutine pxfrm2 ( from, to, etfrom, etto, rotate )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: from  !! Name of the frame to transform from.
      character(*) :: to  !! Name of the frame to transform to.
      real(wp) :: etfrom  !! Evaluation time of FROM frame.
      real(wp) :: etto  !! Evaluation time of TO frame.
      real(wp) :: rotate  ( 3, 3 )  !! A position transformation matrix from frame FROM to frame TO.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pxfrm2 ( from, to, etfrom, etto, rotate )
      end subroutine pxfrm2

      !> Read the next non-blank line of text from a text file.
      !>
      !> Legacy interface: calls ctx%rdnbl with the legacy context.
      subroutine rdnbl ( file, line, eof )
      implicit none
      character(*) :: file  !! Input text file.
      character(*) :: line  !! Next non-blank line from the input text file.
      logical :: eof  !! End-of-file indicator.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: file is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%rdnbl ( (file), line, eof )
      end subroutine rdnbl

      !> Replace a marker with the text representation of a
      !> cardinal number.
      !>
      !> Legacy interface: calls ctx%repmct with the legacy context.
      subroutine repmct ( in, marker, value, rtcase, out )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      character(*) :: marker  !! Marker to be replaced.
      integer(ip) :: value  !! Replacement value.
      character(1) :: rtcase  !! Case of replacement text.
      character(*) :: out  !! Output string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: in, marker, rtcase are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%repmct ( (in), (marker), value, (rtcase), out )
      end subroutine repmct

      !> Replace a marker with the text representation of a logical value.
      !>
      !> Legacy interface: calls ctx%repml with the legacy context.
      subroutine repml ( in, marker, value, rtcase, out )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      character(*) :: marker  !! Marker to be replaced.
      logical :: value  !! Replacement logical value.
      character(1) :: rtcase  !! Case of replacement text.
      character(*) :: out  !! Output string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: in, marker, rtcase are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%repml ( (in), (marker), value, (rtcase), out )
      end subroutine repml

      !> Transform a vector to a new reference frame rotated by ANGLE
      !> radians about axis IAXIS. This transformation rotates V1 by
      !> -ANGLE radians about the specified axis.
      !>
      !> Legacy interface: calls ctx%rotvec with the legacy context.
      subroutine rotvec ( v1, angle, iaxis, vout )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: v1    ( 3 )  !! Vector whose coordinate system is to be rotated.
      real(wp) :: angle  !! Angle of rotation in radians.
      integer(ip) :: iaxis  !! Axis of rotation (X=1, Y=2, Z=3).
      real(wp) :: vout  ( 3 )  !! Resulting vector expressed in the new frame.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: v1 is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%rotvec ( (v1), angle, iaxis, vout )
      end subroutine rotvec

      !> Convert a character representation of a type 1 spacecraft clock
      !> count to ticks.
      !>
      !> Legacy interface: calls ctx%scps01 with the legacy context.
      subroutine scps01 ( sc, clkstr, error, msg, ticks )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sclk
      implicit none
      integer(ip) :: sc  !! NAIF spacecraft ID code.
      character(*) :: clkstr  !! Character representation of a clock count.
      logical :: error  !! Parsing error flag.
      character(*) :: msg  !! Output message for parsing error.
      real(wp) :: ticks  !! Number of ticks represented by the clock count.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: clkstr is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%scps01 ( sc, (clkstr), error, msg, ticks )
      end subroutine scps01

      !> Convert an SCLK name string to a NAIF integer code.
      !>
      !> Legacy interface: calls ctx%scn2id with the legacy context.
      subroutine scn2id ( clknam, clkid, found )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: clknam  !! String giving spacecraft clock name.
      integer(ip) :: clkid  !! NAIF integer code of spacecraft clock.
      logical :: found  !! Flag indicating whether item was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%scn2id ( clknam, clkid, found )
      end subroutine scn2id

      !> Convert a NAIF integer code for a spacecraft clock to an SCLK name
      !> string.
      !>
      !> Legacy interface: calls ctx%scid2n with the legacy context.
      subroutine scid2n ( clkid, clknam, found )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: clknam  !! String giving spacecraft clock name.
      integer(ip) :: clkid  !! NAIF integer code of spacecraft clock.
      logical :: found  !! Flag indicating whether item was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%scid2n ( clkid, clknam, found )
      end subroutine scid2n

      !> Return the distance between two points on a sphere, measured
      !> along the shortest great circle arc connecting them.
      !>
      !> Legacy interface: calls ctx%sphsd with the legacy context.
      real(wp) function sphsd ( radius, lon1, lat1, &
                                                lon2, lat2  )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: radius  !! Radius of sphere.
      real(wp) :: lon1  !! Longitude and latitude of first point in radians.
      real(wp) :: lat1  !! Longitude and latitude of first point in radians.
      real(wp) :: lon2  !! Longitude and latitude of second point in radians.
      real(wp) :: lat2  !! Longitude and latitude of second point in radians.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      sphsd = ctx%sphsd ( radius, lon1, lat1, lon2, lat2 )
      end function sphsd

      !> Return the state (position and velocity) of a target body
      !> relative to some center of motion in a specified frame.
      !>
      !> Legacy interface: calls ctx%spkpv with the legacy context.
      subroutine spkpv ( handle, descr, et, ref, state, center )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      character(*) :: ref  !! Target reference frame.
      real(wp) :: state    ( 6 )  !! Position, velocity.
      integer(ip) :: center  !! Center of state.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: descr is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkpv ( handle, (descr), et, ref, state, center )
      end subroutine spkpv

      !> Convert planetocentric latitude and longitude of a surface
      !> point on a specified body to rectangular coordinates.
      !>
      !> Legacy interface: calls ctx%srfrec with the legacy context.
      subroutine srfrec ( body, lon, lat, rectan )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: body  !! NAIF integer code of an extended body.
      real(wp) :: lon  !! Longitude of point in radians.
      real(wp) :: lat  !! Latitude of point in radians.
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of the point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%srfrec ( body, lon, lat, rectan )
      end subroutine srfrec

      !> Determine the intersection of a line-of-sight vector with the
      !> surface of an ellipsoid.
      !>
      !> Legacy interface: calls ctx%surfpt with the legacy context.
      subroutine surfpt ( positn, u, a, b, c, point, found )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: positn ( 3 )  !! Position of the observer in body-fixed frame.
      real(wp) :: u      ( 3 )  !! Vector from the observer in some direction.
      real(wp) :: a  !! Length of ellipsoid semi-axis along the x-axis.
      real(wp) :: b  !! Length of ellipsoid semi-axis along the y-axis.
      real(wp) :: c  !! Length of ellipsoid semi-axis along the z-axis.
      real(wp) :: point  ( 3 )  !! Point on the ellipsoid pointed to by U.
      logical :: found  !! Flag indicating if U points at the ellipsoid.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: positn, u are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%surfpt ( (positn), (u), a, b, c, point, found )
      end subroutine surfpt

      !> Find the state transformation from a base frame to the
      !> right-handed frame defined by two state vectors: one state
      !> vector defining a specified axis and a second state vector
      !> defining a specified coordinate plane.
      !>
      !> Legacy interface: calls ctx%twovxf with the legacy context.
      subroutine twovxf ( axdef, indexa, plndef, indexp, xform )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: axdef  ( 6 )  !! State defining a principal axis.
      integer(ip) :: indexa  !! Principal axis number of AXDEF (X=1, Y=2, Z=3).
      real(wp) :: plndef ( 6 )  !! State defining (with AXDEF) a principal plane.
      integer(ip) :: indexp  !! Second axis number (with INDEXA) of principal plane.
      real(wp) :: xform  ( 6, 6 )  !! Output state transformation matrix.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: axdef, plndef are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%twovxf ( (axdef), indexa, (plndef), indexp, xform )
      end subroutine twovxf

      !> Calculate the first derivative of a caller-specified scalar
      !> function using a three-point estimation.
      !>
      !> Legacy interface: calls ctx%uddf with the legacy context.
      subroutine uddf ( udfunc, x, dx, deriv )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: x  !! Independent variable of UDFUNC.
      real(wp) :: dx  !! Interval from X for derivative calculation.
      real(wp) :: deriv  !! Approximate derivative of UDFUNC at X.
      procedure(legacy_gf_scalar) :: udfunc  !! the gf_scalar routine
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call ctx%uddf ( bridge_gf_scalar, x, dx, deriv )
      current_gf_scalar => outer_udfunc
      end subroutine uddf

      !> Serve as a dummy function for GF routines expecting an UDFUNS
      !> argument. It is a no-op routine with an argument signature
      !> matching UDFUNS.
      !>
      !> Legacy interface: calls ctx%udf with the legacy context.
      subroutine udf ( x, value )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! Double precision value, unused.
      real(wp) :: value  !! Double precision value, unused.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%udf ( x, value )
      end subroutine udf

      !> Find the minimum or maximum angular separation between a
      !> specified ray and ellipse.
      !>
      !> Legacy interface: calls zz routine zzasryel with the legacy context.
      subroutine zzasryel ( extrem, ellips, vertex, dir, angle, extpt )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: extrem  !! Type of extremum to find.
      real(wp) :: ellips ( ubel )  !! SPICE ellipse.
      real(wp) :: vertex ( 3 )  !! Vertex and direction vector of ray.
      real(wp) :: dir    ( 3 )  !! Vertex and direction vector of ray.
      real(wp) :: angle  !! Angular separation of ray and ellipse (radians).
      real(wp) :: extpt  ( 3 )  !! Point on ellipse where extremum is achieved.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: ellips, vertex, dir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzasryel ( ctx, extrem, (ellips), (vertex), (dir), angle, extpt )
      end subroutine zzasryel

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbquad with the legacy context.
      subroutine zzbquad ( a, b, c, ub, n, nx, r1, r2 )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a  !! Coefficients of quadratic equation.
      real(wp) :: b  !! Coefficients of quadratic equation.
      real(wp) :: c  !! Coefficients of quadratic equation.
      real(wp) :: ub  !! Upper bound on magnitude of roots.
      integer(ip) :: n  !! Number of real roots within bound.
      integer(ip) :: nx  !! Number of real roots exceeding bound.
      real(wp) :: r1  !! Root of smaller absolute value.
      real(wp) :: r2  !! Root of larger absolute value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzbquad ( ctx, a, b, c, ub, n, nx, r1, r2 )
      end subroutine zzbquad

      !> Given the latitude of a midpoint of chord on a circle of constant
      !> latitude, and given the longitude extent of the chord, compute
      !> the latitude of the chord's endpoints. The coordinate system is
      !> "latitudinal," aka planetocentric.
      !>
      !> Legacy interface: calls zz routine zzchrlat with the legacy context.
      subroutine zzchrlat ( midlat, dlon, eptlat )
      use pepper_kinds, only: wp
      use pepper_inc_dsktol
      implicit none
      real(wp) :: midlat  !! is the latitude of the midpoint of a chord of a latitude circle.
      real(wp) :: dlon  !! is the longitude extent of the chord.
      real(wp) :: eptlat  !! is the latitude of the endpoints of the chord.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzchrlat ( ctx, midlat, dlon, eptlat )
      end subroutine zzchrlat

      !> Fetch the current CPU date and time and store the result
      !> as a double precision 6-vector.
      !>
      !> Legacy interface: calls zz routine zzcputim with the legacy context.
      subroutine zzcputim ( tvec )
      use pepper_kinds, only: ip, wp
      real(wp) :: tvec ( 6 )  !! contains year, month, day, hours, minutes, seconds
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzcputim ( ctx, tvec )
      end subroutine zzcputim

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzcxbrut with the legacy context.
      subroutine zzcxbrut ( apex,   axis,   angle, &
                            endpt1, endpt2, xpt,   isbrck )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: apex   ( 3 )  !! Apex of cone.
      real(wp) :: axis   ( 3 )  !! Axis of cone.
      real(wp) :: angle  !! Angle of cone.
      real(wp) :: endpt1 ( 3 )  !! Endpoints of line segment.
      real(wp) :: endpt2 ( 3 )  !! Endpoints of line segment.
      real(wp) :: xpt    ( 3 )  !! Intersection point, if it exists.
      logical :: isbrck  !! Logical flag indicating whether root is bracketed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: apex, axis, endpt1, endpt2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzcxbrut ( ctx, (apex), (axis), angle, (endpt1), (endpt2), xpt, isbrck )
      end subroutine zzcxbrut

      !> Safely calculate the value NUMR/DENOM, avoiding the possibility
      !> of floating point exceptions (FPE), due to numeric underflow,
      !> numeric overflow, or divide-by-zero.
      !>
      !> Legacy interface: calls zz routine zzdiv with the legacy context.
      real(wp) function zzdiv ( numr, denom )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: numr  !! Numerator of division.
      real(wp) :: denom  !! Denominator of division.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdiv = pepper_zzdiv ( ctx, numr, denom )
      end function zzdiv

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdskbbl with the legacy context.
      subroutine zzdskbbl ( bodyid )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: bodyid  !! Body ID code.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdskbbl ( ctx, bodyid )
      end subroutine zzdskbbl

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdsksbi with the legacy context.
      subroutine zzdsksbi ( maxbod, stsize, btbody, btnbod, btsegp, &
                            btstsz, sthan,  stdscr, stdlad, stfree, &
                            stoff,  stctr,  strad                  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: maxbod  !! Maximum size of body table.
      integer(ip) :: stsize  !! Maximum size of segment table.
      integer(ip) :: btbody ( * )  !! Body table's body ID array.
      integer(ip) :: btnbod  !! Number of entries in body table.
      integer(ip) :: btsegp ( * )  !! Array of pointers from bodies to segment table.
      integer(ip) :: btstsz ( * )  !! Array of sizes of body segment lists.
      integer(ip) :: sthan  ( * )  !! Array of handles of segments.
      real(wp) :: stdscr ( dskdsz, * )  !! Array of DSK descriptors of segments.
      integer(ip) :: stdlad ( dladsz, * )  !! Array of DLA descriptors of segments.
      integer(ip) :: stfree  !! Index of first free entry in segment table.
      real(wp) :: stoff  ( 3, * )  !! Offsets of segment frame centers from body.
      real(wp) :: stctr  ( 3, * )  !! Centers of bounding spheres for segments.
      real(wp) :: strad  ( * )  !! Radii of bounding spheres for segments.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdsksbi ( ctx, maxbod, stsize, btbody, btnbod, btsegp, btstsz, sthan, stdscr, stdlad, stfree, stoff, stctr, strad )
      end subroutine zzdsksbi

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdsksbr with the legacy context.
      subroutine zzdsksbr ( needed, &
                            maxbod, stsize, btbody, btnbod, btsegp, &
                            btstsz, sthan,  stdscr, stdlad, stfree, &
                            stoff,  stctr,  strad                   )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: needed  !! Number of segment table entries needed.
      integer(ip) :: maxbod  !! Maximum size of body table.
      integer(ip) :: stsize  !! Maximum size of segment table.
      integer(ip) :: btbody ( * )  !! Body table's body ID array.
      integer(ip) :: btnbod  !! Number of entries in body table.
      integer(ip) :: btsegp ( * )  !! Array of pointers from bodies to segment table.
      integer(ip) :: btstsz ( * )  !! Array of sizes of body segment lists.
      integer(ip) :: sthan  ( * )  !! Array of handles of segments.
      real(wp) :: stdscr ( dskdsz, * )  !! Array of DSK descriptors of segments.
      integer(ip) :: stdlad ( dladsz, * )  !! Array of DLA descriptors of segments.
      integer(ip) :: stfree  !! Index of first free entry in segment table.
      real(wp) :: stoff  ( 3, * )  !! Offsets of segment frame centers from body.
      real(wp) :: stctr  ( 3, * )  !! Centers of bounding spheres for segments.
      real(wp) :: strad  ( * )  !! Radii of bounding spheres for segments.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdsksbr ( ctx, needed, maxbod, stsize, btbody, btnbod, btsegp, btstsz, sthan, stdscr, stdlad, stfree, stoff, stctr, strad )
      end subroutine zzdsksbr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdsksbd with the legacy context.
      logical function zzdsksbd ( bodyid )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: bodyid  !! ID code of target body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdsksbd = pepper_zzdsksbd ( ctx, bodyid )
      end function zzdsksbd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdskbdc with the legacy context.
      logical function zzdskbdc ( handle, dladsc, dskdsc )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: handle  !! Handle of DSK containing segment to be checked.
      integer(ip) :: dladsc ( * )  !! DLA descriptor of segment to be checked.
      real(wp) :: dskdsc ( * )  !! DSK descriptor of segment to be checked.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdskbdc = pepper_zzdskbdc ( ctx, handle, dladsc, dskdsc )
      end function zzdskbdc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdsknot with the legacy context.
      logical function zzdsknot ( handle, dladsc, dskdsc )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: handle  !! Handle of DSK containing segment to be checked.
      integer(ip) :: dladsc ( * )  !! DLA descriptor of segment to be checked.
      real(wp) :: dskdsc ( * )  !! DSK descriptor of segment to be checked.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdsknot = pepper_zzdsknot ( ctx, handle, dladsc, dskdsc )
      end function zzdsknot

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdsksit with the legacy context.
      logical function zzdsksit ( bodyid, nsurf, srflst, et )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: nsurf  !! Surface ID count.
      integer(ip) :: srflst ( * )  !! Surface ID list.
      integer(ip) :: bodyid  !! ID code of target body.
      real(wp) :: et  !! Epoch, expressed as seconds past J2000 TDB.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdsksit = pepper_zzdsksit ( ctx, bodyid, nsurf, srflst, et )
      end function zzdsksit

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdskcit with the legacy context.
      logical function zzdskcit ( handle, dladsc, dskdsc )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: handle  !! Handle of DSK containing segment to be checked.
      integer(ip) :: dladsc ( * )  !! DLA descriptor of segment to be checked.
      real(wp) :: dskdsc ( * )  !! DSK descriptor of segment to be checked.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdskcit = pepper_zzdskcit ( ctx, handle, dladsc, dskdsc )
      end function zzdskcit

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdskusc with the legacy context.
      logical function zzdskusc ( bodyid, et, cor1, cor2 )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: bodyid  !! ID code of target body.
      real(wp) :: cor1  !! First coordinate.
      real(wp) :: cor2  !! Second coordinate.
      real(wp) :: et  !! Epoch, expressed as seconds past J2000 TDB.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdskusc = pepper_zzdskusc ( ctx, bodyid, et, cor1, cor2 )
      end function zzdskusc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdskumc with the legacy context.
      logical function zzdskumc ( handle, dladsc, dskdsc )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: handle  !! Handle of DSK containing segment to be checked.
      integer(ip) :: dladsc ( * )  !! DLA descriptor of segment to be checked.
      real(wp) :: dskdsc ( * )  !! DSK descriptor of segment to be checked.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdskumc = pepper_zzdskumc ( ctx, handle, dladsc, dskdsc )
      end function zzdskumc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdskmsc with the legacy context.
      logical function zzdskmsc ( bodyid, surfid, framid, corsys, corpar, et, cor1, cor2 )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: surfid  !! ID code of target surface.
      integer(ip) :: bodyid  !! ID code of target body.
      integer(ip) :: corsys  !! Coordinate system code.
      real(wp) :: corpar ( * )  !! Coordinate system parameters.
      real(wp) :: cor1  !! First coordinate.
      real(wp) :: cor2  !! Second coordinate.
      integer(ip) :: framid  !! ID code of body-fixed reference frame.
      real(wp) :: et  !! Epoch, expressed as seconds past J2000 TDB.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdskmsc = pepper_zzdskmsc ( ctx, bodyid, surfid, framid, corsys, corpar, et, cor1, cor2 )
      end function zzdskmsc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdskmmc with the legacy context.
      logical function zzdskmmc ( handle, dladsc, dskdsc )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: handle  !! Handle of DSK containing segment to be checked.
      integer(ip) :: dladsc ( * )  !! DLA descriptor of segment to be checked.
      real(wp) :: dskdsc ( * )  !! DSK descriptor of segment to be checked.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdskmmc = pepper_zzdskmmc ( ctx, handle, dladsc, dskdsc )
      end function zzdskmmc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdsksrc with the legacy context.
      logical function zzdsksrc ( surfid, bodyid, dclass, et, framid, pos )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: surfid  !! ID code of target surface.
      integer(ip) :: bodyid  !! ID code of target body.
      integer(ip) :: dclass  !! Data class of segment.
      integer(ip) :: framid  !! ID code of body-fixed reference frame.
      real(wp) :: pos    ( 3 )  !! Cartesian position vector.
      real(wp) :: et  !! Epoch, expressed as seconds past J2000 TDB.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdsksrc = pepper_zzdsksrc ( ctx, surfid, bodyid, dclass, et, framid, pos )
      end function zzdsksrc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdskmrc with the legacy context.
      logical function zzdskmrc ( handle, dladsc, dskdsc )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: handle  !! Handle of DSK containing segment to be checked.
      integer(ip) :: dladsc ( * )  !! DLA descriptor of segment to be checked.
      real(wp) :: dskdsc ( * )  !! DSK descriptor of segment to be checked.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdskmrc = pepper_zzdskmrc ( ctx, handle, dladsc, dskdsc )
      end function zzdskmrc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdsksgr with the legacy context.
      real(wp) function zzdsksgr ( dskdsc )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dskdsc
      implicit none
      real(wp) :: dskdsc ( * )  !! is a DSK descriptor.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzdsksgr = pepper_zzdsksgr ( ctx, dskdsc )
      end function zzdsksgr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzedterm with the legacy context.
      subroutine zzedterm ( type,   a,      b,     c, &
                            srcrad, srcpos, npts,  trmpts )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: type  !! Terminator type.
      real(wp) :: a  !! Length of ellipsoid semi-axis lying on the x-axis.
      real(wp) :: b  !! Length of ellipsoid semi-axis lying on the y-axis.
      real(wp) :: c  !! Length of ellipsoid semi-axis lying on the z-axis.
      real(wp) :: srcrad  !! Radius of light source.
      real(wp) :: srcpos ( 3 )  !! Position of center of light source.
      integer(ip) :: npts  !! Number of points in terminator point set.
      real(wp) :: trmpts ( 3, npts )  !! Terminator point set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: srcpos is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzedterm ( ctx, type, a, b, c, srcrad, (srcpos), npts, trmpts )
      end subroutine zzedterm

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzedtmpt with the legacy context.
      subroutine zzedtmpt ( umbral, a, b, c, r, axis, plnvec, point )
      use pepper_kinds, only: ip, wp
      implicit none
      logical :: umbral  !! Flag indicating whether terminator is umbral.
      real(wp) :: a  !! Semi-axis length in the X direction.
      real(wp) :: b  !! Semi-axis length in the Y direction.
      real(wp) :: c  !! Semi-axis length in the Z direction.
      real(wp) :: r  !! Radius of illumination source.
      real(wp) :: axis   ( 3 )  !! Axis vector from target to source.
      real(wp) :: plnvec ( 3 )  !! Vector in cutting half-plane.
      real(wp) :: point  ( 3 )  !! Terminator point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: axis, plnvec are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzedtmpt ( ctx, umbral, a, b, c, r, (axis), (plnvec), point )
      end subroutine zzedtmpt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzellbds with the legacy context.
      subroutine zzellbds ( a, b, hmax, hmin, amax, bmax, amin, bmin )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: a  !! Input spheroid's semi-major axis length.
      real(wp) :: b  !! Input spheroid's semi-minor axis length.
      real(wp) :: hmax  !! Maximum height relative to input spheroid.
      real(wp) :: hmin  !! Minimum height relative to input spheroid.
      real(wp) :: amax  !! Outer spheroid's semi-major axis length.
      real(wp) :: bmax  !! Outer spheroid's semi-minor axis length.
      real(wp) :: amin  !! Inner spheroid's semi-major axis length.
      real(wp) :: bmin  !! Inner spheroid's semi-minor axis length.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzellbds ( ctx, a, b, hmax, hmin, amax, bmax, amin, bmin )
      end subroutine zzellbds

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzelnaxx with the legacy context.
      subroutine zzelnaxx ( a, b, lat, xxpt, yxpt )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: a  !! Ellipse's semi-axis length in the X direction.
      real(wp) :: b  !! Ellipse's semi-axis length in the Y direction.
      real(wp) :: lat  !! Planetodetic latitude in radians.
      real(wp) :: xxpt  !! Normal line intercept on the X-axis.
      real(wp) :: yxpt  !! Normal line intercept on the Y-axis.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzelnaxx ( ctx, a, b, lat, xxpt, yxpt )
      end subroutine zzelnaxx

      !> Return the 1976 IAU Earth precession matrix for a specified time.
      !>
      !> Legacy interface: calls zz routine zzeprcss with the legacy context.
      subroutine zzeprcss ( et, precm )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: et  !! Ephemeris time, in seconds past J2000.
      real(wp) :: precm ( 3, 3 )  !! Precession matrix at ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzeprcss ( ctx, et, precm )
      end subroutine zzeprcss

      !> Given a the "lines" of a two-line element set, parse the
      !> lines and return the elements in units suitable for use
      !> in SPICE software.
      !>
      !> Legacy interface: calls zz routine zzgetelm with the legacy context.
      subroutine zzgetelm ( frstyr, lines, epoch, elems, ok, error )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: frstyr  !! year of earliest representable two-line elements
      character(*) :: lines ( 2 )  !! a pair of "lines" containing two-line elements
      real(wp) :: epoch  !! The epoch of the elements in seconds past J2000
      real(wp) :: elems ( * )  !! The elements converted to SPICE units.
      logical :: ok  !! Boolean indicating error state.
      character(*) :: error  !! String describing error.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: lines is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzgetelm ( ctx, frstyr, (lines), epoch, elems, ok, error )
      end subroutine zzgetelm

      !> Constants assignments equivalent to 21 July 2006 "getgravconst"
      !> by David Vallado, AIAA-2006-6753.
      !>
      !> Legacy interface: calls zz routine zzgrav with the legacy context.
      subroutine zzgrav ( grav )
      use pepper_kinds, only: wp
      use pepper_inc_zzsgp4
      implicit none
      real(wp) :: grav( ngravs, ngravc )  !! Array of geophysical constants.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgrav ( ctx, grav )
      end subroutine zzgrav

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzholdd with the legacy context.
      subroutine zzholdd ( op, id, ok, value )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzholdd
      implicit none
      integer(ip) :: op  !! Key for operation to execute.
      integer(ip) :: id  !! The ID for the item to apply OP.
      logical :: ok  !! Boolean indicating success of get operation.
      real(wp) :: value  !! Double precision value returned or to store.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzholdd ( ctx, op, id, ok, value )
      end subroutine zzholdd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhullax with the legacy context.
      subroutine zzhullax ( inst, n, bounds, axis )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: inst  !! Instrument name.
      integer(ip) :: n  !! Number of FOV boundary vectors.
      real(wp) :: bounds ( 3, n )  !! FOV boundary vectors.
      real(wp) :: axis   ( 3 )  !! Instrument FOV axis vector.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: bounds is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzhullax ( ctx, inst, n, (bounds), axis )
      end subroutine zzhullax

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzingrd with the legacy context.
       logical function zzingrd ( nvox, voxel )
       use pepper_kinds, only: ip
      implicit              none
      integer(ip) :: nvox  ( 3 )  !! Dimensions of voxel grid.
      integer(ip) :: voxel ( 3 )  !! Coordinates of voxel in voxel grid units.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzingrd = pepper_zzingrd ( ctx, nvox, voxel )
      end function zzingrd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzinrec with the legacy context.
      subroutine zzinrec ( p, bounds, margin, exclud, inside )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: p      ( 3 )  !! Input point.
      real(wp) :: bounds ( 2, 3 )  !! Coordinate bounds of element.
      real(wp) :: margin  !! Element expansion margin.
      integer(ip) :: exclud  !! Index of coordinate to exclude from test.
      logical :: inside  !! Flag indicating whether point is in element.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzinrec ( ctx, p, bounds, margin, exclud, inside )
      end subroutine zzinrec

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzinrypl with the legacy context.
      subroutine zzinrypl ( vertex, udir, uplnml, &
                            const,  maxd, nxpts,  xpt )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: vertex ( 3 )  !! Vertex and unit length direction vector of ray.
      real(wp) :: udir   ( 3 )  !! Vertex and unit length direction vector of ray.
      real(wp) :: uplnml ( 3 )  !! Unit plane normal vector.
      real(wp) :: const  !! Plane constant.
      real(wp) :: maxd  !! Maximum distance of intersection from vertex.
      integer(ip) :: nxpts  !! Number of intersection points of ray and plane.
      real(wp) :: xpt    ( 3 )  !! Intersection point, if NXPTS = 1.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, udir, uplnml are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzinrypl ( ctx, (vertex), (udir), (uplnml), const, maxd, nxpts, xpt )
      end subroutine zzinrypl

      !> Create a bounding box for a latitudinal volume element.
      !>
      !> Legacy interface: calls zz routine zzlatbox with the legacy context.
      subroutine zzlatbox ( bounds, center, lr, lt, lz, radius )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: bounds ( 2, 3 )  !! Array of coordinate bounds for segment.
      real(wp) :: center ( 3 )  !! Center of bounding box.
      real(wp) :: lr  !! Extent of box in the cylindrical radial direction.
      real(wp) :: lt  !! Extent of box in the tangential direction.
      real(wp) :: lz  !! Extent of box in the Z direction.
      real(wp) :: radius  !! Radius of box.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: bounds is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzlatbox ( ctx, (bounds), center, lr, lt, lz, radius )
      end subroutine zzlatbox

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzlexmet with the legacy context.
      subroutine zzlexmet ( method, maxn, n, begs, ends )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: method  !! Method string.
      integer(ip) :: maxn  !! Maximum number of tokens that can be returned.
      integer(ip) :: n  !! Number of tokens found.
      integer(ip) :: begs   ( * )  !! Beginning indices of tokens.
      integer(ip) :: ends   ( * )  !! Ending indices of tokens.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzlexmet ( ctx, method, maxn, n, begs, ends )
      end subroutine zzlexmet

      !> Safely calculate the value A*B, avoiding the possibility
      !> of floating point exceptions (FPE) due to numeric underflow
      !> or numeric overflow.
      !>
      !> Legacy interface: calls zz routine zzmult with the legacy context.
      real(wp) function zzmult ( a, b )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: a  !! Multiplier.
      real(wp) :: b  !! Multiplicand.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzmult = pepper_zzmult ( ctx, a, b )
      end function zzmult

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zznrmlon with the legacy context.
      subroutine zznrmlon ( inmin, inmax, tol, outmin, outmax )
      use pepper_kinds, only: wp
      use pepper_constants, only: twopi
      implicit none
      real(wp) :: inmin  !! Longitude interval lower bound.
      real(wp) :: inmax  !! Longitude interval upper bound.
      real(wp) :: tol  !! Round-off tolerance.
      real(wp) :: outmin  !! Adjusted longitude interval lower bound.
      real(wp) :: outmax  !! Adjusted longitude interval upper bound.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zznrmlon ( ctx, inmin, inmax, tol, outmin, outmax )
      end subroutine zznrmlon

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzrecbox with the legacy context.
      subroutine zzrecbox ( bounds, center, lx, ly, lz, radius )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: bounds ( 2, 3 )  !! Array of coordinate bounds for segment.
      real(wp) :: center ( 3 )  !! Center of bounding box.
      real(wp) :: lx  !! Extent of box in the X direction.
      real(wp) :: ly  !! Extent of box in the Y direction.
      real(wp) :: lz  !! Extent of box in the Z direction.
      real(wp) :: radius  !! Radius of box.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: bounds is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzrecbox ( ctx, (bounds), center, lx, ly, lz, radius )
      end subroutine zzrecbox

      !> Given a vector, return a transformation matrix that maps from the
      !> vector's base reference frame to the RTN
      !> (radial-tangential-normal) frame associated with the vector.
      !>
      !> Legacy interface: calls zz routine zzrtnmat with the legacy context.
      subroutine zzrtnmat ( v, m )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: v ( 3 )  !! A 3-dimensional vector.
      real(wp) :: m ( 3, 3 )  !! Base frame to RTN frame rotation matrix.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: v is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzrtnmat ( ctx, (v), m )
      end subroutine zzrtnmat

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzryxsph with the legacy context.
      subroutine zzryxsph ( vertex, udir, r, xpt, found )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: vertex ( 3 )  !! Vertex and unit length direction vector of ray.
      real(wp) :: udir   ( 3 )  !! Vertex and unit length direction vector of ray.
      real(wp) :: r  !! Radius of sphere.
      real(wp) :: xpt    ( 3 )  !! Intersection point, if NXPTS = 1.
      logical :: found  !! Flag indicating whether intersection exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, udir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzryxsph ( ctx, (vertex), (udir), r, xpt, found )
      end subroutine zzryxsph

      !> Routine to calculate the dot terms for the secular perturbation
      !> of a vehicle.
      !>
      !> Legacy interface: calls zz routine zzsecprt with the legacy context.
      subroutine zzsecprt ( isynfl, dg   , del   , xni  , &
                            omegao, atime, omgdot, xli  , &
                            xfact , xldot, xndot , xnddt )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: isynfl  !! Resonance flag
      real(wp) :: omegao  !! Original argument of perigee
      real(wp) :: atime  !! An intermediate time term passed from the calling routine
      real(wp) :: omgdot  !! Time rate of change of argument of perigee
      real(wp) :: xli  !! An intermediate angular term passed from the calling routine
      real(wp) :: dg     ( 10 )  !! Parameter array
      real(wp) :: xfact  !! The value BFACT - XNQ
      real(wp) :: xldot  !! Time rate of change of XL
      real(wp) :: xni  !! An intermediate linear term passed from the calling routine
      real(wp) :: del    (  3 )  !! Parameter array of delta values
      real(wp) :: xnddt  !! Time rate of change of XNDOT
      real(wp) :: xndot  !! Time rate of change of XN
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: dg, del are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzsecprt ( ctx, isynfl, (dg), (del), xni, omegao, atime, omgdot, xli, xfact, &
         xldot, xndot, xnddt )
      end subroutine zzsecprt

      !> Compute the angular separation between two spherical or
      !> point objects.
      !>
      !> Legacy interface: calls zz routine zzsepq with the legacy context.
      real(wp) function zzsepq ( et, &
                                         bod1, bod2, &
                                         r1,   r2, &
                                         obs,  abcorr, ref )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB
      integer(ip) :: bod1  !! NAIF ID for first target
      integer(ip) :: bod2  !! NAIF ID for second target
      real(wp) :: r1  !! Radius of a spherical model for BOD1
      real(wp) :: r2  !! Radius of a spherical model for BOD2
      integer(ip) :: obs  !! NAIF ID of observer
      character(*) :: abcorr  !! Aberration correction flag
      character(*) :: ref  !! Reference frame of the angular separation
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzsepq = pepper_zzsepq ( ctx, et, bod1, bod2, r1, r2, obs, abcorr, ref )
      end function zzsepq

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsfxcor with the legacy context.
      subroutine zzsfxcor ( udnear, udmaxr, udrayx, trgcde, et, abcorr, uselt, usecn, usestl, xmit, fixref, obscde, dfrcde, dclass, dcentr, dvec, spoint, trgepc, srfvec, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_zzabcorr
      implicit none
      integer(ip) :: trgcde  !! Target body ID code.
      real(wp) :: et  !! Epoch at observer.
      character(*) :: abcorr  !! Aberration correction string.
      logical :: uselt  !! "Use light time" flag.
      logical :: usecn  !! "Use converged Newtonian correction" flag.
      logical :: usestl  !! "Use stellar aberration" flag.
      logical :: xmit  !! "Perform transmission corrections" flag.
      character(*) :: fixref  !! Name of target body-fixed frame.
      integer(ip) :: obscde  !! Observer body ID code.
      integer(ip) :: dfrcde  !! Ray direction frame ID code.
      integer(ip) :: dclass  !! Ray direction frame class.
      integer(ip) :: dcentr  !! Ray direction frame center body ID code.
      real(wp) :: dvec    ( 3 )  !! Ray direction vector.
      real(wp) :: spoint  ( 3 )  !! Surface intercept, if found.
      real(wp) :: trgepc  !! Target epoch, if intercept was found.
      real(wp) :: srfvec  ( 3 )  !! Observer-to-intercept vector, if found.
      logical :: found  !! Found flag.
      procedure(legacy_zzsfx_near) :: udnear  !! the zzsfx_near routine
      procedure(legacy_zzsfx_maxrad) :: udmaxr  !! the zzsfx_maxrad routine
      procedure(legacy_zzsfx_rayx) :: udrayx  !! the zzsfx_rayx routine
      type(pepper_context), pointer :: ctx
      procedure(legacy_zzsfx_near), pointer :: outer_udnear  !! the outer call's routine
      procedure(legacy_zzsfx_maxrad), pointer :: outer_udmaxr  !! the outer call's routine
      procedure(legacy_zzsfx_rayx), pointer :: outer_udrayx  !! the outer call's routine
      ctx => legacy_context()
      outer_udnear => current_zzsfx_near
      current_zzsfx_near => udnear
      outer_udmaxr => current_zzsfx_maxrad
      current_zzsfx_maxrad => udmaxr
      outer_udrayx => current_zzsfx_rayx
      current_zzsfx_rayx => udrayx
      ! PEPPER: dvec is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzsfxcor ( ctx, bridge_zzsfx_near, bridge_zzsfx_maxrad, bridge_zzsfx_rayx, &
         trgcde, et, abcorr, uselt, usecn, usestl, xmit, fixref, obscde, dfrcde, dclass, dcentr, &
         (dvec), spoint, trgepc, srfvec, found )
      current_zzsfx_near => outer_udnear
      current_zzsfx_maxrad => outer_udmaxr
      current_zzsfx_rayx => outer_udrayx
      end subroutine zzsfxcor

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsglatx with the legacy context.
      subroutine zzsglatx ( p1, p2, minlat, minp, maxlat, maxp )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: p1     ( 3 )  !! First line segment endpoint.
      real(wp) :: p2     ( 3 )  !! Second line segment endpoint.
      real(wp) :: minlat  !! Minimum latitude on segment.
      real(wp) :: minp   ( 3 )  !! Point where minimum latitude is attained.
      real(wp) :: maxlat  !! Maximum latitude on segment.
      real(wp) :: maxp   ( 3 )  !! Point where maximum latitude is attained.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: p1, p2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzsglatx ( ctx, (p1), (p2), minlat, minp, maxlat, maxp )
      end subroutine zzsglatx

      !> Check/initialize the quantities describing an angular separation
      !> between two spherical or point objects.
      !>
      !> Legacy interface: calls zz routine zzspin with the legacy context.
      subroutine zzspin ( targ1, shape1, frame1, targ2, shape2, frame2, obsrvr, abcorr, bods, frames, mxrad, obs )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: targ1  !! Name for first target
      character(*) :: shape1  !! Shape corresponding to TARG1
      character(*) :: frame1  !! Body fixed frame for TARG1
      character(*) :: targ2  !! Name for second target
      character(*) :: shape2  !! Shape corresponding to TARG2
      character(*) :: frame2  !! Body fixed frame for TARG2
      character(*) :: obsrvr  !! Name for observer body
      character(*) :: abcorr  !! Aberration correction flag
      integer(ip) :: bods   (2)  !! Array(2) of body/SPK IDs for TARG1 and TARG2
      integer(ip) :: frames (2)  !! Array(2) of frame IDs for TARG1 and TARG2
      real(wp) :: mxrad  (2)  !! Array(2) of max radii values for TARG1 and TARG2
      integer(ip) :: obs  !! Body/SPK IDs of OBSRVR
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzspin ( ctx, targ1, shape1, frame1, targ2, shape2, frame2, obsrvr, abcorr, bods, frames, mxrad, obs )
      end subroutine zzspin

      !> **Deprecated:** This routine has been superseded by SPKAPS. This
      !> routine is supported for purposes of backward compatibility only.
      !>
      !> Legacy interface: calls spkapp (PEPPER: the copies of SPICELIB for each recursion level are gone) with the legacy context.
      subroutine zzspkap0 ( targ, et, ref, sobs, abcorr, starg, lt )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of observer's state.
      real(wp) :: sobs     ( 6 )  !! State of observer wrt. solar system barycenter.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: sobs is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkapp ( targ, et, ref, (sobs), abcorr, starg, lt )
      end subroutine zzspkap0

      !> **Deprecated:** This routine has been superseded by SPKAPS. This
      !> routine is supported for purposes of backward compatibility only.
      !>
      !> Legacy interface: calls spkapp (PEPPER: the copies of SPICELIB for each recursion level are gone) with the legacy context.
      subroutine zzspkap1 ( targ, et, ref, sobs, abcorr, starg, lt )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: targ  !! Target body.
      real(wp) :: et  !! Observer epoch.
      character(*) :: ref  !! Inertial reference frame of observer's state.
      real(wp) :: sobs     ( 6 )  !! State of observer wrt. solar system barycenter.
      character(*) :: abcorr  !! Aberration correction flag.
      real(wp) :: starg    ( 6 )  !! State of target.
      real(wp) :: lt  !! One way light time between observer and target.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: sobs is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%spkapp ( targ, et, ref, (sobs), abcorr, starg, lt )
      end subroutine zzspkap1

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zztanslv with the legacy context.
      subroutine zztanslv ( udcond, udstep, udrefn, &
                            cstep,  step,   start,  finish, &
                            tol,    result, points, endflg  )
      use pepper_kinds, only: ip, wp
      implicit none
      logical :: cstep  !! Logical indicating constant step size.
      real(wp) :: step  !! Constant step size for finding geometric events.
      real(wp) :: start  !! Beginning of the search interval.
      real(wp) :: finish  !! End of the search interval.
      real(wp) :: tol  !! Maximum error in detection of state transitions.
      real(wp) :: result ( lbcell : * )  !! SPICE window containing results.
      real(wp) :: points ( 3, * )  !! Array of points associated with transitions.
      logical :: endflg ( 2 )  !! Endpoint transition flags.
      procedure(legacy_zztan_cond) :: udcond  !! the zztan_cond routine
      procedure(legacy_gf_step) :: udstep  !! the gf_step routine
      procedure(legacy_gf_refine) :: udrefn  !! the gf_refine routine
      type(pepper_context), pointer :: ctx
      real(wp), allocatable :: res(:), pts(:,:)
      integer(ip) :: n, room
      procedure(legacy_zztan_cond), pointer :: outer_udcond  !! the outer call's routine
      procedure(legacy_gf_step), pointer :: outer_udstep  !! the outer call's routine
      procedure(legacy_gf_refine), pointer :: outer_udrefn  !! the outer call's routine
      ctx => legacy_context()
      outer_udcond => current_zztan_cond
      current_zztan_cond => udcond
      outer_udstep => current_gf_step
      current_gf_step => udstep
      outer_udrefn => current_gf_refine
      current_gf_refine => udrefn
      ! PEPPER: the context routine grows RESULT and POINTS (allocatable);
      ! this interface keeps SPICELIB's fixed arrays: the cell is copied
      ! in and out, and ROOM gives SPICELIB's errors for lack of room.
      room = nint ( result(-1), ip )
      allocate ( res(lbcell:room), pts(3,max(room,1)) )
      res = result(lbcell:room)
      pts = 0.0_wp
      call pepper_zztanslv ( ctx, bridge_zztan_cond, bridge_gf_step, bridge_gf_refine, cstep, step, start, finish, tol, res, pts, endflg, room )
      n = nint ( res(0), ip )
      if ( n > room .and. .not. ctx%failed() ) then
         call ctx%setmsg ( 'The result has # elements; the size of the cell is #.' )
         call ctx%errint ( '#', n )
         call ctx%errint ( '#', room )
         call ctx%sigerr ( 'SPICE(WINDOWEXCESS)' )
      end if
      n = min ( n, room )
      result(lbcell:0) = res(lbcell:0)
      result(0) = real ( n, wp )
      result(1:n) = res(1:n)
      points(:,1:min(n,size(pts,2,kind=ip))) = pts(:,1:min(n,size(pts,2,kind=ip)))
      current_zztan_cond => outer_udcond
      current_gf_step => outer_udstep
      current_gf_refine => outer_udrefn
      end subroutine zztanslv

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zztogrid with the legacy context.
       subroutine zztogrid ( modxyz, voxori, voxsiz, grdxyz )
       use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: voxsiz  !! Voxel size in model coordinates.
      real(wp) :: modxyz  ( 3 )  !! Coordinates of point in model coordinates.
      real(wp) :: voxori  ( 3 )  !! Origin of voxel grid in model coordinates.
      real(wp) :: grdxyz  ( 3 )  !! Coordinates of point in voxel grid coordinates.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: modxyz, voxori are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zztogrid ( ctx, (modxyz), (voxori), voxsiz, grdxyz )
      end subroutine zztogrid

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzwind with the legacy context.
      integer(ip) function zzwind ( plane, n, vertcs, point )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: plane  ( ubpl )  !! A SPICELIB plane.
      integer(ip) :: n  !! Number of vertices of polygon.
      real(wp) :: vertcs ( 3, * )  !! Vertices of polygon.
      real(wp) :: point  ( 3 )  !! Point in PLANE.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzwind = pepper_zzwind ( ctx, plane, n, vertcs, point )
      end function zzwind

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzwind2d with the legacy context.
      integer(ip) function zzwind2d ( n, vertcs, point )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: n  !! Number of vertices of polygon.
      real(wp) :: vertcs ( 2, n )  !! Vertices of polygon.
      real(wp) :: point  ( 2 )  !! Point in PLANE.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzwind2d = pepper_zzwind2d ( ctx, n, vertcs, point )
      end function zzwind2d

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to planetographic coordinates.
      !>
      !> Legacy interface: calls ctx%dpgrdr with the legacy context.
      subroutine dpgrdr ( body, x, y, z, re, f, jacobi )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: body  !! Body with which coordinate system is associated.
      real(wp) :: x  !! X-coordinate of point.
      real(wp) :: y  !! Y-coordinate of point.
      real(wp) :: z  !! Z-coordinate of point.
      real(wp) :: re  !! Equatorial radius of the reference spheroid.
      real(wp) :: f  !! Flattening coefficient.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dpgrdr ( body, x, y, z, re, f, jacobi )
      end subroutine dpgrdr

      !> Compute the Jacobian matrix of the transformation from
      !> planetographic to rectangular coordinates.
      !>
      !> Legacy interface: calls ctx%drdpgr with the legacy context.
      subroutine drdpgr ( body, lon, lat, alt, re, f, jacobi )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: body  !! Name of body with which coordinates are associated.
      real(wp) :: lon  !! Planetographic longitude of a point (radians).
      real(wp) :: lat  !! Planetographic latitude of a point (radians).
      real(wp) :: alt  !! Altitude of a point above reference spheroid.
      real(wp) :: re  !! Equatorial radius of the reference spheroid.
      real(wp) :: f  !! Flattening coefficient.
      real(wp) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%drdpgr ( body, lon, lat, alt, re, f, jacobi )
      end subroutine drdpgr

      !> Calculate the time derivative of the separation angle between
      !> two input states, S1 and S2.
      !>
      !> Legacy interface: calls ctx%dvsep with the legacy context.
      real(wp) function dvsep ( s1, s2 )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: s1 (6)  !! State vector of the first body.
      real(wp) :: s2 (6)  !! State vector of the second body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      dvsep = ctx%dvsep ( s1, s2 )
      end function dvsep

      !> Compute a set of points on the umbral or penumbral terminator of
      !> a specified target body, where the target shape is modeled as an
      !> ellipsoid.
      !>
      !> Legacy interface: calls ctx%edterm with the legacy context.
      subroutine edterm ( trmtyp, source, target, et, fixref, abcorr, obsrvr, npts, trgepc, obspos, trmpts )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: trmtyp  !! Terminator type.
      character(*) :: source  !! Light source.
      character(*) :: target  !! Target body.
      real(wp) :: et  !! Observation epoch.
      character(*) :: fixref  !! Body-fixed frame associated with target.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Observer.
      integer(ip) :: npts  !! Number of points in terminator set.
      real(wp) :: trgepc  !! Epoch associated with target center.
      real(wp) :: obspos ( 3 )  !! Position of observer in body-fixed frame.
      real(wp) :: trmpts ( 3, npts )  !! Terminator point set.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%edterm ( trmtyp, source, target, et, fixref, abcorr, obsrvr, npts, trgepc, obspos, trmpts )
      end subroutine edterm

      !> Parse the "lines" of a two-line element set, returning the
      !> elements in units suitable for use in SPICE software.
      !>
      !> Legacy interface: calls ctx%getelm with the legacy context.
      subroutine getelm ( frstyr, lines, epoch, elems )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: frstyr  !! Year of earliest representable two-line elements.
      character(*) :: lines ( 2  )  !! A pair of "lines" containing two-line elements.
      real(wp) :: epoch  !! The epoch of the elements in seconds past J2000.
      real(wp) :: elems ( 10 )  !! The elements converted to SPICE units.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%getelm ( frstyr, lines, epoch, elems )
      end subroutine getelm

      !> Return the field-of-view (FOV) parameters for a specified
      !> instrument. The instrument is specified by name.
      !>
      !> Legacy interface: calls ctx%getfvn with the legacy context.
      subroutine getfvn ( inst, room, shape, frame, bsight, n, bounds )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: inst  !! Name of an instrument.
      integer(ip) :: room  !! Maximum number of vectors that can be returned.
      character(*) :: shape  !! Instrument FOV shape.
      character(*) :: frame  !! Name of the frame in which FOV vectors are defined.
      real(wp) :: bsight ( 3    )  !! Boresight vector.
      integer(ip) :: n  !! Number of boundary vectors returned.
      real(wp) :: bounds ( 3, * )  !! FOV boundary vectors.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: inst is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%getfvn ( (inst), room, shape, frame, bsight, n, bounds )
      end subroutine getfvn

      !> Scan a string from a specified starting position for the
      !> end of a decimal number.
      !>
      !> Legacy interface: calls ctx%lx4dec with the legacy context.
      subroutine lx4dec ( string, first, last, nchar )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! any character string
      integer(ip) :: first  !! first character to scan from in STRING
      integer(ip) :: last  !! last character that is part of a decimal number
      integer(ip) :: nchar  !! number of characters in the decimal number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lx4dec ( string, first, last, nchar )
      end subroutine lx4dec

      !> Determine the set of osculating conic orbital elements that
      !> corresponds to the state (position, velocity) of a body at some
      !> epoch. In additional to the classical elements, return the true
      !> anomaly, semi-major axis, and period, if applicable.
      !>
      !> Legacy interface: calls ctx%oscltx with the legacy context.
      subroutine oscltx ( state, et, mu, elts )
      use pepper_kinds, only: wp
      use pepper_constants, only: dpmax
      use pepper_inc_oscltx
      implicit none
      real(wp) :: state  ( 6 )  !! State of body at epoch of elements.
      real(wp) :: et  !! Epoch of elements.
      real(wp) :: mu  !! Gravitational parameter (GM) of primary body.
      real(wp) :: elts   ( *  )  !! Extended set of classical conic elements.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: state is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%oscltx ( (state), et, mu, elts )
      end subroutine oscltx

      !> Convert planetographic coordinates to rectangular coordinates.
      !>
      !> Legacy interface: calls ctx%pgrrec with the legacy context.
      subroutine pgrrec ( body, lon, lat, alt, re, f, rectan )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: body  !! Body with which coordinate system is associated.
      real(wp) :: lon  !! Planetographic longitude of a point (radians).
      real(wp) :: lat  !! Planetographic latitude of a point (radians).
      real(wp) :: alt  !! Altitude of a point above reference spheroid.
      real(wp) :: re  !! Equatorial radius of the reference spheroid.
      real(wp) :: f  !! Flattening coefficient.
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of the point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pgrrec ( body, lon, lat, alt, re, f, rectan )
      end subroutine pgrrec

      !> Convert rectangular coordinates to planetographic coordinates.
      !>
      !> Legacy interface: calls ctx%recpgr with the legacy context.
      subroutine recpgr ( body, rectan, re, f, lon, lat, alt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: body  !! Body with which coordinate system is associated.
      real(wp) :: rectan ( 3 )  !! Rectangular coordinates of a point.
      real(wp) :: re  !! Equatorial radius of the reference spheroid.
      real(wp) :: f  !! Flattening coefficient.
      real(wp) :: lon  !! Planetographic longitude of the point (radians).
      real(wp) :: lat  !! Planetographic latitude of the point (radians).
      real(wp) :: alt  !! Altitude of the point above reference spheroid.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: rectan is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%recpgr ( body, (rectan), re, f, lon, lat, alt )
      end subroutine recpgr

      !> Find semi-axis vectors of an ellipse generated by two arbitrary
      !> three-dimensional vectors.
      !>
      !> Legacy interface: calls ctx%saelgv with the legacy context.
      subroutine saelgv ( vec1, vec2, smajor, sminor )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: vec1   ( 3 )  !! Two vectors used to generate an ellipse.
      real(wp) :: vec2   ( 3 )  !! Two vectors used to generate an ellipse.
      real(wp) :: smajor ( 3 )  !! Semi-major axis of ellipse.
      real(wp) :: sminor ( 3 )  !! Semi-minor axis of ellipse.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vec1, vec2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%saelgv ( (vec1), (vec2), smajor, sminor )
      end subroutine saelgv

      !> Parse a character representation of spacecraft clock time and
      !> encode it as a double precision number.
      !>
      !> Legacy interface: calls ctx%scpars with the legacy context.
      subroutine scpars ( sc, sclkch, error, msg, sclkdp )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sclk
      implicit none
      integer(ip) :: sc  !! NAIF spacecraft identification code.
      character(*) :: sclkch  !! Character representation of a spacecraft clock.
      logical :: error  !! Flag to indicate if string parsed correctly.
      character(*) :: msg  !! Error message if string did not parse.
      real(wp) :: sclkdp  !! Encoded representation of the clock count.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: sclkch is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%scpars ( sc, (sclkch), error, msg, sclkdp )
      end subroutine scpars

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine SUBPNT. This routine is supported for purposes of
      !> backward compatibility only.
      !>
      !> Legacy interface: calls ctx%subpt with the legacy context.
      subroutine subpt ( method, target, et, abcorr, obsrvr, spoint, alt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Name of observing body.
      real(wp) :: spoint ( 3 )  !! Sub-observer point on the target body.
      real(wp) :: alt  !! Altitude of the observer above the target body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%subpt ( method, target, et, abcorr, obsrvr, spoint, alt )
      end subroutine subpt

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine SUBSLR. This routine is supported for purposes of
      !> backward compatibility only.
      !>
      !> Legacy interface: calls ctx%subsol with the legacy context.
      subroutine subsol ( method, target, et, abcorr, obsrvr, spoint )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Name of observing body.
      real(wp) :: spoint ( 3 )  !! Sub-solar point on the target body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%subsol ( method, target, et, abcorr, obsrvr, spoint )
      end subroutine subsol

      !> Find the state (position and velocity) of the surface intercept
      !> defined by a specified ray, ray velocity, and ellipsoid.
      !>
      !> Legacy interface: calls ctx%surfpv with the legacy context.
      subroutine surfpv ( stvrtx, stdir, a, b, c, stx, found )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: stvrtx ( 6 )  !! State of ray's vertex.
      real(wp) :: stdir  ( 6 )  !! State of ray's direction vector.
      real(wp) :: a  !! Length of ellipsoid semi-axis along the x-axis.
      real(wp) :: b  !! Length of ellipsoid semi-axis along the y-axis.
      real(wp) :: c  !! Length of ellipsoid semi-axis along the z-axis.
      real(wp) :: stx    ( 6 )  !! State of surface intercept.
      logical :: found  !! Flag indicating whether intercept state was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: stvrtx, stdir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%surfpv ( (stvrtx), (stdir), a, b, c, stx, found )
      end subroutine surfpv

      !> Write a line of text to standard output.
      !>
      !> Legacy interface: calls ctx%tostdo with the legacy context.
      subroutine tostdo ( line )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: line  !! is a line of text to be written to standard output
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%tostdo ( line )
      end subroutine tostdo

      !> Compute the angular separation in radians between two spherical
      !> or point objects.
      !>
      !> Legacy interface: calls ctx%trgsep with the legacy context.
      real(wp) function trgsep ( et, &
                                         targ1,  shape1, frame1, &
                                         targ2,  shape2, frame2, &
                                         obsrvr, abcorr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzabcorr
      use pepper_inc_zzdyn
      implicit none
      real(wp) :: et  !! Ephemeris seconds past J2000 TDB.
      character(*) :: targ1  !! First target body name.
      character(*) :: shape1  !! First target body shape.
      character(*) :: frame1  !! Reference frame of first target.
      character(*) :: targ2  !! Second target body name.
      character(*) :: shape2  !! First target body shape.
      character(*) :: frame2  !! Reference frame of second target.
      character(*) :: obsrvr  !! Observing body name.
      character(*) :: abcorr  !! Aberration corrections flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      trgsep = ctx%trgsep ( et, targ1, shape1, frame1, targ2, shape2, frame2, obsrvr, abcorr )
      end function trgsep

      !> Return .TRUE. if the derivative of the callback function UDFUNC
      !> at a given abscissa value is negative.
      !>
      !> Legacy interface: calls ctx%uddc with the legacy context.
      subroutine uddc ( udfunc, x, dx, isdecr )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: x  !! Independent variable of UDFUNC.
      real(wp) :: dx  !! Interval from X for derivative calculation.
      logical :: isdecr  !! Boolean indicating if the derivative is negative.
      procedure(legacy_gf_scalar) :: udfunc  !! the gf_scalar routine
      type(pepper_context), pointer :: ctx
      procedure(legacy_gf_scalar), pointer :: outer_udfunc  !! the outer call's routine
      ctx => legacy_context()
      outer_udfunc => current_gf_scalar
      current_gf_scalar => udfunc
      call ctx%uddc ( bridge_gf_scalar, x, dx, isdecr )
      current_gf_scalar => outer_udfunc
      end subroutine uddc

      !> Write the value of a variable in a double precision symbol
      !> table to a NAIF ASCII kernel file.
      !>
      !> Legacy interface: calls ctx%wrkvar with the legacy context.
      subroutine wrkvar ( unit, name, dirctv, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: unit  !! Output logical unit.
      character(*) :: name  !! Name of the variable.
      character(*) :: dirctv  !! Kernel directive: '=' or '+='.
      character(*) :: tabsym     ( lbcell:* )  !! Double precision symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Double precision symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Double precision symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wrkvar ( unit, name, dirctv, tabsym, tabptr, tabval )
      end subroutine wrkvar

      !> Return a list of voxels that a given ray intersects in a given
      !> voxel grid.
      !>
      !> Legacy interface: calls ctx%xdda with the legacy context.
      subroutine xdda ( vertex, raydir, grdext, maxnvx, nvx, voxlst )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: vertex ( 3 )  !! Voxel grid coordinates of ray's vertex.
      real(wp) :: raydir ( 3 )  !! Direction vector of ray.
      integer(ip) :: grdext ( 3 )  !! Dimensions of grid in voxel units.
      integer(ip) :: maxnvx  !! Maximum value of VOXLST.
      integer(ip) :: nvx  !! Number of voxels in the VOXLST list.
      integer(ip) :: voxlst ( 3, * )  !! List of voxels intersected by ray.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: grdext is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%xdda ( vertex, raydir, (grdext), maxnvx, nvx, voxlst )
      end subroutine xdda

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzcnquad with the legacy context.
      subroutine zzcnquad ( a, b, c, ub, n, r1, r2 )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a  !! Coefficients of quadratic equation.
      real(wp) :: b  !! Coefficients of quadratic equation.
      real(wp) :: c  !! Coefficients of quadratic equation.
      real(wp) :: ub  !! Upper bound on magnitude of roots.
      integer(ip) :: n  !! Number of real roots within bound.
      real(wp) :: r1  !! Root of smaller absolute value.
      real(wp) :: r2  !! Root of larger absolute value.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzcnquad ( ctx, a, b, c, ub, n, r1, r2 )
      end subroutine zzcnquad

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdsksph with the legacy context.
      subroutine zzdsksph ( bodyid, nsurf, srflst, minrad, maxrad )
      use pepper_kinds, only: ip, wp
      use pepper_inc_errhnd
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: bodyid  !! ID code of target body.
      integer(ip) :: nsurf  !! Number of IDs in surface list.
      integer(ip) :: srflst ( * )  !! List of surface IDs.
      real(wp) :: minrad  !! Radius of inner bounding sphere for body.
      real(wp) :: maxrad  !! Radius of outer bounding sphere for body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdsksph ( ctx, bodyid, nsurf, srflst, minrad, maxrad )
      end subroutine zzdsksph

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzfovaxi with the legacy context.
      subroutine zzfovaxi ( inst, n, bounds, axis )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: inst  !! Instrument name.
      integer(ip) :: n  !! Number of FOV boundary vectors.
      real(wp) :: bounds ( 3, n )  !! FOV boundary vectors.
      real(wp) :: axis   ( 3 )  !! Instrument FOV axis vector.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: bounds is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzfovaxi ( ctx, inst, n, (bounds), axis )
      end subroutine zzfovaxi

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzinlat0 with the legacy context.
      subroutine zzinlat0 ( r, lon, lat, bounds, exclud, inside )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dsktol
      implicit none
      real(wp) :: r  !! Radius of point.
      real(wp) :: lon  !! Planetocentric longitude of point.
      real(wp) :: lat  !! Planetocentric latitude of point.
      real(wp) :: bounds ( 2, 3 )  !! Coordinate bounds of element.
      integer(ip) :: exclud  !! Index of coordinate to exclude from test.
      logical :: inside  !! Flag indicating whether point is in element.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzinlat0 ( ctx, r, lon, lat, bounds, exclud, inside )
      end subroutine zzinlat0

      !> Entrance for deep space initialization.  This section is called
      !> once per element set.
      !>
      !> Legacy interface: calls zz routine zzdpinit with the legacy context.
      subroutine zzdpinit ( ao, xlldot, omgdot, xnodot, xnodp, elems )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: ao  !! Original semimajor axis
      real(wp) :: elems ( 10 )  !! Array of orbit elements
      real(wp) :: omgdot  !! Time rate of change of argument of perigee
      real(wp) :: xlldot  !! Time rate of change of XLL
      real(wp) :: xnodot  !! Time rate of change of mean motion
      real(wp) :: xnodp  !! Original mean motion
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdpinit ( ctx, ao, xlldot, omgdot, xnodot, xnodp, elems )
      end subroutine zzdpinit

      !> Entrance for deep space secular effects
      !>
      !> Legacy interface: calls zz routine zzdpsec with the legacy context.
      subroutine zzdpsec ( xll, omgasm, xnodes, em, xinc, xn, t, elems, omgdot )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: elems ( 10 )  !! The two line elements array
      real(wp) :: em  !! Perturbed eccentricity of the orbit at time T
      real(wp) :: omgasm  !! Perturbed argument of perigee
      real(wp) :: omgdot  !! Time derivative of the argument of perigee
      real(wp) :: t  !! Time to calculate perturbation
      real(wp) :: xinc  !! Perturbed inclination of the orbit plane at time T
      real(wp) :: xll  !! Long-period periodic term
      real(wp) :: xn  !! Perturbed mean motion of the orbit at time T
      real(wp) :: xnodes  !! Perturbed argument of ascending node
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: elems is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzdpsec ( ctx, xll, omgasm, xnodes, em, xinc, xn, t, (elems), omgdot )
      end subroutine zzdpsec

      !> Entrances for lunar-solar periodics
      !>
      !> Legacy interface: calls zz routine zzdpper with the legacy context.
      subroutine zzdpper ( t, em, xinc, omgasm, xnodes, xll )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: em  !! Perturbed eccentricity of the orbit at time T
      real(wp) :: omgasm  !! Perturbed argument of perigee
      real(wp) :: t  !! Time to calculate perturbations
      real(wp) :: xinc  !! Perturbed inclination of the orbit plane at time T
      real(wp) :: xll  !! Long-period periodic term
      real(wp) :: xnodes  !! Perturbed argument of ascending node
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdpper ( ctx, t, em, xinc, omgasm, xnodes, xll )
      end subroutine zzdpper

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzpdcmpl with the legacy context.
      subroutine zzpdcmpl ( re, f, p, lat, rel )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: re  !! Equatorial radius.
      real(wp) :: f  !! Flattening coefficient.
      real(wp) :: p   ( 3 )  !! Three-dimensional point.
      real(wp) :: lat  !! Planetodetic latitude.
      integer(ip) :: rel  !! Relation code.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzpdcmpl ( ctx, re, f, p, lat, rel )
      end subroutine zzpdcmpl

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzpdpltc with the legacy context.
      logical function zzpdpltc ( re, f, p, lat )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: re  !! Equatorial radius.
      real(wp) :: f  !! Flattening coefficient.
      real(wp) :: p   ( 3 )  !! Three-dimensional point.
      real(wp) :: lat  !! Planetodetic latitude.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzpdpltc = pepper_zzpdpltc ( ctx, re, f, p, lat )
      end function zzpdpltc

      !> Create a bounding box for a planetodetic volume element.
      !>
      !> Legacy interface: calls zz routine zzpdtbox with the legacy context.
      subroutine zzpdtbox ( bounds, corpar, center, &
                            lr,     lt,     lz,     radius )
      use pepper_kinds, only: wp
      use pepper_inc_dskdsc
      use pepper_inc_dsktol
      implicit none
      real(wp) :: bounds ( 2, 3 )  !! Array of coordinate bounds for segment.
      real(wp) :: corpar ( * )  !! Array containing coordinate parameters.
      real(wp) :: center ( 3 )  !! Center of bounding box.
      real(wp) :: lr  !! Extent of box in the cylindrical radial direction.
      real(wp) :: lt  !! Extent of box in the tangential direction.
      real(wp) :: lz  !! Extent of box in the Z direction.
      real(wp) :: radius  !! Radius of box.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: bounds is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzpdtbox ( ctx, (bounds), corpar, center, lr, lt, lz, radius )
      end subroutine zzpdtbox

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzraybox with the legacy context.
      subroutine zzraybox ( vertex, raydir, boxori, extent, xpt, found )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: vertex ( 3 )  !! Vertex of ray.
      real(wp) :: raydir ( 3 )  !! Direction vector of ray.
      real(wp) :: boxori ( 3 )  !! Box origin: corner having minimum coordinates.
      real(wp) :: extent ( 3 )  !! Box extents in X, Y, Z directions.
      real(wp) :: xpt    ( 3 )  !! Ray-box intercept.
      logical :: found  !! Flag indicating whether intercept was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir, boxori, extent are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzraybox ( ctx, (vertex), (raydir), (boxori), (extent), xpt, found )
      end subroutine zzraybox

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsrfker with the legacy context.
      !> PEPPER's ZZSRFKER fills a surface_names record, with no limit on the
      !> number or length of the names; this keeps SPICELIB's interface: it
      !> signals SPICE(TOOMANYSURFACES) when the kernel variables do not fit
      !> its MXNSRF arrays and copies the names, IDs and bodies into them
      !> (the hash arrays are not set).
      subroutine zzsrfker ( kernam, nornam, kersid, kerbid, extker, nkvar, snmhls, snmpol, snmidx, sidhls, sidpol, sididx )
      use pepper_states, only: surface_names
      use pepper_kinds, only: ip
      use pepper_inc_srftrn
      use pepper_inc_zzsrftrn
      implicit none
      character(sfnmln) :: kernam (          * )  !! Array of surface names from kernel pool.
      character(sfnmln) :: nornam (          * )  !! Array of normalized surface names.
      integer(ip) :: kersid (          * )  !! Array of surface ID codes from kernel pool.
      integer(ip) :: kerbid (          * )  !! Array of body ID codes from kernel pool.
      logical :: extker  !! Logical flag indicating kernel data are present.
      integer(ip) :: nkvar  !! Number of surface name, code, body tuples.
      integer(ip) :: snmhls (          * )  !! Surface name-based hash head node pointer list
      integer(ip) :: snmpol ( lbsngl : * )  !! Surface name-based hash node collision list
      integer(ip) :: snmidx (          * )  !! Surface name-based hash index storage array
      integer(ip) :: sidhls (          * )  !! Surface ID-based hash head node pointer list
      integer(ip) :: sidpol ( lbsngl : * )  !! Surface ID-based hash node collision list
      integer(ip) :: sididx (          * )  !! Surface ID-based hash index storage array
      type(pepper_context), pointer :: ctx
      type(surface_names) :: srf
      character(1) :: type(3)
      integer(ip) :: n(3), i
      logical :: found(3)
      ctx => legacy_context()
      if ( ctx%failed() ) return
      extker = .false.
      nkvar  = 0
      call ctx%dtpool ( 'NAIF_SURFACE_NAME', found(1), n(1), type(1) )
      call ctx%dtpool ( 'NAIF_SURFACE_CODE', found(2), n(2), type(2) )
      call ctx%dtpool ( 'NAIF_SURFACE_BODY', found(3), n(3), type(3) )
      if (       all ( found ) .and. type(1) == 'C' .and. type(2) == 'N' &
           .and. type(3) == 'N' .and. any ( n > mxnsrf ) ) then
         call ctx%setmsg ( 'Surface mapping kernel variable sizes ' &
         //            'are: # = #; # = #; # = #. Maximum ' &
         //            'allowed size is #.'                    )
         call ctx%errch  ( '#', 'NAIF_SURFACE_NAME' )
         call ctx%errint ( '#', n(1)                )
         call ctx%errch  ( '#', 'NAIF_SURFACE_CODE' )
         call ctx%errint ( '#', n(2)                )
         call ctx%errch  ( '#', 'NAIF_SURFACE_BODY' )
         call ctx%errint ( '#', n(3)                )
         call ctx%errint ( '#', mxnsrf              )
         call ctx%sigerr ( 'SPICE(TOOMANYSURFACES)' )
         return
      end if
      call pepper_zzsrfker ( ctx, srf )
      if ( ctx%failed() ) return
      extker = srf%extker
      nkvar  = srf%n
      do i = 1, nkvar
         kernam(i) = srf%names(i)%s
         nornam(i) = srf%nornam(i)%s
         kersid(i) = srf%ids(i)
         kerbid(i) = srf%bodies(i)
      end do
      end subroutine zzsrfker

      !> Form a SPICE ellipse from a center vector and two generating
      !> vectors.
      !>
      !> Legacy interface: calls ctx%cgv2el with the legacy context.
      subroutine cgv2el ( center, vec1, vec2, ellips )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: center (    3 )  !! Center and two generating vectors for an ellipse.
      real(wp) :: vec1   (    3 )  !! Center and two generating vectors for an ellipse.
      real(wp) :: vec2   (    3 )  !! Center and two generating vectors for an ellipse.
      real(wp) :: ellips ( ubel )  !! The SPICE ellipse defined by the input vectors.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: center, vec1, vec2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%cgv2el ( (center), (vec1), (vec2), ellips )
      end subroutine cgv2el

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine EVSGP4. *ALL* TLE evaluations should use that routine.
      !> NAIF supports DPSPCE only for backward compatibility.
      !>
      !> Legacy interface: calls ctx%dpspce with the legacy context.
      subroutine dpspce ( et, geophs, elems, state )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Time for state evaluation in seconds past ephemeris epoch J2000.
      real(wp) :: geophs ( 8  )  !! The array of geophysical constants.
      real(wp) :: elems  ( 10 )  !! Array of orbit elements.
      real(wp) :: state  ( 6  )  !! State vector at ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: geophs, elems are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%dpspce ( et, (geophs), (elems), state )
      end subroutine dpspce

      !> Compute the local solar time for a given ephemeris epoch ET
      !> for an object on the surface of a body at a specified longitude.
      !>
      !> Legacy interface: calls ctx%et2lst with the legacy context.
      subroutine et2lst ( et, body, lon, type, hr, mn, sc, time, ampm )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Epoch in seconds past J2000 epoch
      integer(ip) :: body  !! ID-code of the body of interest
      real(wp) :: lon  !! Longitude of surface point (RADIANS)
      character(*) :: type  !! Type of longitude 'PLANETOCENTRIC', etc.
      integer(ip) :: hr  !! Local hour on a "24 hour" clock
      integer(ip) :: mn  !! Minutes past the hour
      integer(ip) :: sc  !! Seconds past the minute
      character(*) :: time  !! String giving local time on 24 hour clock
      character(*) :: ampm  !! String giving time on A.M./ P.M. scale
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: type is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%et2lst ( et, body, lon, (type), hr, mn, sc, time, ampm )
      end subroutine et2lst

      !> Compute the points of intersection of a specified nappe of a cone
      !> and a line segment.
      !>
      !> Legacy interface: calls ctx%incnsg with the legacy context.
      subroutine incnsg ( apex, axis, angle, endpt1, endpt2, nxpts, xpt1, xpt2 )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: apex   ( 3 )  !! Apex of cone.
      real(wp) :: axis   ( 3 )  !! Axis of cone.
      real(wp) :: angle  !! Angle of cone.
      real(wp) :: endpt1 ( 3 )  !! Endpoints of line segment.
      real(wp) :: endpt2 ( 3 )  !! Endpoints of line segment.
      integer(ip) :: nxpts  !! Number of intersection points.
      real(wp) :: xpt1   ( 3 )  !! First intersection point, if it exists.
      real(wp) :: xpt2   ( 3 )  !! Second intersection point, if it exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: apex, axis, endpt1, endpt2 are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%incnsg ( (apex), (axis), angle, (endpt1), (endpt2), nxpts, xpt1, xpt2 )
      end subroutine incnsg

      !> Scan a string from a specified starting position for the
      !> end of a number.
      !>
      !> Legacy interface: calls ctx%lx4num with the legacy context.
      subroutine lx4num ( string, first, last, nchar )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! any character string
      integer(ip) :: first  !! first character to scan from in STRING
      integer(ip) :: last  !! last character that is part of a number
      integer(ip) :: nchar  !! number of characters in the number.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%lx4num ( string, first, last, nchar )
      end subroutine lx4num

      !> Read the next variable from a SPICE ASCII kernel file into a
      !> double precision symbol table.
      !>
      !> Legacy interface: calls ctx%rdkvar with the legacy context.
      subroutine rdkvar ( tabsym, tabptr, tabval, name, eof )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: tabsym     ( lbcell:* )  !! Symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Symbol table.
      character(*) :: name  !! Name of the variable.
      logical :: eof  !! End of file indicator.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rdkvar ( tabsym, tabptr, tabval, name, eof )
      end subroutine rdkvar

      !> Transform a state between coordinate systems.
      !>
      !> Legacy interface: calls ctx%xfmsta with the legacy context.
      subroutine xfmsta ( istate, icosys, ocosys, body, ostate )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      real(wp) :: istate(6)  !! Input state.
      character(*) :: icosys  !! Current (input) coordinate system.
      character(*) :: ocosys  !! Desired (output) coordinate system.
      character(*) :: body  !! Name or NAIF ID of body with which coordinates are associated (if applicable).
      real(wp) :: ostate(6)  !! Converted output state.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: istate is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%xfmsta ( (istate), icosys, ocosys, body, ostate )
      end subroutine xfmsta

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzilusta with the legacy context.
      subroutine zzilusta ( method, target, illum,  et, &
                            fixref, abcorr, obsrvr, spoint, &
                            normal, phssta, incsta, emista  )
      use pepper_kinds, only: wp
      use pepper_inc_zzabcorr
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body associated with surface point.
      character(*) :: illum  !! Name of illumination source.
      real(wp) :: et  !! Observation epoch (TDB).
      character(*) :: fixref  !! Body-fixed reference frame.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Name of observer.
      real(wp) :: spoint ( 3 )  !! Surface point.
      real(wp) :: normal ( 3 )  !! Outward normal vector at surface point.
      real(wp) :: phssta ( 2 )  !! Phase angle state.
      real(wp) :: incsta ( 2 )  !! Solar incidence angle state.
      real(wp) :: emista ( 2 )  !! Emission angle state.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: spoint, normal are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzilusta ( ctx, method, target, illum, et, fixref, abcorr, obsrvr, (spoint), &
         (normal), phssta, incsta, emista )
      end subroutine zzilusta

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzinlat with the legacy context.
      subroutine zzinlat ( p, bounds, margin, exclud, inside )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: halfpi, twopi
      use pepper_inc_dsktol
      implicit none
      real(wp) :: p      ( 3 )  !! Input point.
      real(wp) :: bounds ( 2, 3 )  !! Coordinate bounds of element.
      real(wp) :: margin  !! Margin used for inclusion testing.
      integer(ip) :: exclud  !! Index of coordinate to exclude from test.
      logical :: inside  !! Flag indicating whether point is in element.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzinlat ( ctx, p, bounds, margin, exclud, inside )
      end subroutine zzinlat

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzinpdt0 with the legacy context.
      subroutine zzinpdt0 ( p, lon, bounds, corpar, exclud, inside )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi
      use pepper_inc_dsktol
      implicit none
      real(wp) :: p      ( 3 )  !! Cartesian coordinates of point.
      real(wp) :: lon  !! Planetocentric longitude of point.
      real(wp) :: bounds ( 2, 3 )  !! Coordinate bounds of element.
      real(wp) :: corpar ( * )  !! Coordinate parameters.
      integer(ip) :: exclud  !! Index of coordinate to exclude from test.
      logical :: inside  !! Flag indicating whether point is in element.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzinpdt0 ( ctx, p, lon, bounds, corpar, exclud, inside )
      end subroutine zzinpdt0

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzrytrec with the legacy context.
      subroutine zzrytrec ( vertex, raydir, bounds, &
                            margin, nxpts,  xpt    )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: vertex ( 3 )  !! Ray's vertex.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      real(wp) :: bounds ( 2, 3 )  !! Bounds of rectangular volume element.
      real(wp) :: margin  !! Margin used for element expansion.
      integer(ip) :: nxpts  !! Number of intercept points.
      real(wp) :: xpt    ( 3 )  !! Intercept.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir, bounds are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzrytrec ( ctx, (vertex), (raydir), (bounds), margin, nxpts, xpt )
      end subroutine zzrytrec

      !> Create a bounding box for a DSK segment volume element.
      !> The outputs are the box's center and radius. The center
      !> is relative to the segment's frame center.
      !>
      !> Legacy interface: calls zz routine zzsegbox with the legacy context.
      subroutine zzsegbox ( dskdsc, boxctr, maxr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dskdsc
      real(wp) :: dskdsc ( * )  !! DSK segment descriptor.
      real(wp) :: boxctr ( 3 )  !! Center of bounding box.
      real(wp) :: maxr  !! Radius of box.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzsegbox ( ctx, dskdsc, boxctr, maxr )
      end subroutine zzsegbox

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsrfn2c with the legacy context.
      subroutine zzsrfn2c ( srfnam, bodyid, surfid, found )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_inc_srftrn
      use pepper_inc_zzctr
      use pepper_inc_zzsrftrn
      implicit none
      integer(ip) :: bodyid  !! Body ID code.
      character(*) :: srfnam  !! Surface name.
      integer(ip) :: surfid  !! Surface ID code.
      logical :: found  !! Found flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzsrfn2c ( ctx, srfnam, bodyid, surfid, found )
      end subroutine zzsrfn2c

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsrfc2n with the legacy context.
      subroutine zzsrfc2n ( surfid, bodyid, srfnam, found )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_inc_srftrn
      use pepper_inc_zzctr
      use pepper_inc_zzsrftrn
      implicit none
      integer(ip) :: bodyid  !! Body ID code.
      character(*) :: srfnam  !! Surface name.
      integer(ip) :: surfid  !! Surface ID code.
      logical :: found  !! Found flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzsrfc2n ( ctx, surfid, bodyid, srfnam, found )
      end subroutine zzsrfc2n

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsrftrk with the legacy context.
      subroutine zzsrftrk ( usrctr, update )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_inc_srftrn
      use pepper_inc_zzctr
      use pepper_inc_zzsrftrn
      implicit none
      integer(ip) :: usrctr ( 2 )  !! User counter.
      logical :: update  !! Update flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzsrftrk ( ctx, usrctr, update )
      end subroutine zzsrftrk

      !> Find the intersection of a triaxial ellipsoid and a plane.
      !>
      !> Legacy interface: calls ctx%inedpl with the legacy context.
      subroutine inedpl ( a, b, c, plane, ellips, found )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a  !! Length of ellipsoid semi-axis lying on the x-axis.
      real(wp) :: b  !! Length of ellipsoid semi-axis lying on the y-axis.
      real(wp) :: c  !! Length of ellipsoid semi-axis lying on the z-axis.
      real(wp) :: plane  ( ubpl )  !! Plane that intersects ellipsoid.
      real(wp) :: ellips ( ubel )  !! Intersection ellipse, when FOUND is .TRUE.
      logical :: found  !! Flag indicating whether ellipse was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: plane is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%inedpl ( a, b, c, (plane), ellips, found )
      end subroutine inedpl

      !> Project an ellipse onto a plane, orthogonally.
      !>
      !> Legacy interface: calls ctx%pjelpl with the legacy context.
      subroutine pjelpl ( elin, plane, elout )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: elin  ( ubel )  !! A SPICE ellipse to be projected.
      real(wp) :: plane ( ubpl )  !! A plane onto which ELIN is to be projected.
      real(wp) :: elout ( ubel )  !! A SPICE ellipse resulting from the projection.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: elin, plane are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%pjelpl ( (elin), (plane), elout )
      end subroutine pjelpl

      !> Translate a surface ID code, together with a body ID code, to the
      !> corresponding surface name. If no such name exists, return a
      !> string representation of the surface ID code.
      !>
      !> Legacy interface: calls ctx%srfc2s with the legacy context.
      subroutine srfc2s ( code, bodyid, srfstr, isname )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: code  !! Integer surface ID code to translate to a string.
      integer(ip) :: bodyid  !! ID code of body associated with surface.
      character(*) :: srfstr  !! String corresponding to surface ID code.
      logical :: isname  !! Logical flag indicating output is a surface name.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%srfc2s ( code, bodyid, srfstr, isname )
      end subroutine srfc2s

      !> Translate a surface ID code, together with a body string, to the
      !> corresponding surface name. If no such surface name exists,
      !> return a string representation of the surface ID code.
      !>
      !> Legacy interface: calls ctx%srfcss with the legacy context.
      subroutine srfcss ( code, bodstr, srfstr, isname )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: code  !! Integer surface ID code to translate to a string.
      character(*) :: bodstr  !! Name or ID of body associated with surface.
      character(*) :: srfstr  !! String corresponding to surface ID code.
      logical :: isname  !! Flag indicating whether output is a surface name.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: bodstr is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%srfcss ( code, (bodstr), srfstr, isname )
      end subroutine srfcss

      !> Translate a surface string, together with a body string, to the
      !> corresponding surface ID code. The input strings may contain
      !> names or integer ID codes.
      !>
      !> Legacy interface: calls ctx%srfs2c with the legacy context.
      subroutine srfs2c ( srfstr, bodstr, code, found )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: srfstr  !! Surface name or ID string.
      character(*) :: bodstr  !! Body name or ID string.
      integer(ip) :: code  !! Integer surface ID code.
      logical :: found  !! Flag indicating whether surface ID was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%srfs2c ( srfstr, bodstr, code, found )
      end subroutine srfs2c

      !> Translate a surface string, together with a body ID code, to the
      !> corresponding surface ID code. The input surface string may
      !> contain a name or an integer ID code.
      !>
      !> Legacy interface: calls ctx%srfscc with the legacy context.
      subroutine srfscc ( srfstr, bodyid, code, found )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: srfstr  !! Surface name or ID string.
      integer(ip) :: bodyid  !! Body ID code.
      integer(ip) :: code  !! Integer surface ID code.
      logical :: found  !! Flag indicating whether surface ID was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%srfscc ( srfstr, bodyid, code, found )
      end subroutine srfscc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdsksba with the legacy context.
      subroutine zzdsksba ( bodyid, &
                            maxbod, stsize, btbody, btnbod, btsegp, &
                            btstsz, sthan,  stdscr, stdlad, stfree, &
                            stoff,  stctr,  strad                  )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      implicit none
      integer(ip) :: bodyid  !! Body ID code.
      integer(ip) :: maxbod  !! Maximum size of body table.
      integer(ip) :: stsize  !! Maximum size of segment table.
      integer(ip) :: btbody ( * )  !! Body table's body ID array.
      integer(ip) :: btnbod  !! Number of entries in body table.
      integer(ip) :: btsegp ( * )  !! Array of pointers from bodies to segment table.
      integer(ip) :: btstsz ( * )  !! Array of sizes of body segment lists.
      integer(ip) :: sthan  ( * )  !! Array of handles of segments.
      real(wp) :: stdscr ( dskdsz, * )  !! Array of DSK descriptors of segments.
      integer(ip) :: stdlad ( dladsz, * )  !! Array of DLA descriptors of segments.
      integer(ip) :: stfree  !! Index of first free entry in segment table.
      real(wp) :: stoff  ( 3, * )  !! Offsets of segment frame centers from body.
      real(wp) :: stctr  ( 3, * )  !! Centers of bounding spheres for segments.
      real(wp) :: strad  ( * )  !! Radii of bounding spheres for segments.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdsksba ( ctx, bodyid, maxbod, stsize, btbody, btnbod, btsegp, btstsz, sthan, stdscr, stdlad, stfree, stoff, stctr, strad )
      end subroutine zzdsksba

      !> Determine whether a specified ellipse intersects the pyramid
      !> defined by a polygonal field of view.
      !>
      !> Legacy interface: calls zz routine zzelvupy with the legacy context.
      subroutine zzelvupy ( ellips, vertex, axis, n, bounds, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_errhnd
      implicit none
      real(wp) :: ellips ( ubel )  !! A SPICELIB ellipse.
      real(wp) :: vertex ( 3 )  !! Vertex of a pyramid.
      real(wp) :: axis   ( 3 )  !! Axis of a pyramid.
      integer(ip) :: n  !! Number of boundary vectors of the pyramid.
      real(wp) :: bounds ( 3, n )  !! Boundary vectors of the pyramid.
      logical :: found  !! Flag indicating whether intersection was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzelvupy ( ctx, ellips, vertex, axis, n, bounds, found )
      end subroutine zzelvupy

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzinpdt with the legacy context.
      subroutine zzinpdt ( p, bounds, corpar, margin, exclud, inside )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsktol
      implicit none
      real(wp) :: p      ( 3 )  !! Input point.
      real(wp) :: bounds ( 2, 3 )  !! Coordinate bounds of element.
      real(wp) :: corpar ( * )  !! Coordinate system parameters.
      real(wp) :: margin  !! Margin used for inclusion testing.
      integer(ip) :: exclud  !! Index of coordinate to exclude from test.
      logical :: inside  !! Flag indicating whether point is in element.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzinpdt ( ctx, p, bounds, corpar, margin, exclud, inside )
      end subroutine zzinpdt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzrytlat with the legacy context.
      subroutine zzrytlat ( vertex, raydir, bounds, margin, nxpts, xpt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsktol
      implicit none
      real(wp) :: vertex ( 3 )  !! Ray's vertex.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      real(wp) :: bounds ( 2, 3 )  !! Bounds of latitudinal volume element.
      real(wp) :: margin  !! Margin used for element expansion.
      integer(ip) :: nxpts  !! Number of intercept points.
      real(wp) :: xpt    ( 3 )  !! Intercept.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir, bounds are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzrytlat ( ctx, (vertex), (raydir), (bounds), margin, nxpts, xpt )
      end subroutine zzrytlat

      !> Find the limb of a triaxial ellipsoid, viewed from a specified
      !> point.
      !>
      !> Legacy interface: calls ctx%edlimb with the legacy context.
      subroutine edlimb ( a, b, c, viewpt, limb )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a  !! Length of ellipsoid semi-axis lying on the x-axis.
      real(wp) :: b  !! Length of ellipsoid semi-axis lying on the y-axis.
      real(wp) :: c  !! Length of ellipsoid semi-axis lying on the z-axis.
      real(wp) :: viewpt (    3 )  !! Location of viewing point.
      real(wp) :: limb   ( ubel )  !! Limb of ellipsoid as seen from viewing point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: viewpt is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%edlimb ( a, b, c, (viewpt), limb )
      end subroutine edlimb

      !> Find nearest point on a triaxial ellipsoid to a specified line,
      !> and the distance from the ellipsoid to the line.
      !>
      !> Legacy interface: calls ctx%npedln with the legacy context.
      subroutine npedln ( a, b, c, linept, linedr, pnear, dist )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: a  !! Length of ellipsoid's semi-axis in the x direction
      real(wp) :: b  !! Length of ellipsoid's semi-axis in the y direction
      real(wp) :: c  !! Length of ellipsoid's semi-axis in the z direction
      real(wp) :: linept ( 3 )  !! Point on line
      real(wp) :: linedr ( 3 )  !! Direction vector of line
      real(wp) :: pnear  ( 3 )  !! Nearest point on ellipsoid to line
      real(wp) :: dist  !! Distance of ellipsoid from line
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: linept, linedr are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%npedln ( a, b, c, (linept), (linedr), pnear, dist )
      end subroutine npedln

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzinvelt with the legacy context.
      subroutine zzinvelt ( p,      corsys, corpar, bounds, &
                            margin, exclud, inside         )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsktol
      use pepper_inc_dskdsc
      implicit none
      real(wp) :: p      ( 3 )  !! Input point.
      integer(ip) :: corsys  !! Coordinate system code.
      real(wp) :: corpar ( * )  !! Coordinate system parameters.
      real(wp) :: bounds ( 2, 3 )  !! Coordinate bounds of element.
      real(wp) :: margin  !! Margin used for inclusion testing.
      integer(ip) :: exclud  !! Index of coordinate to exclude from test.
      logical :: inside  !! Flag indicating whether point is in element.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzinvelt ( ctx, p, corsys, corpar, bounds, margin, exclud, inside )
      end subroutine zzinvelt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzprsmet with the legacy context.
      subroutine zzprsmet ( bodyid, method, mxnsrf, shape,  subtyp, &
                            pri,    nsurf,  srflst, pntdef, trmtyp )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: bodyid  !! Body ID code.
      character(*) :: method  !! Method string.
      integer(ip) :: mxnsrf  !! Maximum number of surface IDs that can be returned.
      character(*) :: shape  !! Target shape.
      character(*) :: subtyp  !! Sub-point type.
      logical :: pri  !! Prioritization flag.
      integer(ip) :: nsurf  !! Number of surface IDs in list.
      integer(ip) :: srflst ( * )  !! Surface ID list.
      character(*) :: pntdef  !! Limb or terminator point definition.
      character(*) :: trmtyp  !! Terminator type.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: method is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzprsmet ( ctx, bodyid, (method), mxnsrf, shape, subtyp, pri, nsurf, srflst, &
         pntdef, trmtyp )
      end subroutine zzprsmet

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzrytpdt with the legacy context.
      subroutine zzrytpdt ( vertex, raydir, bounds, corpar, margin, nxpts, xpt )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsktol
      implicit none
      real(wp) :: vertex ( 3 )  !! Ray's vertex.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      real(wp) :: bounds ( 2, 3 )  !! Bounds of planetodetic volume element.
      real(wp) :: corpar ( * )  !! Coordinate parameters.
      real(wp) :: margin  !! Margin used for element expansion.
      integer(ip) :: nxpts  !! Number of intercept points.
      real(wp) :: xpt    ( 3 )  !! Intercept.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir, bounds are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzrytpdt ( ctx, (vertex), (raydir), (bounds), corpar, margin, nxpts, xpt )
      end subroutine zzrytpdt

      !> Determine the plate ID and body-fixed coordinates of the
      !> intersection of a specified ray with the surface defined by a
      !> type 2 DSK plate model.
      !>
      !> Legacy interface: calls ctx%dskx02 with the legacy context.
      subroutine dskx02 ( handle, dladsc, vertex, raydir, plid, xpt, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      use pepper_inc_dsktol
      implicit none
      integer(ip) :: handle  !! Handle of DSK kernel containing plate model.
      integer(ip) :: dladsc   ( * )  !! DLA descriptor of plate model segment.
      real(wp) :: vertex   ( 3 )  !! Ray's vertex in the  body fixed frame.
      real(wp) :: raydir   ( 3 )  !! Ray direction in the body fixed frame.
      integer(ip) :: plid  !! ID code of the plate intersected by the ray.
      real(wp) :: xpt      ( 3 )  !! Intercept.
      logical :: found  !! Flag indicating whether intercept exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%dskx02 ( handle, dladsc, (vertex), (raydir), plid, xpt, found )
      end subroutine dskx02

      !> **Deprecated:** This routine has been superseded by the SPICELIB
      !> routine SINCPT. This routine is supported for purposes of
      !> backward compatibility only.
      !>
      !> Legacy interface: calls ctx%srfxpt with the legacy context.
      subroutine srfxpt ( method, target, et, abcorr, obsrvr, dref, dvec, spoint, dist, trgepc, obspos, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_zzctr
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Name of observing body.
      character(*) :: dref  !! Reference frame of input direction vector.
      real(wp) :: dvec   ( 3 )  !! Ray's direction vector.
      real(wp) :: spoint ( 3 )  !! Surface intercept point on the target body.
      real(wp) :: dist  !! Distance from the observer to the intercept point.
      real(wp) :: trgepc  !! Intercept epoch.
      real(wp) :: obspos ( 3 )  !! Observer position relative to target center.
      logical :: found  !! Flag indicating whether intercept was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: dvec is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%srfxpt ( method, target, et, abcorr, obsrvr, dref, (dvec), spoint, dist, trgepc, &
         obspos, found )
      end subroutine srfxpt

      !> Compute, for a given observer, ray emanating from the observer,
      !> and target, the "tangent point": the point on the ray nearest
      !> to the target's surface. Also compute the point on the target's
      !> surface nearest to the tangent point.
      !>
      !> Legacy interface: calls ctx%tangpt with the legacy context.
      subroutine tangpt ( method, target, et, fixref, abcorr, corloc, obsrvr, dref, dvec, tanpt, alt, range, srfpt, trgepc, srfvec )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: corloc  !! Aberration correction locus: 'TANGENT POINT' or 'SURFACE POINT'.
      character(*) :: obsrvr  !! Name of observing body.
      character(*) :: dref  !! Reference frame of ray direction vector.
      real(wp) :: dvec   ( 3 )  !! Ray direction vector.
      real(wp) :: tanpt  ( 3 )  !! "Tangent point": point on ray nearest to surface.
      real(wp) :: alt  !! Altitude of tangent point above surface.
      real(wp) :: range  !! Distance of tangent point from observer.
      real(wp) :: srfpt  ( 3 )  !! Point on surface nearest to tangent point.
      real(wp) :: trgepc  !! Epoch associated with correction locus.
      real(wp) :: srfvec ( 3 )  !! Vector from observer to surface point SRFPT.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: dvec is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%tangpt ( method, target, et, fixref, abcorr, corloc, obsrvr, dref, (dvec), tanpt, &
         alt, range, srfpt, trgepc, srfvec )
      end subroutine tangpt

      !> Indicate whether one triaxial ellipsoid is occulted by another as
      !> seen from a specified viewing location.
      !>
      !> Legacy interface: calls zz routine zzocced with the legacy context.
      integer(ip) function zzocced ( viewpt, &
                                 centr1, semax1, centr2, semax2 )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzocced
      implicit none
      real(wp) :: viewpt ( 3 )  !! Observation location.
      real(wp) :: centr1 ( 3 )  !! Center of first ellipsoid.
      real(wp) :: semax1 ( 3, 3 )  !! Semi-major axis matrix for first ellipsoid.
      real(wp) :: centr2 ( 3 )  !! Center of second ellipsoid.
      real(wp) :: semax2 ( 3, 3 )  !! Semi-major axis matrix for second ellipsoid.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzocced = pepper_zzocced ( ctx, viewpt, centr1, semax1, centr2, semax2 )
      end function zzocced

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzptpl02 with the legacy context.
      subroutine zzptpl02 ( handle, dladsc, dskdsc, point, plid, plate, verts, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsk02
      use pepper_inc_dsktol
      implicit none
      integer(ip) :: handle  !! DSK file handle.
      integer(ip) :: dladsc ( * )  !! DLA descriptor of segment.
      real(wp) :: dskdsc ( * )  !! DSK descriptor of segment.
      real(wp) :: point  ( 3 )  !! Input point.
      integer(ip) :: plid  !! Plate ID.
      integer(ip) :: plate  ( 3 )  !! Plate, expressed as an array of three vertex IDs.
      real(wp) :: verts  ( 3, 3 )  !! Vertices of plate.
      logical :: found  !! Found flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: point is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzptpl02 ( ctx, handle, dladsc, dskdsc, (point), plid, plate, verts, found )
      end subroutine zzptpl02

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzrytelt with the legacy context.
      subroutine zzrytelt ( vertex, raydir, dskdsc, &
                            margin, nxpts,  xpt    )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dskdsc
      implicit none
      real(wp) :: vertex ( 3 )  !! Ray's vertex.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      real(wp) :: dskdsc ( dskdsz )  !! DSK descriptor of segment.
      real(wp) :: margin  !! Margin used for element expansion.
      integer(ip) :: nxpts  !! Number of intercept points.
      real(wp) :: xpt    ( 3 )  !! Intercept.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir, dskdsc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzrytelt ( ctx, (vertex), (raydir), (dskdsc), margin, nxpts, xpt )
      end subroutine zzrytelt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdskbun with the legacy context.
      subroutine zzdskbun ( bodyid, nsurf,  srflst, et,     fixfid, &
                            nseg,   hanbuf, dlabuf, dskbuf, offbuf, &
                            ctrbuf, radbuf, point,  normal         )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsktol
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: bodyid  !! ID code of target body.
      integer(ip) :: nsurf  !! Number of IDs in surface list.
      integer(ip) :: srflst ( * )  !! List of surface IDs.
      real(wp) :: et  !! Lookup epoch, expressed as seconds past J2000 TDB.
      integer(ip) :: fixfid  !! Frame ID of body-fixed frame for output vectors.
      integer(ip) :: nseg  !! Number of segments in buffers.
      integer(ip) :: hanbuf ( * )  !! DSK handle buffer.
      integer(ip) :: dlabuf ( dladsz, * )  !! DLA segment descriptor buffer.
      real(wp) :: dskbuf ( dskdsz, * )  !! DSK segment descriptor buffer.
      real(wp) :: offbuf ( 3,      * )  !! Frame center offset buffer.
      real(wp) :: ctrbuf ( 3,      * )  !! Bounding sphere center buffer.
      real(wp) :: radbuf ( * )  !! Bounding sphere radius buffer.
      real(wp) :: point  ( 3 )  !! Surface point.
      real(wp) :: normal ( 3 )  !! Outward unit normal vector at surface point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: point is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzdskbun ( ctx, bodyid, nsurf, srflst, et, fixfid, nseg, hanbuf, dlabuf, dskbuf, &
         offbuf, ctrbuf, radbuf, (point), normal )
      end subroutine zzdskbun

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdsksgx with the legacy context.
      subroutine zzdsksgx ( handle, dladsc, dtype, et, vertex, &
                            raydir, xpt,    dc,    ic, found   )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! DSK file handle.
      integer(ip) :: dladsc ( * )  !! DLA descriptor of segment.
      integer(ip) :: dtype  !! Data type code.
      real(wp) :: et  !! Epoch, expressed as seconds past J2000 TDB.
      real(wp) :: vertex ( 3 )  !! Ray's vertex.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      real(wp) :: xpt    ( 3 )  !! Surface intercept, if found.
      real(wp) :: dc     ( * )  !! D.p. component of source info.
      integer(ip) :: ic     ( * )  !! Integer component of source info.
      logical :: found  !! Found flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzdsksgx ( ctx, handle, dladsc, dtype, et, (vertex), (raydir), xpt, dc, ic, &
         found )
      end subroutine zzdsksgx

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzdskbux with the legacy context.
      subroutine zzdskbux ( bodyid, nsurf,  srflst, et,     fixfid, &
                            nseg,   hanbuf, dlabuf, dskbuf, offbuf, &
                            ctrbuf, radbuf, vertex, raydir, xpt, &
                            segidx, dc,     ic,     found          )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_dsktol
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: bodyid  !! ID code of target body.
      integer(ip) :: nsurf  !! Number of IDs in surface list.
      integer(ip) :: srflst ( * )  !! List of surface IDs.
      real(wp) :: et  !! Lookup epoch, expressed as seconds past J2000 TDB.
      integer(ip) :: fixfid  !! Frame ID of body-fixed frame for output vectors.
      integer(ip) :: nseg  !! Number of segments in buffers.
      integer(ip) :: hanbuf ( * )  !! DSK handle buffer.
      integer(ip) :: dlabuf ( dladsz, * )  !! DLA segment descriptor buffer.
      real(wp) :: dskbuf ( dskdsz, * )  !! DSK segment descriptor buffer.
      real(wp) :: offbuf ( 3,      * )  !! Frame center offset buffer.
      real(wp) :: ctrbuf ( 3,      * )  !! Bounding sphere center buffer.
      real(wp) :: radbuf ( * )  !! Bounding sphere radius buffer.
      real(wp) :: vertex ( 3 )  !! Ray's vertex.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      real(wp) :: xpt    ( 3 )  !! Intercept point.
      integer(ip) :: segidx  !! Index of segment in input buffers.
      real(wp) :: dc     ( * )  !! D.p. component of source info.
      integer(ip) :: ic     ( * )  !! Integer component of source info.
      logical :: found  !! Found flag. True if and only if intercept exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzdskbux ( ctx, bodyid, nsurf, srflst, et, fixfid, nseg, hanbuf, dlabuf, dskbuf, &
         offbuf, ctrbuf, radbuf, (vertex), (raydir), xpt, segidx, dc, ic, found )
      end subroutine zzdskbux

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsbfxr with the legacy context.
      subroutine zzsbfxr ( bodyid, nsurf, srflst, et, fixfid, vertex, raydir, xpt, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: bodyid  !! ID code of target body.
      integer(ip) :: nsurf  !! Number of surface IDs in list.
      integer(ip) :: srflst ( * )  !! Surface ID list.
      real(wp) :: et  !! Evaluation epoch, seconds past J2000 TDB.
      integer(ip) :: fixfid  !! Frame ID of ray and intercept.
      real(wp) :: vertex ( 3 )  !! Ray's vertex.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      real(wp) :: xpt    ( 3 )  !! Surface intercept point.
      logical :: found  !! Found flag. True if intercept exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzsbfxr ( ctx, bodyid, nsurf, srflst, et, fixfid, (vertex), (raydir), xpt, found )
      end subroutine zzsbfxr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsbfxri with the legacy context.
      subroutine zzsbfxri ( bodyid, nsurf, srflst, et, fixfid, vertex, raydir, xpt, handle, dladsc, dskdsc, dc, ic, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: bodyid  !! ID code of target body.
      integer(ip) :: nsurf  !! Number of surface IDs in list.
      integer(ip) :: srflst ( * )  !! Surface ID list.
      real(wp) :: et  !! Evaluation epoch, seconds past J2000 TDB.
      integer(ip) :: fixfid  !! Frame ID of ray and intercept.
      real(wp) :: vertex ( 3 )  !! Ray's vertex.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      real(wp) :: xpt    ( 3 )  !! Surface intercept point.
      integer(ip) :: handle  !! Handle of DSK file.
      integer(ip) :: dladsc ( * )  !! DLA descriptor of segment.
      real(wp) :: dskdsc ( * )  !! DSK descriptor of segment.
      real(wp) :: dc     ( * )  !! Double precision component of source info.
      integer(ip) :: ic     ( * )  !! Integer component of source info.
      logical :: found  !! Found flag. True if intercept exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzsbfxri ( ctx, bodyid, nsurf, srflst, et, fixfid, (vertex), (raydir), xpt, &
         handle, dladsc, dskdsc, dc, ic, found )
      end subroutine zzsbfxri

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsbfnrm with the legacy context.
      subroutine zzsbfnrm ( bodyid, nsurf, srflst, et, fixfid, point, normal )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: bodyid  !! ID code of target body.
      integer(ip) :: nsurf  !! Number of surface IDs in list.
      integer(ip) :: srflst ( * )  !! Surface ID list.
      real(wp) :: et  !! Evaluation epoch, seconds past J2000 TDB.
      integer(ip) :: fixfid  !! Frame ID of surface point and normal vector.
      real(wp) :: point  ( 3 )  !! Surface point.
      real(wp) :: normal ( 3 )  !! Unit length outward normal at input point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: point is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzsbfnrm ( ctx, bodyid, nsurf, srflst, et, fixfid, (point), normal )
      end subroutine zzsbfnrm

      !> Compute a ray-surface intercept using data provided by
      !> multiple loaded DSK segments. Return information about
      !> the source of the data defining the surface on which the
      !> intercept was found: DSK handle, DLA and DSK descriptors,
      !> and DSK data type-dependent parameters.
      !>
      !> Legacy interface: calls ctx%dskxsi with the legacy context.
      subroutine dskxsi ( pri, target, nsurf, srflst, et, fixref, vertex, raydir, maxd, maxi, xpt, handle, dladsc, dskdsc, dc, ic, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsktol
      use pepper_inc_dsksrc
      use pepper_inc_srftrn
      use pepper_inc_zzctr
      implicit none
      logical :: pri  !! Data prioritization flag.
      character(*) :: target  !! Target body name.
      integer(ip) :: nsurf  !! Number of surface IDs in list.
      integer(ip) :: srflst ( * )  !! Surface ID list.
      real(wp) :: et  !! Epoch, expressed as seconds past J2000 TDB.
      character(*) :: fixref  !! Name of target body-fixed reference frame.
      real(wp) :: vertex ( 3 )  !! Vertex of ray.
      real(wp) :: raydir ( 3 )  !! Direction vector of ray.
      integer(ip) :: maxd  !! Size of DC array.
      integer(ip) :: maxi  !! Size of IC array.
      real(wp) :: xpt    ( 3 )  !! Intercept point.
      integer(ip) :: handle  !! Handle of segment contributing surface data.
      integer(ip) :: dladsc ( * )  !! DLA descriptor of segment.
      real(wp) :: dskdsc ( * )  !! DSK descriptor of segment.
      real(wp) :: dc     ( * )  !! Double precision component of source info.
      integer(ip) :: ic     ( * )  !! Integer component of source info.
      logical :: found  !! Found flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%dskxsi ( pri, target, nsurf, srflst, et, fixref, (vertex), (raydir), maxd, maxi, &
         xpt, handle, dladsc, dskdsc, dc, ic, found )
      end subroutine dskxsi

      !> Compute ray-surface intercepts for a set of rays, using data
      !> provided by multiple loaded DSK segments.
      !>
      !> Legacy interface: calls ctx%dskxv with the legacy context.
      subroutine dskxv ( pri, target, nsurf, srflst, et, fixref, nrays, vtxarr, dirarr, xptarr, fndarr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsktol
      use pepper_inc_zzctr
      implicit none
      logical :: pri  !! Data prioritization flag.
      character(*) :: target  !! Target body name.
      integer(ip) :: nsurf  !! Number of surface IDs in list.
      integer(ip) :: srflst ( * )  !! Surface ID list.
      real(wp) :: et  !! Epoch, expressed as seconds past J2000 TDB.
      character(*) :: fixref  !! Name of target body-fixed reference frame.
      integer(ip) :: nrays  !! Number of rays.
      real(wp) :: vtxarr ( 3, * )  !! Array of vertices of rays.
      real(wp) :: dirarr ( 3, * )  !! Array of direction vectors of rays.
      real(wp) :: xptarr ( 3, * )  !! Intercept point array.
      logical :: fndarr ( * )  !! Found flag array.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskxv ( pri, target, nsurf, srflst, et, fixref, nrays, vtxarr, dirarr, xptarr, fndarr )
      end subroutine dskxv

      !> Find the illumination angles (phase, incidence, and
      !> emission) at a specified surface point of a target body.
      !>
      !> Legacy interface: calls ctx%illumg with the legacy context.
      subroutine illumg ( method, target, ilusrc, et, fixref, abcorr, obsrvr, spoint, trgepc, srfvec, phase, incdnc, emissn )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      use pepper_inc_zzdsk
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      character(*) :: ilusrc  !! Name of illumination source.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      character(*) :: abcorr  !! Desired aberration correction.
      character(*) :: obsrvr  !! Name of observing body.
      real(wp) :: spoint ( 3 )  !! Body-fixed coordinates of a target surface point.
      real(wp) :: trgepc  !! Target surface point epoch.
      real(wp) :: srfvec ( 3 )  !! Vector from observer to target surface point.
      real(wp) :: phase  !! Phase angle at the surface point.
      real(wp) :: incdnc  !! Source incidence angle at the surface point.
      real(wp) :: emissn  !! Emission angle at the surface point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: spoint is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%illumg ( method, target, ilusrc, et, fixref, abcorr, obsrvr, (spoint), trgepc, &
         srfvec, phase, incdnc, emissn )
      end subroutine illumg

      !> Map array of surface points on a specified target body to
      !> the corresponding unit length outward surface normal vectors.
      !>
      !> Legacy interface: calls ctx%srfnrm with the legacy context.
      subroutine srfnrm ( method, target, et, fixref, npts, srfpts, normls )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dsktol
      use pepper_inc_gf
      use pepper_inc_zzctr
      use pepper_inc_zzdsk
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in TDB seconds past J2000 TDB.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      integer(ip) :: npts  !! Number of surface points in input array.
      real(wp) :: srfpts ( 3, * )  !! Array of surface points.
      real(wp) :: normls ( 3, * )  !! Array of outward, unit length normal vectors.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%srfnrm ( method, target, et, fixref, npts, srfpts, normls )
      end subroutine srfnrm

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsuelin with the legacy context.
      subroutine zzsuelin ( trgcde )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: trgcde  !! Target body ID code.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzsuelin ( ctx, trgcde )
      end subroutine zzsuelin

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsudski with the legacy context.
      subroutine zzsudski ( trgcde, nsurf, srflst, fixfid )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: trgcde  !! Target body ID code.
      integer(ip) :: nsurf  !! Number of surface IDs in surface list.
      integer(ip) :: srflst ( * )  !! Surface list.
      integer(ip) :: fixfid  !! Target body-fixed frame ID.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzsudski ( ctx, trgcde, nsurf, srflst, fixfid )
      end subroutine zzsudski

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzraysfx with the legacy context.
      subroutine zzraysfx ( vertex, raydir, et, spoint, found )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Evaluation epoch, seconds past J2000 TDB.
      real(wp) :: vertex ( 3 )  !! Ray's vertex.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      real(wp) :: spoint ( 3 )  !! Surface intercept point.
      logical :: found  !! Found flag. .TRUE. if intercept exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzraysfx ( ctx, (vertex), (raydir), et, spoint, found )
      end subroutine zzraysfx

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzmaxrad with the legacy context.
      subroutine zzmaxrad ( maxrad )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: maxrad  !! Radius of outer bounding sphere for target body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzmaxrad ( ctx, maxrad )
      end subroutine zzmaxrad

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzminrad with the legacy context.
      subroutine zzminrad ( minrad )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: minrad  !! Radius of inner bounding sphere for target body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzminrad ( ctx, minrad )
      end subroutine zzminrad

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzraynp with the legacy context.
      subroutine zzraynp ( vertex, raydir, et, pnear, dist )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Evaluation epoch, seconds past J2000 TDB.
      real(wp) :: vertex ( 3 )  !! Ray's vertex.
      real(wp) :: raydir ( 3 )  !! Ray's direction vector.
      real(wp) :: pnear  ( 3 )  !! Near point to ray on surface.
      real(wp) :: dist  !! Distance between near point and ray.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: vertex, raydir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzraynp ( ctx, (vertex), (raydir), et, pnear, dist )
      end subroutine zzraynp

      !> Compute the illumination angles---phase, incidence, and
      !> emission---at a specified point on a target body. Return logical
      !> flags indicating whether the surface point is visible from
      !> the observer's position and whether the surface point is
      !> illuminated.
      !>
      !> Legacy interface: calls ctx%illumf with the legacy context.
      subroutine illumf ( method, target, ilusrc, et, fixref, abcorr, obsrvr, spoint, trgepc, srfvec, phase, incdnc, emissn, visibl, lit )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      use pepper_inc_zzdsk
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      character(*) :: ilusrc  !! Name of illumination source.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      character(*) :: abcorr  !! Desired aberration correction.
      character(*) :: obsrvr  !! Name of observing body.
      real(wp) :: spoint ( 3 )  !! Body-fixed coordinates of a target surface point.
      real(wp) :: trgepc  !! Target surface point epoch.
      real(wp) :: srfvec ( 3 )  !! Vector from observer to target surface point.
      real(wp) :: phase  !! Phase angle at the surface point.
      real(wp) :: incdnc  !! Source incidence angle at the surface point.
      real(wp) :: emissn  !! Emission angle at the surface point.
      logical :: visibl  !! Visibility flag (.TRUE. = visible).
      logical :: lit  !! Illumination flag (.TRUE. = illuminated).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: spoint is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%illumf ( method, target, ilusrc, et, fixref, abcorr, obsrvr, (spoint), trgepc, &
         srfvec, phase, incdnc, emissn, visibl, lit )
      end subroutine illumf

      !> Find the illumination angles (phase, solar incidence, and
      !> emission) at a specified surface point of a target body.
      !>
      !> Legacy interface: calls ctx%ilumin with the legacy context.
      subroutine ilumin ( method, target, et,     fixref, &
                          abcorr, obsrvr, spoint, trgepc, &
                          srfvec, phase,  incdnc, emissn  )
      use pepper_kinds, only: wp
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      character(*) :: abcorr  !! Desired aberration correction.
      character(*) :: obsrvr  !! Name of observing body.
      real(wp) :: spoint ( 3 )  !! Body-fixed coordinates of a target surface point.
      real(wp) :: trgepc  !! Target surface point epoch.
      real(wp) :: srfvec ( 3 )  !! Vector from observer to target surface point.
      real(wp) :: phase  !! Phase angle at the surface point.
      real(wp) :: incdnc  !! Solar incidence angle at the surface point.
      real(wp) :: emissn  !! Emission angle at the surface point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: spoint is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%ilumin ( method, target, et, fixref, abcorr, obsrvr, (spoint), trgepc, srfvec, &
         phase, incdnc, emissn )
      end subroutine ilumin

      !> Map array of planetocentric longitude/latitude coordinate pairs
      !> to surface points on a specified target body.
      !>
      !> Legacy interface: calls ctx%latsrf with the legacy context.
      subroutine latsrf ( method, target, et, fixref, npts, lonlat, srfpts )
      use pepper_kinds, only: ip, wp
      use pepper_inc_gf
      use pepper_inc_zzctr
      use pepper_inc_zzdsk
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in TDB seconds past J2000 TDB.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      integer(ip) :: npts  !! Number of coordinate pairs in input array.
      real(wp) :: lonlat ( 2, * )  !! Array of longitude/latitude coordinate pairs.
      real(wp) :: srfpts ( 3, * )  !! Array of surface points.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%latsrf ( method, target, et, fixref, npts, lonlat, srfpts )
      end subroutine latsrf

      !> Compute, for a given observer and a ray emanating from the
      !> observer, the surface intercept of the ray on a target body at
      !> a specified epoch, optionally corrected for light time and
      !> stellar aberration.
      !>
      !> Legacy interface: calls ctx%sincpt with the legacy context.
      subroutine sincpt ( method, target, et, fixref, abcorr, obsrvr, dref, dvec, spoint, trgepc, srfvec, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_gf
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      use pepper_inc_zzdsk
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Name of observing body.
      character(*) :: dref  !! Reference frame of ray's direction vector.
      real(wp) :: dvec   ( 3 )  !! Ray's direction vector.
      real(wp) :: spoint ( 3 )  !! Surface intercept point on the target body.
      real(wp) :: trgepc  !! Intercept epoch.
      real(wp) :: srfvec ( 3 )  !! Vector from observer to intercept point.
      logical :: found  !! Flag indicating whether intercept was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: dvec is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sincpt ( method, target, et, fixref, abcorr, obsrvr, dref, (dvec), spoint, trgepc, &
         srfvec, found )
      end subroutine sincpt

      !> Compute the rectangular coordinates of the sub-observer point on
      !> a target body at a specified epoch, optionally corrected for
      !> light time and stellar aberration.
      !>
      !> Legacy interface: calls ctx%subpnt with the legacy context.
      subroutine subpnt ( method, target, et, fixref, abcorr, obsrvr, spoint, trgepc, srfvec )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_gf
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      use pepper_inc_zzdsk
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Name of observing body.
      real(wp) :: spoint ( 3 )  !! Sub-observer point on the target body.
      real(wp) :: trgepc  !! Sub-observer point epoch.
      real(wp) :: srfvec ( 3 )  !! Vector from observer to sub-observer point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%subpnt ( method, target, et, fixref, abcorr, obsrvr, spoint, trgepc, srfvec )
      end subroutine subpnt

      !> Compute the rectangular coordinates of the sub-solar point on
      !> a target body at a specified epoch, optionally corrected for
      !> light time and stellar aberration.
      !>
      !> Legacy interface: calls ctx%subslr with the legacy context.
      subroutine subslr ( method, target, et, fixref, abcorr, obsrvr, spoint, trgepc, srfvec )
      use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_gf
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      use pepper_inc_zzdsk
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: obsrvr  !! Name of observing body.
      real(wp) :: spoint ( 3 )  !! Sub-solar point on the target body.
      real(wp) :: trgepc  !! Sub-solar point epoch.
      real(wp) :: srfvec ( 3 )  !! Vector from observer to sub-solar point.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%subslr ( method, target, et, fixref, abcorr, obsrvr, spoint, trgepc, srfvec )
      end subroutine subslr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zztanini with the legacy context.
      subroutine zztanini ( curve, srcrad, shape, trgcde, nsurf, srflst, fixfid, et, plnvec, axis )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzdsk
      implicit none
      integer(ip) :: curve  !! Type of curve: limb or terminator type.
      real(wp) :: srcrad  !! Radius of illumination source.
      integer(ip) :: shape  !! Target shape.
      integer(ip) :: trgcde  !! Target body ID code.
      integer(ip) :: nsurf  !! Number of surfaces in list.
      integer(ip) :: srflst ( * )  !! Surface ID list.
      integer(ip) :: fixfid  !! Frame ID of target body-fixed frame.
      real(wp) :: et  !! Epoch, expressed as seconds past J2000 TDB.
      real(wp) :: plnvec ( 3 )  !! Reference vector contained in cutting half-plane.
      real(wp) :: axis   ( 3 )  !! Axis vector: edge of cutting half-plane.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zztanini ( ctx, curve, srcrad, shape, trgcde, nsurf, srflst, fixfid, et, plnvec, axis )
      end subroutine zztanini

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zztansta with the legacy context.
      subroutine zztansta ( angle, ocultd, point )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzdsk
      implicit none
      real(wp) :: angle  !! Angle of ray.
      logical :: ocultd  !! Occultation state flag. True if ray is occulted.
      real(wp) :: point  ( 3 )  !! Tangent point on target.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zztansta ( ctx, angle, ocultd, point )
      end subroutine zztansta

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zztangnt with the legacy context.
      subroutine zztangnt ( curve,  srcrad, shape,  trgcde, nsurf, &
                            srflst, fixfid, et,     plnvec, axis, &
                            schstp, soltol, result, points        )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzdsk
      implicit none
      integer(ip) :: curve  !! Type of curve: limb or terminator type.
      real(wp) :: srcrad  !! Radius of illumination source.
      integer(ip) :: shape  !! Target shape.
      integer(ip) :: trgcde  !! Target body ID code.
      integer(ip) :: nsurf  !! Number of surfaces in list.
      integer(ip) :: srflst ( * )  !! Surface ID list.
      integer(ip) :: fixfid  !! Frame ID of target body-fixed frame.
      real(wp) :: et  !! Epoch, expressed as seconds past J2000 TDB.
      real(wp) :: plnvec ( 3 )  !! Reference vector contained in cutting half-plane.
      real(wp) :: axis   ( 3 )  !! Axis vector: edge of cutting half-plane.
      real(wp) :: schstp  !! Angular step size for searching.
      real(wp) :: soltol  !! Solution convergence tolerance.
      real(wp) :: result ( lbcell : * )  !! Cell (not window) containing transition angles.
      real(wp) :: points ( 3, * )  !! Array of tangent points.
      type(pepper_context), pointer :: ctx
      real(wp), allocatable :: res(:), pts(:,:)
      integer(ip) :: n, room
      ctx => legacy_context()
      ! PEPPER: the context routine grows RESULT and POINTS (allocatable);
      ! this interface keeps SPICELIB's fixed arrays: the cell is copied
      ! in and out, and a result larger than the cell is an error.
      room = nint ( result(-1), ip )
      allocate ( res(lbcell:room), pts(3,max(room,1)) )
      res = result(lbcell:room)
      pts = 0.0_wp
      call pepper_zztangnt ( ctx, curve, srcrad, shape, trgcde, nsurf, srflst, fixfid, et, plnvec, axis, schstp, soltol, res, pts )
      n = nint ( res(0), ip )
      if ( n > room .and. .not. ctx%failed() ) then
         call ctx%setmsg ( 'The result has # elements; the size of the cell is #.' )
         call ctx%errint ( '#', n )
         call ctx%errint ( '#', room )
         call ctx%sigerr ( 'SPICE(WINDOWEXCESS)' )
      end if
      n = min ( n, room )
      result(lbcell:0) = res(lbcell:0)
      result(0) = real ( n, wp )
      result(1:n) = res(1:n)
      points(:,1:min(n,size(pts,2,kind=ip))) = pts(:,1:min(n,size(pts,2,kind=ip)))
      end subroutine zztangnt

      !> Find limb points on a target body. The limb is the set of points
      !> of tangency on the target of rays emanating from the observer.
      !> The caller specifies half-planes bounded by the observer-target
      !> center vector in which to search for limb points.
      !>
      !> Legacy interface: calls ctx%limbpt with the legacy context.
      subroutine limbpt ( method, target, et, fixref, abcorr, corloc, obsrvr, refvec, rolstp, ncuts, schstp, soltol, maxn, npts, points, epochs, tangts )
       use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_gf
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      use pepper_inc_zzdsk
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: corloc  !! Aberration correction locus.
      character(*) :: obsrvr  !! Name of observing body.
      real(wp) :: refvec ( 3 )  !! Reference vector for cutting half-planes.
      real(wp) :: rolstp  !! Roll angular step for cutting half-planes.
      integer(ip) :: ncuts  !! Number of cutting half-planes.
      real(wp) :: schstp  !! Angular step size for searching.
      real(wp) :: soltol  !! Solution convergence tolerance.
      integer(ip) :: maxn  !! Maximum number of entries in output arrays.
      integer(ip) :: npts   ( * )  !! Counts of limb points corresponding to cuts.
      real(wp) :: points ( 3, * )  !! Limb points.
      real(wp) :: epochs ( * )  !! Times associated with limb points.
      real(wp) :: tangts ( 3, * )  !! Tangent vectors emanating from the observer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: refvec is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%limbpt ( method, target, et, fixref, abcorr, corloc, obsrvr, (refvec), rolstp, &
         ncuts, schstp, soltol, maxn, npts, points, epochs, tangts )
      end subroutine limbpt

      !> Find terminator points on a target body. The caller specifies
      !> half-planes, bounded by the illumination source center-target
      !> center vector, in which to search for terminator points.
      !>
      !> Legacy interface: calls ctx%termpt with the legacy context.
      subroutine termpt ( method, ilusrc, target, et, fixref, abcorr, corloc, obsrvr, refvec, rolstp, ncuts, schstp, soltol, maxn, npts, points, epochs, trmvcs )
       use pepper_kinds, only: ip, wp
      use pepper_inc_frmtyp
      use pepper_inc_gf
      use pepper_inc_zzabcorr
      use pepper_inc_zzctr
      use pepper_inc_zzdsk
      implicit none
      character(*) :: method  !! Computation method.
      character(*) :: ilusrc  !! Illumination source.
      character(*) :: target  !! Name of target body.
      real(wp) :: et  !! Epoch in ephemeris seconds past J2000 TDB.
      character(*) :: fixref  !! Body-fixed, body-centered target body frame.
      character(*) :: abcorr  !! Aberration correction.
      character(*) :: corloc  !! Aberration correction locus.
      character(*) :: obsrvr  !! Name of observing body.
      real(wp) :: refvec ( 3 )  !! Reference vector for cutting half-planes.
      real(wp) :: rolstp  !! Roll angular step for cutting half-planes.
      integer(ip) :: ncuts  !! Number of cutting half-planes.
      real(wp) :: schstp  !! Angular step size for searching.
      real(wp) :: soltol  !! Solution convergence tolerance.
      integer(ip) :: maxn  !! Maximum number of entries in output arrays.
      integer(ip) :: npts   ( * )  !! Counts of terminator points corresponding to cuts.
      real(wp) :: points ( 3, * )  !! Terminator points.
      real(wp) :: epochs ( * )  !! Times associated with terminator points.
      real(wp) :: trmvcs ( 3, * )  !! Terminator vectors emanating from the observer.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: refvec is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%termpt ( method, ilusrc, target, et, fixref, abcorr, corloc, obsrvr, (refvec), &
         rolstp, ncuts, schstp, soltol, maxn, npts, points, epochs, trmvcs )
      end subroutine termpt

end module pepper_legacy_geometry
