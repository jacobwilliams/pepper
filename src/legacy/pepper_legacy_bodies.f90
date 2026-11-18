!> The legacy interface's routines of the subsystem bodies (33 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_bodies

   use pepper_instance_module, only: pepper_context, pepper_zzidmap => zzidmap, &
      pepper_zzhashi => zzhashi, pepper_zzhscini => zzhscini, pepper_zzhscadd => zzhscadd, &
      pepper_zzhscchk => zzhscchk, pepper_zzhscavl => zzhscavl, pepper_zzhscinf => zzhscinf, &
      pepper_zzhsiini => zzhsiini, pepper_zzhsiadd => zzhsiadd, pepper_zzhsichk => zzhsichk, &
      pepper_zzhsiavl => zzhsiavl, pepper_zzhsiinf => zzhsiinf, pepper_zzbodini => zzbodini, &
      pepper_zzbodker => zzbodker, pepper_zzbodget => zzbodget, pepper_zzbodlst => zzbodlst, &
      pepper_zzbodn2c => zzbodn2c, pepper_zzbodc2n => zzbodc2n, pepper_zzboddef => zzboddef, &
      pepper_zzbodkik => zzbodkik, pepper_zzbodrst => zzbodrst, pepper_zzbctrck => zzbctrck, &
      pepper_instance, pepper_zzbods2c => zzbods2c, pepper_zzbodvcd => zzbodvcd
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbpool

   implicit none

   private
   public :: zzidmap, zzhashi, zzhscini, zzhscadd, zzhscchk, zzhscavl, zzhscinf, zzhsiini, &
      zzhsiadd, zzhsichk, zzhsiavl, zzhsiinf, zzbodini, zzbodker, zzbodget, zzbodlst, zzbodn2c, &
      zzbodc2n, zzboddef, zzbodkik, zzbodrst, zzbctrck, bodn2c, bodc2n, bods2c, bodc2s, boddef, &
      bodfnd, zzbods2c, zzbodvcd, bodvar, bodvcd, bodvrd

contains

      !> The default SPICE body/ID mapping assignments available
      !> to the SPICE library.
      !>
      !> Legacy interface: calls zz routine zzidmap with the legacy context.
      subroutine  zzidmap( bltcod, bltnam )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      implicit none
      integer(ip) :: bltcod(nperm)  !! List of default integer ID codes.
      character(maxl) :: bltnam(nperm)  !! List of default names.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzidmap ( ctx, bltcod, bltnam )
      end subroutine zzidmap

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhashi with the legacy context.
      integer(ip) function zzhashi ( n, m )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: n  !! An integer number.
      integer(ip) :: m  !! Divisor used for the hash function
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzhashi = pepper_zzhashi ( ctx, n, m )
      end function zzhashi

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhscini with the legacy context.
      subroutine zzhscini ( hashsz, hedlst, collst )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_constants, only: intmax
      implicit none
      integer(ip) :: hashsz  !! Hash size
      integer(ip) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip) :: collst  ( lbpool : * )  !! Hash node collision list
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzhscini ( ctx, hashsz, hedlst, collst )
      end subroutine zzhscini

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhscadd with the legacy context.
      subroutine zzhscadd ( hedlst, collst, items, item, itemat, new )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_constants, only: intmax
      implicit none
      integer(ip) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip) :: collst  ( lbpool : * )  !! Hash node collision list
      character(*) :: items   (          * )  !! Hash item list
      character(*) :: item  !! Item to be checked for/added
      integer(ip) :: itemat  !! Item index in node collision and item lists
      logical :: new  !! Flag indicting if item was added
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: item is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzhscadd ( ctx, hedlst, collst, items, (item), itemat, new )
      end subroutine zzhscadd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhscchk with the legacy context.
      subroutine zzhscchk ( hedlst, collst, items, item, itemat )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_constants, only: intmax
      implicit none
      integer(ip) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip) :: collst  ( lbpool : * )  !! Hash node collision list
      character(*) :: items   (          * )  !! Hash item list
      character(*) :: item  !! Item to find
      integer(ip) :: itemat  !! Item index in node collision and item lists
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzhscchk ( ctx, hedlst, collst, items, item, itemat )
      end subroutine zzhscchk

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhscavl with the legacy context.
      subroutine zzhscavl ( collst, avail )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_constants, only: intmax
      implicit none
      integer(ip) :: collst  ( lbpool : * )  !! Hash node collision list
      integer(ip) :: avail  !! Room available in the hash
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzhscavl ( ctx, collst, avail )
      end subroutine zzhscavl

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhscinf with the legacy context.
      subroutine zzhscinf ( hedlst, collst, items, item, avail )
      use pepper_kinds, only: ip
      use pepper_hash, only: name_hash
      use pepper_constants, only: intmax
      implicit none
      integer(ip) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip) :: collst  ( lbpool : * )  !! Hash node collision list
      character(*) :: items   (          * )  !! Hash item list
      character(*) :: item  !! parameter to report
      integer(ip) :: avail  !! parameter Value
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzhscinf ( ctx, hedlst, collst, items, item, avail )
      end subroutine zzhscinf

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhsiini with the legacy context.
      subroutine zzhsiini ( hashsz, hedlst, collst )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: hashsz  !! Hash size
      integer(ip) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip) :: collst  ( lbpool : * )  !! Hash node collision list
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzhsiini ( ctx, hashsz, hedlst, collst )
      end subroutine zzhsiini

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhsiadd with the legacy context.
      subroutine zzhsiadd ( hedlst, collst, items, item, itemat, new )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip) :: collst  ( lbpool : * )  !! Hash node collision list
      integer(ip) :: items   (          * )  !! Hash item list
      integer(ip) :: item  !! Item to be checked for/added
      integer(ip) :: itemat  !! Item index in node collision and item lists
      logical :: new  !! Flag indicting if item was added
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzhsiadd ( ctx, hedlst, collst, items, item, itemat, new )
      end subroutine zzhsiadd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhsichk with the legacy context.
      subroutine zzhsichk ( hedlst, collst, items, item, itemat )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip) :: collst  ( lbpool : * )  !! Hash node collision list
      integer(ip) :: items   (          * )  !! Hash item list
      integer(ip) :: item  !! Item to find
      integer(ip) :: itemat  !! Item index in node collision and item lists
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzhsichk ( ctx, hedlst, collst, items, item, itemat )
      end subroutine zzhsichk

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhsiavl with the legacy context.
      subroutine zzhsiavl ( collst, avail )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: collst  ( lbpool : * )  !! Hash node collision list
      integer(ip) :: avail  !! Room available in the hash
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzhsiavl ( ctx, collst, avail )
      end subroutine zzhsiavl

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzhsiinf with the legacy context.
      subroutine zzhsiinf ( hedlst, collst, items, param, avail )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: hedlst  (          * )  !! Hash head node pointer list
      integer(ip) :: collst  ( lbpool : * )  !! Hash node collision list
      integer(ip) :: items   (          * )  !! Hash item list
      character(*) :: param  !! parameter to report
      integer(ip) :: avail  !! parameter Value
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzhsiinf ( ctx, hedlst, collst, items, param, avail )
      end subroutine zzhsiinf

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbodini with the legacy context.
      subroutine zzbodini (  names,  nornam, codes,  nvals, maxval, &
                             bnmlst, bnmpol, bnmnms, bnmidx, &
                             bidlst, bidpol, bidids, bididx  )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      implicit none
      character(maxl) :: names  (          * )  !! Array of names
      character(maxl) :: nornam (          * )  !! Array of normalized names
      integer(ip) :: codes  (          * )  !! Array of ID codes for NAMES/NORNAM
      integer(ip) :: nvals  !! Length of NAMES, NORNAM, and CODES arrays
      integer(ip) :: maxval  !! Size of the hash arrays
      integer(ip) :: bnmlst (          * )  !! Body name-based hash head node pointer list
      integer(ip) :: bnmpol ( lbpool : * )  !! Body name-based hash node collision list
      character(maxl) :: bnmnms (          * )  !! Body name-based hash item list
      integer(ip) :: bnmidx (          * )  !! Body name-based hash index storage array
      integer(ip) :: bidlst (          * )  !! Body ID-based hash head node pointer list
      integer(ip) :: bidpol ( lbpool : * )  !! Body ID-based hash node collision list
      integer(ip) :: bidids (          * )  !! Body ID-based hash item list
      integer(ip) :: bididx (          * )  !! Body ID-based hash index storage array
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzbodini ( ctx, names, nornam, codes, nvals, maxval, bnmlst, bnmpol, bnmnms, bnmidx, bidlst, bidpol, bidids, bididx )
      end subroutine zzbodini

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbodker with the legacy context.
      !> PEPPER's ZZBODKER fills a body_names record, with no limit
      !> on the number or length of the names; this keeps SPICELIB's
      !> interface: it signals SPICE(KERVARTOOBIG) when the pool vectors do
      !> not fit its NROOM arrays, copies the names and codes into them,
      !> and builds SPICELIB's hash tables with ZZBODINI.
      subroutine zzbodker ( names,  nornam, codes,  nvals,  extker, &
                            bnmlst, bnmpol, bnmnms, bnmidx, &
                            bidlst, bidpol, bidids, bididx  )
      use pepper_states, only: body_names
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      implicit none
      character(maxl) :: names  (          nroom )  !! Array of kernel pool assigned names.
      character(maxl) :: nornam (          nroom )  !! Array of normalized kernel pool assigned
                                                    !! names.
      integer(ip) :: codes  (          nroom )  !! Array of ID codes for NAMES/NORNAM.
      integer(ip) :: nvals  !! Length of NAMES, NORNAM, and CODES arrays.
      logical :: extker  !! Logical indicating presence of kernel pool names.
      integer(ip) :: bnmlst (          nroom )  !! Body name-based hash head node pointer list
      integer(ip) :: bnmpol ( lbpool : nroom )  !! Body name-based hash node collision list
      character(maxl) :: bnmnms (          nroom )  !! Body name-based hash item list
      integer(ip) :: bnmidx (          nroom )  !! Body name-based hash index storage array
      integer(ip) :: bidlst (          nroom )  !! Body ID-based hash head node pointer list
      integer(ip) :: bidpol ( lbpool : nroom )  !! Body ID-based hash node collision list
      integer(ip) :: bidids (          nroom )  !! Body ID-based hash item list
      integer(ip) :: bididx (          nroom )  !! Body ID-based hash index storage array
      type(pepper_context), pointer :: ctx
      type(body_names) :: ker
      character(1) :: type(2)
      integer(ip) :: nsiz(2), i
      logical :: found(2)
      ctx => legacy_context()
      if ( ctx%failed() ) return
      extker = .false.
      call ctx%dtpool ( 'NAIF_BODY_NAME', found(1), nsiz(1), type(1) )
      call ctx%dtpool ( 'NAIF_BODY_CODE', found(2), nsiz(2), type(2) )
      if (       found(1) .and. type(1) == 'C' &
           .and. found(2) .and. type(2) == 'N' &
           .and. ( nsiz(1) > nroom .or. nsiz(2) > nroom ) ) then
         call ctx%setmsg ( 'The kernel pool vectors used to ' &
         //            'define the names/ID-codes mapping' &
         //            'exceeds the max size. The size of ' &
         //            'the NAME vector is #1. The size of ' &
         //            'the CODE vector is #2. The max ' &
         //            'number allowed of elements is #3.'   )
         call ctx%errint ( '#1', nsiz(1)                         )
         call ctx%errint ( '#2', nsiz(2)                         )
         call ctx%errint ( '#3', nroom                           )
         call ctx%sigerr ( 'SPICE(KERVARTOOBIG)'                 )
         return
      end if
      call pepper_zzbodker ( ctx, ker )
      if ( ctx%failed() .or. .not. ker%extker ) return
      nvals = ker%nvals
      do i = 1, nvals
         names(i)  = ker%names(i)%s
         nornam(i) = ker%nornam(i)%s
         codes(i)  = ker%codes(i)
      end do
      ! PEPPER: names, nornam, codes are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzbodini ( ctx, (names), (nornam), (codes), nvals, nroom, bnmlst, bnmpol, bnmnms, &
         bnmidx, bidlst, bidpol, bidids, bididx )
      if ( ctx%failed() ) return
      extker = .true.
      end subroutine zzbodker

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbodget with the legacy context.
      subroutine zzbodget ( room, names, nornam, codes, nvals )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      implicit none
      integer(ip) :: room  !! Space available in NAMES, NORNAM, and CODES.
      character(*) :: names  ( * )  !! Array of built-in body names.
      character(*) :: nornam ( * )  !! Array of normalized built-in body names.
      integer(ip) :: codes  ( * )  !! Array of built-in ID codes for NAMES/NORNAM.
      integer(ip) :: nvals  !! Length of NAMES, NORNAM, CODES, and ORDNOM arrays.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzbodget ( ctx, room, names, nornam, codes, nvals )
      end subroutine zzbodget

      !> Output the complete list of built-in body/ID mappings to
      !> some output device. Thw routine generates 2 lists: one
      !> sorted by ID number, one sorted by name.
      !>
      !> Legacy interface: calls zz routine zzbodlst with the legacy context.
      subroutine zzbodlst ( device, reqst )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      implicit none
      character(*) :: device  !! Device name to receive the output.
      character(*) :: reqst  !! Data list name to output.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzbodlst ( ctx, device, reqst )
      end subroutine zzbodlst

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbodn2c with the legacy context.
      subroutine zzbodn2c ( name, code, found )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Body name to be translated.
      integer(ip) :: code  !! Integer code for that body.
      logical :: found  !! True if translated, otherwise false.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzbodn2c ( ctx, name, code, found )
      end subroutine zzbodn2c

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbodc2n with the legacy context.
      subroutine zzbodc2n ( code, name, found )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Common name for the body identified by CODE.
      integer(ip) :: code  !! Integer code to be translated.
      logical :: found  !! True if translated, otherwise false.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzbodc2n ( ctx, code, name, found )
      end subroutine zzbodc2n

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzboddef with the legacy context.
      subroutine zzboddef ( name, code )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Common name of some body.
      integer(ip) :: code  !! Integer code for that body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzboddef ( ctx, name, code )
      end subroutine zzboddef

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbodkik with the legacy context.
      subroutine zzbodkik (  )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzbodkik ( ctx )
      end subroutine zzbodkik

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbodrst with the legacy context.
      subroutine zzbodrst (  )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzbodrst ( ctx )
      end subroutine zzbodrst

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbctrck with the legacy context.
      subroutine zzbctrck ( usrctr, update )
      use pepper_kinds, only: ip
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: usrctr    ( ctrsiz )  !! ZZBODTRN state counter tracked by the caller
      logical :: update  !! Flag indicating if input counter was updated
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzbctrck ( ctx, usrctr, update )
      end subroutine zzbctrck

      !> Translate the name of a body or object to the corresponding SPICE
      !> integer ID code.
      !>
      !> Legacy interface: calls ctx%bodn2c with the legacy context.
      subroutine bodn2c ( name, code, found )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Body name to be translated into a SPICE ID code.
      integer(ip) :: code  !! SPICE integer ID code for the named body.
      logical :: found  !! .TRUE. if translated, otherwise false.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%bodn2c ( name, code, found )
      end subroutine bodn2c

      !> Translate the SPICE integer code of a body into a common name
      !> for that body.
      !>
      !> Legacy interface: calls ctx%bodc2n with the legacy context.
      subroutine bodc2n ( code, name, found )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: code  !! Integer ID code to be translated into a name.
      character(*) :: name  !! A common name for the body identified by CODE.
      logical :: found  !! .TRUE. if translated, otherwise false.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%bodc2n ( code, name, found )
      end subroutine bodc2n

      !> Translate a string containing a body name or ID code to an
      !> integer code.
      !>
      !> Legacy interface: calls ctx%bods2c with the legacy context.
      subroutine bods2c ( name, code, found )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! String to be translated to an ID code.
      integer(ip) :: code  !! Integer ID code corresponding to NAME.
      logical :: found  !! Flag indicating whether translation succeeded.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%bods2c ( name, code, found )
      end subroutine bods2c

      !> Translate a body ID code to either the corresponding name
      !> or if no name to ID code mapping exists, the string
      !> representation of the body ID value.
      !>
      !> Legacy interface: calls ctx%bodc2s with the legacy context.
      subroutine bodc2s ( code, name )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: code  !! Integer ID code to translate to a string.
      character(*) :: name  !! String corresponding to CODE.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%bodc2s ( code, name )
      end subroutine bodc2s

      !> Define a body name/ID code pair for later translation via
      !> BODN2C or BODC2N.
      !>
      !> Legacy interface: calls spice%boddef of the legacy instance.
      subroutine boddef ( name, code )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Common name of some body.
      integer(ip) :: code  !! Integer code for that body.
      type(pepper_instance), pointer :: spice
      spice => legacy_spice()
      call spice%boddef ( name, code )
      end subroutine boddef

      !> Determine whether values exist for some item for any body
      !> in the kernel pool.
      !>
      !> Legacy interface: calls ctx%bodfnd with the legacy context.
      logical function bodfnd ( body, item )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: body  !! ID code of body.
      character(*) :: item  !! Item to find ('RADII', 'NUT_AMP_RA', etc.).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      bodfnd = ctx%bodfnd ( body, item )
      end function bodfnd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbods2c with the legacy context.
      subroutine zzbods2c ( usrctr, savnam, savcde, savfnd, &
                                    name,   code,   found    )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: usrctr    ( ctrsiz )  !! ZZBODTRN state counter saved by the caller.
      character(*) :: savnam  !! Body name saved by the caller.
      integer(ip) :: savcde  !! Body ID code saved by the caller.
      logical :: savfnd  !! Translation success flag saved in the caller.
      character(*) :: name  !! Body name.
      integer(ip) :: code  !! Body ID code.
      logical :: found  !! Translation success flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call pepper_zzbods2c ( ctx, usrctr, savnam, savcde, savfnd, (name), code, found )
      end subroutine zzbods2c

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbodvcd with the legacy context.
      subroutine zzbodvcd ( bodyid, item, maxn, varctr, n, values )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: bodyid  !! Body ID code.
      character(*) :: item  !! Item for which values are desired.
      integer(ip) :: maxn  !! Maximum number of values that may be returned.
      integer(ip) :: varctr ( ctrsiz )  !! POOL state counter saved by the caller.
      integer(ip) :: n  !! Number of values returned.
      real(wp) :: values ( * )  !! Array of double precision values.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzbodvcd ( ctx, bodyid, item, maxn, varctr, n, values )
      end subroutine zzbodvcd

      !> **Deprecated:** This routine has been superseded by BODVCD and
      !> BODVRD. This routine is supported for purposes of backward
      !> compatibility only.
      !>
      !> Legacy interface: calls ctx%bodvar with the legacy context.
      subroutine bodvar ( body, item, dim, values )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: body  !! ID code of body.
      character(*) :: item  !! Item for which values are desired.
      integer(ip) :: dim  !! Number of values returned.
      real(wp) :: values  ( * )  !! Values.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%bodvar ( body, item, dim, values )
      end subroutine bodvar

      !> Fetch from the kernel pool the double precision values
      !> of an item associated with a body, where the body is
      !> specified by an integer ID code.
      !>
      !> Legacy interface: calls ctx%bodvcd with the legacy context.
      subroutine bodvcd ( bodyid, item, maxn, dim, values )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: bodyid  !! Body ID code.
      character(*) :: item  !! Item for which values are desired. ('RADII', 'NUT_PREC_ANGLES', etc.
                            !! )
      integer(ip) :: maxn  !! Maximum number of values that may be returned.
      integer(ip) :: dim  !! Number of values returned.
      real(wp) :: values  ( * )  !! Values.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%bodvcd ( bodyid, item, maxn, dim, values )
      end subroutine bodvcd

      !> Fetch from the kernel pool the double precision values
      !> of an item associated with a body.
      !>
      !> Legacy interface: calls ctx%bodvrd with the legacy context.
      subroutine bodvrd ( bodynm, item, maxn, dim, values )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      character(*) :: bodynm  !! Body name.
      character(*) :: item  !! Item for which values are desired. ('RADII', 'NUT_PREC_ANGLES', etc.
                            !! )
      integer(ip) :: maxn  !! Maximum number of values that may be returned.
      integer(ip) :: dim  !! Number of values returned.
      real(wp) :: values  ( * )  !! Values.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%bodvrd ( bodynm, item, maxn, dim, values )
      end subroutine bodvrd

end module pepper_legacy_bodies
