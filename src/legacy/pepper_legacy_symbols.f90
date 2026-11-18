!> The legacy interface's routines of the subsystem symbols (45 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_symbols

   use pepper_instance_module, only: pepper_context
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbcell

   implicit none

   private
   public :: sydelc, sydeld, sydeli, sydimc, sydimd, sydimi, sydupc, sydupd, sydupi, syfetc, &
      syfetd, syfeti, sygetc, sygetd, sygeti, synthc, synthd, synthi, syordc, syordd, syordi, &
      sypopc, sypopd, sypopi, syputc, syputd, syputi, syselc, syseld, syseli, sysetc, sysetd, &
      syseti, sytrnc, sytrnd, sytrni, syenqc, syenqd, syenqi, sypshc, sypshd, sypshi, syrenc, &
      syrend, syreni

contains

      !> Delete a symbol from a character symbol table. The symbol and its
      !> associated values are deleted.
      !>
      !> Legacy interface: calls ctx%sydelc with the legacy context.
      subroutine sydelc ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol to be deleted.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sydelc ( (name), tabsym, tabptr, tabval )
      end subroutine sydelc

      !> Delete a symbol from a double precision symbol table. The symbol
      !> and its associated values are deleted.
      !>
      !> Legacy interface: calls ctx%sydeld with the legacy context.
      subroutine sydeld ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol to be deleted.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sydeld ( (name), tabsym, tabptr, tabval )
      end subroutine sydeld

      !> Delete a symbol from an integer symbol table. The symbol
      !> and its associated values are deleted.
      !>
      !> Legacy interface: calls ctx%sydeli with the legacy context.
      subroutine sydeli ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol to be deleted.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sydeli ( (name), tabsym, tabptr, tabval )
      end subroutine sydeli

      !> Return the dimension of a particular symbol in a character symbol
      !> table. If the symbol is not found, the function returns the value
      !> zero.
      !>
      !> Legacy interface: calls ctx%sydimc with the legacy context.
      integer(ip) function sydimc ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose dimension is desired.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      sydimc = ctx%sydimc ( name, tabsym, tabptr, tabval )
      end function sydimc

      !> Return the dimension of a particular symbol in a double precision
      !> symbol table. If the symbol is not found, the function returns the
      !> value zero.
      !>
      !> Legacy interface: calls ctx%sydimd with the legacy context.
      integer(ip) function sydimd ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol whose dimension is desired.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      sydimd = ctx%sydimd ( name, tabsym, tabptr, tabval )
      end function sydimd

      !> Return the dimension of a particular symbol in an integer symbol
      !> table. If the symbol is not found, the function returns the
      !> value zero.
      !>
      !> Legacy interface: calls ctx%sydimi with the legacy context.
      integer(ip) function sydimi ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose dimension is desired.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      sydimi = ctx%sydimi ( name, tabsym, tabptr, tabval )
      end function sydimi

      !> Create a duplicate of a symbol within a character symbol table.
      !> If a symbol with the new name already exists, its components
      !> are replaced.
      !>
      !> Legacy interface: calls ctx%sydupc with the legacy context.
      subroutine sydupc ( name, copy, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol to be duplicated.
      character(*) :: copy  !! Name of the new symbol.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name, copy are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sydupc ( (name), (copy), tabsym, tabptr, tabval )
      end subroutine sydupc

      !> Create a duplicate of a symbol within a double precision symbol
      !> table. If a symbol with the new name already exists, its
      !> components are replaced.
      !>
      !> Legacy interface: calls ctx%sydupd with the legacy context.
      subroutine sydupd ( name, copy, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol to be duplicated.
      character(*) :: copy  !! Name of the new symbol.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name, copy are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sydupd ( (name), (copy), tabsym, tabptr, tabval )
      end subroutine sydupd

      !> Create a duplicate of a symbol within an integer symbol table.
      !> If a symbol with the new name already exists, its components
      !> are replaced.
      !>
      !> Legacy interface: calls ctx%sydupi with the legacy context.
      subroutine sydupi ( name, copy, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol to be duplicated.
      character(*) :: copy  !! Name of the new symbol.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name, copy are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sydupi ( (name), (copy), tabsym, tabptr, tabval )
      end subroutine sydupi

      !> Fetch the Nth symbol in a character symbol table.
      !>
      !> Legacy interface: calls ctx%syfetc with the legacy context.
      subroutine syfetc ( nth, tabsym, tabptr, tabval, name, found )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: nth  !! Index of symbol to be fetched.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: name  !! Name of the NTH symbol in the symbol table.
      logical :: found  !! .TRUE. if the NTH symbol is in the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%syfetc ( nth, tabsym, tabptr, tabval, name, found )
      end subroutine syfetc

      !> Fetch the Nth symbol in a double precision symbol table.
      !>
      !> Legacy interface: calls ctx%syfetd with the legacy context.
      subroutine syfetd ( nth, tabsym, tabptr, tabval, name, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: nth  !! Index of symbol to be fetched.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: name  !! Name of the NTH symbol in the symbol table.
      logical :: found  !! .TRUE. if the NTH symbol is in the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%syfetd ( nth, tabsym, tabptr, tabval, name, found )
      end subroutine syfetd

      !> Fetch the Nth symbol in an integer symbol table.
      !>
      !> Legacy interface: calls ctx%syfeti with the legacy context.
      subroutine syfeti ( nth, tabsym, tabptr, tabval, name, found )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: nth  !! Index of symbol to be fetched.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: name  !! Name of the NTH symbol in the symbol table.
      logical :: found  !! .TRUE. if the NTH symbol is in the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%syfeti ( nth, tabsym, tabptr, tabval, name, found )
      end subroutine syfeti

      !> Return the dimension and associated values for a particular
      !> symbol.
      !>
      !> Legacy interface: calls ctx%sygetc with the legacy context.
      subroutine sygetc ( name, &
                          tabsym, tabptr, tabval, &
                          n,      values, &
                          found                     )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose components are to be returned.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: n  !! Dimension of the symbol.
      character(*) :: values     (        * )  !! Values associated with the symbol.
      logical :: found  !! .TRUE. if the symbol NAME is in the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sygetc ( (name), tabsym, tabptr, tabval, n, values, found )
      end subroutine sygetc

      !> Return the dimension and associated values for a particular
      !> symbol.
      !>
      !> Legacy interface: calls ctx%sygetd with the legacy context.
      subroutine sygetd ( name, &
                          tabsym, tabptr, tabval, &
                          n,      values, &
                          found                     )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol whose components are to be returned.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: n  !! Dimension of the symbol.
      real(wp) :: values     (        * )  !! Values associated with the symbol.
      logical :: found  !! .TRUE. if the symbol NAME is in the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sygetd ( name, tabsym, tabptr, tabval, n, values, found )
      end subroutine sygetd

      !> Return the dimension and associated values for a particular
      !> symbol.
      !>
      !> Legacy interface: calls ctx%sygeti with the legacy context.
      subroutine sygeti ( name, &
                          tabsym, tabptr, tabval, &
                          n,      values, &
                          found                     )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose components are to be returned.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: n  !! Dimension of the symbol.
      integer(ip) :: values     (        * )  !! Values associated with the symbol.
      logical :: found  !! .TRUE. if the symbol NAME is in the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sygeti ( name, tabsym, tabptr, tabval, n, values, found )
      end subroutine sygeti

      !> Return the Nth value associated with a particular symbol in a
      !> character symbol table.
      !>
      !> Legacy interface: calls ctx%synthc with the legacy context.
      subroutine synthc ( name, &
                          nth, &
                          tabsym, tabptr, tabval, &
                          value, &
                          found                               )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose Nth associated value is to be returned.
      integer(ip) :: nth  !! Index of the value to be returned.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: value  !! Nth value associated with the symbol.
      logical :: found  !! .TRUE. if the Nth value of the symbol exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%synthc ( (name), nth, tabsym, tabptr, tabval, value, found )
      end subroutine synthc

      !> Return the Nth component of a particular symbol in a double
      !> precision symbol table.
      !>
      !> Legacy interface: calls ctx%synthd with the legacy context.
      subroutine synthd ( name, &
                          nth, &
                          tabsym, tabptr, tabval, &
                          value, &
                          found                               )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol whose Nth component is to be returned.
      integer(ip) :: nth  !! Index of the value to be returned.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: value  !! Nth value associated with the symbol.
      logical :: found  !! .TRUE. if the Nth value of the symbol exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%synthd ( name, nth, tabsym, tabptr, tabval, value, found )
      end subroutine synthd

      !> Return the Nth component of a particular symbol in an integer
      !> symbol table.
      !>
      !> Legacy interface: calls ctx%synthi with the legacy context.
      subroutine synthi ( name, &
                          nth, &
                          tabsym, tabptr, tabval, &
                          value, &
                          found                               )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose Nth component is to be returned.
      integer(ip) :: nth  !! Index of the value to be returned.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: value  !! Nth value associated with the symbol.
      logical :: found  !! .TRUE. if the Nth value of the symbol exists, false if it does not.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%synthi ( name, nth, tabsym, tabptr, tabval, value, found )
      end subroutine synthi

      !> Order the components of a single symbol in a character symbol
      !> table. The components are ordered according to the ASCII collating
      !> sequence.
      !>
      !> Legacy interface: calls ctx%syordc with the legacy context.
      subroutine syordc ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose components are to be ordered.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syordc ( (name), tabsym, tabptr, tabval )
      end subroutine syordc

      !> Order the components of a single symbol in a double precision
      !> symbol table. The components are sorted in increasing order.
      !>
      !> Legacy interface: calls ctx%syordd with the legacy context.
      subroutine syordd ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol whose components are to be ordered.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syordd ( (name), tabsym, tabptr, tabval )
      end subroutine syordd

      !> Order the components of a single symbol in an integer symbol
      !> table. The components are sorted in increasing order.
      !>
      !> Legacy interface: calls ctx%syordi with the legacy context.
      subroutine syordi ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose components are to be ordered.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syordi ( (name), tabsym, tabptr, tabval )
      end subroutine syordi

      !> Pop a value associated with a particular symbol in a character
      !> symbol table. The first value associated with the symbol is
      !> removed, and subsequent values are moved forward.
      !>
      !> Legacy interface: calls ctx%sypopc with the legacy context.
      subroutine sypopc ( name, tabsym, tabptr, tabval, value, found )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose associated value is to be popped.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: value  !! Value that was popped.
      logical :: found  !! .TRUE. if the symbol exists, .FALSE. otherwise.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sypopc ( (name), tabsym, tabptr, tabval, value, found )
      end subroutine sypopc

      !> Pop a value associated with a particular symbol in a double
      !> precision symbol table. The first value associated with the
      !> symbol is removed, and subsequent values are moved forward.
      !>
      !> Legacy interface: calls ctx%sypopd with the legacy context.
      subroutine sypopd ( name, tabsym, tabptr, tabval, value, found )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol whose associated value is to be popped.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: value  !! Value that was popped.
      logical :: found  !! .TRUE. if the symbol exists, .FALSE. otherwise.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sypopd ( (name), tabsym, tabptr, tabval, value, found )
      end subroutine sypopd

      !> Pop a value associated with a particular symbol in an integer
      !> symbol table. The first value associated with the symbol is
      !> removed, and subsequent values are moved forward.
      !>
      !> Legacy interface: calls ctx%sypopi with the legacy context.
      subroutine sypopi ( name, tabsym, tabptr, tabval, value, found )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose associated value is to be popped.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: value  !! Value that was popped.
      logical :: found  !! .TRUE. if the symbol exists, .FALSE. otherwise.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sypopi ( (name), tabsym, tabptr, tabval, value, found )
      end subroutine sypopi

      !> Set the values of a particular symbol in a character symbol table.
      !> If the symbol already exists, the previous values associated with
      !> it are removed, otherwise a new symbol is created.
      !>
      !> Legacy interface: calls ctx%syputc with the legacy context.
      subroutine syputc ( name, values, n, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose associated values are to be put into the symbol table.
      character(*) :: values     (        * )  !! Values to be associated with the symbol NAME.
      integer(ip) :: n  !! Number of values in VALUES.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syputc ( (name), values, n, tabsym, tabptr, tabval )
      end subroutine syputc

      !> Set the values of a particular symbol in a double precision
      !> symbol table. If the symbol already exists, the previous values
      !> associated with it are removed, otherwise a new symbol is created.
      !>
      !> Legacy interface: calls ctx%syputd with the legacy context.
      subroutine syputd ( name, values, n, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol whose associated values are to be put into the symbol table.
      real(wp) :: values     (        * )  !! Values to be associated with the symbol NAME.
      integer(ip) :: n  !! Number of values in VALUES.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syputd ( (name), values, n, tabsym, tabptr, tabval )
      end subroutine syputd

      !> Set the values of a particular symbol in an integer symbol table.
      !> If the symbol already exists, the previous values associated with
      !> it are removed, otherwise a new symbol is created.
      !>
      !> Legacy interface: calls ctx%syputi with the legacy context.
      subroutine syputi ( name, values, n, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose associated values are to be put into the symbol table.
      integer(ip) :: values     (        * )  !! Values to be associated with the symbol NAME.
      integer(ip) :: n  !! Number of values in VALUES.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syputi ( (name), values, n, tabsym, tabptr, tabval )
      end subroutine syputi

      !> Select a subset of the values associated with a particular
      !> symbol in a character symbol table.
      !>
      !> Legacy interface: calls ctx%syselc with the legacy context.
      subroutine syselc ( name, &
                          begin, &
                          end, &
                          tabsym, tabptr, tabval, &
                          values, &
                          found                     )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose associated values are to be returned.
      integer(ip) :: begin  !! Index of the first associated value to be returned.
      integer(ip) :: end  !! Index of the last associated value to be returned.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: values     (        * )  !! Subset of the values associated with the symbol NAME.
      logical :: found  !! .TRUE. if the subset of values exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syselc ( (name), begin, end, tabsym, tabptr, tabval, values, found )
      end subroutine syselc

      !> Select a subset of the values associated with a particular
      !> symbol in a double precision symbol table.
      !>
      !> Legacy interface: calls ctx%syseld with the legacy context.
      subroutine syseld ( name, &
                          begin, &
                          end, &
                          tabsym, tabptr, tabval, &
                          values, &
                          found                     )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol whose associated values are to be returned.
      integer(ip) :: begin  !! Index of the first associated value to be returned.
      integer(ip) :: end  !! Index of the last associated value to be returned.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: values     (        * )  !! Subset of the values associated with the symbol NAME.
      logical :: found  !! .TRUE. if the subset of values exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%syseld ( name, begin, end, tabsym, tabptr, tabval, values, found )
      end subroutine syseld

      !> Select a subset of the values associated with a particular
      !> symbol in an integer symbol table.
      !>
      !> Legacy interface: calls ctx%syseli with the legacy context.
      subroutine syseli ( name, &
                          begin, &
                          end, &
                          tabsym, tabptr, tabval, &
                          values, &
                          found                     )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose associated values are to be returned.
      integer(ip) :: begin  !! Index of the first associated value to be returned.
      integer(ip) :: end  !! Index of the last associated value to be returned.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: values     (        * )  !! Subset of the values associated with the symbol NAME.
      logical :: found  !! .TRUE. if the subset of values exists.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%syseli ( name, begin, end, tabsym, tabptr, tabval, values, found )
      end subroutine syseli

      !> Set the value of a particular symbol in a character symbol table.
      !> If the symbol already exists, the previous values associated with
      !> it are removed, otherwise a new symbol is created.
      !>
      !> Legacy interface: calls ctx%sysetc with the legacy context.
      subroutine sysetc ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose associated value is to be set.
      character(*) :: value  !! Associated value of the symbol NAME.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name, value are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sysetc ( (name), (value), tabsym, tabptr, tabval )
      end subroutine sysetc

      !> Set the value of a particular symbol in a double precision symbol
      !> table. If the symbol already exists, the previous values
      !> associated with it are removed, otherwise a new symbol is created.
      !>
      !> Legacy interface: calls ctx%sysetd with the legacy context.
      subroutine sysetd ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol whose associated value is to be set.
      real(wp) :: value  !! Associated value of the symbol NAME.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sysetd ( (name), value, tabsym, tabptr, tabval )
      end subroutine sysetd

      !> Set the value of a particular symbol in an integer symbol table.
      !> If the symbol already exists, the previous values associated with
      !> it are removed, otherwise a new symbol is created.
      !>
      !> Legacy interface: calls ctx%syseti with the legacy context.
      subroutine syseti ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose associated value is to be set.
      integer(ip) :: value  !! Associated value of the symbol NAME.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syseti ( (name), value, tabsym, tabptr, tabval )
      end subroutine syseti

      !> Transpose two values associated with a particular symbol in a
      !> character symbol table.
      !>
      !> Legacy interface: calls ctx%sytrnc with the legacy context.
      subroutine sytrnc ( name, idx1, idx2, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose associated values are to be transposed.
      integer(ip) :: idx1  !! Index of first associated value to be transposed.
      integer(ip) :: idx2  !! Index of second associated value to be transposed.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sytrnc ( (name), idx1, idx2, tabsym, tabptr, tabval )
      end subroutine sytrnc

      !> Transpose two values associated with a particular symbol in a
      !> double precision symbol table.
      !>
      !> Legacy interface: calls ctx%sytrnd with the legacy context.
      subroutine sytrnd ( name, idx1, idx2, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol whose associated values are to be transposed.
      integer(ip) :: idx1  !! Index of first associated value to be transposed.
      integer(ip) :: idx2  !! Index of second associated value to be transposed.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sytrnd ( (name), idx1, idx2, tabsym, tabptr, tabval )
      end subroutine sytrnd

      !> Transpose two values associated with a particular symbol in an
      !> integer symbol table.
      !>
      !> Legacy interface: calls ctx%sytrni with the legacy context.
      subroutine sytrni ( name, idx1, idx2, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol whose associated values are to be transposed.
      integer(ip) :: idx1  !! Index of first associated value to be transposed.
      integer(ip) :: idx2  !! Index of second associated value to be transposed.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sytrni ( (name), idx1, idx2, tabsym, tabptr, tabval )
      end subroutine sytrni

      !> Enqueue a value onto a particular symbol in a character
      !> symbol table. If the symbol is not in the table, a new one
      !> is created.
      !>
      !> Legacy interface: calls ctx%syenqc with the legacy context.
      subroutine syenqc ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol onto which the value is enqueued.
      character(*) :: value  !! Value to be enqueued.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name, value are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syenqc ( (name), (value), tabsym, tabptr, tabval )
      end subroutine syenqc

      !> Enqueue a value onto a particular symbol in a double precision
      !> symbol table. If the symbol is not in the table, a new one
      !> is created.
      !>
      !> Legacy interface: calls ctx%syenqd with the legacy context.
      subroutine syenqd ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol onto which the value is enqueued.
      real(wp) :: value  !! Value to be enqueued.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syenqd ( (name), value, tabsym, tabptr, tabval )
      end subroutine syenqd

      !> Enqueue a value onto a particular symbol in an integer
      !> symbol table. If the symbol is not in the table, a new one
      !> is created.
      !>
      !> Legacy interface: calls ctx%syenqi with the legacy context.
      subroutine syenqi ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol onto which the value is enqueued.
      integer(ip) :: value  !! Value to be enqueued.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syenqi ( (name), value, tabsym, tabptr, tabval )
      end subroutine syenqi

      !> Push a value onto a particular symbol in a character symbol table.
      !> The previous value(s) associated with the symbol is extended at
      !> the front. A new symbol is created if necessary.
      !>
      !> Legacy interface: calls ctx%sypshc with the legacy context.
      subroutine sypshc ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol onto which the value is to be pushed.
      character(*) :: value  !! Value that is to be pushed onto the symbol NAME.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name, value are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sypshc ( (name), (value), tabsym, tabptr, tabval )
      end subroutine sypshc

      !> Push a value onto a particular symbol in a double precision
      !> symbol table. The previous value(s) associated with the symbol
      !> is extended at the front. A new symbol is created if necessary.
      !>
      !> Legacy interface: calls ctx%sypshd with the legacy context.
      subroutine sypshd ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: name  !! Name of the symbol onto which the value is to be pushed.
      real(wp) :: value  !! Value that is to be pushed onto the symbol NAME.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sypshd ( (name), value, tabsym, tabptr, tabval )
      end subroutine sypshd

      !> Push a value onto a particular symbol in an integer symbol table.
      !> The previous value(s) associated with the symbol is extended at
      !> the front. A new symbol is created if necessary.
      !>
      !> Legacy interface: calls ctx%sypshi with the legacy context.
      subroutine sypshi ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: name  !! Name of the symbol onto which the value is to be pushed.
      integer(ip) :: value  !! Value that is to be pushed onto the symbol NAME.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: name is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%sypshi ( (name), value, tabsym, tabptr, tabval )
      end subroutine sypshi

      !> Rename an existing symbol in a character symbol table.
      !>
      !> Legacy interface: calls ctx%syrenc with the legacy context.
      subroutine syrenc ( old, new, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: old  !! Name of the symbol to be renamed.
      character(*) :: new  !! New name of the symbol.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: old, new are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syrenc ( (old), (new), tabsym, tabptr, tabval )
      end subroutine syrenc

      !> Rename an existing symbol in a double precision symbol table.
      !>
      !> Legacy interface: calls ctx%syrend with the legacy context.
      subroutine syrend ( old, new, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: old  !! Name of the symbol to be renamed.
      character(*) :: new  !! New name of the symbol.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: old, new are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syrend ( (old), (new), tabsym, tabptr, tabval )
      end subroutine syrend

      !> Rename an existing symbol in an integer symbol table.
      !>
      !> Legacy interface: calls ctx%syreni with the legacy context.
      subroutine syreni ( old, new, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: old  !! Name of the symbol to be renamed.
      character(*) :: new  !! New name of the symbol.
      character(*) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: old, new are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%syreni ( (old), (new), tabsym, tabptr, tabval )
      end subroutine syreni

end module pepper_legacy_symbols
