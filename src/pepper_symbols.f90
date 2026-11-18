!> Symbol tables: names associated with sets of character, double precision or integer values (SY*).
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]).
submodule (pepper_instance_module) pepper_symbols

   implicit none

contains

      !> Delete a symbol from a character symbol table. The symbol and its
      !> associated values are deleted.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol to be deleted from the symbol
      !>              table.
      !>
      !>              If the symbol does not exist, the symbol table remains
      !>              unchanged. This subroutine is case sensitive. NAME must
      !>              the symbol exactly.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>              On input, the table may or may not contain the symbol
      !>              NAME.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>              On output, the symbol table no longer contains the symbol
      !>              NAME or its associated values. If NAME is not a symbol,
      !>              the components of the symbol table remain unchanged.
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
      !>     None.
      !>
      !>## Examples
      !>
      !>     In the following example the subroutine SYDELC is used to delete
      !>     the symbol "BOHR" and its values from the symbol table.
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>
      !>     The call
      !>
      !>        CALL SYDELC ( 'BOHR', TABSYM, TABPTR, TABVAL )
      !>
      !>     deletes the symbol "BOHR" from the symbol table. The components
      !>     of the symbol table on output are:
      !>
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sydelc ( ctx, name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol to be deleted.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: dimval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardc ( ctx, tabval )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, we're done. If it is, we can proceed
!     without fear of overflow.
!
      if ( locsym > 0 ) then

         locval = sum(tabptr(1:locsym-1)) + 1
         dimval = tabptr(locsym)

         call remlac ( ctx, 1, locsym, tabsym(1), nsym )
         call scardc ( ctx, nsym, tabsym )

         call remlai ( ctx, 1, locsym, tabptr(1), nptr )
         call scardi ( ctx, nptr, tabptr )

         call remlac ( ctx, dimval, locval, tabval(1), nval )
         call scardc ( ctx, nval, tabval )

      end if

      return
      end subroutine sydelc

      !> Delete a symbol from a double precision symbol table. The symbol
      !> and its associated values are deleted.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol to be deleted from the symbol
      !>              table.
      !>
      !>              If the symbol does not exist, the symbol table remains
      !>              unchanged. This subroutine is case sensitive, NAME must
      !>              match the symbol exactly.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>              On input, the table may or may not contain the symbol
      !>              NAME.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>              On output, the symbol table no longer contains the symbol
      !>              NAME or its associated values. If NAME is not a symbol,
      !>              the components of the symbol table remain unchanged.
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
      !>     None.
      !>
      !>## Examples
      !>
      !>     In the following example the subroutine SYDELD is used to delete
      !>     the symbol "MEAN_ANOM" and its values from the symbol table.
      !>
      !>     The contents of the symbol table are:
      !>
      !>        DELTA_T_A -->   32.184
      !>        K         -->    1.657D-3
      !>        MEAN_ANOM -->    6.239996D0
      !>                         1.99096871D-7
      !>        ORBIT_ECC -->    1.671D-2
      !>
      !>     The call
      !>
      !>        CALL SYDELC ( 'MEAN_ANOM', TABSYM, TABPTR, TABVAL )
      !>
      !>     deletes the symbol "MEAN_ANOM" from the symbol table. The
      !>     components of the symbol table on output are:
      !>
      !>        DELTA_T_A -->   32.184
      !>        K         -->    1.657D-3
      !>        ORBIT_ECC -->    1.671D-2
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sydeld ( ctx, name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol to be deleted.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: dimval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardd ( ctx, tabval )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, we're done. If it is, we can proceed
!     without fear of overflow.
!
      if ( locsym > 0 ) then

         locval = sum(tabptr(1:locsym-1)) + 1
         dimval = tabptr(locsym)

         call remlac ( ctx, 1, locsym, tabsym(1), nsym )
         call scardc ( ctx, nsym, tabsym )

         call remlai ( ctx, 1, locsym, tabptr(1), nptr )
         call scardi ( ctx, nptr, tabptr )

         call remlad ( ctx, dimval, locval, tabval(1), nval )
         call scardd ( ctx, nval, tabval )

      end if

      return
      end subroutine sydeld

      !> Delete a symbol from an integer symbol table. The symbol
      !> and its associated values are deleted.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol to be deleted from the symbol
      !>              table.
      !>
      !>              If the symbol does not exist, the symbol table remains
      !>              unchanged. This subroutine is case sensitive, NAME must
      !>              match the symbol exactly.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>              On input, the table may or may not contain the symbol
      !>              NAME.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>              On output, the symbol table no longer contains the symbol
      !>              NAME or its associated values. If NAME is not a symbol,
      !>              the components of the symbol table remain unchanged.
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
      !>     None.
      !>
      !>## Examples
      !>
      !>     In the following example the subroutine SYDELI is used to delete
      !>     the symbol "pens" and its values from the symbol table.
      !>
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     The call
      !>
      !>        CALL SYDELI ( 'pens', TABSYM, TABPTR, TABVAL )
      !>
      !>     deletes the symbol "pens" from the symbol table. The
      !>     components of the symbol table on output are:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sydeli ( ctx, name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol to be deleted.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: dimval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardi ( ctx, tabval )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, we're done. If it is, we can proceed
!     without fear of overflow.
!
      if ( locsym > 0 ) then

         locval = sum(tabptr(1:locsym-1)) + 1
         dimval = tabptr(locsym)

         call remlac ( ctx, 1, locsym, tabsym(1), nsym )
         call scardc ( ctx, nsym, tabsym )

         call remlai ( ctx, 1, locsym, tabptr(1), nptr )
         call scardi ( ctx, nptr, tabptr )

         call remlai ( ctx, dimval, locval, tabval(1), nval )
         call scardi ( ctx, nval, tabval )

      end if

      return
      end subroutine sydeli

      !> Return the dimension of a particular symbol in a character symbol
      !> table. If the symbol is not found, the function returns the value
      !> zero.
      !>
      !> The function returns the dimension of the symbol NAME. If NAME is
      !> not in the symbol table, the function returns the value zero.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose dimension is to be
      !>              returned. If the symbol is not in the symbol table, the
      !>              function returns the value zero. This function is case
      !>              sensitive, NAME must match a symbol exactly.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table. The table
      !>              may or may not contain the symbol NAME.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the dimension of the symbol NAME. The
      !>     dimension of a symbol is the number of values associated with
      !>     that symbol. If NAME is not in the symbol table, the function
      !>     returns the value zero.
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
      !>     None.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>
      !>     Perhaps we want to know how many subjects are associated with
      !>     certain scientists. The following code returns the values of
      !>     NUMSUB indicated in the table.
      !>
      !>     NUMSUB = SYDIMC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL )
      !>     NUMSUB = SYDIMC ( 'BOHR',     TABSYM, TABPTR, TABVAL )
      !>     NUMSUB = SYDIMC ( 'FERMI',    TABSYM, TABPTR, TABVAL )
      !>     NUMSUB = SYDIMC ( 'MILLIKAN', TABSYM, TABPTR, TABVAL )
      !>
      !>     ----SYMBOL----------NUMSUB------
      !>     | EINSTEIN     |       3       |
      !>     | BOHR         |       1       |
      !>     | FERMI        |       1       |
      !>     | MILLIKAN     |       0       |
      !>     --------------------------------
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
      !>-    SPICELIB Version 1.2.0, 07-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        $Brief_I/O to indicate that TABSYM, TABPTR, TABVAL are input
      !>        arguments.
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
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module integer(ip) function sydimc ( ctx, name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose dimension is desired.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: locsym

!
!     Standard SPICE error handling
!
      if ( ctx%failed() ) then
         sydimc = 0
         return
      end if

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, return zero. Otherwise, look up
!     the dimension directly.
!
      if ( locsym == 0 ) then
         sydimc = 0

      else
         sydimc = tabptr(locsym)
      end if

      return
      end function sydimc

      !> Return the dimension of a particular symbol in a double precision
      !> symbol table. If the symbol is not found, the function returns the
      !> value zero.
      !>
      !> The function returns the dimension of the symbol NAME. If NAME is
      !> not in the symbol table, the function returns the value zero.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose dimension is to be
      !>              returned. If the symbol is not in the symbol table, the
      !>              function returns the value zero. This function is case
      !>              sensitive, NAME must match a symbol exactly.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>              The table may or may not contain the symbol NAME.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the dimension of the symbol NAME. The
      !>     dimension of a symbol is the number of values associated with that
      !>     symbol. If NAME is not in the symbol table, the function returns
      !>     the value zero.
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
      !>     None.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        DELTA_T_A -->   32.184
      !>        K         -->    1.657D-3
      !>        MEAN_ANOM -->    6.239996D0
      !>                         1.99096871D-7
      !>
      !>     Let NUMVAL be equal to the dimension of the symbols in the table.
      !>     The following code returns the values of NUMVAL indicated in the
      !>     table.
      !>
      !>     NUMVAL = SYDIMD ( 'MEAN_ANOM',   TABSYM, TABPTR, TABVAL )
      !>     NUMVAL = SYDIMD ( 'K',           TABSYM, TABPTR, TABVAL )
      !>     NUMVAL = SYDIMD ( 'DELTA_T_A',   TABSYM, TABPTR, TABVAL )
      !>     NUMVAL = SYDIMD ( 'BODY10_AXES', TABSYM, TABPTR, TABVAL )
      !>
      !>     ----SYMBOL----------NUMVAL------
      !>     | MEAN_ANOM    |       2       |
      !>     | K            |       1       |
      !>     | DELTA_T_A    |       1       |
      !>     | BODY10_AXES  |       0       |
      !>     --------------------------------
      !>
      !>     Note that the dimension of "BODY10_AXES" is zero. This is due to
      !>     the fact that "BODY10_AXES" is not in the symbol table.
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
      !>-    SPICELIB Version 1.2.0, 07-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        $Brief_I/O to indicate that TABSYM, TABPTR, TABVAL are input
      !>        arguments.
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
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module integer(ip) function sydimd ( ctx, name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose dimension is desired.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: locsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) then
         sydimd = 0
         return
      end if

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, return zero. Otherwise, look up
!     the dimension directly.
!
      if ( locsym == 0 ) then
         sydimd = 0

      else
         sydimd = tabptr(locsym)
      end if

      return
      end function sydimd

      !> Return the dimension of a particular symbol in an integer symbol
      !> table. If the symbol is not found, the function returns the
      !> value zero.
      !>
      !> The function returns the dimension of the symbol NAME. If NAME is
      !> not in the symbol table, the function returns the value zero.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose dimension is to be
      !>              returned. If the symbol is not in the symbol table, the
      !>              function returns the value zero. This function is case
      !>              sensitive, NAME must match a symbol exactly.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table. The table
      !>              may or may not contain the symbol NAME.
      !>
      !>## Detailed Output
      !>
      !>     The function returns the dimension of the symbol NAME. The
      !>     dimension of a symbol is the number of values associated with that
      !>     symbol. If NAME is not in the symbol table, the function returns
      !>     the value zero.
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
      !>     None.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>                      8
      !>        erasers -->   6
      !>        pencils -->  12
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     Let NUMVAL be equal to the dimension of the symbols in the table.
      !>     The following code returns the values of NUMVAL indicated in the
      !>     table.
      !>
      !>     NUMVAL = SYDIMI ( 'books',    TABSYM, TABPTR, TABVAL )
      !>     NUMVAL = SYDIMI ( 'pencils',  TABSYM, TABPTR, TABVAL )
      !>     NUMVAL = SYDIMI ( 'pens',     TABSYM, TABPTR, TABVAL )
      !>     NUMVAL = SYDIMI ( 'erasers',  TABSYM, TABPTR, TABVAL )
      !>     NUMVAL = SYDIMI ( 'tablets',  TABSYM, TABPTR, TABVAL )
      !>
      !>     ----SYMBOL----------NUMVAL------
      !>     | books        |       2       |
      !>     | pencils      |       1       |
      !>     | pens         |       3       |
      !>     | erasers      |       1       |
      !>     | tablets      |       0       |
      !>     --------------------------------
      !>
      !>     Note that the dimension of "tablets" is zero. This is due to the
      !>     fact that "tablets" is not in the symbol table.
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
      !>-    SPICELIB Version 1.2.0, 07-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Updated
      !>        $Brief_I/O to indicate that TABSYM, TABPTR, TABVAL are input
      !>        arguments.
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
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module integer(ip) function sydimi ( ctx, name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose dimension is desired.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: locsym

!
!     Standard SPICE error handling
!
      if ( ctx%failed() ) then
         sydimi = 0
         return
      end if

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, return zero. Otherwise, look up
!     the dimension directly.
!
      if ( locsym == 0 ) then
         sydimi = 0

      else
         sydimi = tabptr(locsym)
      end if

      return
      end function sydimi

      !> Create a duplicate of a symbol within a character symbol table.
      !> If a symbol with the new name already exists, its components
      !> are replaced.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol to be duplicated.
      !>
      !>              The components associated with NAME will be given to the
      !>              new symbol COPY. If NAME is not in the symbol table, no
      !>              duplicate symbol can be made.
      !>
      !>     COPY     is the name of the new symbol. If a symbol with the name
      !>              COPY already exists in the symbol table, its components
      !>              are replaced by the components of NAME.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>              On output, the symbol table contains a new symbol COPY
      !>              whose components are the same as the components of NAME.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the symbol NAME is not in the symbol table, the error
      !>         SPICE(NOSUCHSYMBOL) is signaled.
      !>
      !>     2)  If duplication of the symbol causes an overflow in the
      !>         name table, the error SPICE(NAMETABLEFULL) is signaled.
      !>
      !>     3)  If duplication of the symbol causes an overflow in the
      !>         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
      !>
      !>     4)  If duplication of the symbol causes an overflow in the
      !>         value table, the error SPICE(VALUETABLEFULL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If the symbol NAME is not in the symbol table, no duplicate symbol
      !>     can be made.
      !>
      !>     If the symbol COPY is already in the symbol table, its components
      !>     are replaced by the components of NAME.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>
      !>     The code,
      !>
      !>        CALL SYDUPC ( 'FERMI', 'HAHN', TABSYM, TABPTR, TABVAL )
      !>
      !>     produces the symbol table:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>        HAHN      -->   NUCLEAR FISSION
      !>
      !>     The code,
      !>
      !>        CALL SYDUPC ( 'STRASSMAN', 'HAHN', TABSYM, TABPTR, TABVAL )
      !>
      !>     produces the error SPICE(NOSUCHSYMBOL) because the symbol
      !>     "STRASSMAN" is not in the symbol table.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sydupc ( ctx, name, copy, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol to be duplicated.
      character(*), intent(in) :: copy  !! Name of the new symbol.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym  ( 2 )
      integer(ip) :: locval  ( 2 )
      integer(ip) :: dimval  ( 2 )
      logical :: oldsym  ( 2 )
      integer(ip) :: newsym
      integer(ip) :: newval
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardc ( ctx, tabval )

!
!     Where do these symbols belong? Are they already in the table?
!
      locsym(1) = lstlec ( name, nsym, tabsym(1) )
      locsym(2) = lstlec ( copy, nsym, tabsym(1) )

      oldsym(1) = (       locsym(1)         /= 0 &
                    .and. tabsym(locsym(1)) == name )

      oldsym(2) = (       locsym(2)         /= 0 &
                    .and. tabsym(locsym(2)) == copy )

!
!     If the original symbol is not in the table, we can't make a copy.
!
      if ( .not. oldsym(1) ) then

         call ctx%setmsg ( 'SYDUPC: The symbol to be duplicated, #, is ' // &
                       'not in the symbol table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(NOSUCHSYMBOL)' )

!
!     Otherwise, we need to know the dimension, to check for overflow.
!
      else

         locval(1) = sum(tabptr(1:locsym(1)-1)) + 1
         dimval(1) = tabptr(locsym(1))

!
!        If the new symbol already exists, we need to know its dimension
!        too, for the same reason.
!
         if ( oldsym(2) ) then
            locval(2) = sum(tabptr(1:locsym(2)-1)) + 1
            dimval(2) = tabptr(locsym(2))
            newsym    = 0
         else
            locval(2) = sum(tabptr(1:locsym(2))) + 1
            dimval(2) = 0
            newsym    = 1
         end if

         newval = dimval(1) - dimval(2)

!
!        Can we make a copy without overflow?
!
         if ( nsym+newsym > sizec ( ctx, tabsym ) ) then

            call ctx%setmsg ( 'SYDUPC: Duplication of the symbol # '    // &
                          'causes an overflow in the name table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(NAMETABLEFULL)' )

         else if ( nptr+newsym > sizei ( ctx, tabptr ) ) then

            call ctx%setmsg ( 'SYDUPC: Duplication of the symbol # '    // &
                          'causes an overflow in the pointer table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(POINTERTABLEFULL)' )

         else if ( nval+newval > sizec ( ctx, tabval ) ) then

            call ctx%setmsg ( 'SYDUPC: Duplication of the symbol # '    // &
                          'causes an overflow in the value table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!        Looks like we can.
!
         else

!
!           If the copy exists, remove the current contents and
!           change the dimension. Otherwise add the new name and
!           dimension to the name and pointer tables.
!
            if ( dimval(2) > 0 ) then
              call remlac ( ctx, dimval(2), locval(2), tabval(1), nval )
              call scardc ( ctx, nval, tabval )

              tabptr(locsym(2)) = dimval(1)

              if ( locval(1) > locval(2) ) then
                 locval(1) = locval(1) - dimval(2)
              end if

            else
              call inslac ( ctx, copy, 1, locsym(2)+1, tabsym(1), nsym )
              call scardc ( ctx, nsym, tabsym )

              call inslai ( ctx, dimval(1), 1, locsym(2)+1, tabptr(1), nptr )
              call scardi ( ctx, nptr, tabptr )
            end if

!
!           In either case, allocate space for the new symbol values,
!           and copy them in one by one. (INSLAx won't work if the
!           copy is earlier in the table than the original.)
!
            do i = nval, locval(2), -1
              tabval(i+dimval(1)) = tabval(i)
            end do

            if ( locval(1) > locval(2) ) then
               locval(1) = locval(1) + dimval(1)
            end if

            do i = 0, dimval(1) - 1
              tabval(locval(2)+i) = tabval(locval(1)+i)
            end do

            call scardc ( ctx, nval + dimval(1), tabval )

         end if

      end if

      return
      end subroutine sydupc

      !> Create a duplicate of a symbol within a double precision symbol
      !> table. If a symbol with the new name already exists, its
      !> components are replaced.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol to be duplicated.
      !>
      !>              The components associated with NAME will be given to the
      !>              new symbol COPY. If NAME is not in the symbol table, no
      !>              duplicate symbol can be made.
      !>
      !>     COPY     is the name of the new symbol. If a symbol with the
      !>              name COPY already exists in the symbol table, its
      !>              components are replaced by the components of NAME.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>              On output, the symbol table contains a new symbol COPY
      !>              whose components are the same as the components of
      !>              NAME.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the symbol NAME is not in the symbol table, the error
      !>         SPICE(NOSUCHSYMBOL) is signaled.
      !>
      !>     2)  If duplication of the symbol causes an overflow in the
      !>         name table, the error SPICE(NAMETABLEFULL) is signaled.
      !>
      !>     3)  If duplication of the symbol causes an overflow in the
      !>         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
      !>
      !>     4)  If duplication of the symbol causes an overflow in the
      !>         value table, the error SPICE(VALUETABLEFULL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If the symbol NAME is not in the symbol table, no duplicate symbol
      !>     can be made.
      !>
      !>     If the symbol COPY is already in the symbol table, its components
      !>     are replaced by the components of NAME.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        DELTA_T_A -->   32.184
      !>        K         -->    1.657D-3
      !>        MEAN_ANOM -->    6.239996D0
      !>                         1.99096871D-7
      !>        ORBIT_ECC -->    1.671D-2
      !>
      !>     The code,
      !>
      !>        CALL SYDUPD ( 'K', 'EB', TABSYM, TABPTR, TABVAL )
      !>
      !>     produces the symbol table:
      !>
      !>        DELTA_T_A -->   32.184
      !>        EB        -->    1.657D-3
      !>        K         -->    1.657D-3
      !>        MEAN_ANOM -->    6.239996D0
      !>                         1.99096871D-7
      !>        ORBIT_ECC -->    1.671D-2
      !>
      !>     The code,
      !>
      !>        CALL SYDUPD ( 'M0', 'M1', TABSYM, TABPTR, TABVAL )
      !>
      !>     produces the error SPICE(NOSUCHSYMBOL) because the symbol "M0" is
      !>     not in the symbol table.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sydupd ( ctx, name, copy, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol to be duplicated.
      character(*), intent(in) :: copy  !! Name of the new symbol.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym  ( 2 )
      integer(ip) :: locval  ( 2 )
      integer(ip) :: dimval  ( 2 )
      logical :: oldsym  ( 2 )
      integer(ip) :: newsym
      integer(ip) :: newval
      integer(ip) :: i

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardd ( ctx, tabval )

!
!     Where do these symbols belong? Are they already in the table?
!
      locsym(1) = lstlec ( name, nsym, tabsym(1) )
      locsym(2) = lstlec ( copy, nsym, tabsym(1) )

      oldsym(1) = (       locsym(1)         /= 0 &
                    .and. tabsym(locsym(1)) == name )

      oldsym(2) = (       locsym(2)         /= 0 &
                    .and. tabsym(locsym(2)) == copy )

!
!     If the original symbol is not in the table, we can't make a copy.
!
      if ( .not. oldsym(1) ) then

         call ctx%setmsg ( 'SYDUPD: The symbol to be duplicated, #, is ' // &
                       'not in the symbol table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(NOSUCHSYMBOL)' )

!
!     Otherwise, we need to know the dimension, to check for overflow.
!
      else

         locval(1) = sum(tabptr(1:locsym(1)-1)) + 1
         dimval(1) = tabptr(locsym(1))

!
!        If the new symbol already exists, we need to know its dimension
!        too, for the same reason.
!
         if ( oldsym(2) ) then
            locval(2) = sum(tabptr(1:locsym(2)-1)) + 1
            dimval(2) = tabptr(locsym(2))
            newsym    = 0
         else
            locval(2) = sum(tabptr(1:locsym(2))) + 1
            dimval(2) = 0
            newsym    = 1
         end if

         newval = dimval(1) - dimval(2)

!
!        Can we make a copy without overflow?
!
         if ( nsym+newsym > sizec ( ctx, tabsym ) ) then

            call ctx%setmsg ( 'SYDUPD: Duplication of the symbol # '    // &
                          'causes an overflow in the name table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(NAMETABLEFULL)' )

         else if ( nptr+newsym > sizei ( ctx, tabptr ) ) then

            call ctx%setmsg ( 'SYDUPD: Duplication of the symbol # '    // &
                          'causes an overflow in the pointer table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(POINTERTABLEFULL)' )

         else if ( nval+newval > sized ( ctx, tabval ) ) then

            call ctx%setmsg ( 'SYDUPD: Duplication of the symbol # '    // &
                          'causes an overflow in the value table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!        Looks like we can.
!
         else

!
!           If the copy exists, remove the current contents and
!           change the dimension. Otherwise add the new name and
!           dimension to the name and pointer tables.
!
            if ( dimval(2) > 0 ) then
              call remlad ( ctx, dimval(2), locval(2), tabval(1), nval )
              call scardd ( ctx, nval, tabval )

              tabptr(locsym(2)) = dimval(1)

              if ( locval(1) > locval(2) ) then
                 locval(1) = locval(1) - dimval(2)
              end if

            else
              call inslac ( ctx, copy, 1, locsym(2)+1, tabsym(1), nsym )
              call scardc ( ctx, nsym, tabsym )

              call inslai ( ctx, dimval(1), 1, locsym(2)+1, tabptr(1), nptr )
              call scardi ( ctx, nptr, tabptr )
            end if

!
!           In either case, allocate space for the new symbol values,
!           and copy them in one by one. (INSLAx won't work if the
!           copy is earlier in the table than the original.)
!
            do i = nval, locval(2), -1
              tabval(i+dimval(1)) = tabval(i)
            end do

            if ( locval(1) > locval(2) ) then
               locval(1) = locval(1) + dimval(1)
            end if

            do i = 0, dimval(1) - 1
              tabval(locval(2)+i) = tabval(locval(1)+i)
            end do

            call scardd ( ctx, nval + dimval(1), tabval )

         end if

      end if

      return
      end subroutine sydupd

      !> Create a duplicate of a symbol within an integer symbol table.
      !> If a symbol with the new name already exists, its components
      !> are replaced.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol to be duplicated.
      !>
      !>              The components associated with NAME will be given to the
      !>              new symbol COPY. If NAME is not in the symbol table,
      !>              no duplicate symbol can be made.
      !>
      !>     COPY     is the name of the new symbol. If a symbol with the name
      !>              COPY already exists in the symbol table, its components
      !>              are replaced by the components of NAME.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>              On output, the symbol table contains a new symbol COPY
      !>              whose components are the same as the components of NAME.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the symbol NAME is not in the symbol table, the error
      !>         SPICE(NOSUCHSYMBOL) is signaled.
      !>
      !>     2)  If duplication of the symbol causes an overflow in the
      !>         name table, the error SPICE(NAMETABLEFULL) is signaled.
      !>
      !>     3)  If duplication of the symbol causes an overflow in the
      !>         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
      !>
      !>     4)  If duplication of the symbol causes an overflow in the
      !>         value table, the error SPICE(VALUETABLEFULL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If the symbol NAME is not in the symbol table, no duplicate symbol
      !>     can be made.
      !>
      !>     If the symbol COPY is already in the symbol table, its components
      !>     are replaced by the components of NAME.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     The code,
      !>
      !>        CALL SYDUPI ( 'books', 'tablets', TABSYM, TABPTR, TABVAL )
      !>
      !>     produces the symbol table:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>        tablets -->   5
      !>
      !>     The code,
      !>
      !>        CALL SYDUPC ( 'desks', 'chairs', TABSYM, TABPTR, TABVAL )
      !>
      !>     produces the error SPICE(NOSUCHSYMBOL) because the symbol
      !>     "desks" is not in the symbol table.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sydupi ( ctx, name, copy, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol to be duplicated.
      character(*), intent(in) :: copy  !! Name of the new symbol.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym  ( 2 )
      integer(ip) :: locval  ( 2 )
      integer(ip) :: dimval  ( 2 )
      logical :: oldsym  ( 2 )
      integer(ip) :: newsym
      integer(ip) :: newval
      integer(ip) :: i

!
!     Standard SPICE error handling
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardi ( ctx, tabval )

!
!     Where do these symbols belong? Are they already in the table?
!
      locsym(1) = lstlec ( name, nsym, tabsym(1) )
      locsym(2) = lstlec ( copy, nsym, tabsym(1) )

      oldsym(1) = (       locsym(1)         /= 0 &
                    .and. tabsym(locsym(1)) == name )

      oldsym(2) = (       locsym(2)         /= 0 &
                    .and. tabsym(locsym(2)) == copy )

!
!     If the original symbol is not in the table, we can't make a copy.
!
      if ( .not. oldsym(1) ) then

         call ctx%setmsg ( 'SYDUPI: The symbol to be duplicated, #, is ' // &
                       'not in the symbol table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(NOSUCHSYMBOL)' )

!
!     Otherwise, we need to know the dimension, to check for overflow.
!
      else

         locval(1) = sum(tabptr(1:locsym(1)-1)) + 1
         dimval(1) = tabptr(locsym(1))

!
!        If the new symbol already exists, we need to know its dimension
!        too, for the same reason.
!
         if ( oldsym(2) ) then
            locval(2) = sum(tabptr(1:locsym(2)-1)) + 1
            dimval(2) = tabptr(locsym(2))
            newsym    = 0
         else
            locval(2) = sum(tabptr(1:locsym(2))) + 1
            dimval(2) = 0
            newsym    = 1
         end if

         newval = dimval(1) - dimval(2)

!
!        Can we make a copy without overflow?
!
         if ( nsym+newsym > sizec ( ctx, tabsym ) ) then

            call ctx%setmsg ( 'SYDUPI: Duplication of the symbol # '      // &
                          'causes an overflow in the name table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(NAMETABLEFULL)' )

         else if ( nptr+newsym > sizei ( ctx, tabptr ) ) then

            call ctx%setmsg ( 'SYDUPI: Duplication of the symbol # '      // &
                          'causes an overflow in the pointer table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(POINTERTABLEFULL)' )

         else if ( nval+newval > sizei ( ctx, tabval ) ) then

            call ctx%setmsg ( 'SYDUPI: Duplication of the symbol # '      // &
                          'causes an overflow in the value table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!        Looks like we can.
!
         else

!
!           If the copy exists, remove the current contents and
!           change the dimension. Otherwise add the new name and
!           dimension to the name and pointer tables.
!
            if ( dimval(2) > 0 ) then
              call remlai ( ctx, dimval(2), locval(2), tabval(1), nval )
              call scardi ( ctx, nval, tabval )

              tabptr(locsym(2)) = dimval(1)

              if ( locval(1) > locval(2) ) then
                 locval(1) = locval(1) - dimval(2)
              end if

            else
              call inslac ( ctx, copy, 1, locsym(2)+1, tabsym(1), nsym )
              call scardc ( ctx, nsym, tabsym )

              call inslai ( ctx, dimval(1), 1, locsym(2)+1, tabptr(1), nptr )
              call scardi ( ctx, nptr, tabptr )
            end if

!
!           In either case, allocate space for the new symbol values,
!           and copy them in one by one. (INSLAx won't work if the
!           copy is earlier in the table than the original.)
!
            do i = nval, locval(2), -1
              tabval(i+dimval(1)) = tabval(i)
            end do

            if ( locval(1) > locval(2) ) then
               locval(1) = locval(1) + dimval(1)
            end if

            do i = 0, dimval(1) - 1
              tabval(locval(2)+i) = tabval(locval(1)+i)
            end do

            call scardi ( ctx, nval + dimval(1), tabval )

         end if

      end if

      return
      end subroutine sydupi

      !> Fetch the Nth symbol in a character symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NTH      is the index of the symbol to be fetched. If the NTH
      !>              symbol does not exist, FOUND is .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>              The NTH symbol may or may not be in the symbol
      !>              table. The symbol table is not modified by this
      !>              subroutine.
      !>
      !>## Detailed Output
      !>
      !>     NAME     is the name of the NTH symbol in the symbol table.
      !>
      !>     FOUND    is .TRUE. if the NTH symbol is in the symbol table.
      !>              If the NTH symbol is not in the table, FOUND is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a
      !>         character symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the character symbol table is corrupted.
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
      !>     The contents of the symbol table are:
      !>
      !>         BOHR      -->   HYDROGEN ATOM
      !>         EINSTEIN  -->   SPECIAL RELATIVITY
      !>                         PHOTOELECTRIC EFFECT
      !>                         BROWNIAN MOTION
      !>         FERMI     -->   NUCLEAR FISSION
      !>
      !>      The calls,
      !>
      !>      CALL SYFETC (  2,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>      CALL SYFETC (  3,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>      CALL SYFETC ( -1,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>      CALL SYFETC (  4,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>
      !>      result in the values for NAME and FOUND:
      !>
      !>         NAME        FOUND
      !>         ----------  -------
      !>         EINSTEIN     .TRUE.
      !>         FERMI        .TRUE.
      !>                     .FALSE.
      !>                     .FALSE.
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
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added entry #1
      !>        in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
      !>
      !>         Declaration of the unused variable SUMAI removed.
      module subroutine syfetc ( ctx, nth, tabsym, tabptr, tabval, name, found )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nth  !! Index of symbol to be fetched.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(out) :: name  !! Name of the NTH symbol in the symbol table.
      logical, intent(out) :: found  !! .TRUE. if the NTH symbol is in the symbol table.


      integer(ip) :: nsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     If the value of NTH is out of range, that's a problem.
!
      if ( nth < 1  .or.  nth > nsym ) then

         found = .false.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!
      else

         found = .true.
         name  = tabsym(nth)

      end if

      return
      end subroutine syfetc

      !> Fetch the Nth symbol in a double precision symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NTH      is the index of the symbol to be fetched. If the NTH
      !>              symbol does not exist, FOUND is .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>              The NTH symbol may or may not be in the symbol table.
      !>              The symbol table is not modified by this subroutine.
      !>
      !>## Detailed Output
      !>
      !>     NAME     is the name of the NTH symbol in the symbol table.
      !>
      !>     FOUND    is .TRUE. if the NTH symbol is in the symbol table.
      !>              If the NTH symbol is not in the table, FOUND is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a double
      !>         precision symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the double precision symbol table is corrupted.
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
      !>     The contents of the symbol table are:
      !>
      !>         DELTA_T_A -->   32.184
      !>         K         -->    1.657D-3
      !>         MEAN_ANOM -->    6.239996D0
      !>                          1.99096871D-7
      !>         ORBIT_ECC -->    1.671D-2
      !>
      !>      The calls,
      !>
      !>      CALL SYFETD (  2,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>      CALL SYFETD (  3,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>      CALL SYFETD ( -1,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>      CALL SYFETD (  5,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>
      !>      result in the values for NAME and FOUND:
      !>
      !>         NAME        FOUND
      !>         ----------  -------
      !>         K            .TRUE.
      !>         MEAN_ANOM    .TRUE.
      !>                     .FALSE.
      !>                     .FALSE.
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
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added entry #1
      !>        in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
      !>
      !>         Declaration of the unused variable SUMAI removed.
      module subroutine syfetd ( ctx, nth, tabsym, tabptr, tabval, name, found )
      use pepper_kinds, only: ip, wp
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nth  !! Index of symbol to be fetched.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(out) :: name  !! Name of the NTH symbol in the symbol table.
      logical, intent(out) :: found  !! .TRUE. if the NTH symbol is in the symbol table.


      integer(ip) :: nsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     If the value of NTH is out of range, that's a problem.
!
      if ( nth < 1  .or.  nth > nsym ) then

         found = .false.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!
      else

         found = .true.
         name  = tabsym(nth)

      end if

      return
      end subroutine syfetd

      !> Fetch the Nth symbol in an integer symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NTH      is the index of the symbol to be fetched. If the NTH
      !>              symbol does not exist, FOUND is .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>              The NTH symbol may or may not be in the symbol
      !>              table. The symbol table is not modified by this
      !>              subroutine.
      !>
      !>## Detailed Output
      !>
      !>     NAME     is the name of the NTH symbol in the symbol table.
      !>
      !>     FOUND    is .TRUE. if the NTH symbol is in the symbol table.
      !>              If the NTH symbol is not in the table, FOUND is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a
      !>         integer symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the integer symbol table is corrupted.
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
      !>     The contents of the symbol table are:
      !>
      !>         books   -->   5
      !>         erasers -->   6
      !>         pencils -->  12
      !>         pens    -->  10
      !>                      12
      !>                      24
      !>
      !>      The calls,
      !>
      !>      CALL SYFETI (  2,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>      CALL SYFETI (  3,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>      CALL SYFETI ( -1,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>      CALL SYFETI (  6,  TABSYM, TABPTR, TABVAL, NAME, FOUND )
      !>
      !>      result in the values for NAME and FOUND:
      !>
      !>         NAME        FOUND
      !>         ----------  -------
      !>         erasers      .TRUE.
      !>         pencils      .TRUE.
      !>                     .FALSE.
      !>                     .FALSE.
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
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added entry #1
      !>        in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
      !>
      !>         Declaration of the unused variable SUMAI removed.
      module subroutine syfeti ( ctx, nth, tabsym, tabptr, tabval, name, found )
      use pepper_kinds, only: ip
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: nth  !! Index of symbol to be fetched.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(out) :: name  !! Name of the NTH symbol in the symbol table.
      logical, intent(out) :: found  !! .TRUE. if the NTH symbol is in the symbol table.


      integer(ip) :: nsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     If the value of NTH is out of range, that's a problem.
!
      if ( nth < 1  .or.  nth > nsym ) then

         found = .false.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!
      else

         found = .true.
         name  = tabsym(nth)

      end if

      return
      end subroutine syfeti

      !> Return the dimension and associated values for a particular
      !> symbol.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose components are to be
      !>              returned. If NAME is not in the symbol table, FOUND is
      !>              .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>              The symbol NAME may or may not be in the symbol
      !>              table. The symbol table is not modified by this
      !>              subroutine.
      !>
      !>## Detailed Output
      !>
      !>     N        is the dimension of the symbol NAME. The dimension is
      !>              the number of values associated with the given symbol.
      !>              N is defined only if the output argument FOUND is
      !>              .TRUE.
      !>
      !>     VALUES   is an array containing the values associated with the
      !>              symbol. If the array is not large enough to hold all
      !>              of the values associated with NAME, as many as will
      !>              fit are returned. VALUES is defined only if the
      !>              output argument FOUND is .TRUE.
      !>
      !>     FOUND    is .TRUE. if NAME is in the symbol table. If NAME is not
      !>              in the table, FOUND is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a
      !>         character symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the character symbol table is corrupted.
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
      !>     The contents of the symbol table are:
      !>
      !>         BOHR      -->   HYDROGEN ATOM
      !>         EINSTEIN  -->   SPECIAL RELATIVITY
      !>                         PHOTOELECTRIC EFFECT
      !>                         BROWNIAN MOTION
      !>         FERMI     -->   NUCLEAR FISSION
      !>
      !>      Let the dimension of VALUES be 3.
      !>
      !>      The calls,
      !>
      !>      CALL SYGETC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL,
      !>     .               N,         VALUES, FOUND           )
      !>
      !>      CALL SYGETC ( 'MILLIKAN', TABSYM, TABPTR, TABVAL,
      !>     .               N,         VALUES, FOUND           )
      !>
      !>      CALL SYGETC ( 'BORN',     TABSYM, TABPTR, TABVAL,
      !>     .               N,         VALUES, FOUND           )
      !>
      !>      return the values for N, VALUES, and FOUND associated with NAME:
      !>
      !>         NAME         N        VALUES                      FOUND
      !>         ----------  ---      -----------------------     -------
      !>         EINSTEIN     3        SPECIAL RELATIVITY          .TRUE.
      !>                               PHOTOELECTRIC EFFECT
      !>                               BROWNIAN MOTION
      !>         MILLIKAN                                         .FALSE.
      !>         BORN         1        HYDROGEN ATOM               .TRUE.
      !>
      !>## Restrictions
      !>
      !>     1)  This subroutine does not check to see if the output array
      !>         VALUES is large enough to hold all of the values associated
      !>         with the symbol NAME. The caller must provide the required
      !>         space.
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
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved entry
      !>        from $Exceptions to $Restrictions and added entry #1 in
      !>        $Exceptions.
      !>
      !>-    SPICELIB Version 1.0.2, 03-NOV-2005 (NJB)
      !>
      !>        Various header corrections were made. In particular,
      !>        the header no longer asserts that this routine will
      !>        "return as many values as will fit" in the output array
      !>        VALUES.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sygetc ( ctx, name, tabsym, tabptr, tabval, n, values, found )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose components are to be returned.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(out) :: n  !! Dimension of the symbol.
      character(*), intent(out) :: values     (        * )  !! Values associated with the symbol.
      logical, intent(out) :: found  !! .TRUE. if the symbol NAME is in the symbol table.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!     We trust that the user has supplied enough room.
!
      else

         found = .true.

         locval = sum(tabptr(1:locsym-1)) + 1
         n      = tabptr(locsym)

         values(1:n) = tabval(locval:locval+n-1)

      end if

      return
      end subroutine sygetc

      !> Return the dimension and associated values for a particular
      !> symbol.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose components are to be
      !>              returned. If NAME is not in the symbol table, FOUND is
      !>              .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>              The symbol NAME may or may not be in the symbol
      !>              table. The symbol table is not modified by this
      !>              subroutine.
      !>
      !>## Detailed Output
      !>
      !>     N        is the dimension of the symbol NAME. The dimension is
      !>              the number of values associated with the given symbol.
      !>              N is defined only if the output argument FOUND is
      !>              .TRUE.
      !>
      !>     VALUES   is an array containing the values associated with the
      !>              symbol. If the array is not large enough to hold all
      !>              of the values associated with NAME, as many as will
      !>              fit are returned. VALUES is defined only if the
      !>              output argument FOUND is .TRUE.
      !>
      !>     FOUND    is .TRUE. if NAME is in the symbol table.
      !>              If NAME is not in the table, FOUND is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a double
      !>         precision symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the double precision symbol table is corrupted.
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
      !>     The contents of the symbol table are:
      !>
      !>         BODY4_POLE_RA -->    3.17681D2
      !>                              1.08D-1
      !>                              0.0D0
      !>         DELTA_T_A     -->    3.2184D1
      !>         K             -->    1.657D-3
      !>         MEAN_ANOM     -->    6.239996D0
      !>                              1.99096871D-7
      !>         ORBIT_ECC     -->    1.671D-2
      !>
      !>      Let the dimension of VALUES be 3.
      !>
      !>      The calls,
      !>
      !>      CALL SYGETD ( 'K',            TABSYM, TABPTR, TABVAL,
      !>     .               N,             VALUES, FOUND           )
      !>
      !>      CALL SYGETD ( 'BODY4_POLE_RA', TABSYM, TABPTR, TABVAL,
      !>     .               N,              VALUES, FOUND            )
      !>
      !>      CALL SYGETD ( 'BODY4_PRIME',   TABSYM, TABPTR, TABVAL,
      !>     .               N,              VALUES, FOUND           )
      !>
      !>      return the values for N, VALUES, and FOUND associated with NAME:
      !>
      !>         NAME            N        VALUES                      FOUND
      !>         ----------     ---      -----------------------     -------
      !>         K               1          1.657D-3                  .TRUE.
      !>         BODY4_POLE_RA   3          3.17681D2                 .TRUE.
      !>                                    1.08D-1
      !>                                    0.0D0
      !>         BODY4_PRIME                                         .FALSE.
      !>
      !>## Restrictions
      !>
      !>     1)  This subroutine does not check to see if the output array
      !>         VALUES is large enough to hold all of the values associated
      !>         with the symbol NAME. The caller must provide the required
      !>         space.
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
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved entry
      !>        from $Exceptions to $Restrictions and added entry #1 in
      !>        $Exceptions.
      !>
      !>-    SPICELIB Version 1.0.2, 03-NOV-2005 (NJB)
      !>
      !>        Various header corrections were made. In particular,
      !>        the header no longer asserts that this routine will
      !>        "return as many values as will fit" in the output array
      !>        VALUES.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sygetd ( ctx, name, tabsym, tabptr, tabval, n, values, found )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose components are to be returned.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(out) :: n  !! Dimension of the symbol.
      real(wp), intent(out) :: values     (        * )  !! Values associated with the symbol.
      logical, intent(out) :: found  !! .TRUE. if the symbol NAME is in the symbol table.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!     We trust that the user has supplied enough room.
!
      else

         found = .true.

         locval = sum(tabptr(1:locsym-1)) + 1
         n      = tabptr(locsym)

         values(1:n) = tabval(locval:locval+n-1)

      end if

      return
      end subroutine sygetd

      !> Return the dimension and associated values for a particular
      !> symbol.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose components are to be
      !>              returned. If NAME is not in the symbol table, FOUND is
      !>              .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>              The symbol NAME may or may not be in the symbol
      !>              table. The symbol table is not modified by this
      !>              subroutine.
      !>
      !>## Detailed Output
      !>
      !>     N        is the dimension of the symbol NAME. The dimension is
      !>              the number of values associated with the given symbol.
      !>              N is defined only if the output argument FOUND is
      !>              .TRUE.
      !>
      !>     VALUES   is an array containing the values associated with the
      !>              symbol. If the array is not large enough to hold all
      !>              of the values associated with NAME, as many as will
      !>              fit are returned. VALUES is defined only if the
      !>              output argument FOUND is .TRUE.
      !>
      !>     FOUND    is .TRUE. if NAME is in the symbol table.
      !>              If NAME is not in the table, FOUND is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a
      !>         integer symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the integer symbol table is corrupted.
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
      !>     The contents of the symbol table are:
      !>
      !>         books   -->   5
      !>         erasers -->   6
      !>         pencils -->  12
      !>                      24
      !>         pens    -->  10
      !>                      12
      !>                      24
      !>
      !>      Let the dimension of VALUES be 3.
      !>
      !>      The calls,
      !>
      !>      CALL SYGETI ( 'pencils', TABSYM, TABPTR, TABVAL,
      !>     .               N,         VALUES, FOUND           )
      !>
      !>      CALL SYGETI ( 'pens',     TABSYM, TABPTR, TABVAL,
      !>     .               N,         VALUES, FOUND           )
      !>
      !>      CALL SYGETI ( 'desks',    TABSYM, TABPTR, TABVAL,
      !>     .               N,         VALUES, FOUND           )
      !>
      !>      return the values for N, VALUES, and FOUND associated with NAME:
      !>
      !>         NAME         N      VALUES    FOUND
      !>         ----------  ---    --------  -------
      !>         pencils      2        12      .TRUE.
      !>                               24
      !>         pens         3        10      .TRUE.
      !>                               12
      !>                               24
      !>         desks                        .FALSE.
      !>
      !>## Restrictions
      !>
      !>     1)  This subroutine does not check to see if the output array
      !>         VALUES is large enough to hold all of the values associated
      !>         with the symbol NAME. The caller must provide the required
      !>         space.
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
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved entry
      !>        from $Exceptions to $Restrictions and added entry #1 in
      !>        $Exceptions.
      !>
      !>-    SPICELIB Version 1.0.2, 03-NOV-2005 (NJB)
      !>
      !>        Various header corrections were made. In particular,
      !>        the header no longer asserts that this routine will
      !>        "return as many values as will fit" in the output array
      !>        VALUES.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sygeti ( ctx, name, tabsym, tabptr, tabval, n, values, found )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose components are to be returned.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(out) :: n  !! Dimension of the symbol.
      integer(ip), intent(out) :: values     (        * )  !! Values associated with the symbol.
      logical, intent(out) :: found  !! .TRUE. if the symbol NAME is in the symbol table.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!     We trust that the user has supplied enough room.
!
      else

         found = .true.

         locval = sum(tabptr(1:locsym-1)) + 1
         n      = tabptr(locsym)

         values(1:n) = tabval(locval:locval+n-1)

      end if

      return
      end subroutine sygeti

      !> Return the Nth value associated with a particular symbol in a
      !> character symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose Nth associated value
      !>              is to be returned. If NAME is not in the symbol table,
      !>              FOUND is .FALSE.
      !>
      !>     NTH      is the index of the value to be returned. If the
      !>              value of NTH is out of range ( NTH < 1 or NTH is
      !>              greater than the dimension of the symbol ) FOUND is
      !>              .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>              The symbol table is not modified by this subroutine.
      !>
      !>## Detailed Output
      !>
      !>     VALUES   is the NTH value associated with the symbol NAME.
      !>
      !>     FOUND    is .TRUE. if NAME is in the symbol table and the NTH
      !>              value associated with NAME exists. Otherwise FOUND
      !>              is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a
      !>         character symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the character symbol table is corrupted.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Two conditions will cause the value of FOUND to be .FALSE.:
      !>
      !>     1)  The symbol NAME is not in the symbol table.
      !>
      !>     2)  NTH is out of range ( NTH < 1 or NTH is greater than the
      !>         dimension of the symbol ).
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>         BOHR      -->   HYDROGEN ATOM
      !>         EINSTEIN  -->   SPECIAL RELATIVITY
      !>                         PHOTOELECTRIC EFFECT
      !>                         BROWNIAN MOTION
      !>         FERMI     -->   NUCLEAR FISSION
      !>
      !>      The calls,
      !>
      !>      CALL SYNTHC ( 'EINSTEIN', 2, TABSYM, TABPTR, TABVAL, VALUE,
      !>     .               FOUND                                        )
      !>
      !>      CALL SYNTHC ( 'BORN',     2, TABSYM, TABPTR, TABVAL, VALUE,
      !>     .               FOUND                                        )
      !>
      !>      CALL SYNTHC ( 'MAXWELL',  5, TABSYM, TABPTR, TABVAL, VALUE,
      !>     .               FOUND                                        )
      !>
      !>      return the values of VALUE and FOUND corresponding to NAME and
      !>      NTH:
      !>
      !>         NAME          NTH           VALUE                 FOUND
      !>         ----------   -----     ----------------------    -------
      !>         EINSTEIN       2       PHOTOELECTRIC EFFECT       .TRUE.
      !>         BORN           2                                 .FALSE.
      !>         MAXWELL        5                                 .FALSE.
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
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added entry #1
      !>        in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine synthc ( ctx, name, nth, tabsym, tabptr, tabval, value, found )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose Nth associated value is to be returned.
      integer(ip), intent(in) :: nth  !! Index of the value to be returned.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(out) :: value  !! Nth value associated with the symbol.
      logical, intent(out) :: found  !! .TRUE. if the Nth value of the symbol exists.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

!
!     If the value of NTH is out of range, that's a problem too.
!
      else if ( nth < 1  .or.  nth > tabptr(locsym) ) then

         found = .false.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!
      else

         found  = .true.

         locval = sum(tabptr(1:locsym-1)) + nth
         value  = tabval(locval)

      end if

      return
      end subroutine synthc

      !> Return the Nth component of a particular symbol in a double
      !> precision symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose Nth component is to be
      !>              returned. If NAME is not in the symbol table, FOUND is
      !>              .FALSE.
      !>
      !>     NTH      is the index of the component to be returned. If the
      !>              value of NTH is out of range ( NTH < 1 or NTH is
      !>              greater than the dimension of the symbol ) FOUND is
      !>              .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>              The symbol table is not modified by this subroutine.
      !>
      !>## Detailed Output
      !>
      !>     VALUES   is the NTH component of the symbol NAME.
      !>
      !>     FOUND    is .TRUE. if NAME is in the symbol table and the NTH
      !>              component of NAME exists. Otherwise FOUND is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a double
      !>         precision symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the double precision symbol table is corrupted.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Two conditions will cause the value of FOUND to be .FALSE.:
      !>
      !>     1)  The symbol NAME is not in the symbol table.
      !>
      !>     2)  NTH is out of range ( NTH < 1 or NTH is greater than the
      !>         dimension of the symbol ).
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>         BODY4_POLE_RA -->    3.17681D2
      !>                              1.08D-1
      !>                              0.0D0
      !>         DELTA_T_A     -->    3.2184D1
      !>         K             -->    1.657D-3
      !>         MEAN_ANOM     -->    6.239996D0
      !>                              1.99096871D-7
      !>         ORBIT_ECC     -->    1.671D-2
      !>
      !>      The calls,
      !>
      !>      CALL SYNTHD ( 'MEAN_ANOM',   2, TABSYM, TABPTR, TABVAL, VALUE,
      !>      .               FOUND                                          )
      !>
      !>      CALL SYNTHD ( 'BODY4_PRIME', 1, TABSYM, TABPTR, TABVAL, VALUE,
      !>      .               FOUND                                          )
      !>
      !>      CALL SYNTHD ( 'ORBIT_ECC',  -5, TABSYM, TABPTR, TABVAL, VALUE,
      !>      .               FOUND                                          )
      !>
      !>      return the values of VALUE and FOUND corresponding to NAME and
      !>      NTH:
      !>
      !>         NAME            NTH           VALUE            FOUND
      !>         ----------     -----     ----------------      -------
      !>         MEAN_ANOM        2        1.99096871D-7         .TRUE.
      !>         BODY4_PRIME      1                             .FALSE.
      !>         ORBIT_ECC       -5                             .FALSE.
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
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added entry #1
      !>        in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine synthd ( ctx, name, nth, tabsym, tabptr, tabval, value, found )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose Nth component is to be returned.
      integer(ip), intent(in) :: nth  !! Index of the value to be returned.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(out) :: value  !! Nth value associated with the symbol.
      logical, intent(out) :: found  !! .TRUE. if the Nth value of the symbol exists.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

!
!     If the value of NTH is out of range, that's a problem too.
!
      else if ( nth < 1  .or.  nth > tabptr(locsym) ) then

         found = .false.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!
      else

         found  = .true.

         locval = sum(tabptr(1:locsym-1)) + nth
         value  = tabval(locval)

      end if

      return
      end subroutine synthd

      !> Return the Nth component of a particular symbol in an integer
      !> symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose Nth component is to be
      !>              returned. If NAME is not in the symbol table, FOUND is
      !>              .FALSE.
      !>
      !>     NTH      is the index of the component to be returned. If the
      !>              value of NTH is out of range ( NTH < 1 or NTH is
      !>              greater than the dimension of the symbol ) FOUND is
      !>              .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>              The symbol table is not modified by this subroutine.
      !>
      !>## Detailed Output
      !>
      !>     VALUES   is the NTH component of the symbol NAME.
      !>
      !>     FOUND    is .TRUE. if NAME is in the symbol table and the NTH
      !>              component of NAME exists. Otherwise FOUND is .FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a
      !>         integer symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the integer symbol table is corrupted.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Two conditions will cause the value of FOUND to be .FALSE.:
      !>
      !>     1)  The symbol NAME is not in the symbol table.
      !>
      !>     2)  NTH is out of range ( NTH < 1 or NTH is greater than the
      !>         dimension of the symbol ).
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>         books   -->   5
      !>         erasers -->   6
      !>         pencils -->  12
      !>                      24
      !>         pens    -->  10
      !>                      12
      !>                      24
      !>
      !>      The calls,
      !>
      !>      CALL SYNTHI ( 'pens',    2, TABSYM, TABPTR, TABVAL, VALUE,
      !>     .               FOUND                                       )
      !>
      !>      CALL SYNTHI ( 'pencils', 3, TABSYM, TABPTR, TABVAL, VALUE,
      !>     .               FOUND                                       )
      !>
      !>      CALL SYNTHI ( 'chairs',  1, TABPTR, TABVAL, TABVAL, VALUE,
      !>     .               FOUND                                       )
      !>
      !>      return the values of VALUE and FOUND corresponding to NAME and
      !>      NTH:
      !>
      !>         NAME            NTH       VALUE      FOUND
      !>         ----------     -----     -------    -------
      !>         pens             2         12        .TRUE.
      !>         pencils                             .FALSE.
      !>         chairs                              .FALSE.
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
      !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Added entry #1
      !>        in $Exceptions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine synthi ( ctx, name, nth, tabsym, tabptr, tabval, value, found )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose Nth component is to be returned.
      integer(ip), intent(in) :: nth  !! Index of the value to be returned.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(out) :: value  !! Nth value associated with the symbol.
      logical, intent(out) :: found  !! .TRUE. if the Nth value of the symbol exists, false if it does not.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

!
!     If the value of NTH is out of range, that's a problem too.
!
      else if ( nth < 1  .or.  nth > tabptr(locsym) ) then

         found = .false.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!
      else

         found  = .true.

         locval = sum(tabptr(1:locsym-1)) + nth
         value  = tabval(locval)

      end if

      return
      end subroutine synthi

      !> Order the components of a single symbol in a character symbol
      !> table. The components are ordered according to the ASCII collating
      !> sequence.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose components are to be
      !>              ordered.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>              On output, the components of the symbol are sorted
      !>              according to ASCII collating sequence. If NAME is not in
      !>              the symbol table, the symbol table is not modified.
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
      !>     If the symbol NAME is not in the symbol table, the symbol table
      !>     is not modified.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>
      !>     The call,
      !>
      !>        CALL SYORDC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   BROWNIAN MOTION
      !>                        PHOTOELECTRIC EFFECT
      !>                        SPECIAL RELATIVITY
      !>        FERMI     -->   NUCLEAR FISSIONC
      !>
      !>     Note that the call,
      !>
      !>        CALL SYORDC ( 'MAXWELL', TABSYM, TABPTR, TABVAL )
      !>
      !>     will not modify the symbol table because the symbol "MAXWELL" is
      !>     not in the symbol table.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine syordc ( ctx, name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc, shellc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose components are to be ordered.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: n

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If so, sort the components in place.
!
      if ( locsym > 0 ) then

         locval = sum(tabptr(1:locsym-1)) + 1
         n      = tabptr(locsym)

         call shellc ( tabptr(locsym), tabval(locval) )

      end if

      return
      end subroutine syordc

      !> Order the components of a single symbol in a double precision
      !> symbol table. The components are sorted in increasing order.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose components are to be
      !>              ordered.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>              On output, Tte components of the symbol are sorted in
      !>              increasing order. If NAME is not in the symbol table, the
      !>              symbol table is not modified.
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
      !>     If the symbol NAME is not in the symbol table, the symbol table
      !>     is not modified.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>     The call,
      !>
      !>        CALL SYORDD ( 'BODY4_POLE_RA', TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BODY4_POLE_RA -->    0.0D0
      !>                             1.08D-1
      !>                             3.17681D2
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>     Note that the call,
      !>
      !>        CALL SYORDD ( 'BODY4_PRIME', TABSYM, TABPTR, TABVAL )
      !>
      !>     will not modify the symbol table because the symbol "BODY4_PRIME"
      !>     is not in the symbol table.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine syordd ( ctx, name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchc, shelld
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose components are to be ordered.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: n

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If so, sort the components in place.
!
      if ( locsym > 0 ) then

         locval = sum(tabptr(1:locsym-1)) + 1
         n      = tabptr(locsym)

         call shelld ( tabptr(locsym), tabval(locval) )

      end if

      return
      end subroutine syordd

      !> Order the components of a single symbol in an integer symbol
      !> table. The components are sorted in increasing order.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose components are to be
      !>              ordered.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>              On output, the components of the symbol are sorted in
      !>              increasing order. If NAME is not in the symbol table, the
      !>              symbol table is not modified.
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
      !>     If the symbol NAME is not in the symbol table, the symbol table
      !>     is not modified.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     24
      !>        pens    -->  10
      !>                     24
      !>                     12
      !>                     36
      !>                      4
      !>
      !>     The call,
      !>
      !>        CALL SYORDI ( 'pens', TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     24
      !>        pens    -->   4
      !>                     10
      !>                     12
      !>                     24
      !>                     36
      !>
      !>     Note that the call,
      !>
      !>        CALL SYORDI ( 'desks', TABSYM, TABPTR, TABVAL )
      !>
      !>     will not modify the symbol table because the symbol "desks" is
      !>     not in the symbol table.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine syordi ( ctx, name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc, shelli
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose components are to be ordered.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: n

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If so, sort the components in place.
!
      if ( locsym > 0 ) then

         locval = sum(tabptr(1:locsym-1)) + 1
         n      = tabptr(locsym)

         call shelli ( tabptr(locsym), tabval(locval) )

      end if

      return
      end subroutine syordi

      !> Pop a value associated with a particular symbol in a character
      !> symbol table. The first value associated with the symbol is
      !> removed, and subsequent values are moved forward.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated value is to
      !>              be popped.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>              On output, the value is removed from the symbol table,
      !>              and the remaining values associated with the symbol are
      !>              moved forward in the value table. If no other values are
      !>              associated with the symbol, the symbol is removed from
      !>              the symbol table.
      !>
      !>     VALUE    is the value that was popped. This value was the first
      !>              value in the symbol table that was associated with the
      !>              symbol NAME.
      !>
      !>     FOUND    is .TRUE. if NAME is in the symbol table, otherwise it is
      !>              .FALSE.
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
      !>     If there are no remaining values associated with the symbol
      !>     after VALUE has been popped, the symbol is removed from the
      !>     symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>
      !>     The call,
      !>
      !>        CALL SYPOPC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL,
      !>       .                                  VALUE,  FOUND  )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>
      !>     FOUND is .TRUE., and VALUE is 'SPECIAL RELATIVITY'.
      !>
      !>     The next call,
      !>
      !>        CALL SYPOPC ( 'FERMI', TABSYM, TABPTR, TABVAL, VALUE, FOUND )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>
      !>      FOUND is .TRUE., and VALUE is 'NUCLEAR FISSION'. Note that
      !>      because "FERMI" had only one value associated with it, it was
      !>      removed from the symbol table.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sypopc ( ctx, name, tabsym, tabptr, tabval, value, found )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated value is to be popped.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(out) :: value  !! Value that was popped.
      logical, intent(out) :: found  !! .TRUE. if the symbol exists, .FALSE. otherwise.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardc ( ctx, tabval )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

!
!     If it is in the table, we can proceed without fear of overflow.
!
      else

         found = .true.

!
!        Begin by saving and removing the initial value for this
!        symbol from the value table.
!
         locval = sum(tabptr(1:locsym-1)) + 1
         value  = tabval(locval)

         call remlac ( ctx, 1, locval, tabval(1), nval )
         call scardc ( ctx, nval, tabval )

!
!        If this was the sole value for the symbol, remove the
!        symbol from the name and pointer tables. Otherwise just
!        decrement the dimension.
!
         if ( tabptr(locsym) == 1 ) then

            call remlac ( ctx, 1, locsym, tabsym(1), nsym )
            call scardc ( ctx, nsym, tabsym )

            call remlai ( ctx, 1, locsym, tabptr(1), nptr )
            call scardi ( ctx, nptr, tabptr )

         else
            tabptr(locsym) = tabptr(locsym) - 1
         end if

      end if

      return
      end subroutine sypopc

      !> Pop a value associated with a particular symbol in a double
      !> precision symbol table. The first value associated with the
      !> symbol is removed, and subsequent values are moved forward.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated value is to
      !>              be popped.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>              The value is removed from the symbol table, and the
      !>              remaining values associated with the symbol are moved
      !>              forward in the value table. If no other values are
      !>              associated with the symbol, the symbol is removed from
      !>              the symbol table.
      !>
      !>     VALUE    is the value that was popped. This value was the first
      !>              value in the symbol table that was associated with the
      !>              symbol NAME.
      !>
      !>     FOUND    is .TRUE. if NAME is in the symbol table, otherwise
      !>              it is .FALSE.
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
      !>     If there are no remaining values associated with the symbol
      !>     after VALUE has been popped, the symbol is removed from the
      !>     symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>     The call,
      !>
      !>        CALL SYPOPD ( 'MEAN_ANOM', TABSYM, TABPTR, TABVAL,
      !>       .                                   VALUE,  FOUND   )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0C
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>     FOUND is .TRUE., and VALUE is 6.239996D0.
      !>
      !>     The next call,
      !>
      !>        CALL SYPOPD ( 'K', TABSYM, TABPTR, TABVAL, VALUE, FOUND )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0C
      !>        DELTA_T_A     -->    3.2184D1
      !>        MEAN_ANOM     -->    1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>      FOUND is .TRUE., and VALUE is  1.657D-3. Note that because
      !>      "K" had only one value associated with it, it was removed
      !>      from the symbol table.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sypopd ( ctx, name, tabsym, tabptr, tabval, value, found )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated value is to be popped.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(out) :: value  !! Value that was popped.
      logical, intent(out) :: found  !! .TRUE. if the symbol exists, .FALSE. otherwise.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardd ( ctx, tabval )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

!
!     If it is in the table, we can proceed without fear of overflow.
!
      else

         found = .true.

!
!        Begin by saving and removing the initial value for this
!        symbol from the value table.
!
         locval = sum(tabptr(1:locsym-1)) + 1
         value  = tabval(locval)

         call remlad ( ctx, 1, locval, tabval(1), nval )
         call scardd ( ctx, nval, tabval )

!
!        If this was the sole value for the symbol, remove the
!        symbol from the name and pointer tables. Otherwise just
!        decrement the dimension.
!
         if ( tabptr(locsym) == 1 ) then

            call remlac ( ctx, 1, locsym, tabsym(1), nsym )
            call scardc ( ctx, nsym, tabsym )

            call remlai ( ctx, 1, locsym, tabptr(1), nptr )
            call scardi ( ctx, nptr, tabptr )

         else
            tabptr(locsym) = tabptr(locsym) - 1
         end if

      end if

      return
      end subroutine sypopd

      !> Pop a value associated with a particular symbol in an integer
      !> symbol table. The first value associated with the symbol is
      !> removed, and subsequent values are moved forward.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated value is to
      !>              be popped.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>              On output, the value is removed from the symbol table,
      !>              and the remaining values associated with the symbol are
      !>              moved forward in the value table. If no other values are
      !>              associated with the symbol, the symbol is removed from
      !>              the symbol table.
      !>
      !>     VALUE    is the value that was popped. This value was the first
      !>              value in the symbol table that was associated with the
      !>              symbol NAME.
      !>
      !>     FOUND    is .TRUE. if NAME is in the symbol table, otherwise it is
      !>              .FALSE.
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
      !>     If there are no remaining values associated with the symbol
      !>     after VALUE has been popped, the symbol is removed from the
      !>     symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  18
      !>                     12
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     The call,
      !>
      !>        CALL SYPOPI ( 'pens', TABSYM, TABPTR, TABVAL, VALUE, FOUND )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  18
      !>                     12
      !>        pens    -->  12
      !>                     24
      !>
      !>     FOUND is .TRUE., and VALUE is 10.
      !>
      !>     The next call,
      !>
      !>        CALL SYPOPI ( 'erasers', TABSYM, TABPTR, TABVAL, VALUE, FOUND )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        books   -->   5
      !>        pencils -->  18
      !>                     12
      !>        pens    -->  12
      !>                     24
      !>
      !>      FOUND is .TRUE., and VALUE is 6. Note that because
      !>      "erasers" had only one value associated with it, it was removed
      !>      from the symbol table.
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
      !>     L.S. Elson         (JPL)
      !>     H.A. Neilan        (JPL)
      !>     W.L. Taber         (JPL)
      !>     I.M. Underwood     (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
      !>
      !>        PLACEHOLDER: Please provide version description.
      !>
      !>-    SPICELIB Version 1.0.2, 10-DEC-2002 (LSE)
      !>
      !>        Fixed typo (FISSION') in header
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sypopi ( ctx, name, tabsym, tabptr, tabval, value, found )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated value is to be popped.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(out) :: value  !! Value that was popped.
      logical, intent(out) :: found  !! .TRUE. if the symbol exists, .FALSE. otherwise.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardi ( ctx, tabval )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

!
!     If it is in the table, we can proceed without fear of overflow.
!
      else

         found = .true.

!
!        Begin by saving and removing the initial value for this
!        symbol from the value table.
!
         locval = sum(tabptr(1:locsym-1)) + 1
         value  = tabval(locval)

         call remlai ( ctx, 1, locval, tabval(1), nval )
         call scardi ( ctx, nval, tabval )

!
!        If this was the sole value for the symbol, remove the
!        symbol from the name and pointer tables. Otherwise just
!        decrement the dimension.
!
         if ( tabptr(locsym) == 1 ) then

            call remlac ( ctx, 1, locsym, tabsym(1), nsym )
            call scardc ( ctx, nsym, tabsym )

            call remlai ( ctx, 1, locsym, tabptr(1), nptr )
            call scardi ( ctx, nptr, tabptr )

         else
            tabptr(locsym) = tabptr(locsym) - 1
         end if

      end if

      return
      end subroutine sypopi

      !> Set the values of a particular symbol in a character symbol table.
      !> If the symbol already exists, the previous values associated with
      !> it are removed, otherwise a new symbol is created.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated values are
      !>              to be set.
      !>
      !>              If NAME has values associated with it, they are removed,
      !>              and the elements of VALUES become the values associated
      !>              with NAME. If NAME is not in the symbol table, a new
      !>              symbol is created, provided there is room in the symbol
      !>              table.
      !>
      !>     VALUES   are the new values associated with the symbol NAME.
      !>
      !>     N        is the number of elements in the VALUES array.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>              If NAME has values associated with it, they are removed,
      !>              and the elements of VALUES become the values associated
      !>              with NAME. If NAME is not in the symbol table, a new
      !>              symbol is created, provided there is room in the symbol
      !>              table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of a new symbol causes an overflow in the
      !>         name table, the error SPICE(NAMETABLEFULL) is signaled.
      !>
      !>     2)  If the addition of a new symbol causes an overflow in the
      !>         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
      !>
      !>     3)  If the addition of new values causes an overflow in the
      !>         value table, the error SPICE(VALUETABLEFULL) is signaled.
      !>
      !>     4)  If N < 1, the error SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This subroutine is like SYSETC, but SYPUTC allows **several**
      !>     values to be associated with a symbol.
      !>
      !>     If NAME has values associated with it, they are removed, and
      !>     the elements of VALUES become the values associated with NAME.
      !>     If NAME is not in the symbol table, a new symbol is created,
      !>     provided there is room in the symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        FERMI     -->   NUCLEAR FISSION
      !>        PAULI     -->   NEUTRINO
      !>
      !>     If VALUES contains the elements,
      !>
      !>          SPECIAL RELATIVITY
      !>          PHOTOELECTRIC EFFECT
      !>          BROWNIAN MOTION
      !>
      !>     the call
      !>
      !>        CALL SYPUTC ( 'EINSTEIN', VALUES, 3, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        ENISTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>        PAULI     -->   NEUTRINO
      !>
      !>     The call,
      !>
      !>        CALL SYPUTC ( 'PAULI', VALUES, 3, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        ENISTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>        PAULI     -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>
      !>     Note that the previous values associated with PAULI have been
      !>     replaced by the values in VALUES.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine syputc ( ctx, name, values, n, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated values are to be put into the symbol table.
      character(*), intent(in) :: values     (        * )  !! Values to be associated with the symbol NAME.
      integer(ip), intent(in) :: n  !! Number of values in VALUES.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: dimval
      logical :: oldsym
      integer(ip) :: newsym
      integer(ip) :: newval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to see if the number of values is a valid quantity.
!
      if ( n < 1 ) then
         call ctx%setmsg ( 'SYPUTC: The dimension of the values array is' // &
                       'less than one.' )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)' )
         return
      end if

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardc ( ctx, tabval )

!
!     Where does this symbol belong? is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If the new symbol already exists, we need to know its dimension
!     to check for overflow.
!
      if ( oldsym ) then
         locval = sum(tabptr(1:locsym-1)) + 1
         dimval = tabptr(locsym)
         newsym = 0
      else
         locval = sum(tabptr(1:locsym)) + 1
         dimval = 0
         newsym = 1
      end if

      newval = n - dimval

!
!     Can we do this without overflow?
!
      if ( nsym+newsym > sizec ( ctx, tabsym ) ) then

         call ctx%setmsg ( 'SYPUTC: Addition of the new symbol # causes ' // &
                       'an overflow in the name table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(NAMETABLEFULL)' )

      else if ( nptr+newsym > sizei ( ctx, tabptr ) ) then

         call ctx%setmsg ( 'SYPUTC: Addition of the new symbol # causes ' // &
                       'an overflow in the pointer table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(POINTERTABLEFULL)' )

      else if ( nval+newval > sizec ( ctx, tabval ) ) then

         call ctx%setmsg ( 'SYPUTC: Addition of the new symbol # causes ' // &
                       'an overflow in the value table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     Looks like we can.
!
      else

!
!        If the symbol exists, remove the current contents and
!        change the dimension. Otherwise add the new name and
!        dimension to the name and pointer tables.
!
         if ( dimval > 0 ) then
            call remlac ( ctx, dimval, locval, tabval(1), nval )
            call scardc ( ctx, nval, tabval )

            tabptr(locsym) = n

         else
            call inslac ( ctx, name, 1, locsym+1, tabsym(1), nsym )
            call scardc ( ctx, nsym, tabsym )

            call inslai ( ctx, [ n ], 1, locsym+1, tabptr(1), nptr )
            call scardi ( ctx, nptr, tabptr )
         end if

!
!        In either case, insert the values from the input array into
!        the value table.
!
         call inslac ( ctx, values, n, locval, tabval(1), nval )
         call scardc ( ctx, nval, tabval )

      end if

      return
      end subroutine syputc

      !> Set the values of a particular symbol in a double precision
      !> symbol table. If the symbol already exists, the previous values
      !> associated with it are removed, otherwise a new symbol is created.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated values are
      !>              to be set.
      !>
      !>              If NAME has values associated with it, they are removed,
      !>              and the elements of VALUES become the values associated
      !>              with NAME. If NAME is not in the symbol table, a new
      !>              symbol is created, provided there is room in the symbol
      !>              table.
      !>
      !>     VALUES   are the new values associated with the symbol NAME.
      !>
      !>     N        is the number of elements in the VALUES array. If N < 1,
      !>              the symbol table is not modified.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>              If NAME has values associated with it, they are
      !>              removed, and the elements of VALUES become the
      !>              values associated with NAME. If NAME is not in the
      !>              symbol table, a new symbol is created, provided
      !>              there is room in the symbol table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of a new symbol causes an overflow in the
      !>         name table, the error SPICE(NAMETABLEFULL) is signaled.
      !>
      !>     2)  If the addition of a new symbol causes an overflow in the
      !>         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
      !>
      !>     3)  If the addition of new values causes an overflow in the
      !>         value table, the error SPICE(VALUETABLEFULL) is signaled.
      !>
      !>     4)  If N < 1, the error SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This subroutine is like SYSETC, but SYPUTC allows **several**
      !>     values to be associated with a symbol.
      !>
      !>     If NAME has values associated with it, they are removed, and
      !>     the elements of VALUES become the values associated with NAME.
      !>     If NAME is not in the symbol table, a new symbol is created,
      !>     provided there is room in the symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>     If VALUES contains the elements,
      !>
      !>          3.17692D2
      !>          1.085D-1
      !>          1.000D-5
      !>
      !>     the call
      !>
      !>        CALL SYPUTC ( 'BODY4_POLE_RA', VALUES, 3,
      !>       .                               TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BODY4_POLE_RA -->    3.17692D2
      !>                             1.085D-1
      !>                             1.000D-5
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2C
      !>
      !>     The call,
      !>
      !>        CALL SYPUTC ( 'K', VALUES, 3, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BODY4_POLE_RA -->    3.17692D2
      !>                             1.085D-1
      !>                             1.000D-5
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    3.17692D2
      !>                             1.085D-1
      !>                             1.000D-5
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>     Note that the previous values associated with "K" have been
      !>     replaced by the values in VALUES.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.2, 06-AUG-1996 (WLT)
      !>
      !>        Fixed the error in the abstract noticed by Ian Jordan
      !>        at the University of Maryland, College Park.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB)
      module subroutine syputd ( ctx, name, values, n, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated values are to be put into the symbol table.
      real(wp), intent(in) :: values     (        * )  !! Values to be associated with the symbol NAME.
      integer(ip), intent(in) :: n  !! Number of values in VALUES.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: dimval
      logical :: oldsym
      integer(ip) :: newsym
      integer(ip) :: newval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to see if the number of values is a valid quantity.
!
      if ( n < 1 ) then
         call ctx%setmsg ( 'SYPUTD: The dimension of the values array is' // &
                       'less than one.' )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)' )
         return
      end if

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardd ( ctx, tabval )

!
!     Where does this symbol belong? is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If the new symbol already exists, we need to know its dimension
!     to check for overflow.
!
      if ( oldsym ) then
         locval = sum(tabptr(1:locsym-1)) + 1
         dimval = tabptr(locsym)
         newsym = 0
      else
         locval = sum(tabptr(1:locsym)) + 1
         dimval = 0
         newsym = 1
      end if

      newval = n - dimval

!
!     Can we do this without overflow?
!

      if ( nsym+newsym > sizec ( ctx, tabsym ) ) then

         call ctx%setmsg ( 'SYPUTD: Addition of the new symbol # causes ' // &
                       'an overflow in the name table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(NAMETABLEFULL)' )

      else if ( nptr+newsym > sizei ( ctx, tabptr ) ) then

         call ctx%setmsg ( 'SYPUTD: Addition of the new symbol # causes ' // &
                       'an overflow in the pointer table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(POINTERTABLEFULL)' )

      else if ( nval+newval > sized ( ctx, tabval ) ) then

         call ctx%setmsg ( 'SYPUTD: Addition of the new symbol # causes ' // &
                       'an overflow in the value table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     Looks like we can.
!
      else

!
!        If the symbol exists, remove the current contents and
!        change the dimension. Otherwise add the new name and
!        dimension to the name and pointer tables.
!
         if ( dimval > 0 ) then
            call remlad ( ctx, dimval, locval, tabval(1), nval )
            call scardd ( ctx, nval, tabval )

            tabptr(locsym) = n

         else
            call inslac ( ctx, name, 1, locsym+1, tabsym(1), nsym )
            call scardc ( ctx, nsym, tabsym )

            call inslai ( ctx, [ n ], 1, locsym+1, tabptr(1), nptr )
            call scardi ( ctx, nptr, tabptr )
         end if

!
!        In either case, insert the values from the input array into
!        the value table.
!
         call inslad ( ctx, values, n, locval, tabval(1), nval )
         call scardd ( ctx, nval, tabval )

      end if

      return
      end subroutine syputd

      !> Set the values of a particular symbol in an integer symbol table.
      !> If the symbol already exists, the previous values associated with
      !> it are removed, otherwise a new symbol is created.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated values are to
      !>              be set.
      !>
      !>              If NAME has values associated with it, they are removed,
      !>              and the elements of VALUES become the values associated
      !>              with NAME. If NAME is not in the symbol table, a new
      !>              symbol is created, provided there is room in the symbol
      !>              table.
      !>
      !>     VALUES   are the new values associated with the symbol NAME.
      !>
      !>     N        is the number of elements in the VALUES array.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>              If NAME has values associated with it, they are removed,
      !>              and the elements of VALUES become the values associated
      !>              with NAME. If NAME is not in the symbol table, a new
      !>              symbol is created, provided there is room in the symbol
      !>              table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of a new symbol causes an overflow in the
      !>         name table, the error SPICE(NAMETABLEFULL) is signaled.
      !>
      !>     2)  If the addition of a new symbol causes an overflow in the
      !>         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
      !>
      !>     3)  If the addition of new values causes an overflow in the
      !>         value table, the error SPICE(VALUETABLEFULL) is signaled.
      !>
      !>     4)  If N < 1, the error SPICE(INVALIDARGUMENT) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This subroutine is like SYSETC, but SYPUTC allows **several**
      !>     values to be associated with a symbol.
      !>
      !>     If NAME has values associated with it, they are removed, and
      !>     the elements of VALUES become the values associated with NAME.
      !>     If NAME is not in the symbol table, a new symbol is created,
      !>     provided there is room in the symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     23
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     If VALUES contains the elements,
      !>
      !>          12
      !>          24
      !>          36
      !>
      !>     the call
      !>
      !>        CALL SYPUTI ( 'desks', VALUES, 3, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        books   -->   5
      !>        desks   -->  12
      !>                     24
      !>                     36
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     23
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>     The call,
      !>
      !>        CALL SYPUTI ( 'pens', VALUES, 3, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        books   -->   5
      !>        desks   -->  12
      !>                     24
      !>                     36
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     23
      !>        pens    -->  12
      !>                     24
      !>                     36
      !>
      !>     Note that the previous values associated with "pens" have been
      !>     replaced by the values in VALUES.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Removed
      !>        unnecessary $Revisions section.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB)
      module subroutine syputi ( ctx, name, values, n, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated values are to be put into the symbol table.
      integer(ip), intent(in) :: values     (        * )  !! Values to be associated with the symbol NAME.
      integer(ip), intent(in) :: n  !! Number of values in VALUES.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: dimval
      logical :: oldsym
      integer(ip) :: newsym
      integer(ip) :: newval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Check to see if the number of values is a valid quantity.
!
      if ( n < 1 ) then
         call ctx%setmsg ( 'SYPUTI: The dimension of the values array is' // &
                       'less than one.' )
         call ctx%sigerr ( 'SPICE(INVALIDARGUMENT)' )
         return
      end if

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardi ( ctx, tabval )

!
!     Where does this symbol belong? is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If the new symbol already exists, we need to know its dimension
!     to check for overflow.
!
      if ( oldsym ) then
         locval = sum(tabptr(1:locsym-1)) + 1
         dimval = tabptr(locsym)
         newsym = 0
      else
         locval = sum(tabptr(1:locsym)) + 1
         dimval = 0
         newsym = 1
      end if

      newval = n - dimval

!
!     Can we do this without overflow?
!

      if ( nsym+newsym > sizec ( ctx, tabsym ) ) then

         call ctx%setmsg ( 'SYPUTI: Addition of the new symbol # causes ' // &
                       'an overflow in the name table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(NAMETABLEFULL)' )

      else if ( nptr+newsym > sizei ( ctx, tabptr ) ) then

         call ctx%setmsg ( 'SYPUTI: Addition of the new symbol # causes ' // &
                       'an overflow in the pointer table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(POINTERTABLEFULL)' )

      else if ( nval+newval > sizei ( ctx, tabval ) ) then

         call ctx%setmsg ( 'SYPUTC: Addition of the new symbol # causes ' // &
                       'an overflow in the value table.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     Looks like we can.
!
      else

!
!        If the symbol exists, remove the current contents and
!        change the dimension. Otherwise add the new name and
!        dimension to the name and pointer tables.
!
         if ( dimval > 0 ) then
            call remlai ( ctx, dimval, locval, tabval(1), nval )
            call scardi ( ctx, nval, tabval )

            tabptr(locsym) = n

         else
            call inslac ( ctx, name, 1, locsym+1, tabsym(1), nsym )
            call scardc ( ctx, nsym, tabsym )

            call inslai ( ctx, [ n ], 1, locsym+1, tabptr(1), nptr )
            call scardi ( ctx, nptr, tabptr )
         end if

!
!        In either case, insert the values from the input array into
!        the value table.
!
         call inslai ( ctx, values, n, locval, tabval(1), nval )
         call scardi ( ctx, nval, tabval )

      end if

      return
      end subroutine syputi

      !> Select a subset of the values associated with a particular
      !> symbol in a character symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose subset of associated
      !>              values to be returned. If NAME is not in the symbol
      !>              table, FOUND is .FALSE.
      !>
      !>     BEGIN    is the index of the first associated value to be
      !>              returned. If BEGIN is out of range (BEGIN < 1 or
      !>              BEGIN > END) FOUND is .FALSE.
      !>
      !>     END      is the index of the last associated value to be
      !>              returned. If END is out of range (END < 1 or
      !>              END > is greater than the dimension of NAME)
      !>              FOUND is .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the character symbol table.
      !>
      !>## Detailed Output
      !>
      !>     VALUES   is a subset of the values associated with the
      !>              symbol NAME. If the subset specified by BEGIN and
      !>              END exists, as many values as will fit in VALUES
      !>              are returned. If the subset does not exist, no
      !>              values are returned and FOUND is .FALSE.
      !>
      !>     FOUND    is .TRUE. if the subset of values is exists.
      !>              FOUND is .FALSE. if BEGIN < 1, BEGIN > END, END < 1,
      !>              END > the dimension of NAME, or NAME is not
      !>              in the symbol table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a
      !>         character symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the character symbol table is corrupted.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     FOUND will be false if the bounds of the subset specified by
      !>     BEGIN and END are out of range. Values of BEGIN and END that
      !>     specify bounds out of range are BEGIN < 1, BEGIN > END,
      !>     END < 1, or END > the dimension of NAME. FOUND is also false
      !>     if the symbol NAME is not in the symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>                        GENERAL RELATIVITY
      !>        FERMI     -->   NUCLEAR FISSION
      !>        PAULI     -->   EXCLUSION PRINCIPLE
      !>                        NEUTRINO
      !>
      !>     Let the dimension of the array VALUES be 4.
      !>
      !>     The ouput values of VALUES and FOUND for the input values of
      !>     NAME, BEGIN, and END are contained in this table:
      !>
      !>        NAME         BEGIN    END        VALUES               FOUND
      !>        -----------  -----    ---    ---------------------   -------
      !>        EINSTEIN       2       3     PHOTOELECTRIC EFFECT     .TRUE.
      !>                                     BROWNIAN MOTION
      !>
      !>        EINSTEIN       1       4     SPECIAL RELATIVITY       .TRUE.
      !>                                     PHOTOELECTRIC EFFECT
      !>                                     BROWNIAN MOTION
      !>                                     GENERAL RELATIVITY
      !>
      !>        MAXWELL        1       5                             .FALSE.
      !>
      !>        PAULI          2       1                             .FALSE.
      !>
      !>        PAULI          1      -2                             .FALSE.
      !>
      !>        BOHR           1       5                             .FALSE.
      !>        ------------------------------------------------------------
      !>
      !>     Note that FOUND is .FALSE. for examples 3 through 6 because:
      !>
      !>     -  In the 3rd example, the symbol 'MAXWELL' is not in the symbol
      !>        table.
      !>
      !>     -  In the 4th example, BEGIN > END.
      !>
      !>     -  In the 5th example, END < 0.
      !>
      !>     -  In the 6th example, END is greater than the dimension of the
      !>        symbol 'BOHR'.
      !>
      !>## Restrictions
      !>
      !>     1)  This subroutine does not check to see if the output array
      !>         VALUES is large enough to hold the selected set of values.
      !>         The caller must provide the required space.
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
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved entry
      !>        from $Exceptions to $Restrictions and added entry #1 in
      !>        $Exceptions.
      !>
      !>-    SPICELIB Version 1.0.2, 03-NOV-2005 (NJB)
      !>
      !>        Various header corrections were made. In particular,
      !>        the header no longer asserts that this routine will
      !>        "return as many values as will fit" in the output array
      !>        VALUES.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine syselc ( ctx, name, begin, end, tabsym, tabptr, tabval, values, found )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated values are to be returned.
      integer(ip), intent(in) :: begin  !! Index of the first associated value to be returned.
      integer(ip), intent(in) :: end  !! Index of the last associated value to be returned.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(out) :: values     (        * )  !! Subset of the values associated with the symbol NAME.
      logical, intent(out) :: found  !! .TRUE. if the subset of values exists.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: n

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

      else

!
!        We could still have a problem: do these components exist?
!        Does this request even make sense?
!
         n = tabptr(locsym)

         if (       begin >= 1 &
              .and. begin <= n &
              .and. end   >= 1 &
              .and. end   <= n &
              .and. begin <= end ) then

            found  = .true.
            locval = sum(tabptr(1:locsym-1)) + 1

            values(1:end-begin+1) = tabval(locval+begin-1:locval+begin-1+end-begin)

         else
            found = .false.
         end if

      end if

      return
      end subroutine syselc

      !> Select a subset of the values associated with a particular
      !> symbol in a double precision symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose subset of associated
      !>              values to be returned. If NAME is not in the symbol
      !>              table, FOUND is .FALSE.
      !>
      !>     BEGIN    is the index of the first associated value to be
      !>              returned. If BEGIN is out of range (BEGIN < 1 or
      !>              BEGIN > END) FOUND is .FALSE.
      !>
      !>     END      is the index of the last associated value to be
      !>              returned. If END is out of range (END < 1 or
      !>              END > is greater than the dimension of NAME)
      !>              FOUND is .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the double precision symbol table.
      !>
      !>## Detailed Output
      !>
      !>     VALUES   is a subset of the values associated with the
      !>              symbol NAME. If the subset specified by BEGIN and
      !>              END exists, as many values as will fit in VALUES
      !>              are returned. If the subset does not exist, no
      !>              values are returned and FOUND is .FALSE.
      !>
      !>     FOUND    is .TRUE. if the subset of values is exists.
      !>              FOUND is .FALSE. if BEGIN < 1, BEGIN > END, END < 1,
      !>              END > the dimension of NAME, or NAME is not
      !>              in the symbol table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a double
      !>         precision symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the double precision symbol table is corrupted.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     FOUND will be false if the bounds of the subset specified by
      !>     BEGIN and END are out of range. Values of BEGIN and END which
      !>     specify bounds out of range are BEGIN < 1, BEGIN > END,
      !>     END < 1, or END > the dimension of NAME. FOUND is also false
      !>     if the symbol NAME is not in the symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>     Let the dimension of the array VALUES be 3.
      !>
      !>     The ouput values of VALUES and FOUND for the input values of
      !>     NAME, BEGIN, and END are contained in this table:
      !>
      !>        NAME            BEGIN    END        VALUES              FOUND
      !>        -------------   -----    ---    --------------------   -------
      !>        MEAN_ANOM         1       2         6.239996D0          .TRUE.
      !>                                            1.99096871D-7
      !>
      !>        BODY4_POLE_RA     1       3         3.17681D2           .TRUE.
      !>                                            1.08D-1
      !>                                            0.0D0
      !>
      !>        BODY4_PRIME       1       3                            .FALSE.
      !>
      !>        MEAN_ANOM         2       1                            .FALSE.
      !>
      !>        ORBIT_ECC         1      -2                            .FALSE.
      !>
      !>        K                 1       5                            .FALSE.
      !>        --------------------------------------------------------------
      !>
      !>     Note that FOUND is .FALSE. for examples 3 through 6 because:
      !>
      !>     -  In the 3rd example, the symbol 'BODY4_PRIME' is not in the
      !>        symbol table.
      !>
      !>     -  In the 4th example, BEGIN > END.
      !>
      !>     -  In the 5th example, END < 0.
      !>
      !>     -  In the 6th example, END is greater than the dimension of the
      !>        symbol 'K'.
      !>
      !>## Restrictions
      !>
      !>     1)  This subroutine does not check to see if the output array
      !>         VALUES is large enough to hold the selected set of values.
      !>         The caller must provide the required space.
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
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved entry
      !>        from $Exceptions to $Restrictions and added entry #1 in
      !>        $Exceptions.
      !>
      !>-    SPICELIB Version 1.0.2, 03-NOV-2005 (NJB)
      !>
      !>        Various header corrections were made. In particular,
      !>        the header no longer asserts that this routine will
      !>        "return as many values as will fit" in the output array
      !>        VALUES.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine syseld ( ctx, name, begin, end, tabsym, tabptr, tabval, values, found )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated values are to be returned.
      integer(ip), intent(in) :: begin  !! Index of the first associated value to be returned.
      integer(ip), intent(in) :: end  !! Index of the last associated value to be returned.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(out) :: values     (        * )  !! Subset of the values associated with the symbol NAME.
      logical, intent(out) :: found  !! .TRUE. if the subset of values exists.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: n

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

      else

!
!        We could still have a problem: do these components exist?
!        Does this request even make sense?
!
         n = tabptr(locsym)

         if (       begin >= 1 &
              .and. begin <= n &
              .and. end   >= 1 &
              .and. end   <= n &
              .and. begin <= end ) then

            found  = .true.
            locval = sum(tabptr(1:locsym-1)) + 1

            values(1:end-begin+1) = tabval(locval+begin-1:locval+begin-1+end-begin)

         else
            found = .false.
         end if

      end if

      return
      end subroutine syseld

      !> Select a subset of the values associated with a particular
      !> symbol in an integer symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose subset of associated
      !>              values to be returned. If NAME is not in the symbol
      !>              table, FOUND is .FALSE.
      !>
      !>     BEGIN    is the index of the first associated value to be
      !>              returned. If BEGIN is out of range (BEGIN < 1 or
      !>              BEGIN > END) FOUND is .FALSE.
      !>
      !>     END      is the index of the last associated value to be
      !>              returned. If END is out of range (END < 1 or
      !>              END > is greater than the dimension of NAME)
      !>              FOUND is .FALSE.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the integer symbol table.
      !>
      !>## Detailed Output
      !>
      !>     VALUES   is a subset of the values associated with the
      !>              symbol NAME. If the subset specified by BEGIN and
      !>              END exists, as many values as will fit in VALUES
      !>              are returned. If the subset does not exist, no
      !>              values are returned and FOUND is .FALSE.
      !>
      !>     FOUND    is .TRUE. if the subset of values is exists.
      !>              FOUND is .FALSE. if BEGIN < 1, BEGIN > END, END < 1,
      !>              END > the dimension of NAME, or NAME is not
      !>              in the symbol table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If there is an issue while reading the components of a
      !>         integer symbol table, an error is signaled by a routine in
      !>         the call tree of this routine. This normally indicates that
      !>         the integer symbol table is corrupted.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     FOUND will be false if the bounds of the subset specified by
      !>     BEGIN and END are out of range. Values of BEGIN and END which
      !>     specify bounds out of range are BEGIN < 1, BEGIN > END,
      !>     END < 1, or END > the dimension of NAME. FOUND is also false
      !>     if the symbol NAME is not in the symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>                     10
      !>        erasers -->   3
      !>        pencils -->  12
      !>                     18
      !>                     24
      !>                     30
      !>        pens    -->  10
      !>                     20
      !>                     30
      !>
      !>     Let the dimension of the array VALUES be 4.
      !>
      !>     The ouput values of VALUES and FOUND for the input values of
      !>     NAME, BEGIN, and END are contained in this table:
      !>
      !>        NAME         BEGIN    END     VALUES    FOUND
      !>        ----------   -----    ---    --------  -------
      !>        pencils        2       3        18      .TRUE.
      !>                                        24
      !>
      !>        pencils        1       4        12      .TRUE.
      !>                                        18
      !>                                        24
      !>                                        30
      !>
      !>        desks          1       5               .FALSE.
      !>
      !>        books          2       1               .FALSE.
      !>
      !>        erasers        1      -2               .FALSE.
      !>
      !>        pens           1       5               .FALSE.
      !>        ----------------------------------------------
      !>
      !>     Note that FOUND is .FALSE. for examples 3 through 6 because:
      !>
      !>     -  In the 3rd example, the symbol 'desks' is not in the
      !>        symbol table.
      !>
      !>     -  In the 4th example, BEGIN > END.
      !>
      !>     -  In the 5th example, END < 0.
      !>
      !>     -  In the 6th example, END is greater than the dimension of the
      !>        symbol 'pens'.
      !>
      !>## Restrictions
      !>
      !>     1)  This subroutine does not check to see if the output array
      !>         VALUES is large enough to hold the selected set of values.
      !>         The caller must provide the required space.
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
      !>-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Moved entry
      !>        from $Exceptions to $Restrictions and added entry #1 in
      !>        $Exceptions.
      !>
      !>-    SPICELIB Version 1.0.2, 03-NOV-2005 (NJB)
      !>
      !>        Various header corrections were made. In particular,
      !>        the header no longer asserts that this routine will
      !>        "return as many values as will fit" in the output array
      !>        VALUES.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine syseli ( ctx, name, begin, end, tabsym, tabptr, tabval, values, found )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated values are to be returned.
      integer(ip), intent(in) :: begin  !! Index of the first associated value to be returned.
      integer(ip), intent(in) :: end  !! Index of the last associated value to be returned.
      character(*), intent(in) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(in) :: tabval     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(out) :: values     (        * )  !! Subset of the values associated with the symbol NAME.
      logical, intent(out) :: found  !! .TRUE. if the subset of values exists.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: n

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

      else

!
!        We could still have a problem: do these components exist?
!        Does this request even make sense?
!
         n = tabptr(locsym)

         if (       begin >= 1 &
              .and. begin <= n &
              .and. end   >= 1 &
              .and. end   <= n &
              .and. begin <= end ) then

            found  = .true.
            locval = sum(tabptr(1:locsym-1)) + 1

            values(1:end-begin+1) = tabval(locval+begin-1:locval+begin-1+end-begin)

         else
            found = .false.
         end if

      end if

      return
      end subroutine syseli

      !> Set the value of a particular symbol in a character symbol table.
      !> If the symbol already exists, the previous values associated with
      !> it are removed, otherwise a new symbol is created.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated value is to
      !>              be set.
      !>
      !>              If NAME has values associated with it, they are removed,
      !>              and VALUE becomes the only value associated with NAME. If
      !>              NAME is not in the symbol table, a new symbol is created,
      !>              provided there is room in the symbol table.
      !>
      !>     VALUE    is the new value associated with the symbol NAME.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>              If NAME has values associated with it, they are removed,
      !>              and VALUE becomes the only value associated with NAME. If
      !>              NAME is not in the symbol table, a new symbol is created,
      !>              provided there is room in the symbol table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of a new symbol causes an overflow in the
      !>         name table, the error SPICE(NAMETABLEFULL) is signaled.
      !>
      !>     2)  If the addition of a new symbol causes an overflow in the
      !>         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
      !>
      !>     3)  If the addition of a new symbol causes an overflow in the
      !>         value table, the error SPICE(VALUETABLEFULL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If NAME has values associated with it, they are removed, and VALUE
      !>     becomes the only value associated with NAME. If NAME is not in the
      !>     symbol table, a new symbol is created, provided there is room in
      !>     the symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>         BOHR      -->   HYDROGEN ATOM
      !>         EINSTEIN  -->   SPECIAL RELATIVITY
      !>                         PHOTOELECTRIC EFFECT
      !>                         BROWNIAN MOTION
      !>         FERMI     -->   NUCLEAR FISSION
      !>         PAULI     -->   EXCLUSION PRINCIPLE
      !>                         NEUTRINO
      !>
      !>      The call,
      !>
      !>      CALL SYSETC ( 'EINSTEIN', 'GENERAL RELATIVITY',
      !>     .               TABSYM,     TABPTR,      TABVAL  )
      !>
      !>      modifies the contents of the symbol table to be:
      !>
      !>         BOHR      -->   HYDROGEN ATOM
      !>         EINSTEIN  -->   GENERAL RELATIVITY
      !>         FERMI     -->   NUCLEAR FISSION
      !>         PAULI     -->   EXCLUSION PRINCIPLE
      !>                         NEUTRINO
      !>
      !>      Note that the previous values associated with the symbol
      !>      "EINSTEIN" have been deleted, and now only the new value is
      !>      associated with the symbol.
      !>
      !>      The next call,
      !>
      !>      CALL SYSETC ( 'MILLIKAN', 'PHOTOELECTRIC EFFECT'
      !>     .               TABSYM,     TABPTR,       TABVAL   )
      !>
      !>      modifies the contents of the symbol table to be:
      !>
      !>         BOHR      -->   HYDROGEN ATOM
      !>         EINSTEIN  -->   GENERAL RELATIVITY
      !>         FERMI     -->   NUCLEAR FISSION
      !>         MILLIKAN  -->   PHOTOELECTRIC EFFECT
      !>         PAULI     -->   EXCLUSION PRINCIPLE
      !>                         NEUTRINOC
      !>
      !>      Note that the new symbol "MILLIKAN" was created by the last call.
      !>      A new symbol is created only if there is room in the symbol
      !>      table.
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
      !>-    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sysetc ( ctx, name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated value is to be set.
      character(*), intent(in) :: value  !! Associated value of the symbol NAME.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: dimval
      logical :: oldsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardc ( ctx, tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If it's already in the table, there's no chance of overflow.
!     Leave the name where it is. Remove all but one of the existing
!     values, replacing that with the new value. And set the dimension
!     to one.
!
      if ( oldsym ) then

         locval = sum(tabptr(1:locsym-1)) + 1
         dimval = tabptr(locsym)

         if ( dimval > 1 ) then
            call remlac ( ctx, dimval-1, locval, tabval(1), nval )
            call scardc ( ctx, nval, tabval )
         end if

         tabptr(locsym) = 1
         tabval(locval) = value

!
!     Otherwise, we can't proceed unless we know that we have enough
!     room for one extra addition in all three tables.
!
      else if ( nsym >= sizec ( ctx, tabsym ) ) then

            call ctx%setmsg ( 'SYSETC: Addition of the new symbol # '   // &
                          'causes an overflow in the name table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(NAMETABLEFULL)' )

      else if ( nptr >= sizei ( ctx, tabptr ) ) then

            call ctx%setmsg ( 'SYSETC: Addition of the new symbol # '     // &
                          'causes an overflow in the pointer table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(POINTERTABLEFULL)' )

      else if ( nval >= sizec ( ctx, tabval ) ) then

            call ctx%setmsg ( 'SYSETC: Addition of the new symbol #  '    // &
                          'causes an overflow in the value table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     If there's room, add the new name to the name table. Give the
!     symbol dimension one, and put the value in the right place.
!
      else

         call inslac ( ctx, name, 1, locsym+1, tabsym(1), nsym )
         call scardc ( ctx, nsym, tabsym )

         call inslai ( ctx, [ 1 ], 1, locsym+1, tabptr(1), nptr )
         call scardi ( ctx, nptr, tabptr )

         locval = sum(tabptr(1:locsym)) + 1

         call inslac ( ctx, value, 1, locval, tabval(1), nval )
         call scardc ( ctx, nval, tabval )

      end if

      return
      end subroutine sysetc

      !> Set the value of a particular symbol in a double precision symbol
      !> table. If the symbol already exists, the previous values
      !> associated with it are removed, otherwise a new symbol is created.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated value is to
      !>              be set.
      !>
      !>              If NAME has values associated with it, they are removed,
      !>              and VALUE becomes the only value associated with NAME. If
      !>              NAME is not in the symbol table, a new symbol is created,
      !>              provided there is room in the symbol table.
      !>
      !>     VALUE    is the new value associated with the symbol NAME.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>              If NAME has values associated with it, they are removed,
      !>              and VALUE becomes the only value associated with NAME. If
      !>              NAME is not in the symbol table, a new symbol is created,
      !>              provided there is room in the symbol table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of a new symbol causes an overflow in the
      !>         name table, the error SPICE(NAMETABLEFULL) is signaled.
      !>
      !>     2)  If the addition of a new symbol causes an overflow in the
      !>         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
      !>
      !>     3)  If the addition of a new symbol causes an overflow in the
      !>         value table, the error SPICE(VALUETABLEFULL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If NAME has values associated with it, they are removed, and VALUE
      !>     becomes the only value associated with NAME. If NAME is not in the
      !>     symbol table, a new symbol is created, provided there is room in
      !>     the symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        DELTA_T_A -->    3.2184D1
      !>        K         -->    1.657D-3
      !>        MEAN_ANOM -->    6.239996D0
      !>                         1.99096871D-7
      !>        ORBIT_ECC -->    1.671D-2
      !>
      !>     The call,
      !>
      !>     CALL SYSETD ( 'ORBIT_ECC', 1.67125D-2, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        DELTA_T_A -->    3.2184D1
      !>        K         -->    1.657D-3
      !>        MEAN_ANOM -->    6.239996D0
      !>                         1.99096871D-7
      !>        ORBIT_ECC -->    1.67125D-2
      !>
      !>     Note that the previous value associated with the symbol
      !>     "ORBIT_ECC" has been deleted, and now the value 1.67125D-2
      !>     is associated with the symbol.
      !>
      !>     The next call,
      !>
      !>     CALL SYSETD ( 'EB', 1.671D-2, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        DELTA_T_A -->    3.2184D1
      !>        EB        -->    1.671D-2
      !>        K         -->    1.657D-3
      !>        MEAN_ANOM -->    6.239996D0
      !>                         1.99096871D-7
      !>        ORBIT_ECC -->    1.67125D-2
      !>
      !>     Note that the new symbol "EB" was created by the last call.
      !>     A new symbol is created only if there is room in the symbol
      !>     table.
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
      !>-    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sysetd ( ctx, name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated value is to be set.
      real(wp), intent(in) :: value  !! Associated value of the symbol NAME.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: dimval
      logical :: oldsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardd ( ctx, tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If it's already in the table, there's no chance of overflow.
!     Leave the name where it is. Remove all but one of the existing
!     values, replacing that with the new value. And set the dimension
!     to one.
!
      if ( oldsym ) then

         locval = sum(tabptr(1:locsym-1)) + 1
         dimval = tabptr(locsym)

         if ( dimval > 1 ) then
            call remlad ( ctx, dimval-1, locval, tabval(1), nval )
            call scardd ( ctx, nval, tabval )
         end if

         tabptr(locsym) = 1
         tabval(locval) = value

!
!     Otherwise, we can't proceed unless we know that we have enough
!     room for one extra addition in all three tables.
!
      else if ( nsym >= sizec ( ctx, tabsym ) ) then

            call ctx%setmsg ( 'SYSETD: Addition of the new symbol # '     // &
                          'causes an overflow in the name table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(NAMETABLEFULL)' )

      else if ( nptr >= sizei ( ctx, tabptr ) ) then

            call ctx%setmsg ( 'SYSETD: Addition of the new symbol # '     // &
                          'causes an overflow in the pointer table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(POINTERTABLEFULL)' )

      else if ( nval >= sized ( ctx, tabval ) ) then

            call ctx%setmsg ( 'SYSETD: Addition of the new symbol # '     // &
                          'causes an overflow in the value table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     If there's room, add the new name to the name table. Give the
!     symbol dimension one, and put the value in the right place.
!
      else

         call inslac ( ctx, name, 1, locsym+1, tabsym(1), nsym )
         call scardc ( ctx, nsym, tabsym )

         call inslai ( ctx, [ 1 ], 1, locsym+1, tabptr(1), nptr )
         call scardi ( ctx, nptr, tabptr )

         locval = sum(tabptr(1:locsym)) + 1

         call inslad ( ctx, [ value ], 1, locval, tabval(1), nval )
         call scardd ( ctx, nval, tabval )

      end if

      return
      end subroutine sysetd

      !> Set the value of a particular symbol in an integer symbol table.
      !> If the symbol already exists, the previous values associated with
      !> it are removed, otherwise a new symbol is created.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated value is to
      !>              be set.
      !>
      !>              If NAME has values associated with it, they are removed,
      !>              and VALUE becomes the only value associated with NAME. If
      !>              NAME is not in the symbol table, a new symbol is created,
      !>              provided there is room in the symbol table.
      !>
      !>     VALUE    is the new value associated with the symbol NAME.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>              If NAME has values associated with it, they are removed,
      !>              and VALUE becomes the only value associated with NAME. If
      !>              NAME is not in the symbol table, a new symbol is created,
      !>              provided there is room in the symbol table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of a new symbol causes an overflow in the
      !>         name table, the error SPICE(NAMETABLEFULL) is signaled.
      !>
      !>     2)  If the addition of a new symbol causes an overflow in the
      !>         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
      !>
      !>     3)  If the addition of a new symbol causes an overflow in the
      !>         value table, the error SPICE(VALUETABLEFULL) is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If NAME has values associated with it, they are removed, and VALUE
      !>     becomes the only value associated with NAME. If NAME is not in the
      !>     symbol table, a new symbol is created, provided there is room in
      !>     the symbol table.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     15
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     The call,
      !>
      !>     CALL SYSETI ( 'pens', 36, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     15
      !>        pens    -->  36
      !>
      !>     Note that the previous values associated with the symbol
      !>     "pens" have been deleted, and now only the new value is
      !>     associated with the symbol.
      !>
      !>     The next call,
      !>
      !>     CALL SYSETI ( 'desks', 31, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        books   -->   5
      !>        desks   -->  31
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     15
      !>        pens    -->  36
      !>
      !>     Note that the new symbol "desks" was created by the last call.
      !>     A new symbol is created only if there is room in the symbol
      !>     table.
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
      !>-    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine syseti ( ctx, name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated value is to be set.
      integer(ip), intent(in) :: value  !! Associated value of the symbol NAME.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nptr
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: dimval
      logical :: oldsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nptr = cardi ( ctx, tabptr )
      nval = cardi ( ctx, tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If it's already in the table, there's no chance of overflow.
!     Leave the name where it is. Remove all but one of the existing
!     values, replacing that with the new value. And set the dimension
!     to one.
!
      if ( oldsym ) then

         locval = sum(tabptr(1:locsym-1)) + 1
         dimval = tabptr(locsym)

         if ( dimval > 1 ) then
            call remlai ( ctx, dimval-1, locval, tabval(1), nval )
            call scardi ( ctx, nval, tabval )
         end if

         tabptr(locsym) = 1
         tabval(locval) = value

!
!     Otherwise, we can't proceed unless we know that we have enough
!     room for one extra addition in all three tables.
!
      else if ( nsym >= sizec ( ctx, tabsym ) ) then

            call ctx%setmsg ( 'SYSETI: Addition of the new symbol # '     // &
                          'causes an overflow in the name table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(NAMETABLEFULL)' )

      else if ( nptr >= sizei ( ctx, tabptr ) ) then

            call ctx%setmsg ( 'SYSETI: Addition of the new symbol # '     // &
                          'causes an overflow in the pointer table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(POINTERTABLEFULL)' )

      else if ( nval >= sizei ( ctx, tabval ) ) then

            call ctx%setmsg ( 'SYSETI: Addition of the new symbol # '     // &
                          'causes an overflow in the value table.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     If there's room, add the new name to the name table. Give the
!     symbol dimension one, and put the value in the right place.
!
      else

         call inslac ( ctx, name, 1, locsym+1, tabsym(1), nsym )
         call scardc ( ctx, nsym, tabsym )

         call inslai ( ctx, [ 1 ], 1, locsym+1, tabptr(1), nptr )
         call scardi ( ctx, nptr, tabptr )

         locval = sum(tabptr(1:locsym)) + 1

         call inslai ( ctx, [ value ], 1, locval, tabval(1), nval )
         call scardi ( ctx, nval, tabval )

      end if

      return
      end subroutine syseti

      !> Transpose two values associated with a particular symbol in a
      !> character symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated values are to
      !>              be transposed.
      !>
      !>     IDX1     is the index of the first associated value to be
      !>              transposed.
      !>
      !>     IDX2     is the index of the second associated value to be
      !>              transposed.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the character symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the character symbol table.
      !>
      !>              If the symbol NAME is not in the symbol table the symbol
      !>              tables are not modified. Otherwise, the values that IDX1
      !>              and IDX2 refer to are transposed in the value table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If IDX1 < 1, IDX2 < 1, IDX1 > the dimension of NAME, or
      !>         IDX2 > the dimension of NAME, the error SPICE(INVALIDINDEX)
      !>         is signaled.
      !>
      !>     2)  If NAME is not in the symbol table, the symbol tables are not
      !>         modified.
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
      !>     The contents of the symbol table are:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>        PAULI     -->   EXCLUSION PRINCIPLE
      !>                        NEUTRINO
      !>
      !>     The call,
      !>
      !>     CALL SYTRNC ( 'EINSTEIN', 2, 3, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        BROWNIAN MOTION
      !>                        PHOTOELECTRIC EFFECT
      !>        FERMI     -->   NUCLEAR FISSION
      !>        PAULI     -->   EXCLUSION PRINCIPLE
      !>                        NEUTRINO
      !>
      !>     The next call,
      !>
      !>     CALL SYTRNC ( 'PAULI', 2, 4, TABSYM, TABPTR, TABVAL )
      !>
      !>     causes the error SPICE(INVALIDINDEX) to be signaled.
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
      !>-    SPICELIB Version 1.2.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Changed the name of input arguments "I" and "J" to "IDX1" and
      !>        "IDX2" for consistency with other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated so no "exchange" occurs if IDX1 equals IDX2.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>         Comment section for permuted index source lines was added
      !>         following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated so no "exchange" occurs if IDX1 equals IDX2.
      !>
      !>-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
      !>
      !>         If one of the indices of the values to be transposed is
      !>         invalid, an error is signaled and the symbol table is
      !>         not modified.
      module subroutine sytrnc ( ctx, name, idx1, idx2, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc, swapc
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated values are to be transposed.
      integer(ip), intent(in) :: idx1  !! Index of first associated value to be transposed.
      integer(ip), intent(in) :: idx2  !! Index of second associated value to be transposed.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: n

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

      if ( locsym > 0 ) then

!
!        Are there enough values associated with the symbol?
!
         n = tabptr(locsym)

!
!        Are the indices valid?
!
         if (        idx1 >= 1 &
              .and.  idx1 <= n &
              .and.  idx2 >= 1 &
              .and.  idx2 <= n ) then

!
!           Exchange the values in place.
!
            if ( idx1 /= idx2 ) then

               locval = sum(tabptr(1:locsym-1)) + 1

               call swapc ( tabval(locval+idx1-1), &
                            tabval(locval+idx2-1) )

            end if

         else

            call ctx%setmsg ( 'The first index was *. ' &
            //            'The second index was *.' )
            call ctx%errint ( '*', idx1                 )
            call ctx%errint ( '*', idx2                 )
            call ctx%sigerr ( 'SPICE(INVALIDINDEX)'     )

         end if

      end if

      return
      end subroutine sytrnc

      !> Transpose two values associated with a particular symbol in a
      !> double precision symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated values are
      !>              to be transposed. If NAME is not in the symbol table,
      !>              the symbol tables are not modified.
      !>
      !>     IDX1     is the index of the first associated value to be
      !>              transposed. If this index is not valid (IDX1 < 1 or
      !>              IDX1 > the dimension of NAME) the symbol table is not
      !>              modified.
      !>
      !>     IDX2     is the index of the second associated value to be
      !>              transposed. If this index is not valid (IDX2 < 1 or
      !>              IDX2 > the dimension of NAME) the symbol table is not
      !>              modified.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the double precision symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the double precision symbol table.
      !>
      !>              If the symbol NAME is not in the symbol table the symbol
      !>              tables are not modified. Otherwise, the values that IDX1
      !>              and IDX2 refer to are transposed in the value table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If IDX1 < 1, IDX2 < 1, IDX1 > the dimension of NAME, or
      !>         IDX2 > the dimension of NAME, the error SPICE(INVALIDINDEX)
      !>         is signaled.
      !>
      !>     2)  If NAME is not in the symbol table, the symbol tables are not
      !>         modified.
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
      !>     The contents of the symbol table are:
      !>
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0
      !>
      !>     The call,
      !>
      !>     CALL SYTRND ( 'BODY4_POLE_RA', 2, 3, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             0.0D0C
      !>                             1.08D-1
      !>     The next call,
      !>
      !>     CALL SYTRND ( 'MEAN_ANOM', 2, 4, TABSYM, TABPTR, TABVAL )
      !>
      !>     causes the error SPICE(INVALIDINDEX) to be signaled.
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
      !>-    SPICELIB Version 1.2.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Changed the name of input arguments "I" and "J" to "IDX1" and
      !>        "IDX2" for consistency with other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated so no "exchange" occurs if IDX1 equals IDX2.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated so no "exchange" occurs if IDX1 equals IDX2.
      !>
      !>-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
      !>
      !>         If one of the indices of the values to be transposed is
      !>         invalid, an error is signaled and the symbol table is
      !>         not modified.
      module subroutine sytrnd ( ctx, name, idx1, idx2, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchc, swapd
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated values are to be transposed.
      integer(ip), intent(in) :: idx1  !! Index of first associated value to be transposed.
      integer(ip), intent(in) :: idx2  !! Index of second associated value to be transposed.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: n

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

      if ( locsym > 0 ) then

!
!        Are there enough values associated with the symbol?
!
         n = tabptr(locsym)

!
!        Are the indices valid?
!

         if (        idx1 >= 1 &
              .and.  idx1 <= n &
              .and.  idx2 >= 1 &
              .and.  idx2 <= n ) then

!
!           Exchange the values in place.
!
            if ( idx1 /= idx2 ) then

               locval = sum(tabptr(1:locsym-1)) + 1

               call swapd ( tabval(locval+idx1-1), &
                            tabval(locval+idx2-1) )

            end if

         else

            call ctx%setmsg ( 'The first index was *. ' &
            //            'The second index was *.' )
            call ctx%errint ( '*', idx1                 )
            call ctx%errint ( '*', idx2                 )
            call ctx%sigerr ( 'SPICE(INVALIDINDEX)'     )

         end if

      end if

      return
      end subroutine sytrnd

      !> Transpose two values associated with a particular symbol in an
      !> integer symbol table.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol whose associated values are
      !>              to be transposed. If NAME is not in the symbol table,
      !>              the symbol tables are not modified.
      !>
      !>     IDX1     is the index of the first associated value to be
      !>              transposed.
      !>
      !>     IDX2     is the index of the second associated value to be
      !>              transposed.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the integer symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the integer symbol table.
      !>
      !>              If the symbol NAME is not in the symbol table the symbol
      !>              tables are not modified. Otherwise, the values that IDX1
      !>              and IDX2 refer to are transposed in the value table.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If IDX1 < 1, IDX2 < 1, IDX1 > the dimension of NAME, or
      !>         IDX2 > the dimension of NAME, the error SPICE(INVALIDINDEX)
      !>         is signaled.
      !>
      !>     2)  If NAME is not in the symbol table, the symbol tables are not
      !>         modified.
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
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     18
      !>                     24
      !>        pens    -->  10
      !>                     20
      !>                     30
      !>                     40
      !>
      !>     The call,
      !>
      !>     CALL SYTRNI ( 'pens', 2, 3, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     18
      !>                     24
      !>        pens    -->  10
      !>                     30
      !>                     20
      !>                     40
      !>     The next call,
      !>
      !>     CALL SYTRNI ( 'pencils', 2, 4, TABSYM, TABPTR, TABVAL )
      !>
      !>     causes the error SPICE(INVALIDINDEX) to be signaled.
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
      !>-    SPICELIB Version 1.2.0, 08-APR-2021 (JDR)
      !>
      !>        Added IMPLICIT NONE statement.
      !>
      !>        Changed the name of input arguments "I" and "J" to "IDX1" and
      !>        "IDX2" for consistency with other routines.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated so no "exchange" occurs if IDX1 equals IDX2.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
      !>
      !>        Updated so no "exchange" occurs if IDX1 equals IDX2.
      !>
      !>-    Beta Version 2.0.0, 16-JAN-1989 (HAN)
      !>
      !>         If one of the indices of the values to be transposed is
      !>         invalid, an error is signaled and the symbol table is
      !>         not modified.
      module subroutine sytrni ( ctx, name, idx1, idx2, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc, swapi
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol whose associated values are to be transposed.
      integer(ip), intent(in) :: idx1  !! Index of first associated value to be transposed.
      integer(ip), intent(in) :: idx2  !! Index of second associated value to be transposed.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym

      integer(ip) :: locsym
      integer(ip) :: locval
      integer(ip) :: n

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols?
!
      nsym = cardc ( ctx, tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

      if ( locsym > 0 ) then

!
!        Are there enough values associated with the symbol?
!
         n = tabptr(locsym)

!
!        Are the indices valid?
!
         if (        idx1 >= 1 &
              .and.  idx1 <= n &
              .and.  idx2 >= 1 &
              .and.  idx2 <= n ) then

!
!           Exchange the values in place.
!
            if ( idx1 /= idx2 ) then

               locval = sum(tabptr(1:locsym-1)) + 1

               call swapi ( tabval(locval+idx1-1), &
                            tabval(locval+idx2-1) )

            end if

         else

            call ctx%setmsg ( 'The first index was *. ' &
            //            'The second index was *.' )
            call ctx%errint ( '*', idx1                 )
            call ctx%errint ( '*', idx2                 )
            call ctx%sigerr ( 'SPICE(INVALIDINDEX)'     )

         end if

      end if

      return
      end subroutine sytrni

      !> Enqueue a value onto a particular symbol in a character
      !> symbol table. If the symbol is not in the table, a new one
      !> is created.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol onto which the value is to
      !>              be enqueued. If NAME is not in the symbol table, a new
      !>              symbol having the value VALUE is created.
      !>
      !>     VALUE    is the value to be enqueued onto the symbol, NAME.
      !>              The value is inserted in the value table after the
      !>              last value associated with the symbol.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>              The symbol NAME may or may not be in the symbol
      !>              table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>              On output, the value table contains the new value in
      !>              addition to the old values associated with the symbol
      !>              NAME. The pointer table is updated to reflect the change
      !>              in the dimension of the symbol.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of the new value to the symbol table causes an
      !>         overflow in the value table, the error SPICE(VALUETABLEFULL)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If the symbol NAME is not in the symbol table, a new symbol is
      !>     created which has the value VALUE.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>         BOHR      -->   HYDROGEN ATOM
      !>         EINSTEIN  -->   SPECIAL RELATIVITY
      !>                         PHOTOELECTRIC EFFECT
      !>                         BROWNIAN MOTION
      !>         FERMI     -->   NUCLEAR FISSION
      !>
      !>      The call,
      !>
      !>         CALL SYENQC ( 'EINSTEIN', 'GENERAL RELATIVITY',
      !>        .               TABSYM, TABPTR, TABVAL           )
      !>
      !>      produces the symbol table:
      !>
      !>         BOHR      -->   HYDROGEN ATOM
      !>         EINSTEIN  -->   SPECIAL RELATIVITY
      !>                         PHOTOELECTRIC EFFECT
      !>                         BROWNIAN MOTION
      !>                         GENERAL RELATIVITY
      !>         FERMI     -->   NUCLEAR FISSION
      !>
      !>      The next call,
      !>
      !>         CALL SYENQC ( 'HAHN', 'NUCLEAR FISSION',
      !>        .              TABSYM, TABPTR, TABVAL     )
      !>
      !>      then produces the symbol table:
      !>
      !>         BOHR      -->   HYDROGEN ATOM
      !>         EINSTEIN  -->   SPECIAL RELATIVITY
      !>                         PHOTOELECTRIC EFFECT
      !>                         BROWNIAN MOTION
      !>                         GENERAL RELATIVITY
      !>         FERMI     -->   NUCLEAR FISSION
      !>         HAHN      -->   NUCLEAR FISSION
      !>
      !>      Note that a new symbol "HAHN" was created by the last call.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine syenqc ( ctx, name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol onto which the value is enqueued.
      character(*), intent(in) :: value  !! Value to be enqueued.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      logical :: oldsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nval = cardc ( ctx, tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If it's not already in the table, use SET to create a brand new
!     symbol.
!
      if ( .not. oldsym ) then

         call sysetc ( ctx, name, value, tabsym, tabptr, tabval )

!
!     If it is in the table, we can't proceed unless we know that we
!     have enough room for one extra addition in the value table.
!
      else if (  nval >= sizec ( ctx, tabval ) ) then

         call ctx%setmsg ( 'SYENQC: The addition of the value $ to the '  // &
                       'symbol # causes an overflow in the value '    // &
                       'table.' )
         call ctx%errch  ( '$', value )
         call ctx%errch  ( '#', name  )
         call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     If there's room, add the new value to the value table at the
!     correct location, and add one to the dimension.
!
      else

         locval = sum(tabptr(1:locsym)) + 1

         call inslac ( ctx, value, 1, locval, tabval(1), nval )
         call scardc ( ctx, nval, tabval )

         tabptr(locsym) = tabptr(locsym) + 1

      end if

      return
      end subroutine syenqc

      !> Enqueue a value onto a particular symbol in a double precision
      !> symbol table. If the symbol is not in the table, a new one
      !> is created.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol onto which the value is to
      !>              be enqueued. If NAME is not in the symbol table, a new
      !>              symbol having the value VALUE is created.
      !>
      !>     VALUE    is the value to be enqueued onto the symbol, NAME.
      !>              The value is inserted in the value table after the
      !>              last value associated with the symbol.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>              The symbol NAME may or may not be in the symbol
      !>              table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>              On output, the value table contains the new value in
      !>              addition to the old values associated with the symbol
      !>              NAME. The pointer table is updated to reflect the change
      !>              in the dimension of the symbol.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of the new value to the symbol table causes an
      !>         overflow in the value table, the error SPICE(VALUETABLEFULL)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If the symbol NAME is not in the symbol table, a new symbol is
      !>     created which has the value VALUE.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>         DELTA_T_A -->   32.184
      !>         K         -->    1.657D-3
      !>         MEAN_ANOM -->    6.239996D0
      !>                          1.99096871D-7
      !>         ORBIT_ECC -->    1.671D-2
      !>
      !>      The call,
      !>
      !>         CALL SYENQD ( 'BODY399_POLE_RA', 0.0D0,
      !>        .              TABSYM, TABPTR, TABVAL    )
      !>
      !>      produces the symbol table:
      !>
      !>         BODY399_POLE_RA -->    0.0D0
      !>         DELTA_T_A       -->   32.184
      !>         K               -->    1.657D-3
      !>         MEAN_ANOM       -->    6.239996D0
      !>                                1.99096871D-7
      !>         ORBIT_ECC       -->    1.671D-2
      !>
      !>      Notice that the new symbol "BODY399_POLE_RA" has been created and
      !>      has the value 0.0D0 associated with it.
      !>
      !>      The next call,
      !>
      !>         CALL SYENQD ( 'BODY399_POLE_RA', -6.4061614D-1,
      !>        .               TABSYM, TABPTR, TABVAL           )
      !>
      !>         CALL SYENQD ( 'BODY399_POLE_RA', -8.386D-5,
      !>        .               TABSYM, TABPTR, TABVAL           )
      !>
      !>      then produces the symbol table:
      !>
      !>         BODY399_POLE_RA -->    0.0D0
      !>                               -6.4061614D-1
      !>                               -8.386D-5
      !>         DELTA_T_A       -->   32.184
      !>         K               -->    1.657D-3
      !>         MEAN_ANOM       -->    6.239996D0
      !>                                1.99096871D-7
      !>         ORBIT_ECC       -->    1.671D-2
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine syenqd ( ctx, name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol onto which the value is enqueued.
      real(wp), intent(in) :: value  !! Value to be enqueued.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      logical :: oldsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nval = cardd ( ctx, tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If it's not already in the table, use SET to create a brand new
!     symbol.
!
      if ( .not. oldsym ) then

         call sysetd ( ctx, name, value, tabsym, tabptr, tabval )

!
!     If it is in the table, we can't proceed unless we know that we
!     have enough room for one extra addition in the value table.
!
      else if (  nval >= sized ( ctx, tabval ) ) then

         call ctx%setmsg ( 'SYENQD: The addition of the value $ to the '  // &
                       'symbol # causes an overflow in the value '    // &
                       'table.' )
         call ctx%errdp  ( '$', value )
         call ctx%errch  ( '#', name  )
         call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     If there's room, add the new value to the value table. Add one
!     to the dimension, and put the value in the right place.
!
      else

         locval = sum(tabptr(1:locsym)) + 1

         call inslad ( ctx, [ value ], 1, locval, tabval(1), nval )
         call scardd ( ctx, nval, tabval )

         tabptr(locsym) = tabptr(locsym) + 1

      end if

      return
      end subroutine syenqd

      !> Enqueue a value onto a particular symbol in an integer
      !> symbol table. If the symbol is not in the table, a new one
      !> is created.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol onto which the value is to
      !>              be enqueued. If NAME is not in the symbol table, a new
      !>              symbol having the value VALUE is created.
      !>
      !>     VALUE    is the value to be enqueued onto the symbol, NAME.
      !>              The value is inserted in the value table after the
      !>              last value associated with the symbol.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>              The symbol NAME may or may not be in the symbol
      !>              table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>              On output, the value table contains the new value in
      !>              addition to the old values associated with the symbol
      !>              NAME. The pointer table is updated to reflect the change
      !>              in the dimension of the symbol.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of the new value to the symbol table causes an
      !>         overflow in the value table, the error SPICE(VALUETABLEFULL)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If the symbol NAME is not in the symbol table, a new symbol is
      !>     created which has the value VALUE.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     The call,
      !>
      !>        CALL SYENQI ( 'books', 12, TABSYM, TABPTR, TABVAL )
      !>
      !>     produces the symbol table:
      !>
      !>        books   -->   5
      !>                     12
      !>        erasers -->   6
      !>        pencils -->  12
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     The next call,
      !>
      !>        CALL SYENQI ( 'desks', 23, TABSYM, TABPTR, TABVAL )
      !>
      !>     then produces the symbol table:
      !>
      !>        books   -->   5
      !>                     12
      !>        desks   -->  23
      !>        erasers -->   6
      !>        pencils -->  12
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     Notice that the symbol "desks" was created by the last call.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine syenqi ( ctx, name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol onto which the value is enqueued.
      integer(ip), intent(in) :: value  !! Value to be enqueued.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      logical :: oldsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nval = cardi ( ctx, tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If it's not already in the table, use SET to create a brand new
!     symbol.
!
      if ( .not. oldsym ) then

         call syseti ( ctx, name, value, tabsym, tabptr, tabval )

!
!     If it is in the table, we can't proceed unless we know that we
!     have enough room for one extra addition in the value table.
!
      else if (  nval >= sizei ( ctx, tabval ) ) then

         call ctx%setmsg ( 'SYENQI: The addition of the value $ to the '  // &
                       'symbol # causes an overflow in the value '    // &
                       'table.' )
         call ctx%errint ( '$', value )
         call ctx%errch  ( '#', name  )
         call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     If there's room, add the new value to the value table. Add one
!     to the dimension, and put the value in the right place.
!
      else

         locval = sum(tabptr(1:locsym)) + 1

         call inslai ( ctx, [ value ], 1, locval, tabval(1), nval )
         call scardi ( ctx, nval, tabval )

         tabptr(locsym) = tabptr(locsym) + 1

      end if

      return
      end subroutine syenqi

      !> Push a value onto a particular symbol in a character symbol table.
      !> The previous value(s) associated with the symbol is extended at
      !> the front. A new symbol is created if necessary.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol onto which the value is to
      !>              be pushed.
      !>
      !>              If NAME is not in the symbol table, a new symbol is
      !>              created.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a character symbol table.
      !>
      !>              The value is added to the symbol table at the front of
      !>              the previous value(s) associated with the symbol NAME. If
      !>              NAME is not originally in the symbol table, a new symbol
      !>              is created.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of the new value to the symbol table causes an
      !>         overflow in the value table, the error SPICE(VALUETABLEFULL)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If the symbol NAME is not in the symbol table, a new symbol
      !>     is created.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>        PAULI     -->   EXCLUSION PRINCIPLE
      !>
      !>     The call,
      !>
      !>        CALL SYPSHC ( 'PAULI', 'NEUTRINO', TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>        PAULI     -->   NEUTRINO
      !>                        EXCLUSION PRINCIPLE
      !>
      !>     The next call,
      !>
      !>        CALL SYPSHC ( 'MILLIKAN', 'PHOTOELECTRIC EFFECT',
      !>       .               TABSYM,     TABPTR,                TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>        MILLIKAN  -->   PHOTOELECTRIC EFFECT
      !>        PAULI     -->   NEUTRINO
      !>                        EXCLUSION PRINCIPLE
      !>
      !>     Note that a new symbol "MILLIKAN" was created by the last call.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sypshc ( ctx, name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol onto which the value is to be pushed.
      character(*), intent(in) :: value  !! Value that is to be pushed onto the symbol NAME.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      logical :: oldsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nval = cardc ( ctx, tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If it's not already in the table, use SET to create a brand new
!     symbol.
!
      if ( .not. oldsym ) then

         call sysetc ( ctx, name, value, tabsym, tabptr, tabval )

!
!     If it is in the table, we can't proceed unless we know that we
!     have enough room for one extra addition in the value table.
!
      else if (  nval >= sizec ( ctx, tabval ) ) then

         call ctx%setmsg ( 'SYPSHC: The addition of the value $ to the '  // &
                       'symbol # causes an overflow in the value '    // &
                       'table.' )
         call ctx%errch  ( '$', value )
         call ctx%errch  ( '#', name  )
         call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     If there's room, add the new value to the value table. Add one
!     to the dimension, and put the value in the right place.
!
      else

         locval = sum(tabptr(1:locsym-1)) + 1

         call inslac ( ctx, value, 1, locval, tabval(1), nval )
         call scardc ( ctx, nval, tabval )

         tabptr(locsym) = tabptr(locsym) + 1

      end if

      return
      end subroutine sypshc

      !> Push a value onto a particular symbol in a double precision
      !> symbol table. The previous value(s) associated with the symbol
      !> is extended at the front. A new symbol is created if necessary.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol onto which the value is to
      !>              be pushed.
      !>
      !>              If NAME is not in the symbol table, a new symbol is
      !>              created.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of a double precision symbol table.
      !>
      !>              The value is added to the symbol table at the front of
      !>              the previous value(s) associated with the symbol NAME. If
      !>              NAME is not originally in the symbol table, a new symbol
      !>              is created.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of the new value to the symbol table causes an
      !>         overflow in the value table, the error SPICE(VALUETABLEFULL)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If the symbol NAME is not in the symbol table, a new symbol
      !>     is created.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        BODY4_POLE_RA -->    1.08D-1
      !>                             0.0D0
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>     The call,
      !>
      !>        CALL SYPSHD ( 'BODY4_POLE_RA',  3.17681D2,
      !>       .               TABSYM,          TABPTR,    TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>     The next call,
      !>
      !>        CALL SYPSHC ( 'BODY4_GM', 4.2826286548993737D4,
      !>       .               TABSYM,     TABPTR,              TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BODY4_GM      -->    4.2826286548993737D4
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>     Note that a new symbol "BODY4_GM" was created by the last call.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sypshd ( ctx, name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol onto which the value is to be pushed.
      real(wp), intent(in) :: value  !! Value that is to be pushed onto the symbol NAME.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      logical :: oldsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nval = cardd ( ctx, tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If it's not already in the table, use SET to create a brand new
!     symbol.
!
      if ( .not. oldsym ) then

         call sysetd ( ctx, name, value, tabsym, tabptr, tabval )

!
!     If it is in the table, we can't proceed unless we know that we
!     have enough room for one extra addition in the value table.
!
      else if (  nval >= sized ( ctx, tabval ) ) then

         call ctx%setmsg ( 'SYPSHD: The addition of the value $ to the '  // &
                       'symbol # causes an overflow in the value '    // &
                       'table.' )
         call ctx%errdp  ( '$', value )
         call ctx%errch  ( '#', name  )
         call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     If there's room, add the new value to the value table. Add one
!     to the dimension, and put the value in the right place.
!
      else

         locval = sum(tabptr(1:locsym-1)) + 1

         call inslad ( ctx, [ value ], 1, locval, tabval(1), nval )
         call scardd ( ctx, nval, tabval )

         tabptr(locsym) = tabptr(locsym) + 1

      end if

      return
      end subroutine sypshd

      !> Push a value onto a particular symbol in an integer symbol table.
      !> The previous value(s) associated with the symbol is extended at
      !> the front. A new symbol is created if necessary.
      !>
      !>## Detailed Input
      !>
      !>     NAME     is the name of the symbol onto which the value is to
      !>              be pushed.
      !>
      !>              If NAME is not in the symbol table, a new symbol is
      !>              created.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are the components of an integer symbol table.
      !>
      !>              The value is added to the symbol table at the front of
      !>              the previous value(s) associated with the symbol NAME. If
      !>              NAME is not originally in the symbol table, a new symbol
      !>              is created.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the addition of the new value to the symbol table causes an
      !>         overflow in the value table, the error SPICE(VALUETABLEFULL)
      !>         is signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     If the symbol NAME is not in the symbol table, a new symbol
      !>     is created.
      !>
      !>## Examples
      !>
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     18
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     The call,
      !>
      !>        CALL SYPSHI ( 'pencils', 45, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        books   -->   5
      !>        erasers -->   6
      !>        pencils -->  45
      !>                     12
      !>                     18
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>     The next call,
      !>
      !>        CALL SYPSHC ( 'desks', 31, TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        books   -->   5
      !>        desks   -->  31
      !>        erasers -->   6
      !>        pencils -->  45
      !>                     12
      !>                     18
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     Note that a new symbol "desks" was created by the last call.
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
      !>-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      module subroutine sypshi ( ctx, name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of the symbol onto which the value is to be pushed.
      integer(ip), intent(in) :: value  !! Value that is to be pushed onto the symbol NAME.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym
      integer(ip) :: nval

      integer(ip) :: locsym
      integer(ip) :: locval
      logical :: oldsym

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( ctx, tabsym )
      nval = cardi ( ctx, tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If it's not already in the table, use SET to create a brand new
!     symbol.
!
      if ( .not. oldsym ) then

         call syseti ( ctx, name, value, tabsym, tabptr, tabval )

!
!     If it is in the table, we can't proceed unless we know that we
!     have enough room for one extra addition in the value table.
!
      else if (  nval >= sizei ( ctx, tabval ) ) then

         call ctx%setmsg ( 'SYPSHI: The addition of the value $ to the '  // &
                       'symbol # causes an overflow in the value '    // &
                       'table.' )
         call ctx%errint ( '$', value )
         call ctx%errch  ( '#', name  )
         call ctx%sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     If there's room, add the new value to the value table. Add one
!     to the dimension, and put the value in the right place.
!
      else

         locval = sum(tabptr(1:locsym-1)) + 1

         call inslai ( ctx, [ value ], 1, locval, tabval(1), nval )
         call scardi ( ctx, nval, tabval )

         tabptr(locsym) = tabptr(locsym) + 1

      end if

      return
      end subroutine sypshi

      !> Rename an existing symbol in a character symbol table.
      !>
      !>## Detailed Input
      !>
      !>     OLD      is the name of the symbol to be renamed. If OLD is
      !>              not in the symbol table, the tables are not modified.
      !>
      !>     NEW      is the new name of the symbol. If the symbol NEW
      !>              already exists in the symbol table, it is deleted.
      !>              OLD is then renamed to NEW.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the character symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the character symbol table.
      !>              The values previously associated with OLD are now
      !>              associated with NEW. If OLD is not in the symbol
      !>              table, the symbol tables are not modified.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the symbol OLD is not in the symbol table, the error
      !>         SPICE(NOSUCHSYMBOL) is signaled.
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
      !>     The contents of the symbol table are:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        FERMI     -->   NUCLEAR FISSION
      !>        HAHN      -->   NUCLEAR FISSION
      !>        PAULI     -->   EXCLUSION PRINCIPLE
      !>                        NEUTRINO
      !>
      !>     The call,
      !>
      !>     CALL SYRENC ( 'FERMI', 'STRASSMAN', TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        HAHN      -->   NUCLEAR FISSION
      !>        PAULI     -->   EXCLUSION PRINCIPLE
      !>                        NEUTRINO
      !>        STRASSMAN -->   NUCLEAR FISSION
      !>
      !>     The next call,
      !>
      !>     CALL SYRENC ( 'HAHN', 'STRASSMAN', TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BOHR      -->   HYDROGEN ATOM
      !>        EINSTEIN  -->   SPECIAL RELATIVITY
      !>                        PHOTOELECTRIC EFFECT
      !>                        BROWNIAN MOTION
      !>        PAULI     -->   EXCLUSION PRINCIPLE
      !>                        NEUTRINO
      !>        HAHN      -->   NUCLEAR FISSION
      !>
      !>     Note that the symbol "STRASSMAN" was deleted from the table,
      !>     and the symbol "HAHN" was then renamed to "STRASSMAN". If the
      !>     new symbol exists, it is deleted from the table before its name
      !>     is given to another symbol.
      !>
      !>     The next call,
      !>
      !>     CALL SYRENC ( 'FERMI', 'HAHN', TABSYM, TABPTR, TABVAL )
      !>
      !>     does not modify the contents of the symbol table. It signals
      !>     the error SPICE(NOSUCHSYMBOL) because the symbol "FERMI" does
      !>     not exist in the symbol table.
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
      !>-    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR)
      !>
      !>        Added IMPLICT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed I/O type
      !>        of arguments TABSYM, TABPTR and TABVAL in $Brief_I/O table.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-    Beta Version 2.0.0, 28-DEC-1989 (HAN)
      !>
      !>        Changed the call to SYDELD to a call to SYDELC. The variable
      !>        TABVAL of type character was being passed to a dummy argument
      !>        of type double precision.
      !>
      !>-    Beta Version 1.1.0, 17-FEB-1989 (NJB)
      !>
      !>        Declaration of the unused function SIZEC removed.
      module subroutine syrenc ( ctx, old, new, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc, lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: old  !! Name of the symbol to be renamed.
      character(*), intent(in) :: new  !! New name of the symbol.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      character(*), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym

      integer(ip) :: oldloc
      integer(ip) :: oldval
      integer(ip) :: olddim
      integer(ip) :: newloc
      integer(ip) :: newval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Where was the old symbol?
!
      nsym   = cardc  ( ctx, tabsym )
      oldloc = bsrchc ( old, nsym, tabsym(1) )

!
!     An overflow is simply not possible here. The only thing that can
!     go wrong is that the old symbol does not exist.
!
      if ( oldloc == 0 ) then

         call ctx%setmsg ( 'SYRENC: The symbol # is not in the symbol ' // &
                       'table.' )
         call ctx%errch  ( '#', old )
         call ctx%sigerr ( 'SPICE(NOSUCHSYMBOL)' )

!
!     Are these the same symbol?
!
      else if ( new /= old ) then

!
!        If the new symbol already exists, delete it.
!
         call sydelc ( ctx, new, tabsym, tabptr, tabval )

         nsym   = cardc  ( ctx,            tabsym    )
         oldloc = bsrchc ( old, nsym, tabsym(1) )

!
!        Swap N elements at the old location with zero elements
!        at the new location.
!
         newloc = lstlec ( new, nsym, tabsym(1) ) + 1

         oldval = sum(tabptr(1:oldloc-1)) + 1
         newval = sum(tabptr(1:newloc-1)) + 1

         olddim = tabptr(oldloc)

         call swapac ( ctx, olddim, oldval, 0, newval, tabval(1) )

!
!        Move the name and dimension the same way.
!
         call swapac ( ctx, 1, oldloc, 0, newloc, tabsym(1) )
         call swapai ( ctx, 1, oldloc, 0, newloc, tabptr(1) )

         if ( oldloc < newloc ) then
            newloc = newloc - 1
         end if

         tabsym(newloc) = new

      end if

      return
      end subroutine syrenc

      !> Rename an existing symbol in a double precision symbol table.
      !>
      !>## Detailed Input
      !>
      !>     OLD      is the name of the symbol to be renamed. If OLD is
      !>              not in the symbol table, the tables are not modified.
      !>
      !>     NEW      is the new name of the symbol. If the symbol NEW
      !>              already exists in the symbol table, it is deleted.
      !>              OLD is then renamed to NEW.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the double precision symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the double precision symbol table.
      !>              The values previously associated with OLD are now
      !>              associated with NEW. If OLD is not in the symbol
      !>              table, the symbol tables are not modified.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the symbol OLD is not in the symbol table, the error
      !>         SPICE(NOSUCHSYMBOL) is signaled.
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
      !>     The contents of the symbol table are:
      !>
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0
      !>        DELTA_T_A     -->    3.2184D1
      !>        K             -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>
      !>     The call,
      !>
      !>     CALL SYREND ( 'K', 'EB', TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0
      !>        DELTA_T_A     -->    3.2184D1
      !>        EB            -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>                             1.08D-1
      !>                             0.0D0
      !>
      !>     The next call,
      !>
      !>     CALL SYREND ( 'EB', 'DELTA_T_A', TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the table to be:
      !>
      !>        BODY4_POLE_RA -->    3.17681D2
      !>                             1.08D-1
      !>                             0.0D0
      !>        DELTA_T_A     -->    1.657D-3
      !>        MEAN_ANOM     -->    6.239996D0
      !>                             1.99096871D-7
      !>        ORBIT_ECC     -->    1.671D-2
      !>                             1.08D-1
      !>                             0.0D0
      !>
      !>     Note that the symbol "DELTA_T_A" was deleted from the table,
      !>     and the symbol "EB" was then renamed to "DELTA_T_A". If the
      !>     new symbol exists, it is deleted from the table before its name
      !>     is given to another symbol.
      !>
      !>     The next call,
      !>
      !>     CALL SYREND ( 'K', 'EB', TABSYM, TABPTR, TABVAL )
      !>
      !>     does not modify the contents of the symbol table. It signals
      !>     the error SPICE(NOSUCHSYMBOL) because the symbol "K" does not
      !>     exist in the symbol table.
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
      !>-    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR)
      !>
      !>        Added IMPLICT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed I/O type
      !>        of arguments TABSYM, TABPTR and TABVAL in $Brief_I/O table.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
      !>
      !>         Declaration of the unused function SIZEC removed.
      module subroutine syrend ( ctx, old, new, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp
      use pepper_arrays, only: bsrchc, lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: old  !! Name of the symbol to be renamed.
      character(*), intent(in) :: new  !! New name of the symbol.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      real(wp), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym

      integer(ip) :: oldloc
      integer(ip) :: oldval
      integer(ip) :: olddim
      integer(ip) :: newloc
      integer(ip) :: newval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Where was the old symbol?
!
      nsym   = cardc  ( ctx, tabsym )
      oldloc = bsrchc ( old, nsym, tabsym(1) )

!
!     An overflow is simply not possible here. The only thing that can
!     go wrong is that the old symbol does not exist.
!
      if ( oldloc == 0 ) then

         call ctx%setmsg ( 'SYREND: The symbol # is not in the symbol ' // &
                       'table.' )
         call ctx%errch  ( '#', old )
         call ctx%sigerr ( 'SPICE(NOSUCHSYMBOL)' )

!
!     Are these the same symbol?
!
      else if ( new /= old ) then

!
!        If the new symbol already exists, delete it.
!
         call sydeld ( ctx, new, tabsym, tabptr, tabval )

         nsym   = cardc  ( ctx,            tabsym    )
         oldloc = bsrchc ( old, nsym, tabsym(1) )

!
!        Swap N elements at the old location with zero elements
!        at the new location.
!
         newloc = lstlec ( new, nsym, tabsym(1) ) + 1

         oldval = sum(tabptr(1:oldloc-1)) + 1
         newval = sum(tabptr(1:newloc-1)) + 1

         olddim = tabptr(oldloc)

         call swapad ( ctx, olddim, oldval, 0, newval, tabval(1) )

!
!        Move the name and dimension the same way.
!
         call swapac ( ctx, 1, oldloc, 0, newloc, tabsym(1) )
         call swapai ( ctx, 1, oldloc, 0, newloc, tabptr(1) )

         if ( oldloc < newloc ) then
            newloc = newloc - 1
         end if

         tabsym(newloc) = new

      end if

      return
      end subroutine syrend

      !> Rename an existing symbol in an integer symbol table.
      !>
      !>## Detailed Input
      !>
      !>     OLD      is the name of the symbol to be renamed. If OLD is
      !>              not in the symbol table, the tables are not modified.
      !>
      !>     NEW      is the new name of the symbol. If the symbol NEW
      !>              already exists in the symbol table, it is deleted.
      !>              OLD is then renamed to NEW.
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the integer symbol table.
      !>
      !>## Detailed Output
      !>
      !>     TABSYM,
      !>     TABPTR,
      !>     TABVAL   are components of the integer symbol table.
      !>              The values previously associated with OLD are now
      !>              associated with NEW. If OLD is not in the symbol
      !>              table, the symbol tables are not modified.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If the symbol OLD is not in the symbol table, the error
      !>         SPICE(NOSUCHSYMBOL) is signaled.
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
      !>     The contents of the symbol table are:
      !>
      !>        books   -->   5
      !>                     10
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     18
      !>        pens    -->  10
      !>                     12
      !>                     24
      !>
      !>     The call,
      !>
      !>     CALL SYRENI ( 'pens', 'desks', TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents of the symbol table to be:
      !>
      !>        books   -->   5
      !>                     10
      !>        desks   -->  10
      !>                     12
      !>                     24
      !>        erasers -->   6
      !>        pencils -->  12
      !>                     18
      !>
      !>     The next call,
      !>
      !>     CALL SYRENI ( 'erasers', 'desks', TABSYM, TABPTR, TABVAL )
      !>
      !>     modifies the contents fo the symbol table to be:
      !>
      !>        books   -->   5
      !>                     10
      !>        desks   -->   6
      !>        pencils -->  12
      !>                     18
      !>
      !>     Note that the symbol "desks" was deleted from the table,
      !>     and the symbol "erasers" was then renamed to "STRASSMAN". If the
      !>     new symbol exists, it is deleted from the table before its name
      !>     is given to another symbol.
      !>
      !>     The next call,
      !>
      !>     CALL SYRENI ( 'chairs', 'stools', TABSYM, TABPTR, TABVAL )
      !>
      !>     does not modify the contents of the symbol table. However, it
      !>     does signal the error SPICE(NOSUCHSYMBOL) because the symbol
      !>     "chairs" does not exist in the symbol table.
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
      !>-    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR)
      !>
      !>        Added IMPLICT NONE statement.
      !>
      !>        Edited the header to comply with NAIF standard. Fixed I/O type
      !>        of arguments TABSYM, TABPTR and TABVAL in $Brief_I/O table.
      !>
      !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
      !>
      !>        Comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
      !>
      !>## Revisions
      !>
      !>-     Beta Version 2.0.0, 28-DEC-1989 (HAN)
      !>
      !>         Changed the call to SYDELD to a call to SYDELI. The variable
      !>         TABVAL of type character was being passed to a dummy argument
      !>         of type double precision.
      !>
      !>-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
      !>
      !>         Declaration of the unused function SIZEC removed.
      module subroutine syreni ( ctx, old, new, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip
      use pepper_arrays, only: bsrchc, lstlec
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: old  !! Name of the symbol to be renamed.
      character(*), intent(in) :: new  !! New name of the symbol.
      character(*), intent(inout) :: tabsym     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabptr     ( lbcell:* )  !! Components of the symbol table.
      integer(ip), intent(inout) :: tabval     ( lbcell:* )  !! Components of the symbol table.


      integer(ip) :: nsym

      integer(ip) :: oldloc
      integer(ip) :: oldval
      integer(ip) :: olddim
      integer(ip) :: newloc
      integer(ip) :: newval

!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

!
!     Where was the old symbol?
!
      nsym   = cardc  ( ctx, tabsym )
      oldloc = bsrchc ( old, nsym, tabsym(1) )

!
!     An overflow is simply not possible here. The only thing that can
!     go wrong is that the old symbol does not exist.
!
      if ( oldloc == 0 ) then

         call ctx%setmsg ( 'SYRENI: The symbol # is not in the symbol ' // &
                       'table.' )
         call ctx%errch  ( '#', old )
         call ctx%sigerr ( 'SPICE(NOSUCHSYMBOL)' )

!
!     Are these the same symbol?
!
      else if ( new /= old ) then

!
!        If the new symbol already exists, delete it.
!
         call sydeli ( ctx, new, tabsym, tabptr, tabval )

         nsym   = cardc  ( ctx,            tabsym    )
         oldloc = bsrchc ( old, nsym, tabsym(1) )

!
!        Swap N elements at the old location with zero elements
!        at the new location.
!
         newloc = lstlec ( new, nsym, tabsym(1) ) + 1

         oldval = sum(tabptr(1:oldloc-1)) + 1
         newval = sum(tabptr(1:newloc-1)) + 1

         olddim = tabptr(oldloc)

         call swapai ( ctx, olddim, oldval, 0, newval, tabval(1) )

!
!        Move the name and dimension the same way.
!
         call swapac ( ctx, 1, oldloc, 0, newloc, tabsym(1) )
         call swapai ( ctx, 1, oldloc, 0, newloc, tabptr(1) )

         if ( oldloc < newloc ) then
            newloc = newloc - 1
         end if

         tabsym(newloc) = new

      end if

      return
      end subroutine syreni

end submodule pepper_symbols
