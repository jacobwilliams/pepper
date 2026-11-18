!> The legacy interface's routines of the subsystem text (12 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_text

   use pepper_instance_module, only: pepper_context
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_bounds, only: lbpool

   implicit none

   private
   public :: cyclec, repmd, repmi, repsub, remsub, inssub, shiftc, chckid, countc, hx2int, &
      locati, locln

contains

      !> Cycle the contents of a character string to the left or right.
      !>
      !> Legacy interface: calls ctx%cyclec with the legacy context.
      subroutine cyclec ( instr, dir, ncycle, outstr )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: instr  !! String to be cycled.
      character(1) :: dir  !! Direction to cycle.
      integer(ip) :: ncycle  !! Number of times to cycle.
      character(*) :: outstr  !! Cycled string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: instr, dir are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%cyclec ( (instr), (dir), ncycle, outstr )
      end subroutine cyclec

      !> Replace a marker with a double precision number.
      !>
      !> Legacy interface: calls ctx%repmd with the legacy context.
      subroutine repmd ( in, marker, value, sigdig, out )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: in  !! Input string.
      character(*) :: marker  !! Marker to be replaced.
      real(wp) :: value  !! Replacement value.
      integer(ip) :: sigdig  !! Significant digits in replacement text.
      character(*) :: out  !! Output string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: in, marker are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%repmd ( (in), (marker), value, sigdig, out )
      end subroutine repmd

      !> Replace a marker with an integer.
      !>
      !> Legacy interface: calls ctx%repmi with the legacy context.
      subroutine repmi ( in, marker, value, out )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      character(*) :: marker  !! Marker to be replaced.
      integer(ip) :: value  !! Replacement value.
      character(*) :: out  !! Output string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: in, marker are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%repmi ( (in), (marker), value, out )
      end subroutine repmi

      !> Replace the substring (LEFT:RIGHT) with a string of any length.
      !>
      !> Legacy interface: calls ctx%repsub with the legacy context.
      subroutine repsub ( in, left, right, string, out )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      integer(ip) :: left  !! Ends of substring to be replaced.
      integer(ip) :: right  !! Ends of substring to be replaced.
      character(*) :: string  !! Replacement string.
      character(*) :: out  !! Resulting string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: in, string are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%repsub ( (in), left, right, (string), out )
      end subroutine repsub

      !> Remove the substring (LEFT:RIGHT) from a character string.
      !>
      !> Legacy interface: calls ctx%remsub with the legacy context.
      subroutine remsub ( in, left, right, out )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      integer(ip) :: left  !! Position of first character to be removed.
      integer(ip) :: right  !! Position of last character to be removed.
      character(*) :: out  !! Output string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: in is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%remsub ( (in), left, right, out )
      end subroutine remsub

      !> Insert a substring into a character string at a specified
      !> location.
      !>
      !> Legacy interface: calls ctx%inssub with the legacy context.
      subroutine inssub ( in, sub, loc, out )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      character(*) :: sub  !! Substring to be inserted.
      integer(ip) :: loc  !! Position at which substring is to be inserted.
      character(*) :: out  !! Output string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: in, sub are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%inssub ( (in), (sub), loc, out )
      end subroutine inssub

      !> Shift the contents of a character string to the left or right.
      !> Characters moved past the beginning or end of the string are
      !> lost. Vacant spaces are filled with a specified character.
      !>
      !> Legacy interface: calls ctx%shiftc with the legacy context.
      subroutine shiftc ( in, dir, nshift, fillc, out )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: in  !! Input string.
      character(1) :: dir  !! Direction to shift.
      integer(ip) :: nshift  !! Number of times to shift.
      character(1) :: fillc  !! Character to fill spaces left vacant.
      character(*) :: out  !! Shifted string.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: in, dir, fillc are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%shiftc ( (in), (dir), nshift, (fillc), out )
      end subroutine shiftc

      !> Validate an ID string: check for non-printing characters
      !> or excessive non-blank length.
      !>
      !> Legacy interface: calls ctx%chckid with the legacy context.
      subroutine chckid ( class, maxlen, id )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: class  !! A description of the class to which ID belongs.
      integer(ip) :: maxlen  !! Maximum allowed non-blank length of ID.
      character(*) :: id  !! The ID string to be validated.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%chckid ( class, maxlen, id )
      end subroutine chckid

      !> Count the characters in a group of lines in a text file.
      !>
      !> Legacy interface: calls ctx%countc with the legacy context.
      integer(ip) function countc ( unit, bline, eline, line )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: unit  !! Logical unit connected to text file.
      integer(ip) :: bline  !! Beginning line number.
      integer(ip) :: eline  !! Ending line number.
      character(*) :: line  !! Workspace.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      countc = ctx%countc ( unit, bline, eline, line )
      end function countc

      !> Convert a signed hexadecimal string representation of an integer
      !> to its equivalent integer.
      !>
      !> Legacy interface: calls ctx%hx2int with the legacy context.
      subroutine hx2int ( string, number, error, errmsg )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmax, intmin
      character(*) :: string  !! Hexadecimal string to be converted to an integer.
      integer(ip) :: number  !! Integer value to be returned.
      logical :: error  !! A logical flag which is .TRUE. on error.
      character(*) :: errmsg  !! A descriptive error message.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: string is passed as a copy: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%hx2int ( (string), number, error, errmsg )
      end subroutine hx2int

      !> Find a given identifier, which consists of an integer array,
      !> within a list of such identifiers, or insert the identifier
      !> into the list. Return the location of the identifier and a flag
      !> indicating whether or not the identifier was already present.
      !>
      !> Legacy interface: calls ctx%locati with the legacy context.
      subroutine locati ( id, idsz, list, pool, at, presnt )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: id   (            * )  !! An array of integers that comprise an identifier.
      integer(ip) :: idsz  !! The number of integer components per identifier.
      integer(ip) :: list ( idsz,      * )  !! A list of known identifiers.
      integer(ip) :: pool ( 2, lbpool: * )  !! A doubly linked list used for search LIST.
      integer(ip) :: at  !! Location of the ID in the LIST.
      logical :: presnt  !! Flag indicating if ID was already in LIST.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%locati ( id, idsz, list, pool, at, presnt )
      end subroutine locati

      !> Locate a group of lines in a text file delimited by markers.
      !>
      !> Legacy interface: calls ctx%locln with the legacy context.
      subroutine locln ( unit, bmark, emark, line, bline, eline, found )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: unit  !! Logical unit connected to text file.
      character(*) :: bmark  !! Begin marker.
      character(*) :: emark  !! End marker.
      character(*) :: line  !! Workspace.
      integer(ip) :: bline  !! Beginning line.
      integer(ip) :: eline  !! Ending line.
      logical :: found  !! Markers found?
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      ! PEPPER: bmark, emark are passed as copies: a SPICELIB caller may pass its output variable (in-place use)
      call ctx%locln ( unit, (bmark), (emark), line, bline, eline, found )
      end subroutine locln

end module pepper_legacy_text
