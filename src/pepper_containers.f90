!> Containers that grow on demand, for the tables that SPICELIB sizes with
!> fixed limits (see plans/limits.md):
!>
!> - `grow`: enlarge an allocatable array to hold at least a given number of
!>   elements, keeping its contents (capacity doubles, so appending n
!>   elements costs O(n) copies in total);
!> - `string`: a variable-length string, for arrays of strings of different
!>   lengths;
!> - `string_map`, `int_map`: hash maps from a string or an integer to an
!>   integer (typically an index into a table), growing and rehashing
!>   internally;
!> - `lnkgrow`: enlarge a SPICELIB doubly linked list pool ([[lnkini]] layout)
!>   without changing the existing nodes.
!>
!> Growing reallocates: a pointer or an `associate` name referring to the
!> old array is invalid afterwards. Grow before entering an `associate`
!> block, not inside it.
!>
!> None of these routines signal SPICE errors; they need no context.
module pepper_containers

   use, intrinsic :: iso_fortran_env, only: int64
   use pepper_kinds, only: ip, wp
   use pepper_bounds, only: lbpool

   implicit none

   private

   public :: string, string_map, int_map
   public :: grow, grown_capacity, grow_cell, lnkgrow

   !> Make room in a SPICE cell held in an allocatable array (lower bound
   !> -5): `grow_cell(cell, needed)`.
   interface grow_cell
      module procedure grow_cell_real, grow_cell_int
   end interface grow_cell

   integer(ip), parameter :: min_capacity = 8  !! smallest capacity `grow` allocates
   integer(ip), parameter :: min_slots = 16  !! initial number of slots of a hash map
   !> A hash map grows when its number of keys would exceed this fraction
   !> (in tenths) of its slots.
   integer(ip), parameter :: max_load_tenths = 7
   integer(int64), parameter :: hash_prime = 2147483647_int64  !! 2**31 - 1, the hash modulus
   integer(int64), parameter :: hash_mult = 48271_int64  !! the final mixing multiplier (mod `hash_prime`)
   integer(int64), parameter :: hash_base = 131_int64  !! the base of the string polynomial

   !> A variable-length string. Arrays of `string` hold strings of
   !> different lengths; `s` is unallocated until set.
   type :: string
      character(:), allocatable :: s  !! the text
   end type string

   !> A hash map from a string to an integer (open addressing, linear
   !> probing). Trailing blanks of keys are not significant, as in Fortran
   !> comparisons; keys are case-sensitive (normalize them before use where
   !> SPICELIB compares names without regard to case).
   type :: string_map
      private
      integer(ip) :: n = 0  !! number of keys
      type(string), allocatable :: keys(:)  !! the key in each slot
      integer(ip), allocatable :: values(:)  !! the value in each slot
      integer(ip), allocatable :: hashes(:)  !! the hash of each slot's key; -1 for an empty slot
   contains
      procedure :: get => string_map_get
      procedure :: set => string_map_set
      procedure :: insert => string_map_insert
      procedure :: remove => string_map_remove
      procedure :: count => string_map_count
      procedure :: clear => string_map_clear
   end type string_map

   !> A hash map from an integer to an integer (open addressing, linear
   !> probing).
   type :: int_map
      private
      integer(ip) :: n = 0  !! number of keys
      integer(ip), allocatable :: keys(:)  !! the key in each slot
      integer(ip), allocatable :: values(:)  !! the value in each slot
      logical, allocatable :: used(:)  !! the slot holds a key
   contains
      procedure :: get => int_map_get
      procedure :: set => int_map_set
      procedure :: remove => int_map_remove
      procedure :: count => int_map_count
      procedure :: clear => int_map_clear
   end type int_map

   !> Enlarge an allocatable array so that it has at least `needed`
   !> elements (in its last dimension), keeping its contents. New elements
   !> are zero, false, blank or unallocated strings. An unallocated array
   !> is allocated. Nothing happens if the array is already large enough.
   interface grow
      module procedure grow_int, grow_real, grow_logical, grow_char, grow_string, &
         grow_int_2d, grow_real_2d
   end interface grow

contains

   !> The capacity to allocate to hold `needed` elements when `capacity`
   !> are allocated: at least double the capacity, at least `needed`, and
   !> at least a small minimum.
   pure integer(ip) function grown_capacity(capacity, needed)
      integer(ip), intent(in) :: capacity  !! the current capacity
      integer(ip), intent(in) :: needed  !! the number of elements needed
      if (capacity >= huge(capacity)/2) then
         grown_capacity = huge(capacity)
      else
         grown_capacity = max(needed, 2*capacity, min_capacity)
      end if
   end function grown_capacity

   !> `grow` for a rank-1 integer array.
   pure subroutine grow_int(a, needed)
      integer(ip), allocatable, intent(inout) :: a(:)  !! the array
      integer(ip), intent(in) :: needed  !! the number of elements needed
      integer(ip), allocatable :: tmp(:)
      integer(ip) :: n
      if (allocated(a)) then
         n = size(a, kind=ip)
         if (n >= needed) return
         allocate (tmp(grown_capacity(n, needed)), source=0_ip)
         tmp(1:n) = a
         call move_alloc(tmp, a)
      else
         allocate (a(grown_capacity(0_ip, needed)), source=0_ip)
      end if
   end subroutine grow_int

   !> `grow` for a rank-1 real array.
   pure subroutine grow_real(a, needed)
      real(wp), allocatable, intent(inout) :: a(:)  !! the array
      integer(ip), intent(in) :: needed  !! the number of elements needed
      real(wp), allocatable :: tmp(:)
      integer(ip) :: n
      if (allocated(a)) then
         n = size(a, kind=ip)
         if (n >= needed) return
         allocate (tmp(grown_capacity(n, needed)), source=0.0_wp)
         tmp(1:n) = a
         call move_alloc(tmp, a)
      else
         allocate (a(grown_capacity(0_ip, needed)), source=0.0_wp)
      end if
   end subroutine grow_real

   !> `grow` for a rank-1 logical array.
   pure subroutine grow_logical(a, needed)
      logical, allocatable, intent(inout) :: a(:)  !! the array
      integer(ip), intent(in) :: needed  !! the number of elements needed
      logical, allocatable :: tmp(:)
      integer(ip) :: n
      if (allocated(a)) then
         n = size(a, kind=ip)
         if (n >= needed) return
         allocate (tmp(grown_capacity(n, needed)), source=.false.)
         tmp(1:n) = a
         call move_alloc(tmp, a)
      else
         allocate (a(grown_capacity(0_ip, needed)), source=.false.)
      end if
   end subroutine grow_logical

   !> `grow` for a rank-1 array of fixed-length strings (any length).
   pure subroutine grow_char(a, needed)
      character(*), allocatable, intent(inout) :: a(:)  !! the array
      integer(ip), intent(in) :: needed  !! the number of elements needed
      character(len(a)), allocatable :: tmp(:)
      integer(ip) :: n
      if (allocated(a)) then
         n = size(a, kind=ip)
         if (n >= needed) return
         allocate (tmp(grown_capacity(n, needed)))
         tmp = ' '
         tmp(1:n) = a
         call move_alloc(tmp, a)
      else
         allocate (a(grown_capacity(0_ip, needed)))
         a = ' '
      end if
   end subroutine grow_char

   !> `grow` for a rank-1 array of variable-length strings.
   pure subroutine grow_string(a, needed)
      type(string), allocatable, intent(inout) :: a(:)  !! the array
      integer(ip), intent(in) :: needed  !! the number of elements needed
      type(string), allocatable :: tmp(:)
      integer(ip) :: n, i
      if (allocated(a)) then
         n = size(a, kind=ip)
         if (n >= needed) return
         allocate (tmp(grown_capacity(n, needed)))
         do i = 1, n
            if (allocated(a(i)%s)) call move_alloc(a(i)%s, tmp(i)%s)
         end do
         call move_alloc(tmp, a)
      else
         allocate (a(grown_capacity(0_ip, needed)))
      end if
   end subroutine grow_string

   !> `grow` for a rank-2 integer array: grows the second dimension (the
   !> number of columns); the first dimension is `rows` (used only when
   !> the array is not allocated).
   pure subroutine grow_int_2d(a, needed, rows)
      integer(ip), allocatable, intent(inout) :: a(:, :)  !! the array
      integer(ip), intent(in) :: needed  !! the number of columns needed
      integer(ip), intent(in) :: rows  !! the number of rows of a new array
      integer(ip), allocatable :: tmp(:, :)
      integer(ip) :: n
      if (allocated(a)) then
         n = size(a, 2, kind=ip)
         if (n >= needed) return
         allocate (tmp(size(a, 1), grown_capacity(n, needed)), source=0_ip)
         tmp(:, 1:n) = a
         call move_alloc(tmp, a)
      else
         allocate (a(rows, grown_capacity(0_ip, needed)), source=0_ip)
      end if
   end subroutine grow_int_2d

   !> `grow` for a rank-2 real array: grows the second dimension (the
   !> number of columns); the first dimension is `rows` (used only when
   !> the array is not allocated).
   pure subroutine grow_real_2d(a, needed, rows)
      real(wp), allocatable, intent(inout) :: a(:, :)  !! the array
      integer(ip), intent(in) :: needed  !! the number of columns needed
      integer(ip), intent(in) :: rows  !! the number of rows of a new array
      real(wp), allocatable :: tmp(:, :)
      integer(ip) :: n
      if (allocated(a)) then
         n = size(a, 2, kind=ip)
         if (n >= needed) return
         allocate (tmp(size(a, 1), grown_capacity(n, needed)), source=0.0_wp)
         tmp(:, 1:n) = a
         call move_alloc(tmp, a)
      else
         allocate (a(rows, grown_capacity(0_ip, needed)), source=0.0_wp)
      end if
   end subroutine grow_real_2d

   !> Make room for `needed` elements in a double precision SPICE cell
   !> (or window) held in an allocatable array with lower bound -5
   !> (LBCELL). The control area and the elements are kept, and the
   !> cell's size (`cell(-1)`) is set to the new capacity. A cell that is
   !> not allocated is allocated empty, with its size set.
   pure subroutine grow_cell_real(cell, needed)
      real(wp), allocatable, intent(inout) :: cell(:)  !! the cell, `cell(-5:size)`
      integer(ip), intent(in) :: needed  !! the number of elements needed
      real(wp), allocatable :: tmp(:)
      integer(ip) :: n, m
      if (allocated(cell)) then
         n = ubound(cell, 1, kind=ip)
         if (n >= needed) return
         m = grown_capacity(n, needed)
         allocate (tmp(-5:m), source=0.0_wp)
         tmp(-5:n) = cell
         call move_alloc(tmp, cell)
      else
         m = grown_capacity(0_ip, needed)
         allocate (cell(-5:m), source=0.0_wp)
      end if
      cell(-1) = real(m, wp)
   end subroutine grow_cell_real

   !> Make room for `needed` elements in an integer SPICE cell (or set)
   !> held in an allocatable array with lower bound -5 (LBCELL). The
   !> control area and the elements are kept, and the cell's size
   !> (`cell(-1)`) is set to the new capacity. A cell that is not
   !> allocated is allocated empty, with its size set.
   pure subroutine grow_cell_int(cell, needed)
      integer(ip), allocatable, intent(inout) :: cell(:)  !! the cell, `cell(-5:size)`
      integer(ip), intent(in) :: needed  !! the number of elements needed
      integer(ip), allocatable :: tmp(:)
      integer(ip) :: n, m
      if (allocated(cell)) then
         n = ubound(cell, 1, kind=ip)
         if (n >= needed) return
         m = grown_capacity(n, needed)
         allocate (tmp(-5:m), source=0_ip)
         tmp(-5:n) = cell
         call move_alloc(tmp, cell)
      else
         m = grown_capacity(0_ip, needed)
         allocate (cell(-5:m), source=0_ip)
      end if
      cell(-1) = m
   end subroutine grow_cell_int

   !> Enlarge a doubly linked list pool (the layout of [[lnkini]]: `pool(2,
   !> lbpool:size)` with the size, the number of free nodes and the head of
   !> the free list in columns 0 and -1) to `size` nodes. The existing
   !> nodes and lists keep their indices; the new nodes are added to the
   !> free list. Nothing happens if the pool already has `size` nodes.
   pure subroutine lnkgrow(pool, size)
      integer(ip), allocatable, intent(inout) :: pool(:, :)  !! the pool, `pool(2, lbpool:n)`
      integer(ip), intent(in) :: size  !! the number of nodes needed
      integer(ip), parameter :: forwrd = 1, bckwrd = 2  !! rows of the links
      integer(ip), parameter :: free = 0  !! backward link of a free node
      integer(ip), allocatable :: tmp(:, :)
      integer(ip) :: old, i
      old = pool(1, 0)
      if (size <= old) return
      allocate (tmp(2, lbpool:size), source=0_ip)
      tmp(:, lbpool:old) = pool(:, lbpool:old)
      ! the new nodes, linked in order, then the old free list
      do i = old + 1, size - 1
         tmp(forwrd, i) = i + 1
         tmp(bckwrd, i) = free
      end do
      tmp(forwrd, size) = pool(1, -1)
      tmp(bckwrd, size) = free
      tmp(1, -1) = old + 1
      tmp(2, 0) = pool(2, 0) + (size - old)
      tmp(1, 0) = size
      call move_alloc(tmp, pool)
   end subroutine lnkgrow

   !> The hash of a string up to its last nonblank character: from 0 to
   !> `hash_prime` - 1 (a polynomial mod 2**48, reduced mod a prime and
   !> mixed).
   pure integer(ip) function string_hash(key)
      character(*), intent(in) :: key  !! the string
      integer(int64) :: h
      integer :: i
      ! The polynomial is kept below 2**48 with a mask (no division per
      ! character); h*hash_base stays below 2**56.
      integer(int64), parameter :: mask48 = 2_int64**48 - 1
      h = 0
      do i = 1, len_trim(key)
         h = iand(h*hash_base + iachar(key(i:i)), mask48)
      end do
      string_hash = int(mod(mod(h, hash_prime)*hash_mult, hash_prime), ip)
   end function string_hash

   !> The hash of an integer: from 0 to `hash_prime` - 1. Consecutive
   !> integers fall in different slots.
   pure integer(ip) function int_hash(key)
      integer(ip), intent(in) :: key  !! the integer
      integer(int64) :: h
      h = modulo(int(key, int64), hash_prime)
      int_hash = int(mod(h*hash_mult, hash_prime), ip)
   end function int_hash

   !> The home slot (1 to `nslots`, a power of 2) of a hash.
   pure integer(ip) function home_slot(hash, nslots)
      integer(ip), intent(in) :: hash  !! the hash
      integer(ip), intent(in) :: nslots  !! the number of slots
      home_slot = iand(hash, nslots - 1) + 1
   end function home_slot

   !> The slot after `i` (1 to `nslots`, wrapping around).
   pure integer(ip) function next_slot(i, nslots)
      integer(ip), intent(in) :: i  !! the slot
      integer(ip), intent(in) :: nslots  !! the number of slots
      next_slot = iand(i, nslots - 1) + 1
   end function next_slot

   !> True if the slot `j` may be moved back to the empty slot `i` during
   !> a removal: its home slot `k` is not cyclically within (i, j].
   pure logical function may_move(i, j, k)
      integer(ip), intent(in) :: i  !! the empty slot
      integer(ip), intent(in) :: j  !! the slot considered
      integer(ip), intent(in) :: k  !! the home slot of its key
      if (i <= j) then
         may_move = k <= i .or. k > j
      else
         may_move = k <= i .and. k > j
      end if
   end function may_move

   !> The value of a key in a string map.
   pure subroutine string_map_get(self, key, value, found)
      class(string_map), intent(in) :: self  !! the map
      character(*), intent(in) :: key  !! the key
      integer(ip), intent(out) :: value  !! its value (0 if not found)
      logical, intent(out) :: found  !! the key is in the map
      integer(ip) :: i
      i = string_map_slot(self, key, string_hash(key))
      found = i > 0
      if (found) then
         if (self%hashes(i) < 0) found = .false.
      end if
      value = 0
      if (found) value = self%values(i)
   end subroutine string_map_get

   !> The slot of a key in a string map, or the empty slot where it would
   !> go; 0 if the map has no slots.
   pure integer(ip) function string_map_slot(self, key, hash) result(i)
      type(string_map), intent(in) :: self  !! the map
      character(*), intent(in) :: key  !! the key
      integer(ip), intent(in) :: hash  !! its hash
      integer(ip) :: nslots, n
      i = 0
      if (.not. allocated(self%hashes)) return
      nslots = size(self%hashes, kind=ip)
      n = len_trim(key)
      i = home_slot(hash, nslots)
      do
         if (self%hashes(i) < 0) return
         if (self%hashes(i) == hash) then
            if (len(self%keys(i)%s) == n) then
               if (self%keys(i)%s == key(1:n)) return
            end if
         end if
         i = next_slot(i, nslots)
      end do
   end function string_map_slot

   !> Set the value of a key in a string map (adding the key if needed).
   pure subroutine string_map_set(self, key, value)
      class(string_map), intent(inout) :: self  !! the map
      character(*), intent(in) :: key  !! the key
      integer(ip), intent(in) :: value  !! its value
      integer(ip) :: hash, i
      hash = string_hash(key)
      call string_map_reserve(self, self%n + 1)
      i = string_map_slot(self, key, hash)
      if (self%hashes(i) < 0) then
         self%n = self%n + 1
         self%hashes(i) = hash
         self%keys(i)%s = key(1:len_trim(key))
      end if
      self%values(i) = value
   end subroutine string_map_set

   !> Add a key with a value, unless the key is in the map already
   !> (one lookup: SET after GET looks it up twice).
   pure subroutine string_map_insert(self, key, value, existing)
      class(string_map), intent(inout) :: self  !! the map
      character(*), intent(in) :: key  !! the key
      integer(ip), intent(in) :: value  !! its value, if it is added
      integer(ip), intent(out) :: existing  !! the key's value if it was in the map; 0 if it was added
      integer(ip) :: hash, i
      hash = string_hash(key)
      call string_map_reserve(self, self%n + 1)
      i = string_map_slot(self, key, hash)
      if (self%hashes(i) < 0) then
         self%n = self%n + 1
         self%hashes(i) = hash
         self%keys(i)%s = key(1:len_trim(key))
         self%values(i) = value
         existing = 0
      else
         existing = self%values(i)
      end if
   end subroutine string_map_insert

   !> Make room for `n` keys in a string map (rehashing into more slots
   !> if needed).
   pure subroutine string_map_reserve(self, n)
      type(string_map), intent(inout) :: self  !! the map
      integer(ip), intent(in) :: n  !! the number of keys
      type(string), allocatable :: keys(:)
      integer(ip), allocatable :: values(:), hashes(:)
      integer(ip) :: nslots, i, j
      nslots = min_slots
      if (allocated(self%hashes)) nslots = size(self%hashes, kind=ip)
      do while (10*int(n, int64) > max_load_tenths*int(nslots, int64))
         nslots = 2*nslots
      end do
      if (allocated(self%hashes)) then
         if (nslots == size(self%hashes, kind=ip)) return
         call move_alloc(self%keys, keys)
         call move_alloc(self%values, values)
         call move_alloc(self%hashes, hashes)
      end if
      allocate (self%keys(nslots), self%values(nslots))
      allocate (self%hashes(nslots), source=-1_ip)
      if (.not. allocated(hashes)) return
      do i = 1, size(hashes, kind=ip)
         if (hashes(i) < 0) cycle
         j = home_slot(hashes(i), nslots)
         do while (self%hashes(j) >= 0)
            j = next_slot(j, nslots)
         end do
         self%hashes(j) = hashes(i)
         self%values(j) = values(i)
         call move_alloc(keys(i)%s, self%keys(j)%s)
      end do
   end subroutine string_map_reserve

   !> Remove a key from a string map (nothing happens if it is absent).
   pure subroutine string_map_remove(self, key)
      class(string_map), intent(inout) :: self  !! the map
      character(*), intent(in) :: key  !! the key
      integer(ip) :: i, j, nslots
      i = string_map_slot(self, key, string_hash(key))
      if (i == 0) return
      if (self%hashes(i) < 0) return
      nslots = size(self%hashes, kind=ip)
      self%n = self%n - 1
      ! Backward-shift deletion: move later keys of the run back into the
      ! hole, so that lookups need no tombstones.
      j = i
      do
         j = next_slot(j, nslots)
         if (self%hashes(j) < 0) exit
         if (may_move(i, j, home_slot(self%hashes(j), nslots))) then
            self%hashes(i) = self%hashes(j)
            self%values(i) = self%values(j)
            call move_alloc(self%keys(j)%s, self%keys(i)%s)
            i = j
         end if
      end do
      self%hashes(i) = -1
      self%values(i) = 0
      if (allocated(self%keys(i)%s)) deallocate (self%keys(i)%s)
   end subroutine string_map_remove

   !> The number of keys in a string map.
   pure integer(ip) function string_map_count(self)
      class(string_map), intent(in) :: self  !! the map
      string_map_count = self%n
   end function string_map_count

   !> Remove all keys from a string map (and release its memory, unless
   !> `keep` is true: then the map keeps its slots for the next keys).
   pure subroutine string_map_clear(self, keep)
      class(string_map), intent(inout) :: self  !! the map
      logical, intent(in), optional :: keep  !! keep the slots (default false)
      integer(ip) :: i
      if (present(keep)) then
         if (keep .and. allocated(self%hashes)) then
            do i = 1, size(self%hashes, kind=ip)
               if (self%hashes(i) < 0) cycle
               self%hashes(i) = -1
               self%values(i) = 0
               if (allocated(self%keys(i)%s)) deallocate (self%keys(i)%s)
            end do
            self%n = 0
            return
         end if
      end if
      self%n = 0
      if (allocated(self%keys)) deallocate (self%keys)
      if (allocated(self%values)) deallocate (self%values)
      if (allocated(self%hashes)) deallocate (self%hashes)
   end subroutine string_map_clear

   !> The value of a key in an integer map.
   pure subroutine int_map_get(self, key, value, found)
      class(int_map), intent(in) :: self  !! the map
      integer(ip), intent(in) :: key  !! the key
      integer(ip), intent(out) :: value  !! its value (0 if not found)
      logical, intent(out) :: found  !! the key is in the map
      integer(ip) :: i
      i = int_map_slot(self, key)
      found = i > 0
      if (found) found = self%used(i)
      value = 0
      if (found) value = self%values(i)
   end subroutine int_map_get

   !> The slot of a key in an integer map, or the empty slot where it
   !> would go; 0 if the map has no slots.
   pure integer(ip) function int_map_slot(self, key) result(i)
      type(int_map), intent(in) :: self  !! the map
      integer(ip), intent(in) :: key  !! the key
      integer(ip) :: nslots
      i = 0
      if (.not. allocated(self%used)) return
      nslots = size(self%used, kind=ip)
      i = home_slot(int_hash(key), nslots)
      do
         if (.not. self%used(i)) return
         if (self%keys(i) == key) return
         i = next_slot(i, nslots)
      end do
   end function int_map_slot

   !> Set the value of a key in an integer map (adding the key if needed).
   pure subroutine int_map_set(self, key, value)
      class(int_map), intent(inout) :: self  !! the map
      integer(ip), intent(in) :: key  !! the key
      integer(ip), intent(in) :: value  !! its value
      integer(ip) :: i
      call int_map_reserve(self, self%n + 1)
      i = int_map_slot(self, key)
      if (.not. self%used(i)) then
         self%n = self%n + 1
         self%used(i) = .true.
         self%keys(i) = key
      end if
      self%values(i) = value
   end subroutine int_map_set

   !> Make room for `n` keys in an integer map (rehashing into more slots
   !> if needed).
   pure subroutine int_map_reserve(self, n)
      type(int_map), intent(inout) :: self  !! the map
      integer(ip), intent(in) :: n  !! the number of keys
      integer(ip), allocatable :: keys(:), values(:)
      logical, allocatable :: used(:)
      integer(ip) :: nslots, i, j
      nslots = min_slots
      if (allocated(self%used)) nslots = size(self%used, kind=ip)
      do while (10*int(n, int64) > max_load_tenths*int(nslots, int64))
         nslots = 2*nslots
      end do
      if (allocated(self%used)) then
         if (nslots == size(self%used, kind=ip)) return
         call move_alloc(self%keys, keys)
         call move_alloc(self%values, values)
         call move_alloc(self%used, used)
      end if
      allocate (self%keys(nslots), self%values(nslots))
      allocate (self%used(nslots), source=.false.)
      if (.not. allocated(used)) return
      do i = 1, size(used, kind=ip)
         if (.not. used(i)) cycle
         j = home_slot(int_hash(keys(i)), nslots)
         do while (self%used(j))
            j = next_slot(j, nslots)
         end do
         self%used(j) = .true.
         self%keys(j) = keys(i)
         self%values(j) = values(i)
      end do
   end subroutine int_map_reserve

   !> Remove a key from an integer map (nothing happens if it is absent).
   pure subroutine int_map_remove(self, key)
      class(int_map), intent(inout) :: self  !! the map
      integer(ip), intent(in) :: key  !! the key
      integer(ip) :: i, j, nslots
      i = int_map_slot(self, key)
      if (i == 0) return
      if (.not. self%used(i)) return
      nslots = size(self%used, kind=ip)
      self%n = self%n - 1
      ! Backward-shift deletion (see string_map_remove).
      j = i
      do
         j = next_slot(j, nslots)
         if (.not. self%used(j)) exit
         if (may_move(i, j, home_slot(int_hash(self%keys(j)), nslots))) then
            self%keys(i) = self%keys(j)
            self%values(i) = self%values(j)
            i = j
         end if
      end do
      self%used(i) = .false.
   end subroutine int_map_remove

   !> The number of keys in an integer map.
   pure integer(ip) function int_map_count(self)
      class(int_map), intent(in) :: self  !! the map
      int_map_count = self%n
   end function int_map_count

   !> Remove all keys from an integer map (and release its memory).
   pure subroutine int_map_clear(self)
      class(int_map), intent(inout) :: self  !! the map
      self%n = 0
      if (allocated(self%keys)) deallocate (self%keys)
      if (allocated(self%values)) deallocate (self%values)
      if (allocated(self%used)) deallocate (self%used)
   end subroutine int_map_clear

end module pepper_containers
