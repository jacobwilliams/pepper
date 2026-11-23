!> PEPPER's C interface: the handles, strings, result buffers, errors,
!> callbacks, programmatic providers and prepared queries. The shims of the
!> routines themselves are generated (`pepper_capi`, by `tools/gen_capi.py`);
!> `include/pepper.h` declares both.
!>
!> - An instance (`pepper_spice_new`) and a context (`pepper_context_new`)
!>   are opaque pointers. Each thread queries through its own context, as
!>   in Fortran.
!> - Strings in are NUL-terminated; a string out is written to a buffer of
!>   the caller (its size given), NUL-terminated, trailing blanks removed.
!> - Every shim returns 1 if an error was signaled (in the context, or for
!>   the kernel changes in the instance), 0 otherwise. The messages are read
!>   with `pepper_getmsg` (`pepper_spice_getmsg`), then cleared with
!>   `pepper_reset` (`pepper_spice_reset`).
!> - Results of any size (an allocatable result in Fortran) stay in buffers
!>   of the context; the shim returns a pointer to them and their size,
!>   valid until the next call through the context.
!> - A kernel builder (`pepper_builder_new`) is an opaque pointer too: SPK
!>   segments added from arrays, loaded as a kernel with no file
!>   (`pepper_spice_load_kernel`). Kernel loaders (Fortran types) cannot be
!>   registered from C.
!> - Arrays: a matrix is column-major (Fortran's order); a list of vectors
!>   (a window, states, points) is an array of rows (C's `[n][k]`, the
!>   memory of Fortran's `(k, n)`).
module pepper_capi_core

   use, intrinsic :: iso_c_binding
   use pepper_kinds, only: ip, wp
   use pepper_containers, only: string
   use pepper_instance_module, only: pepper_instance, pepper_context, state_query, rotation_query, &
                              ephemeris_provider, frame_provider, gf_scalar, kernel_builder, kernel_loader, &
                              zzdynrot
   use pepper_bounds, only: lbcell
   implicit none
   ! Public: the bind(c) procedures must stay global symbols at any
   ! optimization level.

   !> A buffer of reals for a result of any size.
   type :: real_buffer
      real(c_double), allocatable :: v(:)  !! the values
   end type real_buffer

   !> A buffer of integers for a result of any size.
   type :: int_buffer
      integer(c_int), allocatable :: v(:)  !! the values
   end type int_buffer

   !> A buffer of characters for a string or a list of strings (each
   !> NUL-terminated).
   type :: char_buffer
      character(kind=c_char), allocatable :: v(:)  !! the characters
   end type char_buffer

   integer, parameter :: nbuffers = 4  !! result buffers of each kind per context

   !> A context of the C interface: a PEPPER context, with what the shims
   !> keep per thread.
   type, extends(pepper_context), public :: capi_context
      type(c_ptr) :: self = c_null_ptr  !! this context's handle
      type(c_ptr) :: user = c_null_ptr  !! data of the caller (PepperyPy: its Context object)
      type(real_buffer) :: rbuf(nbuffers)  !! real results of any size
      type(int_buffer) :: ibuf(nbuffers)  !! integer results of any size
      type(char_buffer) :: sbuf(nbuffers)  !! string results of any size
      ! The callbacks of the GF search in progress (saved and restored
      ! around a nested search, made by a callback).
      type(c_funptr) :: scalar_fn = c_null_funptr  !! the scalar function ([[pepper_scalar_fn]])
      type(c_ptr) :: scalar_user = c_null_ptr  !! its data
      type(c_funptr) :: bool_fn = c_null_funptr  !! the boolean function or the decreasing test ([[pepper_bool_fn]])
      type(c_ptr) :: bool_user = c_null_ptr  !! its data
      ! The GF hooks ([[pepper_gf_set_hooks]]): the step, refinement,
      ! progress report and interrupt routines of the searches that take
      ! them (GFEVNT, GFFOVE, GFOCCE). Null: PEPPER's own routine.
      type(c_funptr) :: step_fn = c_null_funptr  !! the step ([[pepper_step_fn]])
      type(c_funptr) :: refn_fn = c_null_funptr  !! the refinement ([[pepper_refn_fn]])
      type(c_funptr) :: repi_fn = c_null_funptr  !! the report's start ([[pepper_repi_fn]])
      type(c_funptr) :: repu_fn = c_null_funptr  !! the report's update ([[pepper_repu_fn]])
      type(c_funptr) :: repf_fn = c_null_funptr  !! the report's end ([[pepper_repf_fn]])
      type(c_funptr) :: bail_fn = c_null_funptr  !! the interrupt test ([[pepper_bail_fn]])
      type(c_ptr) :: hooks_user = c_null_ptr  !! the data of the hooks
   end type capi_context

   !> A programmatic body whose states come from a C function.
   type, extends(ephemeris_provider) :: capi_ephemeris
      type(c_funptr) :: fn = c_null_funptr  !! the function ([[pepper_state_fn]])
      type(c_ptr) :: user = c_null_ptr  !! its data
   contains
      procedure :: state => capi_ephemeris_state
   end type capi_ephemeris

   !> A kernel loader whose reading is done by C functions
   !> ([[pepper_spice_add_loader]]).
   type, extends(kernel_loader) :: capi_loader
      type(c_funptr) :: load_fn = c_null_funptr  !! the reader ([[pepper_load_fn]])
      type(c_funptr) :: detect_fn = c_null_funptr  !! the detection ([[pepper_detect_fn]]), or null
      type(c_funptr) :: unload_fn = c_null_funptr  !! the unload callback ([[pepper_unload_fn]]), or null
      type(c_ptr) :: user = c_null_ptr  !! the caller's data
   contains
      procedure :: load => capi_loader_load
      procedure :: detect => capi_loader_detect
      procedure :: unload => capi_loader_unload
   end type capi_loader

   !> A programmatic frame whose transformations come from a C function.
   type, extends(frame_provider) :: capi_frame
      type(c_funptr) :: fn = c_null_funptr  !! the function ([[pepper_xform_fn]])
      type(c_ptr) :: user = c_null_ptr  !! its data
   contains
      procedure :: transform => capi_frame_transform
   end type capi_frame

   abstract interface
      !> A scalar function of time (GF): `value` at `et`. Errors: signal
      !> them in `ctx` (`pepper_sigerr`).
      subroutine pepper_scalar_fn ( ctx, user, et, value ) bind(c)
         import :: c_ptr, c_double
         type(c_ptr), value :: ctx  !! the context of the search
         type(c_ptr), value :: user  !! the caller's data
         real(c_double), value :: et  !! the time (TDB seconds past J2000)
         real(c_double), intent(out) :: value  !! the function's value
      end subroutine pepper_scalar_fn

      !> A boolean function of time (GF): a condition at `et`, or whether
      !> the scalar function is decreasing at `et`.
      subroutine pepper_bool_fn ( ctx, user, et, value ) bind(c)
         import :: c_ptr, c_double, c_int
         type(c_ptr), value :: ctx  !! the context of the search
         type(c_ptr), value :: user  !! the caller's data
         real(c_double), value :: et  !! the time (TDB seconds past J2000)
         integer(c_int), intent(out) :: value  !! the condition (0: false)
      end subroutine pepper_bool_fn

      !> A GF step routine: the time step to take from `time`.
      subroutine pepper_step_fn ( ctx, user, time, step ) bind(c)
         import :: c_ptr, c_double
         type(c_ptr), value :: ctx  !! the context of the search
         type(c_ptr), value :: user  !! the caller's data
         real(c_double), value :: time  !! the time (TDB seconds past J2000)
         real(c_double), intent(out) :: step  !! the time step (seconds)
      end subroutine pepper_step_fn

      !> A GF refinement routine: a time between `t1` and `t2`, at which
      !> the condition is `s1` and `s2` (0: false).
      subroutine pepper_refn_fn ( ctx, user, t1, t2, s1, s2, t ) bind(c)
         import :: c_ptr, c_double, c_int
         type(c_ptr), value :: ctx  !! the context of the search
         type(c_ptr), value :: user  !! the caller's data
         real(c_double), value :: t1  !! one of the times
         real(c_double), value :: t2  !! the other time
         integer(c_int), value :: s1  !! the condition at t1
         integer(c_int), value :: s2  !! the condition at t2
         real(c_double), intent(out) :: t  !! the time between them
      end subroutine pepper_refn_fn

      !> The start of a GF progress report: the window searched (`n`
      !> numbers: the endpoints of its intervals) and the text before and
      !> after the percentage.
      subroutine pepper_repi_fn ( ctx, user, n, window, begmss, endmss ) bind(c)
         import :: c_ptr, c_double, c_int, c_char
         type(c_ptr), value :: ctx  !! the context of the search
         type(c_ptr), value :: user  !! the caller's data
         integer(c_int), value :: n  !! the number of endpoints
         real(c_double), intent(in) :: window(*)  !! the endpoints
         character(kind=c_char), intent(in) :: begmss(*)  !! the text before (NUL-terminated)
         character(kind=c_char), intent(in) :: endmss(*)  !! the text after (NUL-terminated)
      end subroutine pepper_repi_fn

      !> An update of a GF progress report.
      subroutine pepper_repu_fn ( ctx, user, ivbeg, ivend, time ) bind(c)
         import :: c_ptr, c_double
         type(c_ptr), value :: ctx  !! the context of the search
         type(c_ptr), value :: user  !! the caller's data
         real(c_double), value :: ivbeg  !! the start of the interval searched
         real(c_double), value :: ivend  !! its end
         real(c_double), value :: time  !! the time reached
      end subroutine pepper_repu_fn

      !> The end of a GF progress report.
      subroutine pepper_repf_fn ( ctx, user ) bind(c)
         import :: c_ptr
         type(c_ptr), value :: ctx  !! the context of the search
         type(c_ptr), value :: user  !! the caller's data
      end subroutine pepper_repf_fn

      !> A GF interrupt test: nonzero to stop the search.
      integer(c_int) function pepper_bail_fn ( ctx, user ) bind(c)
         import :: c_ptr, c_int
         type(c_ptr), value :: ctx  !! the context of the search
         type(c_ptr), value :: user  !! the caller's data
      end function pepper_bail_fn

      !> The state of a programmatic body: relative to its registered
      !> center, in its registered frame. Returns 0 if no state is
      !> available at `et`.
      integer(c_int) function pepper_state_fn ( ctx, user, body, et, state ) bind(c)
         import :: c_ptr, c_double, c_int
         type(c_ptr), value :: ctx  !! the context of the query (null outside the C interface)
         type(c_ptr), value :: user  !! the caller's data
         integer(c_int), value :: body  !! the body's ID code
         real(c_double), value :: et  !! the epoch (TDB seconds past J2000)
         real(c_double), intent(out) :: state(6)  !! the state (km, km/s)
      end function pepper_state_fn

      !> The state transformation of a programmatic frame: from the frame
      !> to the frame `relative` (column-major 6x6). Returns 0 if none is
      !> available at `et`.
      integer(c_int) function pepper_xform_fn ( ctx, user, frame, et, xform, relative ) bind(c)
         import :: c_ptr, c_double, c_int
         type(c_ptr), value :: ctx  !! the context of the query (null outside the C interface)
         type(c_ptr), value :: user  !! the caller's data
         integer(c_int), value :: frame  !! the frame's ID code
         real(c_double), value :: et  !! the epoch (TDB seconds past J2000)
         real(c_double), intent(out) :: xform(6, 6)  !! the transformation
         integer(c_int), intent(out) :: relative  !! the ID code of the frame transformed to
      end function pepper_xform_fn

      !> A kernel loader's reader: fill `builder` from `file` with the
      !> `pepper_builder_*` functions; errors in `ctx`. Returns 0 if the file
      !> was read.
      integer(c_int) function pepper_load_fn ( ctx, user, builder, file ) bind(c)
         import :: c_ptr, c_int, c_char
         type(c_ptr), value :: ctx  !! a context of the instance, for this call
         type(c_ptr), value :: user  !! the caller's data
         type(c_ptr), value :: builder  !! the builder to fill
         character(kind=c_char), intent(in) :: file(*)  !! the file
      end function pepper_load_fn

      !> A kernel loader's detection: nonzero to read `file`, by its first
      !> `nhead` bytes `head` (not NUL-terminated).
      integer(c_int) function pepper_detect_fn ( user, file, head, nhead ) bind(c)
         import :: c_ptr, c_int, c_char
         type(c_ptr), value :: user  !! the caller's data
         character(kind=c_char), intent(in) :: file(*)  !! the file
         character(kind=c_char), intent(in) :: head(*)  !! its first bytes
         integer(c_int), value :: nhead  !! their number
      end function pepper_detect_fn

      !> A kernel loader's unload callback: the kernel of `file` is unloaded.
      subroutine pepper_unload_fn ( user, file ) bind(c)
         import :: c_ptr, c_char
         type(c_ptr), value :: user  !! the caller's data
         character(kind=c_char), intent(in) :: file(*)  !! the file
      end subroutine pepper_unload_fn
   end interface

   public :: c_to_f, c_to_f_array, f_to_c, status_of, spice_status, context_of, spice_of
   public :: set_real_result, set_int_result, set_string_result, set_strings_result
   public :: capi_gf_scalar, capi_gf_bool, capi_gf_decr

contains

   !> The Fortran string of a NUL-terminated C string.
   subroutine c_to_f ( cstr, fstr )
      character(kind=c_char), intent(in) :: cstr(*)  !! the C string
      character(:), allocatable, intent(out) :: fstr  !! the Fortran string (without the NUL)
      integer :: n, i
      n = 0
      do while (cstr(n + 1) /= c_null_char)
         n = n + 1
      end do
      allocate (character(n) :: fstr)
      do i = 1, n
         fstr(i:i) = cstr(i)
      end do
   end subroutine c_to_f

   !> Fortran strings from a block of `n` fixed-width C strings (each
   !> `width` characters, NUL-padded or full): blank-padded to the longest.
   subroutine c_to_f_array ( block, n, width, fstrs )
      integer(c_int), intent(in) :: n  !! the number of strings
      integer(c_int), intent(in) :: width  !! the width of each string in the block
      character(kind=c_char), intent(in) :: block(width, *)  !! the strings
      character(:), allocatable, intent(out) :: fstrs(:)  !! the Fortran strings
      integer :: i, j, len_max, len_i
      len_max = 1
      do i = 1, n
         len_i = 0
         do j = 1, width
            if (block(j, i) == c_null_char) exit
            len_i = j
         end do
         len_max = max(len_max, len_i)
      end do
      allocate (character(len_max) :: fstrs(n))
      do i = 1, n
         fstrs(i) = ' '
         do j = 1, min(width, len_max)
            if (block(j, i) == c_null_char) exit
            fstrs(i)(j:j) = block(j, i)
         end do
      end do
   end subroutine c_to_f_array

   !> Copy a Fortran string to a C buffer of `size` characters:
   !> NUL-terminated, trailing blanks removed, cut to fit.
   subroutine f_to_c ( fstr, size, cstr )
      character(*), intent(in) :: fstr  !! the Fortran string
      integer(c_int), intent(in) :: size  !! the size of the buffer (with the NUL)
      character(kind=c_char), intent(out) :: cstr(*)  !! the buffer
      integer :: n, i
      if (size < 1) return
      n = min(len_trim(fstr), size - 1)
      do i = 1, n
         cstr(i) = fstr(i:i)
      end do
      cstr(n + 1) = c_null_char
   end subroutine f_to_c

   !> The context of a handle.
   function context_of ( handle ) result(c)
      type(c_ptr), intent(in) :: handle  !! the handle ([[pepper_context_new]])
      type(capi_context), pointer :: c  !! the context
      call c_f_pointer(handle, c)
   end function context_of

   !> The instance of a handle.
   function spice_of ( handle ) result(s)
      type(c_ptr), intent(in) :: handle  !! the handle ([[pepper_spice_new]])
      type(pepper_instance), pointer :: s  !! the instance
      call c_f_pointer(handle, s)
   end function spice_of

   !> The status a shim returns: 1 if an error was signaled in the context.
   integer(c_int) function status_of ( c )
      class(pepper_context), intent(in) :: c  !! the context
      status_of = merge(1_c_int, 0_c_int, c%failed())
   end function status_of

   !> The status a shim of a kernel change returns: 1 if an error was
   !> signaled in the instance.
   integer(c_int) function spice_status ( s )
      type(pepper_instance), intent(in) :: s  !! the instance
      spice_status = merge(1_c_int, 0_c_int, s%failed())
   end function spice_status

   !> Keep a real result in buffer `k` of the context, and return its
   !> address and number of values.
   subroutine set_real_result ( c, k, values, ptr, n )
      type(capi_context), intent(inout), target :: c  !! the context
      integer, intent(in) :: k  !! the buffer
      real(wp), intent(in) :: values(:)  !! the result
      type(c_ptr), intent(out) :: ptr  !! its address (valid until the next call)
      integer(c_int), intent(out) :: n  !! its number of values
      n = int(size(values), c_int)
      if (allocated(c%rbuf(k)%v)) then
         if (size(c%rbuf(k)%v) < max(n, 1)) deallocate (c%rbuf(k)%v)
      end if
      if (.not. allocated(c%rbuf(k)%v)) allocate (c%rbuf(k)%v(max(n, 1)))
      c%rbuf(k)%v(1:n) = values
      ptr = c_loc(c%rbuf(k)%v)
   end subroutine set_real_result

   !> Keep an integer result in buffer `k` of the context, and return its
   !> address and number of values.
   subroutine set_int_result ( c, k, values, ptr, n )
      type(capi_context), intent(inout), target :: c  !! the context
      integer, intent(in) :: k  !! the buffer
      integer(ip), intent(in) :: values(:)  !! the result
      type(c_ptr), intent(out) :: ptr  !! its address (valid until the next call)
      integer(c_int), intent(out) :: n  !! its number of values
      n = int(size(values), c_int)
      if (allocated(c%ibuf(k)%v)) then
         if (size(c%ibuf(k)%v) < max(n, 1)) deallocate (c%ibuf(k)%v)
      end if
      if (.not. allocated(c%ibuf(k)%v)) allocate (c%ibuf(k)%v(max(n, 1)))
      c%ibuf(k)%v(1:n) = values
      ptr = c_loc(c%ibuf(k)%v)
   end subroutine set_int_result

   !> Keep a string result in buffer `k` of the context (NUL-terminated,
   !> trailing blanks kept), and return its address and length.
   subroutine set_string_result ( c, k, str, ptr, n )
      type(capi_context), intent(inout), target :: c  !! the context
      integer, intent(in) :: k  !! the buffer
      character(*), intent(in) :: str  !! the result
      type(c_ptr), intent(out) :: ptr  !! its address (valid until the next call)
      integer(c_int), intent(out) :: n  !! its length (without the NUL)
      integer :: i
      n = int(len(str), c_int)
      if (allocated(c%sbuf(k)%v)) deallocate (c%sbuf(k)%v)
      allocate (c%sbuf(k)%v(n + 1))
      do i = 1, n
         c%sbuf(k)%v(i) = str(i:i)
      end do
      c%sbuf(k)%v(n + 1) = c_null_char
      ptr = c_loc(c%sbuf(k)%v)
   end subroutine set_string_result

   !> Keep a list of strings in buffer `k` of the context: each
   !> NUL-terminated, trailing blanks removed, one after the other. Returns
   !> the address and the number of strings.
   subroutine set_strings_result ( c, k, strs, ptr, n )
      type(capi_context), intent(inout), target :: c  !! the context
      integer, intent(in) :: k  !! the buffer
      type(string), intent(in) :: strs(:)  !! the result
      type(c_ptr), intent(out) :: ptr  !! its address (valid until the next call)
      integer(c_int), intent(out) :: n  !! the number of strings
      integer :: i, j, total, pos, lt
      n = int(size(strs), c_int)
      total = 1
      do i = 1, size(strs)
         if (allocated(strs(i)%s)) then
            total = total + len_trim(strs(i)%s) + 1
         else
            total = total + 1
         end if
      end do
      if (allocated(c%sbuf(k)%v)) deallocate (c%sbuf(k)%v)
      allocate (c%sbuf(k)%v(total))
      pos = 0
      do i = 1, size(strs)
         lt = 0
         if (allocated(strs(i)%s)) lt = len_trim(strs(i)%s)
         do j = 1, lt
            c%sbuf(k)%v(pos + j) = strs(i)%s(j:j)
         end do
         pos = pos + lt + 1
         c%sbuf(k)%v(pos) = c_null_char
      end do
      c%sbuf(k)%v(total) = c_null_char
      ptr = c_loc(c%sbuf(k)%v)
   end subroutine set_strings_result

   ! ------------------------------------------------------------------
   ! Instances and contexts

   !> A new instance (no kernels loaded). Free it with
   !> [[pepper_spice_free]], after its contexts.
   function pepper_spice_new () result(handle) bind(c, name='pepper_spice_new')
      type(c_ptr) :: handle  !! the instance
      type(pepper_instance), pointer :: s
      allocate (s)
      handle = c_loc(s)
   end function pepper_spice_new

   !> Free an instance: unload its kernels and release its memory. Its
   !> contexts must have been freed.
   subroutine pepper_spice_free ( handle ) bind(c, name='pepper_spice_free')
      type(c_ptr), value :: handle  !! the instance
      type(pepper_instance), pointer :: s
      if (.not. c_associated(handle)) return
      s => spice_of(handle)
      call s%kclear()
      deallocate (s)
   end subroutine pepper_spice_free

   !> A new context of an instance, for the queries of one thread at a
   !> time. Free it with [[pepper_context_free]].
   function pepper_context_new ( spice ) result(handle) bind(c, name='pepper_context_new')
      type(c_ptr), value :: spice  !! the instance
      type(c_ptr) :: handle  !! the context
      type(pepper_instance), pointer :: s
      type(capi_context), pointer :: c
      s => spice_of(spice)
      allocate (c)
      c%pepper_context = s%new_context()
      handle = c_loc(c)
      c%self = handle
   end function pepper_context_new

   !> Free a context (closes the files it has open).
   subroutine pepper_context_free ( ctx ) bind(c, name='pepper_context_free')
      type(c_ptr), value :: ctx  !! the context
      type(capi_context), pointer :: c
      if (.not. c_associated(ctx)) return
      c => context_of(ctx)
      deallocate (c)
   end subroutine pepper_context_free

   !> Attach data of the caller to a context (returned by
   !> [[pepper_context_user]]; PepperyPy: its Context object).
   subroutine pepper_context_set_user ( ctx, user ) bind(c, name='pepper_context_set_user')
      type(c_ptr), value :: ctx  !! the context
      type(c_ptr), value :: user  !! the data
      type(capi_context), pointer :: c
      c => context_of(ctx)
      c%user = user
   end subroutine pepper_context_set_user

   !> The data of the caller attached to a context (null if none).
   function pepper_context_user ( ctx ) result(user) bind(c, name='pepper_context_user')
      type(c_ptr), value :: ctx  !! the context
      type(c_ptr) :: user  !! the data
      type(capi_context), pointer :: c
      user = c_null_ptr
      if (.not. c_associated(ctx)) return
      c => context_of(ctx)
      user = c%user
   end function pepper_context_user

   !> Read binary kernels whole into memory when they are loaded (for the
   !> loads that follow).
   subroutine pepper_spice_set_in_memory ( spice, flag ) bind(c, name='pepper_spice_set_in_memory')
      type(c_ptr), value :: spice  !! the instance
      integer(c_int), value :: flag  !! 1: in memory; 0: read records as needed
      type(pepper_instance), pointer :: s
      s => spice_of(spice)
      call s%set_in_memory(flag /= 0)
   end subroutine pepper_spice_set_in_memory

   !> The memory used by binary kernels read into memory (bytes).
   function pepper_spice_kernel_memory ( spice ) result(bytes) bind(c, name='pepper_spice_kernel_memory')
      type(c_ptr), value :: spice  !! the instance
      integer(c_int64_t) :: bytes  !! the memory
      type(pepper_instance), pointer :: s
      s => spice_of(spice)
      bytes = s%kernel_memory()
   end function pepper_spice_kernel_memory

   !> Set the number of files each context keeps open at a time (before
   !> the contexts open files).
   subroutine pepper_spice_set_max_open_files ( spice, n ) bind(c, name='pepper_spice_set_max_open_files')
      type(c_ptr), value :: spice  !! the instance
      integer(c_int), value :: n  !! the number of files
      type(pepper_instance), pointer :: s
      s => spice_of(spice)
      call s%set_max_open_files(n)
   end subroutine pepper_spice_set_max_open_files

   !> The number of files each context keeps open at a time.
   function pepper_spice_max_open_files ( spice ) result(n) bind(c, name='pepper_spice_max_open_files')
      type(c_ptr), value :: spice  !! the instance
      integer(c_int) :: n  !! the number of files
      type(pepper_instance), pointer :: s
      s => spice_of(spice)
      n = s%max_open_files()
   end function pepper_spice_max_open_files

   !> Load a kernel (or the kernels of a meta-kernel). `in_memory`: 1 reads
   !> binary kernels whole into memory, 0 reads records as needed, -1 the
   !> instance's choice.
   integer(c_int) function pepper_spice_furnsh ( spice, file, in_memory ) bind(c, name='pepper_spice_furnsh')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: file(*)  !! the kernel
      integer(c_int), value :: in_memory  !! 1, 0 or -1
      type(pepper_instance), pointer :: s
      character(:), allocatable :: f_file
      s => spice_of(spice)
      call c_to_f(file, f_file)
      if (in_memory < 0) then
         call s%furnsh(f_file)
      else
         call s%furnsh(f_file, in_memory=in_memory /= 0)
      end if
      pepper_spice_furnsh = spice_status(s)
   end function pepper_spice_furnsh

   !> Load a binary DAF (SPK/CK/PCK), ASCII text kernel, or JSON kernel
   !> directly from `nbytes` bytes in memory without touching the filesystem.
   integer(c_int) function pepper_spice_furnsh_memory ( spice, name, bytes, nbytes ) &
         bind(c, name='pepper_spice_furnsh_memory')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: name(*)  !! virtual kernel name
      type(c_ptr), value :: bytes  !! pointer to raw kernel bytes (may be NULL when nbytes == 0)
      integer(c_int64_t), value :: nbytes  !! number of bytes
      type(pepper_instance), pointer :: s
      type(pepper_context), pointer :: own
      character(:), allocatable :: f_name
      integer(c_int8_t), pointer :: f_bytes(:)
      integer(c_int8_t), target :: empty_bytes(0)
      s => spice_of(spice)
      call c_to_f(name, f_name)
      if (nbytes < 0_c_int64_t) then
         own => s%context()
         call own%setmsg('pepper_spice_furnsh_memory: negative byte length.')
         call own%sigerr('SPICE(INVALIDARGUMENT)')
      else if (nbytes == 0_c_int64_t) then
         call s%furnsh_memory(f_name, empty_bytes)
      else if (.not. c_associated(bytes)) then
         own => s%context()
         call own%setmsg('pepper_spice_furnsh_memory: NULL bytes pointer with positive length.')
         call own%sigerr('SPICE(NULLPOINTER)')
      else
         call c_f_pointer(bytes, f_bytes, [nbytes])
         call s%furnsh_memory(f_name, f_bytes)
      end if
      pepper_spice_furnsh_memory = spice_status(s)
   end function pepper_spice_furnsh_memory

   !> Set strings of the kernel pool (PCPOOL): `n` strings of `width`
   !> characters each in `cvals`.
   integer(c_int) function pepper_spice_pcpool ( spice, name, n, width, cvals ) bind(c, name='pepper_spice_pcpool')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: name(*)  !! the variable
      integer(c_int), value :: n  !! the number of strings
      integer(c_int), value :: width  !! the width of each string in the block
      character(kind=c_char), intent(in) :: cvals(*)  !! the strings
      type(pepper_instance), pointer :: s
      character(:), allocatable :: f_name, f_cvals(:)
      s => spice_of(spice)
      call c_to_f(name, f_name)
      call c_to_f_array(cvals, n, width, f_cvals)
      call s%pcpool(f_name, n, f_cvals)
      pepper_spice_pcpool = spice_status(s)
   end function pepper_spice_pcpool

   !> Load kernel pool assignments from strings (LMPOOL): `n` lines of
   !> `width` characters each in `cvals`.
   integer(c_int) function pepper_spice_lmpool ( spice, n, width, cvals ) bind(c, name='pepper_spice_lmpool')
      type(c_ptr), value :: spice  !! the instance
      integer(c_int), value :: n  !! the number of lines
      integer(c_int), value :: width  !! the width of each line in the block
      character(kind=c_char), intent(in) :: cvals(*)  !! the lines
      type(pepper_instance), pointer :: s
      character(:), allocatable :: f_cvals(:)
      s => spice_of(spice)
      call c_to_f_array(cvals, n, width, f_cvals)
      call s%lmpool(f_cvals, n)
      pepper_spice_lmpool = spice_status(s)
   end function pepper_spice_lmpool

   !> Set reals of the kernel pool (PDPOOL).
   integer(c_int) function pepper_spice_pdpool ( spice, name, n, values ) bind(c, name='pepper_spice_pdpool')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: name(*)  !! the variable
      integer(c_int), value :: n  !! the number of values
      real(c_double), intent(in) :: values(*)  !! the values
      type(pepper_instance), pointer :: s
      character(:), allocatable :: f_name
      s => spice_of(spice)
      call c_to_f(name, f_name)
      call s%pdpool(f_name, n, values)
      pepper_spice_pdpool = spice_status(s)
   end function pepper_spice_pdpool

   !> Set integers of the kernel pool (PIPOOL).
   integer(c_int) function pepper_spice_pipool ( spice, name, n, ivals ) bind(c, name='pepper_spice_pipool')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: name(*)  !! the variable
      integer(c_int), value :: n  !! the number of values
      integer(c_int), intent(in) :: ivals(*)  !! the values
      type(pepper_instance), pointer :: s
      character(:), allocatable :: f_name
      s => spice_of(spice)
      call c_to_f(name, f_name)
      call s%pipool(f_name, n, ivals)
      pepper_spice_pipool = spice_status(s)
   end function pepper_spice_pipool

   ! ------------------------------------------------------------------
   ! Errors

   !> 1 if an error was signaled in the context and not reset.
   integer(c_int) function pepper_failed ( ctx ) bind(c, name='pepper_failed')
      type(c_ptr), value :: ctx  !! the context
      pepper_failed = status_of(context_of(ctx))
   end function pepper_failed

   !> An error message of the context: `option` 'SHORT', 'EXPLAIN' or
   !> 'LONG'.
   subroutine pepper_getmsg ( ctx, option, size, msg ) bind(c, name='pepper_getmsg')
      type(c_ptr), value :: ctx  !! the context
      character(kind=c_char), intent(in) :: option(*)  !! which message
      integer(c_int), value :: size  !! the size of the buffer
      character(kind=c_char), intent(out) :: msg(*)  !! the message
      type(capi_context), pointer :: c
      character(:), allocatable :: f_option
      character(len=max(size - 1, 1)) :: f_msg
      c => context_of(ctx)
      call c_to_f(option, f_option)
      call c%getmsg(f_option, f_msg)
      call f_to_c(f_msg, size, msg)
   end subroutine pepper_getmsg

   !> Clear the error status and the messages of the context.
   subroutine pepper_reset ( ctx ) bind(c, name='pepper_reset')
      type(c_ptr), value :: ctx  !! the context
      type(capi_context), pointer :: c
      c => context_of(ctx)
      call c%reset()
   end subroutine pepper_reset

   !> Signal an error in the context: the short message (such as
   !> 'SPICE(BADINPUT)') and the long one.
   subroutine pepper_sigerr ( ctx, short, long ) bind(c, name='pepper_sigerr')
      type(c_ptr), value :: ctx  !! the context
      character(kind=c_char), intent(in) :: short(*)  !! the short message
      character(kind=c_char), intent(in) :: long(*)  !! the long message
      type(capi_context), pointer :: c
      character(:), allocatable :: f_short, f_long
      c => context_of(ctx)
      call c_to_f(short, f_short)
      call c_to_f(long, f_long)
      call c%setmsg(f_long)
      call c%sigerr(f_short)
   end subroutine pepper_sigerr

   !> 1 if an error was signaled in the instance (by a kernel change) and
   !> not reset.
   integer(c_int) function pepper_spice_failed ( spice ) bind(c, name='pepper_spice_failed')
      type(c_ptr), value :: spice  !! the instance
      pepper_spice_failed = spice_status(spice_of(spice))
   end function pepper_spice_failed

   !> An error message of the instance (of a kernel change).
   subroutine pepper_spice_getmsg ( spice, option, size, msg ) bind(c, name='pepper_spice_getmsg')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: option(*)  !! 'SHORT', 'EXPLAIN' or 'LONG'
      integer(c_int), value :: size  !! the size of the buffer
      character(kind=c_char), intent(out) :: msg(*)  !! the message
      type(pepper_instance), pointer :: s
      type(pepper_context), pointer :: own
      character(:), allocatable :: f_option
      character(len=max(size - 1, 1)) :: f_msg
      s => spice_of(spice)
      own => s%context()
      call c_to_f(option, f_option)
      call own%getmsg(f_option, f_msg)
      call f_to_c(f_msg, size, msg)
   end subroutine pepper_spice_getmsg

   !> Clear the error status and the messages of the instance.
   subroutine pepper_spice_reset ( spice ) bind(c, name='pepper_spice_reset')
      type(c_ptr), value :: spice  !! the instance
      type(pepper_instance), pointer :: s
      type(pepper_context), pointer :: own
      s => spice_of(spice)
      own => s%context()
      call own%reset()
   end subroutine pepper_spice_reset

   ! ------------------------------------------------------------------
   ! GF callbacks: the Fortran procedures given to the GF routines, which
   ! call the C functions saved in the context.

   !> [[gf_scalar]] of the C interface: calls the context's scalar function.
   subroutine capi_gf_scalar ( ctx, et, value )
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: et  !! the time
      real(wp), intent(out) :: value  !! the function's value
      procedure(pepper_scalar_fn), pointer :: fn
      value = 0.0_wp
      select type (ctx)
      type is (capi_context)
         call c_f_procpointer(ctx%scalar_fn, fn)
         call fn(ctx%self, ctx%scalar_user, et, value)
      end select
   end subroutine capi_gf_scalar

   !> [[gf_boolean]] of the C interface: calls the context's boolean
   !> function (which knows the scalar function).
   subroutine capi_gf_bool ( ctx, udfunc, et, xbool )
      class(pepper_context), intent(inout) :: ctx  !! the context
      procedure(gf_scalar) :: udfunc  !! the scalar function (the context's)
      real(wp), intent(in) :: et  !! the time
      logical, intent(out) :: xbool  !! the condition
      procedure(pepper_bool_fn), pointer :: fn
      integer(c_int) :: value
      xbool = .false.
      select type (ctx)
      type is (capi_context)
         call c_f_procpointer(ctx%bool_fn, fn)
         value = 0
         call fn(ctx%self, ctx%bool_user, et, value)
         xbool = value /= 0
      end select
   end subroutine capi_gf_bool

   !> [[gf_decr_x]] of the C interface: the context's decreasing test, or
   !> (none given) UDDC's with a step of one second.
   subroutine capi_gf_decr ( ctx, udfunc, et, xbool )
      class(pepper_context), intent(inout) :: ctx  !! the context
      procedure(gf_scalar) :: udfunc  !! the scalar function (the context's)
      real(wp), intent(in) :: et  !! the time
      logical, intent(out) :: xbool  !! the function is decreasing at ET
      xbool = .false.
      select type (ctx)
      type is (capi_context)
         if (c_associated(ctx%bool_fn)) then
            call capi_gf_bool(ctx, udfunc, et, xbool)
         else
            call ctx%uddc(capi_gf_scalar, et, 1.0_wp, xbool)
         end if
      end select
   end subroutine capi_gf_decr

   !> Search for times when a scalar function of time satisfies a
   !> condition (GFUDS with a window of any size). `decr`: the decreasing
   !> test (null: UDDC's, with a step of one second). The result window
   !> stays in the context ([[set_real_result]]): `*nres` intervals.
   integer(c_int) function pepper_gf_user_scalar ( ctx, scalar, decr, user, relate, refval, adjust, step, &
                                                   ncnfine, cnfine, res, nres ) bind(c, name='pepper_gf_user_scalar')
      type(c_ptr), value :: ctx  !! the context
      type(c_funptr), value :: scalar  !! the scalar function ([[pepper_scalar_fn]])
      type(c_funptr), value :: decr  !! the decreasing test ([[pepper_bool_fn]]), or null
      type(c_ptr), value :: user  !! the data of both
      character(kind=c_char), intent(in) :: relate(*)  !! the relation ('=', '<', '>', 'LOCMIN', ...)
      real(c_double), value :: refval  !! the reference value
      real(c_double), value :: adjust  !! the adjustment for absolute extrema
      real(c_double), value :: step  !! the step (seconds)
      integer(c_int), value :: ncnfine  !! the number of intervals of the confinement window
      real(c_double), intent(in) :: cnfine(2, *)  !! the confinement window
      type(c_ptr), intent(out) :: res  !! the result window (intervals)
      integer(c_int), intent(out) :: nres  !! its number of intervals
      type(capi_context), pointer :: c
      character(:), allocatable :: f_relate
      real(wp), allocatable :: result(:, :)
      type(c_funptr) :: saved_scalar, saved_bool
      type(c_ptr) :: saved_scalar_user, saved_bool_user
      integer(c_int) :: n
      c => context_of(ctx)
      call c_to_f(relate, f_relate)
      saved_scalar = c%scalar_fn; saved_scalar_user = c%scalar_user
      saved_bool = c%bool_fn; saved_bool_user = c%bool_user
      c%scalar_fn = scalar; c%scalar_user = user
      c%bool_fn = decr; c%bool_user = user
      call c%gf_user_scalar(capi_gf_scalar, capi_gf_decr, f_relate, refval, adjust, step, &
                            cnfine(:, 1:ncnfine), result)
      c%scalar_fn = saved_scalar; c%scalar_user = saved_scalar_user
      c%bool_fn = saved_bool; c%bool_user = saved_bool_user
      if (.not. allocated(result)) allocate (result(2, 0))
      call set_real_result(c, 1, reshape(result, [size(result)]), res, n)
      nres = n/2
      pepper_gf_user_scalar = status_of(c)
   end function pepper_gf_user_scalar

   !> Search for times when a boolean function of time is true (GFUDB with
   !> a window of any size). `scalar` is given to `boolean`'s callers, as
   !> GFUDB does; it may be null if `boolean` does not use it.
   integer(c_int) function pepper_gf_user_boolean ( ctx, scalar, boolean, user, step, ncnfine, cnfine, &
                                                    res, nres ) bind(c, name='pepper_gf_user_boolean')
      type(c_ptr), value :: ctx  !! the context
      type(c_funptr), value :: scalar  !! the scalar function ([[pepper_scalar_fn]]), or null
      type(c_funptr), value :: boolean  !! the boolean function ([[pepper_bool_fn]])
      type(c_ptr), value :: user  !! the data of both
      real(c_double), value :: step  !! the step (seconds)
      integer(c_int), value :: ncnfine  !! the number of intervals of the confinement window
      real(c_double), intent(in) :: cnfine(2, *)  !! the confinement window
      type(c_ptr), intent(out) :: res  !! the result window (intervals)
      integer(c_int), intent(out) :: nres  !! its number of intervals
      type(capi_context), pointer :: c
      real(wp), allocatable :: result(:, :)
      type(c_funptr) :: saved_scalar, saved_bool
      type(c_ptr) :: saved_scalar_user, saved_bool_user
      integer(c_int) :: n
      c => context_of(ctx)
      saved_scalar = c%scalar_fn; saved_scalar_user = c%scalar_user
      saved_bool = c%bool_fn; saved_bool_user = c%bool_user
      c%scalar_fn = scalar; c%scalar_user = user
      c%bool_fn = boolean; c%bool_user = user
      call c%gf_user_boolean(capi_gf_scalar, capi_gf_bool, step, cnfine(:, 1:ncnfine), result)
      c%scalar_fn = saved_scalar; c%scalar_user = saved_scalar_user
      c%bool_fn = saved_bool; c%bool_user = saved_bool_user
      if (.not. allocated(result)) allocate (result(2, 0))
      call set_real_result(c, 1, reshape(result, [size(result)]), res, n)
      nres = n/2
      pepper_gf_user_boolean = status_of(c)
   end function pepper_gf_user_boolean

   !> Whether a scalar function of time is decreasing at `x` (UDDC): its
   !> derivative from its values at `x - dx` and `x + dx`.
   integer(c_int) function pepper_uddc ( ctx, scalar, user, x, dx, isdecr ) bind(c, name='pepper_uddc')
      type(c_ptr), value :: ctx  !! the context
      type(c_funptr), value :: scalar  !! the scalar function ([[pepper_scalar_fn]])
      type(c_ptr), value :: user  !! its data
      real(c_double), value :: x  !! the time
      real(c_double), value :: dx  !! the step of the derivative
      integer(c_int), intent(out) :: isdecr  !! 1 if decreasing
      type(capi_context), pointer :: c
      type(c_funptr) :: saved_scalar
      type(c_ptr) :: saved_scalar_user
      logical :: decr
      c => context_of(ctx)
      saved_scalar = c%scalar_fn; saved_scalar_user = c%scalar_user
      c%scalar_fn = scalar; c%scalar_user = user
      call c%uddc(capi_gf_scalar, x, dx, decr)
      c%scalar_fn = saved_scalar; c%scalar_user = saved_scalar_user
      isdecr = merge(1_c_int, 0_c_int, decr)
      pepper_uddc = status_of(c)
   end function pepper_uddc

   !> The derivative of a scalar function of time at `x` (UDDF), from its
   !> values at `x - dx` and `x + dx`.
   integer(c_int) function pepper_uddf ( ctx, scalar, user, x, dx, deriv ) bind(c, name='pepper_uddf')
      type(c_ptr), value :: ctx  !! the context
      type(c_funptr), value :: scalar  !! the scalar function ([[pepper_scalar_fn]])
      type(c_ptr), value :: user  !! its data
      real(c_double), value :: x  !! the time
      real(c_double), value :: dx  !! the half-interval of the derivative
      real(c_double), intent(out) :: deriv  !! the derivative
      type(capi_context), pointer :: c
      type(c_funptr) :: saved_scalar
      type(c_ptr) :: saved_scalar_user
      real(wp) :: d
      c => context_of(ctx)
      saved_scalar = c%scalar_fn; saved_scalar_user = c%scalar_user
      c%scalar_fn = scalar; c%scalar_user = user
      call c%uddf(capi_gf_scalar, x, dx, d)
      c%scalar_fn = saved_scalar; c%scalar_user = saved_scalar_user
      deriv = d
      pepper_uddf = status_of(c)
   end function pepper_uddf

   !> Set the GF hooks of a context: the routines GFEVNT, GFFOVE and
   !> GFOCCE call to step, refine, report progress and test for an
   !> interrupt. A null pointer selects PEPPER's routine (GFSTEP, GFREFN,
   !> GFREPI, GFREPU, GFREPF, GFBAIL). They stay set until changed.
   subroutine pepper_gf_set_hooks ( ctx, step, refn, repi, repu, repf, bail, user ) &
         bind(c, name='pepper_gf_set_hooks')
      type(c_ptr), value :: ctx  !! the context
      type(c_funptr), value :: step  !! the step ([[pepper_step_fn]]), or null
      type(c_funptr), value :: refn  !! the refinement ([[pepper_refn_fn]]), or null
      type(c_funptr), value :: repi  !! the report's start ([[pepper_repi_fn]]), or null
      type(c_funptr), value :: repu  !! the report's update ([[pepper_repu_fn]]), or null
      type(c_funptr), value :: repf  !! the report's end ([[pepper_repf_fn]]), or null
      type(c_funptr), value :: bail  !! the interrupt test ([[pepper_bail_fn]]), or null
      type(c_ptr), value :: user  !! the data given to them
      type(capi_context), pointer :: c
      c => context_of(ctx)
      c%step_fn = step; c%refn_fn = refn
      c%repi_fn = repi; c%repu_fn = repu; c%repf_fn = repf
      c%bail_fn = bail
      c%hooks_user = user
   end subroutine pepper_gf_set_hooks

   !> [[gf_step]] of the C interface: the context's step hook, or GFSTEP.
   subroutine capi_gf_step ( ctx, time, step )
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: time  !! the time
      real(wp), intent(out) :: step  !! the step
      procedure(pepper_step_fn), pointer :: fn
      step = 0.0_wp
      select type (ctx)
      type is (capi_context)
         if (c_associated(ctx%step_fn)) then
            call c_f_procpointer(ctx%step_fn, fn)
            call fn(ctx%self, ctx%hooks_user, time, step)
            return
         end if
      end select
      call ctx%gfstep(time, step)
   end subroutine capi_gf_step

   !> [[gf_refine]] of the C interface: the context's refinement hook, or
   !> GFREFN.
   subroutine capi_gf_refn ( ctx, t1, t2, s1, s2, t )
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: t1  !! one of the times
      real(wp), intent(in) :: t2  !! the other time
      logical, intent(in) :: s1  !! the condition at T1
      logical, intent(in) :: s2  !! the condition at T2
      real(wp), intent(out) :: t  !! the time between them
      procedure(pepper_refn_fn), pointer :: fn
      t = 0.0_wp
      select type (ctx)
      type is (capi_context)
         if (c_associated(ctx%refn_fn)) then
            call c_f_procpointer(ctx%refn_fn, fn)
            call fn(ctx%self, ctx%hooks_user, t1, t2, merge(1_c_int, 0_c_int, s1), merge(1_c_int, 0_c_int, s2), t)
            return
         end if
      end select
      call ctx%gfrefn(t1, t2, s1, s2, t)
   end subroutine capi_gf_refn

   !> [[gf_rep_init]] of the C interface: the context's report start
   !> hook, or GFREPI.
   subroutine capi_gf_repi ( ctx, window, begmss, endmss )
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: window(lbcell:*)  !! the window searched
      character(*), intent(in) :: begmss  !! the text before the percentage
      character(*), intent(in) :: endmss  !! the text after it
      procedure(pepper_repi_fn), pointer :: fn
      integer(c_int) :: n
      select type (ctx)
      type is (capi_context)
         if (c_associated(ctx%repi_fn)) then
            call c_f_procpointer(ctx%repi_fn, fn)
            n = int(window(0), c_int)
            call fn(ctx%self, ctx%hooks_user, n, window(1:max(n, 1)), &
                    trim(begmss)//c_null_char, trim(endmss)//c_null_char)
            return
         end if
      end select
      call ctx%gfrepi(window, begmss, endmss)
   end subroutine capi_gf_repi

   !> [[gf_rep_update]] of the C interface: the context's report update
   !> hook, or GFREPU.
   subroutine capi_gf_repu ( ctx, ivbeg, ivend, time )
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: ivbeg  !! the start of the interval searched
      real(wp), intent(in) :: ivend  !! its end
      real(wp), intent(in) :: time  !! the time reached
      procedure(pepper_repu_fn), pointer :: fn
      select type (ctx)
      type is (capi_context)
         if (c_associated(ctx%repu_fn)) then
            call c_f_procpointer(ctx%repu_fn, fn)
            call fn(ctx%self, ctx%hooks_user, ivbeg, ivend, time)
            return
         end if
      end select
      call ctx%gfrepu(ivbeg, ivend, time)
   end subroutine capi_gf_repu

   !> [[gf_rep_final]] of the C interface: the context's report end hook,
   !> or GFREPF.
   subroutine capi_gf_repf ( ctx )
      class(pepper_context), intent(inout) :: ctx  !! the context
      procedure(pepper_repf_fn), pointer :: fn
      select type (ctx)
      type is (capi_context)
         if (c_associated(ctx%repf_fn)) then
            call c_f_procpointer(ctx%repf_fn, fn)
            call fn(ctx%self, ctx%hooks_user)
            return
         end if
      end select
      call ctx%gfrepf()
   end subroutine capi_gf_repf

   !> [[gf_bail]] of the C interface: the context's interrupt hook, or
   !> GFBAIL.
   logical function capi_gf_bail ( ctx )
      class(pepper_context), intent(inout) :: ctx  !! the context
      procedure(pepper_bail_fn), pointer :: fn
      select type (ctx)
      type is (capi_context)
         if (c_associated(ctx%bail_fn)) then
            call c_f_procpointer(ctx%bail_fn, fn)
            capi_gf_bail = fn(ctx%self, ctx%hooks_user) /= 0
            return
         end if
      end select
      capi_gf_bail = ctx%gfbail()
   end function capi_gf_bail

   !> The rotation of a dynamic frame to its base frame at `et` (the private
   !> ZZDYNROT, which SpiceyPy exports).
   integer(c_int) function pepper_zzdynrot ( ctx, infram, center, et, rotate, basfrm ) &
         bind(c, name='pepper_zzdynrot')
      type(c_ptr), value :: ctx  !! the context
      integer(c_int), value :: infram  !! the frame's ID code
      integer(c_int), value :: center  !! its center
      real(c_double), value :: et  !! the epoch
      real(c_double), intent(out) :: rotate(3, 3)  !! the rotation (column-major) to BASFRM
      integer(c_int), intent(out) :: basfrm  !! the base frame's ID code
      type(capi_context), pointer :: c
      real(wp) :: r(3, 3)
      integer(ip) :: b
      c => context_of(ctx)
      r = 0.0_wp
      b = 0
      call zzdynrot(c, int(infram, ip), int(center, ip), et, r, b)
      rotate = r
      basfrm = int(b, c_int)
      pepper_zzdynrot = status_of(c)
   end function pepper_zzdynrot

   !> Close a Fortran logical unit (CSPICE's FTNCLS): a file opened by
   !> TXTOPN or TXTOPR.
   subroutine pepper_ftncls ( unit ) bind(c, name='pepper_ftncls')
      integer(c_int), value :: unit  !! the logical unit
      integer :: ios
      close (unit=unit, iostat=ios)
   end subroutine pepper_ftncls

   ! ------------------------------------------------------------------
   ! Programmatic ephemerides and frames

   !> [[ephemeris_provider]]'s state of the C interface: calls the C
   !> function, with the context's handle if it is one of the C interface.
   subroutine capi_ephemeris_state ( self, ctx, body, et, state, found )
      class(capi_ephemeris), intent(in) :: self  !! the provider
      class(pepper_context), intent(inout) :: ctx  !! the context of the query
      integer(ip), intent(in) :: body  !! the body's ID code
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: state(6)  !! the state
      logical, intent(out) :: found  !! a state is available at ET
      procedure(pepper_state_fn), pointer :: fn
      type(c_ptr) :: handle
      handle = c_null_ptr
      select type (ctx)
      type is (capi_context)
         handle = ctx%self
      end select
      call c_f_procpointer(self%fn, fn)
      state = 0.0_wp
      found = fn(handle, self%user, body, et, state) /= 0
   end subroutine capi_ephemeris_state

   !> [[frame_provider]]'s transform of the C interface: calls the C
   !> function, with the context's handle if it is one of the C interface.
   subroutine capi_frame_transform ( self, ctx, frame, et, xform, relative, found )
      class(capi_frame), intent(in) :: self  !! the provider
      class(pepper_context), intent(inout) :: ctx  !! the context of the query
      integer(ip), intent(in) :: frame  !! the frame's ID code
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: xform(6, 6)  !! the transformation from this frame to RELATIVE
      integer(ip), intent(out) :: relative  !! the frame transformed to
      logical, intent(out) :: found  !! the transformation is available at ET
      procedure(pepper_xform_fn), pointer :: fn
      type(c_ptr) :: handle
      handle = c_null_ptr
      select type (ctx)
      type is (capi_context)
         handle = ctx%self
      end select
      call c_f_procpointer(self%fn, fn)
      xform = 0.0_wp
      relative = 0
      found = fn(handle, self%user, frame, et, xform, relative) /= 0
   end subroutine capi_frame_transform

   ! ----------------------------------------------------------------------
   ! Kernel loaders written in C, and the builder's providers and frames
   ! ----------------------------------------------------------------------

   !> Register a kernel loader written in C (`spice%add_loader`): FURNSH
   !> calls `load_fn` for the files it chooses the loader for (by name with
   !> [[pepper_spice_furnsh_loader]] or KERNEL_LOADERS, by one of the `n`
   !> extensions in `exts` (`width` characters each), or by `detect_fn`).
   !> `load_fn(ctx, user, builder, file)` fills `builder` with the
   !> `pepper_builder_*` functions, signals its errors in `ctx` (a context
   !> of the instance, for this call), and returns 0 (else, with no error
   !> signaled, SPICE(LOADERFAILED)); nothing of the file is loaded after an
   !> error. `detect_fn(user, file, head, nhead)` (NULL: none) returns
   !> nonzero to read a file by its first `nhead` bytes; `unload_fn(user,
   !> file)` (NULL: none) is called when the file's kernel is unloaded.
   !> `user` must stay valid while the loader is registered. `replace` 1:
   !> replace a loader with the name or the extensions.
   integer(c_int) function pepper_spice_add_loader ( spice, name, n, width, exts, load_fn, detect_fn, &
         unload_fn, user, replace ) bind(c, name='pepper_spice_add_loader')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: name(*)  !! the loader's name
      integer(c_int), value :: n  !! the number of extensions
      integer(c_int), value :: width  !! the width of each extension in the block
      character(kind=c_char), intent(in) :: exts(*)  !! the extensions
      type(c_funptr), value :: load_fn  !! the reader ([[pepper_load_fn]])
      type(c_funptr), value :: detect_fn  !! the detection ([[pepper_detect_fn]]), or NULL
      type(c_funptr), value :: unload_fn  !! the unload callback ([[pepper_unload_fn]]), or NULL
      type(c_ptr), value :: user  !! the caller's data
      integer(c_int), value :: replace  !! 1: replace
      type(pepper_instance), pointer :: s
      character(:), allocatable :: f_name, f_exts(:)
      s => spice_of(spice)
      call c_to_f(name, f_name)
      if (n > 0) then
         call c_to_f_array(exts, n, width, f_exts)
         call s%add_loader(f_name, capi_loader(load_fn=load_fn, detect_fn=detect_fn, unload_fn=unload_fn, &
                                               user=user), extensions=f_exts, replace=replace /= 0)
      else
         call s%add_loader(f_name, capi_loader(load_fn=load_fn, detect_fn=detect_fn, unload_fn=unload_fn, &
                                               user=user), replace=replace /= 0)
      end if
      pepper_spice_add_loader = spice_status(s)
   end function pepper_spice_add_loader

   !> Remove a kernel loader (`spice%remove_loader`).
   integer(c_int) function pepper_spice_remove_loader ( spice, name ) bind(c, name='pepper_spice_remove_loader')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: name(*)  !! the loader's name
      type(pepper_instance), pointer :: s
      character(:), allocatable :: f_name
      s => spice_of(spice)
      call c_to_f(name, f_name)
      call s%remove_loader(f_name)
      pepper_spice_remove_loader = spice_status(s)
   end function pepper_spice_remove_loader

   !> Load a kernel with a named kernel loader (FURNSH's `loader`: the
   !> registered loader with that name reads the file, overriding every
   !> other choice; "PEPPER": PEPPER's own loading, registered loaders not
   !> asked; "": FURNSH chooses); `in_memory` as in [[pepper_spice_furnsh]].
   integer(c_int) function pepper_spice_furnsh_loader ( spice, file, in_memory, loader ) &
         bind(c, name='pepper_spice_furnsh_loader')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: file(*)  !! the file
      integer(c_int), value :: in_memory  !! 1, 0 or -1
      character(kind=c_char), intent(in) :: loader(*)  !! the loader's name ("" : FURNSH chooses)
      type(pepper_instance), pointer :: s
      character(:), allocatable :: f_file, f_loader
      s => spice_of(spice)
      call c_to_f(file, f_file)
      call c_to_f(loader, f_loader)
      if (len(f_loader) == 0) f_loader = ' '
      if (in_memory < 0) then
         call s%furnsh(f_file, loader=f_loader)
      else
         call s%furnsh(f_file, in_memory=in_memory /= 0, loader=f_loader)
      end if
      pepper_spice_furnsh_loader = spice_status(s)
   end function pepper_spice_furnsh_loader

   !> [[kernel_loader]]'s load of the C interface: calls the C function with
   !> a context of the instance made for the call (its errors become the
   !> instance's) and a builder that becomes FURNSH's.
   subroutine capi_loader_load ( self, ctx, kb, file )
      class(capi_loader), intent(inout) :: self  !! the loader
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      type(kernel_builder), intent(inout) :: kb  !! the builder to fill
      character(*), intent(in) :: file  !! the file
      procedure(pepper_load_fn), pointer :: fn
      type(kernel_builder), target :: local
      type(capi_context), pointer :: tmp
      type(pepper_instance), pointer :: s
      character(1840) :: short, long
      integer(c_int) :: st
      s => ctx%instance()
      allocate (tmp)
      tmp%pepper_context = s%new_context()
      tmp%self = c_loc(tmp)
      call c_f_procpointer(self%load_fn, fn)
      st = fn(tmp%self, self%user, c_loc(local), trim(file)//c_null_char)
      if (tmp%failed()) then
         call tmp%getmsg('SHORT', short)
         call tmp%getmsg('LONG', long)
         call ctx%setmsg(trim(long))
         call ctx%sigerr(trim(short))
      else if (st /= 0) then
         call ctx%setmsg('The kernel loader (C) returned an error for the file ''#''.')
         call ctx%errch('#', file)
         call ctx%sigerr('SPICE(LOADERFAILED)')
      else
         kb = local
      end if
      deallocate (tmp)
   end subroutine capi_loader_load

   !> [[kernel_loader]]'s detect of the C interface.
   logical function capi_loader_detect ( self, file, head ) result ( accept )
      class(capi_loader), intent(in) :: self  !! the loader
      character(*), intent(in) :: file  !! the file
      character(*), intent(in) :: head  !! its first bytes
      procedure(pepper_detect_fn), pointer :: fn
      accept = .false.
      if (.not. c_associated(self%detect_fn)) return
      call c_f_procpointer(self%detect_fn, fn)
      accept = fn(self%user, trim(file)//c_null_char, head, int(len(head), c_int)) /= 0
   end function capi_loader_detect

   !> [[kernel_loader]]'s unload of the C interface.
   subroutine capi_loader_unload ( self, file )
      class(capi_loader), intent(inout) :: self  !! the loader
      character(*), intent(in) :: file  !! the file unloaded
      procedure(pepper_unload_fn), pointer :: fn
      if (.not. c_associated(self%unload_fn)) return
      call c_f_procpointer(self%unload_fn, fn)
      call fn(self%user, trim(file)//c_null_char)
   end subroutine capi_loader_unload

   !> Add a programmatic ephemeris to a builder (`kernel_builder%add_ephemeris`):
   !> `fn(ctx, user, body, et, state)` as for [[pepper_spice_add_ephemeris]];
   !> owned by the kernel once loaded. `ident` "" for the default. `user`
   !> must stay valid while the kernel is loaded.
   integer(c_int) function pepper_builder_add_ephemeris ( ctx, builder, body, center, frame, tbeg, tend, &
         fn, user, ident ) bind(c, name='pepper_builder_add_ephemeris')
      type(c_ptr), value :: ctx  !! a context (for the errors)
      type(c_ptr), value :: builder  !! the builder
      integer(c_int), value :: body  !! the body
      integer(c_int), value :: center  !! its center
      character(kind=c_char), intent(in) :: frame(*)  !! the frame of the states
      real(c_double), value :: tbeg  !! start of the coverage
      real(c_double), value :: tend  !! end of the coverage
      type(c_funptr), value :: fn  !! the state function ([[pepper_state_fn]])
      type(c_ptr), value :: user  !! its data
      character(kind=c_char), intent(in) :: ident(*)  !! the segment identifier ("" : the default)
      type(capi_context), pointer :: c
      type(kernel_builder), pointer :: kb
      character(:), allocatable :: f_frame, f_ident
      c => context_of(ctx)
      call c_f_pointer(builder, kb)
      call c_to_f(frame, f_frame)
      call c_to_f(ident, f_ident)
      if (len(f_ident) == 0) then
         call kb%add_ephemeris(c, int(body, ip), int(center, ip), f_frame, tbeg, tend, capi_ephemeris(fn=fn, user=user))
      else
         call kb%add_ephemeris(c, int(body, ip), int(center, ip), f_frame, tbeg, tend, capi_ephemeris(fn=fn, user=user), &
                               ident=f_ident)
      end if
      pepper_builder_add_ephemeris = status_of(c)
   end function pepper_builder_add_ephemeris

   !> Add a programmatic frame to a builder (`kernel_builder%add_frame`):
   !> `fn(ctx, user, frame, et, xform, relative)` as for
   !> [[pepper_spice_add_frame]]; owned by the kernel once loaded. `user`
   !> must stay valid while the kernel is loaded.
   integer(c_int) function pepper_builder_add_frame ( ctx, builder, name, code, center, fn, user ) &
         bind(c, name='pepper_builder_add_frame')
      type(c_ptr), value :: ctx  !! a context (for the errors)
      type(c_ptr), value :: builder  !! the builder
      character(kind=c_char), intent(in) :: name(*)  !! the frame's name
      integer(c_int), value :: code  !! its ID code
      integer(c_int), value :: center  !! its center
      type(c_funptr), value :: fn  !! the transformation function ([[pepper_xform_fn]])
      type(c_ptr), value :: user  !! its data
      type(capi_context), pointer :: c
      type(kernel_builder), pointer :: kb
      character(:), allocatable :: f_name
      c => context_of(ctx)
      call c_f_pointer(builder, kb)
      call c_to_f(name, f_name)
      call kb%add_frame(c, f_name, int(code, ip), int(center, ip), capi_frame(fn=fn, user=user))
      pepper_builder_add_frame = status_of(c)
   end function pepper_builder_add_frame

   !> Add a table of rotations to a builder (`kernel_builder%frame_rotations`):
   !> a frame NAME (CODE, CENTER) whose rotations from BASE are, at the `n`
   !> `epochs`, the quaternions `quats` (`double[n][4]`, SPICE's, scalar
   !> first; `rots` NULL) or the matrices `rots` (`n` column-major 3x3;
   !> `quats` NULL). `av`: NULL or `double[n][3]`, the angular velocities
   !> (rad/s, in BASE). `method`: "" (Lagrange), "LAGRANGE" or "HERMITE".
   !> `inverse` 1: the rotations are from the frame to BASE. `bounds` 1: the
   !> coverage is [tbeg, tend]. `ident` "" for the default.
   integer(c_int) function pepper_builder_frame_rotations ( ctx, builder, name, code, center, base, n, epochs, &
         quats, rots, degree, av, method, inverse, bounds, tbeg, tend, ident ) &
         bind(c, name='pepper_builder_frame_rotations')
      type(c_ptr), value :: ctx  !! a context (for the errors)
      type(c_ptr), value :: builder  !! the builder
      character(kind=c_char), intent(in) :: name(*)  !! the frame's name
      integer(c_int), value :: code  !! its ID code
      integer(c_int), value :: center  !! its center
      character(kind=c_char), intent(in) :: base(*)  !! the frame the rotations are relative to
      integer(c_int), value :: n  !! the number of epochs
      real(c_double), intent(in) :: epochs(*)  !! the epochs (ET)
      type(c_ptr), value :: quats  !! the quaternions (n rows of 4), or NULL
      type(c_ptr), value :: rots  !! the matrices (n column-major 3x3), or NULL
      integer(c_int), value :: degree  !! the degree (odd)
      type(c_ptr), value :: av  !! the angular velocities (n rows of 3), or NULL
      character(kind=c_char), intent(in) :: method(*)  !! "", "LAGRANGE" or "HERMITE"
      integer(c_int), value :: inverse  !! 1: from the frame to BASE
      integer(c_int), value :: bounds  !! 1: the coverage is [tbeg, tend]
      real(c_double), value :: tbeg  !! start of the coverage (with bounds)
      real(c_double), value :: tend  !! end of the coverage (with bounds)
      character(kind=c_char), intent(in) :: ident(*)  !! the segment identifier ("" : the default)
      type(capi_context), pointer :: c
      type(kernel_builder), pointer :: kb
      character(:), allocatable :: f_name, f_base, f_method, f_ident
      real(c_double), pointer :: q(:, :), r(:, :, :), w(:, :)
      real(wp), allocatable :: qq(:, :)
      integer(ip) :: m, k
      c => context_of(ctx)
      call c_f_pointer(builder, kb)
      call c_to_f(name, f_name)
      call c_to_f(base, f_base)
      call c_to_f(method, f_method)
      if (len(f_method) == 0) f_method = 'LAGRANGE'
      call c_to_f(ident, f_ident)
      if (len(f_ident) == 0) f_ident = 'ROTATIONS'
      m = max(0_ip, int(n, ip))
      if (c_associated(quats)) then
         call c_f_pointer(quats, q, [4_ip, m])
         qq = q
      else if (c_associated(rots)) then
         call c_f_pointer(rots, r, [3_ip, 3_ip, m])
         allocate (qq(4, m))
         do k = 1, m
            call c%m2q(r(:, :, k), qq(:, k))
            if (c%failed()) then
               pepper_builder_frame_rotations = status_of(c)
               return
            end if
         end do
      else
         call c%setmsg('The rotations of the frame ''#'' are missing (quats and rots are NULL).')
         call c%errch('#', f_name)
         call c%sigerr('SPICE(NULLPOINTER)')
         pepper_builder_frame_rotations = status_of(c)
         return
      end if
      if (c_associated(av)) then
         call c_f_pointer(av, w, [3_ip, m])
         if (bounds /= 0) then
            call kb%frame_rotations(c, f_name, int(code, ip), int(center, ip), f_base, epochs(1:m), qq, &
                                    int(degree, ip), av=w, method=f_method, inverse=inverse /= 0, &
                                    tbeg=tbeg, tend=tend, ident=f_ident)
         else
            call kb%frame_rotations(c, f_name, int(code, ip), int(center, ip), f_base, epochs(1:m), qq, &
                                    int(degree, ip), av=w, method=f_method, inverse=inverse /= 0, ident=f_ident)
         end if
      else
         if (bounds /= 0) then
            call kb%frame_rotations(c, f_name, int(code, ip), int(center, ip), f_base, epochs(1:m), qq, &
                                    int(degree, ip), method=f_method, inverse=inverse /= 0, &
                                    tbeg=tbeg, tend=tend, ident=f_ident)
         else
            call kb%frame_rotations(c, f_name, int(code, ip), int(center, ip), f_base, epochs(1:m), qq, &
                                    int(degree, ip), method=f_method, inverse=inverse /= 0, ident=f_ident)
         end if
      end if
      pepper_builder_frame_rotations = status_of(c)
   end function pepper_builder_frame_rotations

   ! ----------------------------------------------------------------------
   ! Kernels built in memory (kernel_builder, spice%load_kernel)
   ! ----------------------------------------------------------------------

   !> A new, empty kernel builder. It belongs to no instance: fill it from
   !> any thread, load it with [[pepper_spice_load_kernel]], free it with
   !> [[pepper_builder_free]].
   function pepper_builder_new () result(handle) bind(c, name='pepper_builder_new')
      type(c_ptr) :: handle  !! the builder
      type(kernel_builder), pointer :: kb
      allocate (kb)
      handle = c_loc(kb)
   end function pepper_builder_new

   !> Free a kernel builder.
   subroutine pepper_builder_free ( handle ) bind(c, name='pepper_builder_free')
      type(c_ptr), value :: handle  !! the builder
      type(kernel_builder), pointer :: kb
      if (.not. c_associated(handle)) return
      call c_f_pointer(handle, kb)
      deallocate (kb)
   end subroutine pepper_builder_free

   !> Add an SPK segment to a builder: `type` 9 (Lagrange) or 13 (Hermite,
   !> odd degree). `states` is `double[n][6]` (km, km/s), `epochs`
   !> `double[n]` (TDB seconds past J2000, increasing). With `bounds` 1,
   !> the coverage is [tbeg, tend] (inside the epochs), else the epochs'
   !> span. `ident` is the segment identifier ("" for none). `adapt` 1: a
   !> segment too short for its degree has its degree lowered (one state
   !> is dropped). Errors are signaled in `ctx`, any context (the builder
   !> belongs to no instance).
   integer(c_int) function pepper_builder_spk ( ctx, builder, type, body, center, frame, n, epochs, states, &
         degree, bounds, tbeg, tend, ident, adapt ) bind(c, name='pepper_builder_spk')
      type(c_ptr), value :: ctx  !! a context (for the errors)
      type(c_ptr), value :: builder  !! the builder
      integer(c_int), value :: type  !! 9 or 13
      integer(c_int), value :: body  !! the body
      integer(c_int), value :: center  !! its center
      character(kind=c_char), intent(in) :: frame(*)  !! the frame (resolved at the load)
      integer(c_int), value :: n  !! the number of states
      real(c_double), intent(in) :: epochs(*)  !! the epochs (n)
      real(c_double), intent(in) :: states(6, *)  !! the states (n rows of 6)
      integer(c_int), value :: degree  !! the degree of the polynomials
      integer(c_int), value :: bounds  !! 1: the coverage is [tbeg, tend]
      real(c_double), value :: tbeg  !! start of the coverage (with bounds)
      real(c_double), value :: tend  !! end of the coverage (with bounds)
      character(kind=c_char), intent(in) :: ident(*)  !! the segment identifier
      integer(c_int), value :: adapt  !! 1: adapt a short segment
      type(capi_context), pointer :: c
      type(kernel_builder), pointer :: kb
      character(:), allocatable :: f_frame, f_ident
      integer(ip) :: m
      c => context_of(ctx)
      call c_f_pointer(builder, kb)
      call c_to_f(frame, f_frame)
      call c_to_f(ident, f_ident)
      m = max(0_ip, int(n, ip))
      select case (type)
      case (9)
         if (bounds /= 0) then
            call kb%spk_lagrange(c, int(body, ip), int(center, ip), f_frame, epochs(1:m), states(:, 1:m), &
                                 int(degree, ip), tbeg=tbeg, tend=tend, ident=f_ident, adapt=adapt /= 0)
         else
            call kb%spk_lagrange(c, int(body, ip), int(center, ip), f_frame, epochs(1:m), states(:, 1:m), &
                                 int(degree, ip), ident=f_ident, adapt=adapt /= 0)
         end if
      case (13)
         if (bounds /= 0) then
            call kb%spk_hermite(c, int(body, ip), int(center, ip), f_frame, epochs(1:m), states(:, 1:m), &
                                int(degree, ip), tbeg=tbeg, tend=tend, ident=f_ident, adapt=adapt /= 0)
         else
            call kb%spk_hermite(c, int(body, ip), int(center, ip), f_frame, epochs(1:m), states(:, 1:m), &
                                int(degree, ip), ident=f_ident, adapt=adapt /= 0)
         end if
      case default
         call c%setmsg('A kernel builder''s segments are of SPK type 9 (Lagrange) or 13 (Hermite), not #.')
         call c%errint('#', int(type, ip))
         call c%sigerr('SPICE(UNSUPPORTEDINTERP)')
      end select
      pepper_builder_spk = status_of(c)
   end function pepper_builder_spk

   !> Define a body name of the kernel a builder makes: `name` translates to
   !> `code` while the kernel is loaded, and the definition is removed when
   !> it is unloaded (`kernel_builder%define_body`). Errors are signaled in
   !> `ctx`, any context.
   integer(c_int) function pepper_builder_define_body ( ctx, builder, name, code ) &
         bind(c, name='pepper_builder_define_body')
      type(c_ptr), value :: ctx  !! a context (for the errors)
      type(c_ptr), value :: builder  !! the builder
      character(kind=c_char), intent(in) :: name(*)  !! the body's name
      integer(c_int), value :: code  !! its NAIF ID code
      type(capi_context), pointer :: c
      type(kernel_builder), pointer :: kb
      character(:), allocatable :: f_name
      c => context_of(ctx)
      call c_f_pointer(builder, kb)
      call c_to_f(name, f_name)
      call kb%define_body(c, f_name, int(code, ip))
      pepper_builder_define_body = status_of(c)
   end function pepper_builder_define_body

   !> Set the description of the kernel a builder makes (`kernel_format`).
   subroutine pepper_builder_set_format ( builder, text ) bind(c, name='pepper_builder_set_format')
      type(c_ptr), value :: builder  !! the builder
      character(kind=c_char), intent(in) :: text(*)  !! the description
      type(kernel_builder), pointer :: kb
      character(:), allocatable :: f_text
      call c_f_pointer(builder, kb)
      call c_to_f(text, f_text)
      call kb%set_format(f_text)
   end subroutine pepper_builder_set_format

   !> Empty a builder.
   subroutine pepper_builder_clear ( builder ) bind(c, name='pepper_builder_clear')
      type(c_ptr), value :: builder  !! the builder
      type(kernel_builder), pointer :: kb
      call c_f_pointer(builder, kb)
      call kb%clear()
   end subroutine pepper_builder_clear

   !> The number of segments in a builder.
   integer(c_int) function pepper_builder_nsegments ( builder ) bind(c, name='pepper_builder_nsegments')
      type(c_ptr), value :: builder  !! the builder
      type(kernel_builder), pointer :: kb
      call c_f_pointer(builder, kb)
      pepper_builder_nsegments = int(kb%nsegments(), c_int)
   end function pepper_builder_nsegments

   !> Write a kernel built in memory to SPICE files (`spice%write_kernel`):
   !> an SPK (`file`; "" for none), and for its rotation tables a CK
   !> (`ck_file`) with a text kernel (`text_file`) of their clocks, frame
   !> definitions and the kernel's body names. Errors are signaled in the
   !> instance.
   integer(c_int) function pepper_spice_write_kernel ( spice, name, file, ck_file, text_file ) &
         bind(c, name='pepper_spice_write_kernel')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: name(*)  !! the kernel's name
      character(kind=c_char), intent(in) :: file(*)  !! the SPK file ("" for none)
      type(c_ptr), value :: ck_file  !! the CK file of the rotation tables (NULL or "" for none)
      type(c_ptr), value :: text_file  !! the text kernel: clocks, frame definitions, body names (NULL or "" for none)
      type(pepper_instance), pointer :: s
      character(:), allocatable :: f_name, f_file, f_ck, f_text
      s => spice_of(spice)
      call c_to_f(name, f_name)
      call c_to_f(file, f_file)
      call optional_string(ck_file, f_ck)
      call optional_string(text_file, f_text)
      if (f_ck == ' ' .and. f_text == ' ') then
         call s%write_kernel(f_name, f_file)
      else
         call s%write_kernel(f_name, f_file, ck_file=f_ck, text_file=f_text)
      end if
      pepper_spice_write_kernel = spice_status(s)
   contains
      !> The Fortran string of an optional C string (blank if NULL).
      subroutine optional_string ( ptr, fstr )
         type(c_ptr), intent(in) :: ptr  !! the C string, or NULL
         character(:), allocatable, intent(out) :: fstr  !! the string
         character(kind=c_char), pointer :: chars(:)
         if (.not. c_associated(ptr)) then
            fstr = ' '
            return
         end if
         call c_f_pointer(ptr, chars, [huge(1)])
         call c_to_f(chars, fstr)
         if (len(fstr) == 0) fstr = ' '
      end subroutine optional_string
   end function pepper_spice_write_kernel

   !> Load a builder as a kernel named `name`, with no file
   !> (`spice%load_kernel`): used like an SPK file (queries, SPKCOV, KDATA,
   !> UNLOAD). A kernel loaded under the same name is replaced (kept if the
   !> load fails). The builder is emptied, unless `keep` is 1. Errors are
   !> signaled in the instance.
   integer(c_int) function pepper_spice_load_kernel ( spice, name, builder, keep ) &
         bind(c, name='pepper_spice_load_kernel')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: name(*)  !! the kernel's name
      type(c_ptr), value :: builder  !! the builder
      integer(c_int), value :: keep  !! 1: keep the builder's segments
      type(pepper_instance), pointer :: s
      type(kernel_builder), pointer :: kb
      character(:), allocatable :: f_name
      s => spice_of(spice)
      call c_f_pointer(builder, kb)
      call c_to_f(name, f_name)
      call s%load_kernel(f_name, kb, keep=keep /= 0)
      pepper_spice_load_kernel = spice_status(s)
   end function pepper_spice_load_kernel

   !> Add a programmatic body whose states come from a C function (see
   !> `spice%add_ephemeris`). `user` must stay valid until the body is
   !> removed (remove_ephemeris, kclear) or replaced.
   integer(c_int) function pepper_spice_add_ephemeris ( spice, body, center, frame, tbeg, tend, fn, user ) &
         bind(c, name='pepper_spice_add_ephemeris')
      type(c_ptr), value :: spice  !! the instance
      integer(c_int), value :: body  !! the body's ID code
      integer(c_int), value :: center  !! the ID code of its center
      character(kind=c_char), intent(in) :: frame(*)  !! the frame of the states
      real(c_double), value :: tbeg  !! start of the coverage (TDB seconds past J2000)
      real(c_double), value :: tend  !! end of the coverage
      type(c_funptr), value :: fn  !! the state function ([[pepper_state_fn]])
      type(c_ptr), value :: user  !! its data
      type(pepper_instance), pointer :: s
      character(:), allocatable :: f_frame
      s => spice_of(spice)
      call c_to_f(frame, f_frame)
      call s%add_ephemeris(body, center, f_frame, tbeg, tend, capi_ephemeris(fn=fn, user=user))
      pepper_spice_add_ephemeris = spice_status(s)
   end function pepper_spice_add_ephemeris

   !> Add a programmatic frame whose transformations come from a C function
   !> (see `spice%add_frame`). `user` must stay valid until the frame is
   !> removed (remove_frame, kclear) or replaced.
   integer(c_int) function pepper_spice_add_frame ( spice, name, code, center, fn, user ) &
         bind(c, name='pepper_spice_add_frame')
      type(c_ptr), value :: spice  !! the instance
      character(kind=c_char), intent(in) :: name(*)  !! the frame's name
      integer(c_int), value :: code  !! its ID code
      integer(c_int), value :: center  !! the ID code of its center
      type(c_funptr), value :: fn  !! the transformation function ([[pepper_xform_fn]])
      type(c_ptr), value :: user  !! its data
      type(pepper_instance), pointer :: s
      character(:), allocatable :: f_name
      s => spice_of(spice)
      call c_to_f(name, f_name)
      call s%add_frame(f_name, code, center, capi_frame(fn=fn, user=user))
      pepper_spice_add_frame = spice_status(s)
   end function pepper_spice_add_frame

   ! ------------------------------------------------------------------
   ! Prepared queries

   !> A prepared state query (see `ctx%prepare_state`): freed with
   !> [[pepper_state_query_free]]. Used by one context (thread) at a time.
   integer(c_int) function pepper_prepare_state ( ctx, target, observer, frame, abcorr, query ) &
         bind(c, name='pepper_prepare_state')
      type(c_ptr), value :: ctx  !! the context
      character(kind=c_char), intent(in) :: target(*)  !! the target's name or ID code
      character(kind=c_char), intent(in) :: observer(*)  !! the observer's name or ID code
      character(kind=c_char), intent(in) :: frame(*)  !! the frame of the states
      character(kind=c_char), intent(in) :: abcorr(*)  !! the aberration correction
      type(c_ptr), intent(out) :: query  !! the query (null after an error)
      type(capi_context), pointer :: c
      type(state_query), pointer :: q
      character(:), allocatable :: f_target, f_observer, f_frame, f_abcorr
      c => context_of(ctx)
      call c_to_f(target, f_target)
      call c_to_f(observer, f_observer)
      call c_to_f(frame, f_frame)
      call c_to_f(abcorr, f_abcorr)
      allocate (q)
      call c%prepare_state(q, f_target, f_observer, f_frame, f_abcorr)
      if (c%failed()) then
         deallocate (q)
         query = c_null_ptr
      else
         query = c_loc(q)
      end if
      pepper_prepare_state = status_of(c)
   end function pepper_prepare_state

   !> Free a prepared state query.
   subroutine pepper_state_query_free ( query ) bind(c, name='pepper_state_query_free')
      type(c_ptr), value :: query  !! the query
      type(state_query), pointer :: q
      if (.not. c_associated(query)) return
      call c_f_pointer(query, q)
      deallocate (q)
   end subroutine pepper_state_query_free

   !> Evaluate a prepared state query at `n` epochs: `states` `[n][6]`,
   !> `lts` `[n]` (or null).
   integer(c_int) function pepper_query_state ( ctx, query, n, ets, states, lts ) bind(c, name='pepper_query_state')
      type(c_ptr), value :: ctx  !! the context
      type(c_ptr), value :: query  !! the query
      integer(c_int), value :: n  !! the number of epochs
      real(c_double), intent(in) :: ets(n)  !! the epochs
      real(c_double), intent(out) :: states(6, n)  !! the states
      type(c_ptr), value :: lts  !! the light times (`n`), or null
      type(capi_context), pointer :: c
      type(state_query), pointer :: q
      real(c_double), pointer :: f_lts(:)
      c => context_of(ctx)
      call c_f_pointer(query, q)
      if (n == 1) then
         if (c_associated(lts)) then
            call c_f_pointer(lts, f_lts, [n])
            call c%query_state(q, ets(1), states(:, 1), f_lts(1))
         else
            block
               real(wp) :: lt
               call c%query_state(q, ets(1), states(:, 1), lt)
            end block
         end if
      else if (c_associated(lts)) then
         call c_f_pointer(lts, f_lts, [n])
         call c%query_state(q, ets, states, f_lts)
      else
         call c%query_state(q, ets, states)
      end if
      pepper_query_state = status_of(c)
   end function pepper_query_state

   !> A prepared frame transformation query (see `ctx%prepare_rotation`):
   !> freed with [[pepper_rotation_query_free]].
   integer(c_int) function pepper_prepare_rotation ( ctx, from, to, query ) bind(c, name='pepper_prepare_rotation')
      type(c_ptr), value :: ctx  !! the context
      character(kind=c_char), intent(in) :: from(*)  !! the frame transformed from
      character(kind=c_char), intent(in) :: to(*)  !! the frame transformed to
      type(c_ptr), intent(out) :: query  !! the query (null after an error)
      type(capi_context), pointer :: c
      type(rotation_query), pointer :: q
      character(:), allocatable :: f_from, f_to
      c => context_of(ctx)
      call c_to_f(from, f_from)
      call c_to_f(to, f_to)
      allocate (q)
      call c%prepare_rotation(q, f_from, f_to)
      if (c%failed()) then
         deallocate (q)
         query = c_null_ptr
      else
         query = c_loc(q)
      end if
      pepper_prepare_rotation = status_of(c)
   end function pepper_prepare_rotation

   !> Free a prepared frame transformation query.
   subroutine pepper_rotation_query_free ( query ) bind(c, name='pepper_rotation_query_free')
      type(c_ptr), value :: query  !! the query
      type(rotation_query), pointer :: q
      if (.not. c_associated(query)) return
      call c_f_pointer(query, q)
      deallocate (q)
   end subroutine pepper_rotation_query_free

   !> Evaluate a prepared rotation query (PXFORM) at `n` epochs: `n`
   !> column-major 3x3 matrices.
   integer(c_int) function pepper_query_rotation ( ctx, query, n, ets, rotations ) bind(c, name='pepper_query_rotation')
      type(c_ptr), value :: ctx  !! the context
      type(c_ptr), value :: query  !! the query
      integer(c_int), value :: n  !! the number of epochs
      real(c_double), intent(in) :: ets(n)  !! the epochs
      real(c_double), intent(out) :: rotations(3, 3, n)  !! the rotations
      type(capi_context), pointer :: c
      type(rotation_query), pointer :: q
      c => context_of(ctx)
      call c_f_pointer(query, q)
      if (n == 1) then
         call c%query_rotation(q, ets(1), rotations(:, :, 1))
      else
         call c%query_rotation(q, ets, rotations)
      end if
      pepper_query_rotation = status_of(c)
   end function pepper_query_rotation

   !> Evaluate a prepared rotation query as a state transformation (SXFORM)
   !> at `n` epochs: `n` column-major 6x6 matrices.
   integer(c_int) function pepper_query_transform ( ctx, query, n, ets, xforms ) bind(c, name='pepper_query_transform')
      type(c_ptr), value :: ctx  !! the context
      type(c_ptr), value :: query  !! the query
      integer(c_int), value :: n  !! the number of epochs
      real(c_double), intent(in) :: ets(n)  !! the epochs
      real(c_double), intent(out) :: xforms(6, 6, n)  !! the transformations
      type(capi_context), pointer :: c
      type(rotation_query), pointer :: q
      c => context_of(ctx)
      call c_f_pointer(query, q)
      if (n == 1) then
         call c%query_transform(q, ets(1), xforms(:, :, 1))
      else
         call c%query_transform(q, ets, xforms)
      end if
      pepper_query_transform = status_of(c)
   end function pepper_query_transform

end module pepper_capi_core
