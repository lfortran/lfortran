program c_ptr_18
use iso_c_binding, only: c_int, c_ptr, c_funptr, c_null_ptr, c_null_funptr, &
    np => c_null_ptr, c_loc, c_funloc, c_associated
use c_ptr_18_mod, only: imported_null_ptr, imported_null_funptr, &
    imported_pair, imported_proc, imported_c_loc, imported_c_funloc, &
    imported_c_ptr_result, imported_c_funptr_result, check_imported_nulls
implicit none

type :: plain_cptr_t
    integer :: h
    type(c_ptr) :: p
end type

type :: default_cptr_t
    type(c_ptr) :: p = np
end type

type :: alloc_cptr_t
    type(c_ptr), allocatable :: p
    integer :: h
end type

type :: param_cptr_t
    integer :: h = 0
    type(c_ptr) :: p = c_null_ptr
end type

type(c_ptr) :: p, q
type(c_funptr) :: fp
type(c_ptr), parameter :: p_param = c_null_ptr
type(c_funptr), parameter :: fp_param = c_null_funptr
type(param_cptr_t), parameter :: s_param = param_cptr_t(1)
type(plain_cptr_t) :: plain
type(default_cptr_t) :: defaulted
type(alloc_cptr_t) :: allocated_from_null, allocated_from_var
integer(c_int), target :: x = 3

call check_imported_nulls()

plain = plain_cptr_t(9, np)
if (plain%h /= 9) error stop "renamed constructor integer"
if (c_associated(plain%p)) error stop "renamed constructor c_ptr"

if (c_associated(defaulted%p)) error stop "renamed default"

p = np
if (c_associated(p)) error stop "renamed assignment"

allocated_from_null = alloc_cptr_t(c_null_ptr, 1)
if (.not. allocated(allocated_from_null%p)) error stop "c_null_ptr did not allocate"
if (c_associated(allocated_from_null%p)) error stop "allocated c_null_ptr associated"
if (allocated_from_null%h /= 1) error stop "allocated c_null_ptr integer"

q = c_null_ptr
allocated_from_var = alloc_cptr_t(q, 2)
if (.not. allocated(allocated_from_var%p)) error stop "c_ptr variable did not allocate"
if (c_associated(allocated_from_var%p)) error stop "allocated c_ptr variable associated"
if (allocated_from_var%h /= 2) error stop "allocated c_ptr variable integer"

if (c_associated(p_param)) error stop "c_ptr parameter associated"
if (c_associated(fp_param)) error stop "c_funptr parameter associated"
if (c_associated(s_param%p)) error stop "c_ptr component parameter associated"
if (c_associated(imported_null_ptr)) error stop "imported c_ptr parameter associated"
if (c_associated(imported_null_funptr)) error stop "imported c_funptr parameter associated"
if (c_associated(imported_pair%p)) error stop "imported c_ptr pair parameter associated"
if (c_associated(imported_pair%fp)) error stop "imported c_funptr pair parameter associated"

fp = c_null_funptr
if (c_associated(fp)) error stop "c_funptr assignment associated"

p = c_loc(x)
if (.not. c_associated(p)) error stop "c_loc result not associated"

fp = c_funloc(imported_proc)
if (.not. c_associated(fp)) error stop "c_funloc result not associated"

p = imported_c_loc()
if (.not. c_associated(p)) error stop "function c_ptr result from c_loc"

fp = imported_c_funloc()
if (.not. c_associated(fp)) error stop "function c_funptr result from c_funloc"

p = imported_c_ptr_result()
if (c_associated(p)) error stop "function c_ptr null result associated"

fp = imported_c_funptr_result()
if (c_associated(fp)) error stop "function c_funptr null result associated"

deallocate(allocated_from_null%p)
deallocate(allocated_from_var%p)
end program
