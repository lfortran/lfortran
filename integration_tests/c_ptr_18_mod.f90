module c_ptr_18_mod
    use iso_c_binding, only: c_ptr, c_funptr, c_null_ptr, c_null_funptr, c_associated
    implicit none

    type :: c_ptr_18_pair
        type(c_ptr) :: p
        type(c_funptr) :: fp
    end type

    type(c_ptr), parameter :: imported_null_ptr = c_null_ptr
    type(c_funptr), parameter :: imported_null_funptr = c_null_funptr
    type(c_ptr_18_pair), parameter :: imported_pair = &
        c_ptr_18_pair(c_null_ptr, c_null_funptr)
contains
    subroutine check_imported_nulls()
        if (c_associated(imported_null_ptr)) error stop "imported c_null_ptr associated"
        if (c_associated(imported_null_funptr)) error stop "imported c_null_funptr associated"
        if (c_associated(imported_pair%p)) error stop "imported pair c_null_ptr associated"
        if (c_associated(imported_pair%fp)) error stop "imported pair c_null_funptr associated"
    end subroutine
end module
