module c_ptr_18_mod
    use iso_c_binding, only: c_int, c_ptr, c_funptr, c_null_ptr, &
        c_null_funptr, c_loc, c_funloc, c_associated
    implicit none

    type :: c_ptr_18_pair
        type(c_ptr) :: p
        type(c_funptr) :: fp
    end type

    type(c_ptr), parameter :: imported_null_ptr = c_null_ptr
    type(c_funptr), parameter :: imported_null_funptr = c_null_funptr
    type(c_ptr_18_pair), parameter :: imported_pair = &
        c_ptr_18_pair(c_null_ptr, c_null_funptr)
    integer(c_int), target, save :: imported_target = 7
contains
    subroutine imported_proc() bind(c)
    end subroutine

    function imported_c_loc() result(r)
        type(c_ptr) :: r
        r = c_loc(imported_target)
    end function

    function imported_c_funloc() result(r)
        type(c_funptr) :: r
        r = c_funloc(imported_proc)
    end function

    function imported_c_ptr_result() result(r)
        type(c_ptr) :: r
        r = c_null_ptr
    end function

    function imported_c_funptr_result() result(r)
        type(c_funptr) :: r
        r = c_null_funptr
    end function

    subroutine check_imported_nulls()
        if (c_associated(imported_null_ptr)) error stop "imported c_null_ptr associated"
        if (c_associated(imported_null_funptr)) error stop "imported c_null_funptr associated"
        if (c_associated(imported_pair%p)) error stop "imported pair c_null_ptr associated"
        if (c_associated(imported_pair%fp)) error stop "imported pair c_null_funptr associated"
    end subroutine
end module
