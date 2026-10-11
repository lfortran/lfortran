module c_associated_03_mod
    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_bool
    implicit none
contains
    logical(c_bool) function check(a) bind(c)
        type(c_ptr), value :: a
        check = c_associated(a, c_null_ptr)
    end function check

    subroutine f() bind(c)
    end subroutine f
end module c_associated_03_mod

program c_associated_03
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_null_ptr, &
        c_null_funptr, c_associated, c_loc, c_funloc
    use c_associated_03_mod, only: check, f
    implicit none
    integer, target :: i = 1
    type(c_ptr) :: p, q
    type(c_funptr) :: fp

    p = c_loc(i)
    q = c_null_ptr
    if (c_associated(p, c_null_ptr)) error stop 1
    if (c_associated(q, c_null_ptr)) error stop 2
    if (c_associated(c_null_ptr, c_null_ptr)) error stop 3
    if (c_associated(c_loc(i), c_null_ptr)) error stop 4
    if (check(p)) error stop 5
    if (check(q)) error stop 6

    fp = c_funloc(f)
    if (c_associated(fp, c_null_funptr)) error stop 7
    fp = c_null_funptr
    if (c_associated(fp, c_null_funptr)) error stop 8
    print *, "ok"
end program c_associated_03
