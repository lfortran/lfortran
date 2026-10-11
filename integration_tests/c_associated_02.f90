program c_associated_02
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_null_ptr, &
        c_null_funptr, c_associated, c_loc, c_bool
    implicit none
    type :: holder
        type(c_ptr) :: p
    end type holder
    integer, target :: i = 1, j = 2
    type(c_ptr) :: p, q, r, arr(2)
    type(c_funptr) :: fp, fq
    type(holder) :: h

    q = c_null_ptr
    r = c_null_ptr
    if (c_associated(q, r)) error stop 1

    p = c_loc(i)
    if (c_associated(q, p)) error stop 2
    if (c_associated(p, q)) error stop 3
    if (.not. c_associated(p, c_loc(i))) error stop 4
    if (c_associated(p, c_loc(j))) error stop 5

    arr = c_null_ptr
    if (c_associated(arr(1), arr(2))) error stop 6
    h%p = c_null_ptr
    if (c_associated(h%p, q)) error stop 7
    h%p = p
    if (.not. c_associated(h%p, p)) error stop 8

    fp = c_null_funptr
    fq = c_null_funptr
    if (c_associated(fp, fq)) error stop 9

    if (check(q, r)) error stop 10
    if (.not. check(p, p)) error stop 11
contains
    logical(c_bool) function check(a, b) bind(c)
        type(c_ptr), value :: a, b
        check = c_associated(a, b)
    end function check
end program c_associated_02
