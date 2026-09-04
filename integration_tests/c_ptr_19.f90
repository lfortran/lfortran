! Passing type(c_ptr) dummies around: by value (VALUE / intent(in)) and
! by reference (intent(out) / no intent), including forwarding a by-value
! dummy to a bind(C) VALUE dummy.
module c_ptr_19_mod
    use iso_c_binding
    implicit none
contains
    ! bind(C) callee taking the c_ptr by value
    subroutine is_assoc_c(p, res) bind(c, name="c_ptr_19_is_assoc")
        type(c_ptr), value :: p
        integer(c_int), intent(out) :: res
        res = 0
        if (c_associated(p)) res = 1
    end subroutine is_assoc_c

    ! intent(in) dummy is held by value inside; forward it to a VALUE dummy
    subroutine forward(p, res)
        type(c_ptr), intent(in) :: p
        integer(c_int), intent(out) :: res
        call is_assoc_c(p, res)
    end subroutine forward

    ! no intent: passed by reference, assignment must reach the caller
    subroutine set_to(p, x)
        type(c_ptr) :: p
        integer, target :: x
        p = c_loc(x)
    end subroutine set_to

    subroutine reset(p)
        type(c_ptr), intent(out) :: p
        p = c_null_ptr
    end subroutine reset

    ! Source ABI VALUE scalar: callee modification must not reach the caller
    function twice(n) result(r)
        integer, value :: n
        integer :: r
        n = 2 * n
        r = n
    end function twice
end module c_ptr_19_mod

program c_ptr_19
    use iso_c_binding
    use c_ptr_19_mod
    implicit none
    type(c_ptr) :: p
    integer, target :: x
    integer, pointer :: fp
    integer(c_int) :: res
    integer :: n

    x = 5
    p = c_null_ptr
    if (c_associated(p)) error stop "null local should not be associated"

    call set_to(p, x)
    if (.not. c_associated(p)) error stop "set_to did not update p"
    call c_f_pointer(p, fp)
    if (fp /= 5) error stop "p does not point to x"

    call forward(p, res)
    if (res /= 1) error stop "forward(p) should be associated"

    call is_assoc_c(p, res)
    if (res /= 1) error stop "is_assoc_c(p) should be associated"

    call reset(p)
    if (c_associated(p)) error stop "reset did not clear p"

    call forward(p, res)
    if (res /= 0) error stop "forward(null) should not be associated"

    call forward(c_null_ptr, res)
    if (res /= 0) error stop "forward(c_null_ptr) should not be associated"

    call is_assoc_c(c_null_ptr, res)
    if (res /= 0) error stop "is_assoc_c(c_null_ptr) should not be associated"

    n = 3
    if (twice(n) /= 6) error stop "twice(3) /= 6"
    if (n /= 3) error stop "value dummy modified the caller's variable"

    print *, "c_ptr_19 test passed"
end program c_ptr_19
