! A type(c_ptr) actual argument is converted to how the type(c_ptr) dummy is
! passed: by reference for intent(out), intent(inout) or unspecified intent
! without VALUE, and by value otherwise. This holds for every kind of actual:
! a module variable, a dummy of the caller, an array element, an optional
! dummy, a pointer, an allocatable, an ASSOCIATE name, and an expression such
! as c_null_ptr, c_loc(x) or a function result.
module c_ptr_22_mod
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr, c_associated, c_int
    implicit none
    integer, target :: t1 = 1, t2 = 2
    type(c_ptr) :: mp
    integer :: nread = 0
contains
    ! Callees, one per way of declaring the dummy.
    subroutine read_in(p, expected)
        type(c_ptr), intent(in) :: p
        integer, intent(in) :: expected
        call check(p, expected)
    end subroutine

    subroutine read_value(p, expected)
        type(c_ptr), value :: p
        integer, intent(in) :: expected
        call check(p, expected)
    end subroutine

    subroutine read_in_value(p, expected)
        type(c_ptr), intent(in), value :: p
        integer, intent(in) :: expected
        call check(p, expected)
    end subroutine

    subroutine read_bindc_value(p, expected) bind(c)
        type(c_ptr), value :: p
        integer(c_int), value :: expected
        call check(p, int(expected))
    end subroutine

    subroutine read_opt_in(expected, p)
        integer, intent(in) :: expected
        type(c_ptr), optional, intent(in) :: p
        if (expected == 0) then
            if (present(p)) error stop 1
        else
            if (.not. present(p)) error stop 2
            call check(p, expected)
        end if
    end subroutine

    subroutine read_unspec(p, expected)
        type(c_ptr) :: p
        integer, intent(in) :: expected
        call check(p, expected)
    end subroutine

    subroutine read_inout(p, expected)
        type(c_ptr), intent(inout) :: p
        integer, intent(in) :: expected
        call check(p, expected)
    end subroutine

    subroutine set_unspec(p)
        type(c_ptr) :: p
        p = c_loc(t2)
    end subroutine

    subroutine set_inout(p)
        type(c_ptr), intent(inout) :: p
        p = c_loc(t2)
    end subroutine

    subroutine set_opt_unspec(p)
        type(c_ptr), optional :: p
        if (present(p)) p = c_loc(t2)
    end subroutine

    subroutine set_opt_inout(p)
        type(c_ptr), optional, intent(inout) :: p
        if (present(p)) p = c_loc(t2)
    end subroutine

    ! expected: 0 for a null pointer, 1 for c_loc(t1), 2 for c_loc(t2)
    subroutine check(p, expected)
        type(c_ptr), intent(in) :: p
        integer, intent(in) :: expected
        select case (expected)
        case (0)
            if (c_associated(p)) error stop 3
        case (1)
            if (.not. c_associated(p, c_loc(t1))) error stop 4
        case (2)
            if (.not. c_associated(p, c_loc(t2))) error stop 5
        end select
        nread = nread + 1
    end subroutine

    function get_t1() result(r)
        type(c_ptr) :: r
        r = c_loc(t1)
    end function

    ! Callers, one per way of declaring the caller's own dummy.
    subroutine from_inout(q)
        type(c_ptr), intent(inout) :: q
        call read_in(q, 1)
        call read_value(q, 1)
        call read_in_value(q, 1)
        call read_bindc_value(q, 1)
        call read_opt_in(1, q)
    end subroutine

    subroutine from_unspec(q)
        type(c_ptr) :: q
        call read_in(q, 1)
        call read_value(q, 1)
        call read_in_value(q, 1)
        call read_bindc_value(q, 1)
        call read_opt_in(1, q)
    end subroutine

    subroutine from_out(q)
        type(c_ptr), intent(out) :: q
        q = c_loc(t1)
        call read_in(q, 1)
        call read_value(q, 1)
        call read_in_value(q, 1)
        call read_bindc_value(q, 1)
        call read_opt_in(1, q)
    end subroutine

    subroutine from_in(q)
        type(c_ptr), intent(in) :: q
        call read_unspec(q, 1)
    end subroutine

    subroutine from_value(q)
        type(c_ptr), value :: q
        call read_unspec(q, 1)
        call read_inout(q, 1)
    end subroutine

    subroutine from_opt_unspec(q)
        type(c_ptr), optional :: q
        call set_unspec(q)
    end subroutine

    subroutine from_opt_inout(q)
        type(c_ptr), optional, intent(inout) :: q
        call set_inout(q)
    end subroutine
end module

program c_ptr_22
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr, c_associated
    use c_ptr_22_mod
    implicit none
    type(c_ptr), target :: x
    type(c_ptr) :: arr(3)
    type(c_ptr), pointer :: pp
    type(c_ptr), allocatable :: ap

    ! A module variable passed by reference.
    mp = c_loc(t1)
    call read_unspec(mp, 1)
    call read_inout(mp, 1)
    call set_unspec(mp)
    if (.not. c_associated(mp, c_loc(t2))) error stop 10
    mp = c_loc(t1)
    call set_inout(mp)
    if (.not. c_associated(mp, c_loc(t2))) error stop 11

    ! Optional dummies, present and absent.
    x = c_loc(t1)
    call set_opt_unspec(x)
    if (.not. c_associated(x, c_loc(t2))) error stop 12
    x = c_loc(t1)
    call set_opt_inout(x)
    if (.not. c_associated(x, c_loc(t2))) error stop 13
    call set_opt_unspec()
    call set_opt_inout()
    call read_opt_in(0)
    x = c_loc(t1)
    call from_opt_unspec(x)
    if (.not. c_associated(x, c_loc(t2))) error stop 14
    x = c_loc(t1)
    call from_opt_inout(x)
    if (.not. c_associated(x, c_loc(t2))) error stop 15

    ! A dummy passed by reference, passed on to a dummy passed by value.
    x = c_loc(t1)
    call from_inout(x)
    call from_unspec(x)
    x = c_null_ptr
    call from_out(x)
    if (.not. c_associated(x, c_loc(t1))) error stop 16

    ! A dummy passed by value, passed on to a dummy passed by reference.
    call from_in(x)
    call from_value(x)
    if (.not. c_associated(x, c_loc(t1))) error stop 17

    ! An array element passed to a dummy passed by value or by reference.
    arr(1) = c_null_ptr
    arr(2) = c_loc(t1)
    arr(3) = c_loc(t2)
    call read_in(arr(2), 1)
    call read_value(arr(3), 2)
    call read_in_value(arr(2), 1)
    call read_opt_in(2, arr(3))
    call read_unspec(arr(1), 0)
    call set_inout(arr(1))
    if (.not. c_associated(arr(1), c_loc(t2))) error stop 18

    ! A pointer, an allocatable and an ASSOCIATE name.
    x = c_loc(t1)
    pp => x
    call read_in(pp, 1)
    call read_value(pp, 1)
    call read_unspec(pp, 1)
    allocate(ap)
    ap = c_loc(t1)
    call read_in(ap, 1)
    call read_value(ap, 1)
    call set_inout(ap)
    if (.not. c_associated(ap, c_loc(t2))) error stop 19
    deallocate(ap)
    associate (a => x)
        call read_in(a, 1)
        call read_value(a, 1)
        call set_unspec(a)
    end associate
    if (.not. c_associated(x, c_loc(t2))) error stop 20

    ! An expression passed to a dummy passed by reference.
    call read_unspec(c_null_ptr, 0)
    call read_unspec(c_loc(t1), 1)
    call read_unspec(get_t1(), 1)
    call read_in(c_loc(t2), 2)

    if (nread /= 36) error stop 21
    print *, "ok"
end program
