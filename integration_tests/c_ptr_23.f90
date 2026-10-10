! A type(c_ptr), value dummy is a local copy of the actual argument: the
! procedure can define it, and pass it on to a dummy that defines it, without
! changing the caller's actual argument.
module c_ptr_23_mod
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr, c_associated, &
        c_f_pointer, c_int
    implicit none
    integer, target :: t1 = 1, t2 = 2
contains
    subroutine set_unspec(p)
        type(c_ptr) :: p
        p = c_loc(t2)
    end subroutine

    subroutine set_inout(p)
        type(c_ptr), intent(inout) :: p
        if (.not. c_associated(p, c_loc(t1))) error stop 1
        p = c_loc(t2)
    end subroutine

    subroutine set_out(p)
        type(c_ptr), intent(out) :: p
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

    subroutine set_bindc_inout(p) bind(c)
        type(c_ptr), intent(inout) :: p
        if (.not. c_associated(p, c_loc(t1))) error stop 2
        p = c_loc(t2)
    end subroutine

    subroutine read_in(p, expected)
        type(c_ptr), intent(in) :: p
        integer, intent(in) :: expected
        integer, pointer :: ip
        call c_f_pointer(p, ip)
        if (ip /= expected) error stop 3
    end subroutine

    subroutine read_value(p, expected)
        type(c_ptr), value :: p
        integer, intent(in) :: expected
        integer, pointer :: ip
        call c_f_pointer(p, ip)
        if (ip /= expected) error stop 4
    end subroutine

    subroutine read_bindc_value(p, expected) bind(c)
        type(c_ptr), value :: p
        integer(c_int), value :: expected
        integer, pointer :: ip
        call c_f_pointer(p, ip)
        if (ip /= expected) error stop 5
    end subroutine

    ! Defines its VALUE dummy directly.
    subroutine assign(q)
        type(c_ptr), value :: q
        integer, pointer :: ip
        if (.not. c_associated(q, c_loc(t1))) error stop 10
        q = c_loc(t2)
        if (.not. c_associated(q, c_loc(t2))) error stop 11
        call c_f_pointer(q, ip)
        if (ip /= 2) error stop 12
        call read_in(q, 2)
        call read_value(q, 2)
        call read_bindc_value(q, 2)
        q = c_null_ptr
        if (c_associated(q)) error stop 13
    end subroutine

    ! Pass the VALUE dummy on to a dummy that defines it.
    subroutine fwd_unspec(q)
        type(c_ptr), value :: q
        call set_unspec(q)
        if (.not. c_associated(q, c_loc(t2))) error stop 20
    end subroutine

    subroutine fwd_inout(q)
        type(c_ptr), value :: q
        call set_inout(q)
        if (.not. c_associated(q, c_loc(t2))) error stop 21
    end subroutine

    subroutine fwd_out(q)
        type(c_ptr), value :: q
        call set_out(q)
        if (.not. c_associated(q, c_loc(t2))) error stop 22
    end subroutine

    subroutine fwd_opt_unspec(q)
        type(c_ptr), value :: q
        call set_opt_unspec(q)
        if (.not. c_associated(q, c_loc(t2))) error stop 23
    end subroutine

    subroutine fwd_opt_inout(q)
        type(c_ptr), value :: q
        call set_opt_inout(q)
        if (.not. c_associated(q, c_loc(t2))) error stop 24
    end subroutine

    subroutine fwd_bindc_inout(q)
        type(c_ptr), value :: q
        call set_bindc_inout(q)
        if (.not. c_associated(q, c_loc(t2))) error stop 25
    end subroutine

    ! Pass the VALUE dummy on to dummies that only read it.
    subroutine fwd_read(q)
        type(c_ptr), intent(in), value :: q
        integer, pointer :: ip, ia(:)
        if (.not. c_associated(q, c_loc(t1))) error stop 26
        call c_f_pointer(q, ip)
        if (ip /= 1) error stop 27
        call c_f_pointer(q, ia, [1])
        if (ia(1) /= 1) error stop 28
        call read_in(q, 1)
        call read_value(q, 1)
        call read_bindc_value(q, 1)
    end subroutine
end module

program c_ptr_23
    use iso_c_binding, only: c_ptr, c_loc, c_associated
    use c_ptr_23_mod
    implicit none
    type(c_ptr) :: x
    x = c_loc(t1)
    call assign(x)
    call fwd_unspec(x)
    call fwd_inout(x)
    call fwd_out(x)
    call fwd_opt_unspec(x)
    call fwd_opt_inout(x)
    call fwd_bindc_inout(x)
    call fwd_read(x)
    if (.not. c_associated(x, c_loc(t1))) error stop 30
    print *, "ok"
end program
