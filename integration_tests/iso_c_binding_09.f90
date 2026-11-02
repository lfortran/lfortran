! c_loc of a rank-1 assumed-size array returns the address of its first
! element.
module iso_c_binding_09_mod
    use iso_c_binding, only: c_loc, c_ptr, c_associated, c_f_pointer, c_int, &
        c_char, c_float
    implicit none
contains

    subroutine check_int(a, n)
        integer, intent(in) :: n
        integer(c_int), target, intent(in) :: a(*)
        type(c_ptr) :: p
        integer(c_int), pointer :: q(:)
        integer :: i
        p = c_loc(a)
        if (.not. c_associated(p, c_loc(a(1)))) error stop 1
        call c_f_pointer(p, q, [n])
        do i = 1, n
            if (q(i) /= 10*i) error stop 2
        end do
    end subroutine

    subroutine check_char(a)
        character(kind=c_char), target, intent(in) :: a(*)
        type(c_ptr) :: p
        character(kind=c_char), pointer :: q(:)
        p = c_loc(a)
        if (.not. c_associated(p, c_loc(a(1)))) error stop 3
        call c_f_pointer(p, q, [3])
        if (q(1) /= "x" .or. q(2) /= "y" .or. q(3) /= "z") error stop 4
    end subroutine

    subroutine check_real(a, pa)
        real, target, intent(in) :: a(*)
        type(c_ptr), intent(in) :: pa
        type(c_ptr) :: p
        p = c_loc(a)
        if (.not. c_associated(p, pa)) error stop 5
    end subroutine

    subroutine check_bindc(a, pa) bind(c, name="iso_c_binding_09_check_bindc")
        real(c_float), target, intent(in) :: a(*)
        type(c_ptr), value, intent(in) :: pa
        type(c_ptr) :: p
        real(c_float), pointer :: q(:)
        p = c_loc(a)
        if (.not. c_associated(p, pa)) error stop 7
        call c_f_pointer(p, q, [3])
        if (q(1) /= 1.0 .or. q(3) /= 3.0) error stop 8
    end subroutine

    subroutine check_lb(a0, a5, pa)
        real(c_float), target, intent(in) :: a0(0:*), a5(5:*)
        type(c_ptr), intent(in) :: pa
        type(c_ptr) :: p
        real(c_float), pointer :: q(:)
        p = c_loc(a0)
        if (.not. c_associated(p, pa)) error stop 9
        call c_f_pointer(p, q, [3])
        if (q(1) /= 1.0 .or. q(3) /= 3.0) error stop 10
        p = c_loc(a5)
        if (.not. c_associated(p, pa)) error stop 11
        call c_f_pointer(p, q, [3])
        if (q(2) /= 2.0 .or. q(3) /= 3.0) error stop 12
    end subroutine

end module

program iso_c_binding_09
    use iso_c_binding_09_mod
    implicit none
    integer(c_int), target :: x(4)
    character(kind=c_char), target :: s(3)
    real, target :: r(6)
    real(c_float), target :: f(3)
    type(c_ptr) :: pr
    integer :: i
    x = [(10*i, i = 1, 4)]
    call check_int(x, 4)
    s = ["x", "y", "z"]
    call check_char(s)
    pr = c_loc(r)
    call check_real(r, pr)
    call s_host(r)
    f = [1.0, 2.0, 3.0]
    call check_bindc(f, c_loc(f))
    call check_lb(f, f, c_loc(f))
    print *, "ok"
contains

    subroutine s_host(a)
        real, target, intent(in) :: a(*)
        type(c_ptr) :: p
        p = c_loc(a)
        if (.not. c_associated(p, pr)) error stop 6
    end subroutine

end program
