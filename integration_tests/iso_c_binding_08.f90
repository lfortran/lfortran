! c_loc of a rank-2 (and rank-3) assumed-size array returns the address of
! its first element.
module iso_c_binding_08_mod
    use iso_c_binding, only: c_loc, c_ptr, c_associated, c_f_pointer, c_int, &
        c_char
    implicit none
contains

    subroutine check_int(a, n)
        integer, intent(in) :: n
        integer(c_int), target, intent(in) :: a(2, *)
        type(c_ptr) :: p
        integer(c_int), pointer :: q(:)
        integer :: i
        p = c_loc(a)
        if (.not. c_associated(p, c_loc(a(1, 1)))) error stop 1
        call c_f_pointer(p, q, [2*n])
        do i = 1, 2*n
            if (q(i) /= 10*i) error stop 2
        end do
    end subroutine

    subroutine check_char(a)
        character(kind=c_char), target, intent(in) :: a(1, *)
        type(c_ptr) :: p
        character(kind=c_char), pointer :: q(:)
        p = c_loc(a)
        if (.not. c_associated(p, c_loc(a(1, 1)))) error stop 3
        call c_f_pointer(p, q, [3])
        if (q(1) /= "x" .or. q(2) /= "y" .or. q(3) /= "z") error stop 4
    end subroutine

    subroutine check_rank3(a)
        real, target, intent(in) :: a(2, 2, *)
        type(c_ptr) :: p
        real, pointer :: q(:, :)
        p = c_loc(a)
        if (.not. c_associated(p, c_loc(a(1, 1, 1)))) error stop 5
        call c_f_pointer(p, q, [2, 4])
        if (abs(q(1, 1) - 1.0) > 1e-6) error stop 6
        if (abs(q(2, 4) - 8.0) > 1e-6) error stop 7
    end subroutine

end module

program iso_c_binding_08
    use iso_c_binding_08_mod
    implicit none
    integer(c_int), target :: x(2, 3)
    character(kind=c_char), target :: s(1, 3)
    real, target :: r(2, 2, 2)
    integer :: i
    x = reshape([(10*i, i = 1, 6)], [2, 3])
    call check_int(x, 3)
    s(1, :) = ["x", "y", "z"]
    call check_char(s)
    r = reshape([(real(i), i = 1, 8)], [2, 2, 2])
    call check_rank3(r)
    print *, "ok"
end program
