program pointer_section_09
    ! Pointer bounds-remapping onto simply contiguous sections whose `:`
    ! subscripts the optimizations rewrite: of an explicit-shape dummy, of
    ! an assumed-size dummy and of a host array in an internal procedure
    implicit none
    integer, target :: b(4, 5)
    real(8), target :: r(2, 3)
    integer :: i

    b = reshape([(i, i = 1, 20)], [4, 5])
    r = 1
    r(2, 2) = 7
    call explicit_shape(b, 4, 5)
    call assumed_size(r, 2)
    call remap_host()

    print *, "passed"

contains

    subroutine explicit_shape(x, m, n)
        integer, intent(in) :: m, n
        integer, target :: x(m, n)
        integer, pointer :: q(:)
        q(1:2*m) => x(:, 2:3)
        if (size(q) /= 8) error stop 1
        if (q(1) /= 5 .or. q(4) /= 8 .or. q(5) /= 9 .or. q(8) /= 12) error stop 2
    end subroutine explicit_shape

    subroutine assumed_size(x, m)
        integer, intent(in) :: m
        real(8), target :: x(m, *)
        real(8), pointer :: p(:)
        p(1:2*m) => x(:, 1:2)
        if (size(p) /= 4) error stop 3
        if (p(4) /= 7 .or. p(3) /= 1) error stop 4
    end subroutine assumed_size

    subroutine remap_host()
        integer, pointer :: s(:), q(:, :)
        s(1:8) => b(:, 2:3)
        if (s(1) /= 5 .or. s(8) /= 12) error stop 5
        q(1:2, 1:6) => b(:, 3:5)
        if (q(1, 1) /= 9 .or. q(2, 6) /= 20) error stop 6
    end subroutine remap_host

end program pointer_section_09
