module gpu_optional_launch_04_m
    implicit none
contains
    subroutine explicit_input(n, y, x)
        integer, intent(in) :: n
        real, intent(out) :: y(n)
        real, optional, intent(in) :: x(n)
        integer :: i
        do concurrent (i = 1:n)
            y(i) = real(i)
            if (present(x)) y(i) = y(i) + x(i)
        end do
    end subroutine

    subroutine contiguous_input(n, y, x)
        integer, intent(in) :: n
        real, intent(out) :: y(n)
        real, contiguous, optional, intent(in) :: x(:)
        integer :: i
        do concurrent (i = 1:n)
            y(i) = real(i)
            if (present(x)) y(i) = y(i) + x(i)
        end do
    end subroutine

    subroutine strided_update(n, y, x)
        integer, intent(in) :: n
        real, intent(out) :: y(n)
        real, optional, intent(inout) :: x(:)
        integer :: i
        do concurrent (i = 1:n)
            y(i) = real(i)
            if (present(x)) then
                y(i) = y(i) + x(i)
                x(i) = x(i) + 3
            end if
        end do
    end subroutine
end module

program gpu_optional_launch_04
    use gpu_optional_launch_04_m
    implicit none
    real :: x(6), y(3)
    x = [10., 99., 20., 99., 30., 99.]
    call explicit_input(3, y, x(1:3))
    if (any(y /= [11., 101., 23.])) error stop 1
    call explicit_input(3, y)
    if (any(y /= [1., 2., 3.])) error stop 2
    call contiguous_input(3, y, x(1:3))
    if (any(y /= [11., 101., 23.])) error stop 3
    call contiguous_input(3, y)
    if (any(y /= [1., 2., 3.])) error stop 4
    call strided_update(3, y, x(1:6:2))
    if (any(y /= [11., 22., 33.])) error stop 5
    if (any(x /= [13., 99., 23., 99., 33., 99.])) error stop 6
    call strided_update(3, y)
    if (any(y /= [1., 2., 3.])) error stop 7
end program
