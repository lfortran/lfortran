module gpu_optional_launch_03_m
    implicit none
    type :: record
        real, allocatable :: v(:)
    end type
contains
    subroutine apply(n, y, x)
        integer, intent(in) :: n
        real, intent(out) :: y(n)
        type(record), optional, intent(inout) :: x
        integer :: i
        do concurrent (i = 1:n)
            y(i) = real(i)
            if (present(x)) then
                y(i) = y(i) + x%v(i-2)
                x%v(i-2) = x%v(i-2) + 10
            end if
        end do
        if (present(x)) then
            if (x%v(-1) /= 20) error stop 1
            x%v(-1) = x%v(-1) + 100
        end if
    end subroutine
end module

program gpu_optional_launch_03
    use gpu_optional_launch_03_m
    implicit none
    real :: y(3)
    type(record), target :: r
    type(record), pointer :: p => null()
    call apply(3, y, p)
    if (any(y /= [1., 2., 3.])) error stop 2
    call apply(3, y)
    if (any(y /= [1., 2., 3.])) error stop 3
    allocate(r%v(-1:1))
    r%v = [10., 20., 30.]
    p => r
    call apply(3, y, p)
    if (any(y /= [11., 22., 33.])) error stop 4
    if (any(r%v /= [120., 30., 40.])) error stop 5
    nullify(p)
    call apply(3, y, p)
    if (any(y /= [1., 2., 3.])) error stop 6
end program
