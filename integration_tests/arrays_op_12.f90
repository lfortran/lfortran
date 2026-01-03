program main
    implicit none

    real :: z(2)

    z = 0.0
    print *, z

    z = solution(z)
    print *, z
    if (z(1) /= 1.0) error stop
    if (z(2) /= 2.0) error stop

contains

    function solution(y) result(x)
        real, dimension(:), intent(in) :: y
        real, dimension(:), allocatable :: x
        x = [1.0, 2.0]
        print *, y
        if (y(1) /= 0.0) error stop
        if (y(2) /= 0.0) error stop
    end function solution
end program main
