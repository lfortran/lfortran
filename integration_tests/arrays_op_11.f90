program main
    implicit none

    real :: z(2)

    z = solution()
    call compare_solutions(z)

contains

    subroutine compare_solutions(x)
        real,dimension(:),intent(in) :: x
        real,dimension(size(x)) :: diff

        diff = solution() - x

        if (diff(1) /= 0.0) error stop
        if (diff(2) /= 0.0) error stop

        diff = x - solution()

        if (diff(1) /= 0.0) error stop
        if (diff(2) /= 0.0) error stop
    end subroutine

    pure function solution() result(x)
        real, dimension(:), allocatable :: x

        x = [1.0, 2.0]
    end function solution
end program main
