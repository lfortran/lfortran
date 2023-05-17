program main
    implicit none

    call compare_solutions()

    contains

    subroutine compare_solutions()
        real, dimension(:), allocatable :: temp
        integer :: i
        temp = solution()
        print *, temp
        do i = lbound(temp, 1), ubound(temp, 1)
            if (abs(temp(i) * i - 1.0) > 1e-8) error stop
        end do
    end subroutine compare_solutions

    pure function solution() result(x)
        real, dimension(:), allocatable :: x
        x = [1.0, 0.5, 1.0/3.0, 0.25, 0.2]
    end function solution
end program main
