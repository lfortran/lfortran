program main
    implicit none

    call compare_solutions()

    contains

    subroutine compare_solutions()
    implicit none
    real :: reldiff(2)
    real :: absdiff
    real, dimension(:), allocatable :: x
    
    x = solution()

    if ( abs(x(1) - 0.10) >= 1e-7 ) error stop
    if ( abs(x(2) - 0.20) >= 1e-7 ) error stop
    if (size(x) /= 2) error stop

    end subroutine compare_solutions

    pure function solution() result(x)
        implicit none
        real,dimension(:),allocatable :: x
        x = [0.10,0.20]
    end function solution
end program main