program main
    implicit none

    call compare_solutions()

    contains

    subroutine compare_solutions()
    implicit none
    real :: reldiff(2)
    real :: absdiff
    real, dimension(:), allocatable :: x
    
    where (solution() /= 0.0) reldiff = absdiff / abs(solution())

    end subroutine compare_solutions

    pure function solution() result(x)
        implicit none
        real,dimension(:),allocatable :: x
        x = [0.10,0.10]
    end function solution
end program main

