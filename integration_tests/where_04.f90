program main
    implicit none

    call compare_solutions()

    contains

    subroutine compare_solutions()
    implicit none
    real :: reldiff(2)
    real :: absdiff
    reldiff = [0.0,0.0]
    absdiff = 0.5
    
    where (solution() /= 0.0) reldiff = absdiff / abs(solution())

    if (abs(reldiff(1) - 5.0) > 1e-7) error stop
    if (abs(reldiff(2) - 5.0) > 1e-7) error stop

    print *, reldiff

    end subroutine compare_solutions

    pure function solution() result(x)
        implicit none
        real,dimension(:),allocatable :: x
        x = [0.10,0.10]
    end function solution
end program main

