program main
    implicit none

    call compare_solutions()

    contains

    subroutine compare_solutions()
        real, dimension(:), allocatable :: temp
        temp = solution()
        print *, temp
    end subroutine compare_solutions

    pure function solution() result(x)
        real,dimension(:),allocatable :: x
        x = [0.10,0.10,1., .1, .23]
        print *, x
    end function solalution
end program main
