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

        if (abs(diff(1)) /= 0.0) error stop
        if (abs(diff(2)) /= 0.0) error stop

    end subroutine

    pure function solution() result(x)
        real,dimension(:),allocatable :: x

        x = [0.1, 0.1]
    
    end function solution
end program main
