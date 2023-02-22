program main
    implicit none

    call compare_solutions()

contains

    subroutine compare_solutions()
        implicit none
        integer :: i
        real :: x(2)

        print *, size(solution())

        do i = 1, size(solution())
            x(i) = i
        end do

        do i = 1, 2
            print *, x(i)
            if( x(i) /= i ) error stop
        end do

    end subroutine compare_solutions

    pure function solution() result(x)
        implicit none
        real, dimension(2) :: x
        x = [1.0, 1.0]
    end function solution

end program main
