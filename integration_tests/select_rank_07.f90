program select_rank_07
    implicit none

    integer :: a(2) = [4, 2]
    integer :: b(2, 3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    
    call check(a)
    call check(b)
contains

    subroutine check(x)
        integer, intent(inout) :: x(..)
        integer, save :: a(2) = [5, 3]
        integer, save :: b(2, 3) = reshape([6, 5, 4, 3, 2, 1], [2, 3])
    
        select rank(x)
        rank (1)
            x = a
            print *, x
            if (x(1) /= 5 .or. x(2) /= 3) error stop
        rank (2)
            x = b
            print *, x
            if (x(1,1) /= 6 .or. x(2,1) /= 5 .or. x(1,2) /= 4 .or. &
                x(2,2) /= 3 .or. x(1,3) /= 2 .or. x(2,3) /= 1) error stop
        end select
    end subroutine check

end program select_rank_07
