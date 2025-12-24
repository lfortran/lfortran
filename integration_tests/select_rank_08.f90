program select_rank_08
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
            if (any(x /= a)) error stop
        rank (2)
            x = b
            print *, x
            if (any(x /= b)) error stop
        end select
    end subroutine check

end program select_rank_08
