program select_rank_04
    implicit none

    integer :: a(2) = [4, 2]
    integer :: b(2, 3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    
    call check(a)
    call check(b)
contains

    subroutine check(x)
        integer, intent(in) :: x(..)
        integer :: l1(1)
        integer :: l2(2)
        integer :: c(2, 1)
        integer :: d(3, 2)
    
        select rank(x)
        rank (1)
            c = reshape(x, [2, 1]) 
            print *, c
            if (any(c /= reshape([4, 2], [2, 1]))) error stop
        rank (2)
            d = reshape(x, [3, 2])
            print *, d
            if (any(d /= reshape([1, 2, 3, 4, 5, 6], [3, 2]))) error stop
        end select
    end subroutine check

end program select_rank_04