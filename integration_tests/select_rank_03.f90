program select_rank_03
    implicit none

    integer :: a(2) = [4, 2]
    integer :: b(2, 2) = reshape([1, 2, 3, 4], [2, 2])

    call check(a)
    call check(b)

contains

    subroutine check(x)
        integer, intent(in) :: x(..)
        integer :: l1(1)
        integer :: l2(2)
    
        select rank(x)
        rank (1)
            l1 = minloc(x)
            print *, l1
            if (l1(1) /= 2) error stop
        rank (2)
            l2 = minloc(x)
            print *, l2
            if (any(l2 /= [1, 1])) error stop
        end select
    end subroutine check

end program select_rank_03
