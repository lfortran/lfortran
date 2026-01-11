program select_rank_06
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
            print *, x(1)
            if (x(1) /= 4) error stop
        rank (2)
            print *, x(1, 2)
            if (x(1, 2) /= 3) error stop
        end select
    end subroutine check

end program select_rank_06