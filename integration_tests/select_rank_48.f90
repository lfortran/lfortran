program select_rank_48
! The selector of a select rank block in an array constructor: a scalar in
! rank(0), an array in rank(1) and rank(2).
implicit none
integer :: m(2, 2)
m = reshape([1, 2, 3, 4], [2, 2])
call check(7, [7])
call check([5, 6], [5, 6])
call check(m, [1, 2, 3, 4])
contains
    subroutine check(a, expected)
        integer, intent(in) :: a(..)
        integer, intent(in) :: expected(:)
        integer :: x(size(expected))
        select rank (a)
        rank (0)
            x = [a]
        rank (1)
            x = [a]
        rank (2)
            x = [a]
        rank default
            error stop 1
        end select
        if (size(x) /= size(expected)) error stop 2
        if (any(x /= expected)) error stop 3
    end subroutine
end program
