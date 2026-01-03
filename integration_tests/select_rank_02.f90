
program select_rank_test
    implicit none

    real :: b(2) = [1,2]
    real :: c(2,2) = reshape([1,2,3,4],[2,2])
    real :: d(2,2,2) = reshape([1,2,3,4,5,6,7,8],[2,2,2])

    call check(b, 1)
    call check(c, 2)
    call check(d, 3)

contains

    subroutine check(x, expected)
        real, intent(in) :: x(..)
        integer, intent(in) :: expected

        select rank(x)
            rank(0)
                if (expected /= 0) error stop
            rank(1)
                if (expected /= 1) error stop
            rank(2)
                if (expected /= 2) error stop
            rank default
                if (expected /= 3) error stop

        end select
    end subroutine check
end program select_rank_test