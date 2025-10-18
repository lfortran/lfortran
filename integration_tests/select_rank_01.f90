
program select_rank_test
    implicit none

    integer :: b(2) = [1,2]
    integer :: c(2,2) = reshape([1,2,3,4],[2,2])
    integer :: d(2,2,2) = reshape([1,2,3,4,5,6,7,8],[2,2,2])

    call check(b, 1)
    call check(c, 2)
    call check(d, 3)

contains

    subroutine check(x, expected)
        integer, intent(in) :: x(..)
        integer, intent(in) :: expected
        integer :: i

        select rank(x)
            rank(0)
                if (expected /= 0) error stop
            rank(1)
                i = x(2)
                print *, i
                if (i /= 2) error stop
                if (expected /= 1) error stop
            rank(2)
                i = x(1,2)
                print *, i
                if (i /= 3) error stop
                if (expected /= 2) error stop
            rank(3)
                i = x(2,2,1)
                print *, i
                if (i /= 4) error stop
                if (expected /= 3) error stop

        end select
    end subroutine check
end program select_rank_test