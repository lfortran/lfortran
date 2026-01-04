
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
        integer :: minloc_res1(1)
        integer :: minloc_res2(2)
        select rank(y=>x)
            rank(1)
                print *, y
                minloc_res1 = minloc(y)
                if (minloc_res1(1) /= 1) error stop
                if (y(2) /= 2) error stop
                if (expected /= 1) error stop
            rank(2)
                print *, y
                minloc_res2 = minloc(y)
                if (any(minloc_res2 /= [1, 1])) error stop
                print *, minloc(y)
                if (expected /= 2) error stop
            rank(3)
                print *, y
                print *, minloc(y)
                if (expected /= 3) error stop

        end select
    end subroutine check
end program select_rank_test