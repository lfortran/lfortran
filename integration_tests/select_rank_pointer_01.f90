program select_rank_pointer_test
    implicit none

    integer, pointer :: p0, p1(:), p2(:,:)
    integer, target :: t0, t1(2), t2(2,2)

    t0 = 10
    t1 = [10, 20]
    t2 = reshape([1, 2, 3, 4], [2, 2])

    p0 => t0
    p1 => t1
    p2 => t2

    call check_pointer(p0, 0)
    call check_pointer(p1, 1)
    call check_pointer(p2, 2)

contains

    subroutine check_pointer(x, expected)
        integer, pointer, intent(in) :: x(..)
        integer, intent(in) :: expected

        select rank (x)
        rank (0)
            if (expected /= 0) error stop "rank 0 failed"
        rank (1)
            if (expected /= 1) error stop "rank 1 failed"
        rank (2)
            if (expected /= 2) error stop "rank 2 failed"
        rank default
            if (expected < 0 .or. expected > 2) error stop "rank default failed"
        end select
    end subroutine check_pointer

end program select_rank_pointer_test
