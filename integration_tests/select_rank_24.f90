program select_rank_24
    implicit none
    real :: arr(3, 4)
    arr = 1.0
    call test_sub(arr)

contains

    subroutine test_sub(x)
        real, dimension(..), intent(in) :: x
        integer :: n, total
        n = size(x, 1)
        if (n /= 3) error stop
        total = size(x)
        if (total /= 12) error stop
    end subroutine

end program select_rank_24
