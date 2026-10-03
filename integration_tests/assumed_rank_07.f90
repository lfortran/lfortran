program assumed_rank_07
    implicit none
    real :: arr2d(3, 4)
    real :: arr1d(5)
    arr2d = 1.0
    arr1d = 2.0
    call check(arr2d, 12_8)
    call check(arr1d, 5_8)
contains
    subroutine check(a, expected)
        type(*), intent(in), contiguous :: a(..)
        integer(8), intent(in) :: expected
        integer(8) :: n
        n = int(product(shape(a)), 8)
        if (n /= expected) error stop
    end subroutine
end program assumed_rank_07