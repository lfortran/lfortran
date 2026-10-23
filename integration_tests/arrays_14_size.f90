program arrays_14_size
    ! size(x, dim) of an explicit-shape dummy is replaced by the extent it
    ! was declared with, which must be converted to the kind size() returns:
    ! an integer(8) extent with a default-kind size(), and a default-integer
    ! extent with size(..., kind=8).
    implicit none
    integer(8) :: n8
    integer :: n4
    integer :: a(5)
    n8 = 5
    n4 = 5
    a = 0
    call wide_extent(a, n8)
    call default_extent(a, n4)
contains
    subroutine wide_extent(x, n)
        integer(8), intent(in) :: n
        integer, intent(inout) :: x(n)
        integer :: m
        m = size(x, 1)
        print *, m
        if (m /= 5) error stop 1
    end subroutine
    subroutine default_extent(x, n)
        integer, intent(in) :: n
        integer, intent(inout) :: x(n)
        integer(8) :: m
        m = size(x, 1, kind=8)
        print *, m
        if (m /= 5_8) error stop 3
        call check_wide(size(x, 1, kind=8))
    end subroutine
    subroutine check_wide(k)
        integer(8), intent(in) :: k
        if (k /= 5_8) error stop 4
    end subroutine
end program
