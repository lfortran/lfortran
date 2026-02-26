module assumed_rank_02_mod
    use iso_c_binding, only: c_int
    implicit none
contains
    subroutine test_product_shape(a, expected)
        type(*), intent(inout), target, contiguous :: a(..)
        integer(c_int), intent(in) :: expected
        integer :: n
        n = product(shape(a))
        if (n /= expected) error stop
    end subroutine
end module

program assumed_rank_02
    use assumed_rank_02_mod
    implicit none
    integer :: x1(3, 4)
    integer :: x2(2, 3, 5)
    integer :: x3(10)
    real :: r(4, 4)

    x1 = 1
    call test_product_shape(x1, 12)

    x2 = 1
    call test_product_shape(x2, 30)

    x3 = 1
    call test_product_shape(x3, 10)

    r = 1.0
    call test_product_shape(r, 16)

    print *, "ok"
end program
