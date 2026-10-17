module pdt_27_mod
    ! KIND and LEN type parameters together, including generic resolution
    ! by kind and a LEN parameter listed before a KIND parameter.
    implicit none
    integer, parameter :: sp = kind(1.0), dp = kind(1.0d0)

    type :: vector_t(k, n)
        integer, kind :: k = dp
        integer, len :: n = 0
        real(kind=k) :: x(n)
    end type

    type :: pair_t(m, k)
        integer, len :: m
        integer, kind :: k
        integer(kind=k) :: values(m)
    end type

    interface total
        module procedure total_sp, total_dp
    end interface

contains

    real(sp) function total_sp(v)
        type(vector_t(k=sp, n=*)), intent(in) :: v
        total_sp = sum(v%x)
    end function

    real(dp) function total_dp(v)
        type(vector_t(k=dp, n=*)), intent(in) :: v
        total_dp = sum(v%x)
    end function

end module

program pdt_27
    use pdt_27_mod
    implicit none
    type(vector_t(k=sp, n=3)) :: a
    type(vector_t(n=5)) :: b
    type(vector_t(k=dp, n=:)), allocatable :: c
    type(pair_t(3, 8)) :: p
    integer :: i

    do i = 1, a%n
        a%x(i) = real(i, sp)
    end do
    do i = 1, b%n
        b%x(i) = 10.0_dp * i
    end do
    allocate(vector_t(k=dp, n=4) :: c)
    c%x = 1.5_dp
    if (a%k /= sp .or. b%k /= dp .or. c%k /= dp) error stop
    if (kind(a%x) /= sp .or. kind(b%x) /= dp) error stop
    if (abs(total(a) - 6.0_sp) > 1e-5_sp) error stop
    if (abs(total(b) - 150.0_dp) > 1e-12_dp) error stop
    if (abs(total(c) - 6.0_dp) > 1e-12_dp) error stop

    p%values = [1, 2, 3]
    if (p%m /= 3 .or. p%k /= 8 .or. kind(p%values) /= 8) error stop
    if (sum(p%values) /= 6_8) error stop
    print *, total(a), total(b), total(c), p%values
end program
