! Standard-Fortran oracle of traits_protocol_operators_01: the generics spread
! and halve written out for integer, real(real64) and Fixed, whose static
! operator bindings become generic operator interfaces.
module traits_protocol_operators_01_oracle_fixed_m
    implicit none
    private
    public :: Fixed, operator(-), operator(/), operator(<)

    type :: Fixed
        integer :: tenths = 0
    end type Fixed

    interface Fixed
        module procedure from_integer
    end interface Fixed
    interface operator(-)
        module procedure minus
    end interface operator(-)
    interface operator(/)
        module procedure divide
    end interface operator(/)
    interface operator(<)
        module procedure less
    end interface operator(<)

contains

    function from_integer(n) result(res)
        integer, intent(in) :: n
        type(Fixed)         :: res
        res%tenths = 10 * n
    end function from_integer

    function minus(lhs, rhs) result(res)
        type(Fixed), intent(in) :: lhs, rhs
        type(Fixed)             :: res
        res%tenths = lhs%tenths - rhs%tenths
    end function minus

    function divide(lhs, rhs) result(res)
        type(Fixed), intent(in) :: lhs, rhs
        type(Fixed)             :: res
        res%tenths = (10 * lhs%tenths) / rhs%tenths
    end function divide

    function less(lhs, rhs) result(res)
        type(Fixed), intent(in) :: lhs, rhs
        logical                 :: res
        res = lhs%tenths < rhs%tenths
    end function less
end module traits_protocol_operators_01_oracle_fixed_m

program traits_protocol_operators_01_oracle
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_protocol_operators_01_oracle_fixed_m
    implicit none
    type(Fixed) :: f

    if (spread_integer([3, 9, 5]) /= 3) error stop 1
    if (spread_integer([7, 2, 2, 8]) /= 3) error stop 2
    if (spread_real64([1.5_real64, 4.5_real64, 2.0_real64]) /= 1.5_real64) error stop 3
    f = spread_fixed([Fixed(3), Fixed(9), Fixed(5)])
    if (f%tenths /= 30) error stop 4
    f = Fixed(5) / Fixed(2)
    if (f%tenths /= 25) error stop 5
    if (.not. (Fixed(1) < Fixed(2))) error stop 6
    if (halve_integer(9) /= 4) error stop 7
    f = halve_fixed(Fixed(5))
    if (f%tenths /= 25) error stop 8
    print '(a)', 'traits_protocol_operators_01 checks: 8'

contains

    function spread_integer(x) result(r)
        integer, intent(in) :: x(:)
        integer             :: r, low, high
        integer             :: i
        low = x(1)
        high = x(1)
        do i = 2, size(x)
            if (x(i) < low) low = x(i)
            if (high < x(i)) high = x(i)
        end do
        r = (high - low) / 2
    end function spread_integer

    function spread_real64(x) result(r)
        real(real64), intent(in) :: x(:)
        real(real64)             :: r, low, high
        integer                  :: i
        low = x(1)
        high = x(1)
        do i = 2, size(x)
            if (x(i) < low) low = x(i)
            if (high < x(i)) high = x(i)
        end do
        r = (high - low) / real(2, real64)
    end function spread_real64

    function halve_integer(x) result(r)
        integer, intent(in) :: x
        integer             :: r
        r = x / 2
    end function halve_integer

    function halve_fixed(x) result(r)
        type(Fixed), intent(in) :: x
        type(Fixed)             :: r
        r = x / Fixed(2)
    end function halve_fixed

    function spread_fixed(x) result(r)
        type(Fixed), intent(in) :: x(:)
        type(Fixed)             :: r, low, high
        integer                 :: i
        low = x(1)
        high = x(1)
        do i = 2, size(x)
            if (x(i) < low) low = x(i)
            if (high < x(i)) high = x(i)
        end do
        r = (high - low) / Fixed(2)
    end function spread_fixed
end program traits_protocol_operators_01_oracle
