! Operator requirements beyond the paper's: operator(-), operator(/) and a
! comparison operator(<) of itself, with an initializer, implemented by a
! derived type through static bindings and acknowledged by integer and
! real(real64). spread{T} returns (max - min) / T(2) of its argument.
module traits_protocol_operators_01_contracts_m
    implicit none
    private
    public :: IField

    abstract interface :: IField
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(-)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(-)
        function operator(/)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(/)
        function operator(<)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            logical                  :: res
        endfunction
    end interface IField
end module traits_protocol_operators_01_contracts_m

! A decimal with one fractional digit, stored as tenths.
module traits_protocol_operators_01_fixed_m
    use traits_protocol_operators_01_contracts_m, only: IField
    implicit none
    private

    type, public, sealed :: Fixed
        integer :: tenths = 0
    end type Fixed

    implements IField :: Fixed
        initial :: from_integer
        procedure, static :: operator(-) => minus
        procedure, static :: operator(/) => divide
        procedure, static :: operator(<) => less
    end implements Fixed

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
end module traits_protocol_operators_01_fixed_m

module traits_protocol_operators_01_intrinsics_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_protocol_operators_01_contracts_m, only: IField
    implicit none

    implements IField :: integer
    end implements integer

    implements IField :: real(real64)
    end implements real(real64)
end module traits_protocol_operators_01_intrinsics_m

module traits_protocol_operators_01_generic_m
    use traits_protocol_operators_01_contracts_m, only: IField
    implicit none
    private
    public :: spread

contains

    function spread{IField :: T}(x) result(r)
        type(T), intent(in) :: x(:)
        type(T)             :: r, low, high
        integer             :: i
        low = x(1)
        high = x(1)
        do i = 2, size(x)
            if (x(i) < low) low = x(i)
            if (high < x(i)) high = x(i)
        end do
        r = (high - low) / T(2)
    end function spread
end module traits_protocol_operators_01_generic_m

program traits_protocol_operators_01
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_protocol_operators_01_fixed_m, only: Fixed
    use traits_protocol_operators_01_intrinsics_m
    use traits_protocol_operators_01_generic_m, only: spread
    implicit none
    type(Fixed) :: f

    if (spread([3, 9, 5]) /= 3) error stop 1
    if (spread([7, 2, 2, 8]) /= 3) error stop 2
    if (spread([1.5_real64, 4.5_real64, 2.0_real64]) /= 1.5_real64) error stop 3
    f = spread([Fixed(3), Fixed(9), Fixed(5)])
    if (f%tenths /= 30) error stop 4
    f = Fixed(5) / Fixed(2)
    if (f%tenths /= 25) error stop 5
    if (.not. (Fixed(1) < Fixed(2))) error stop 6
    print '(a)', 'traits_protocol_operators_01 checks: 6'
end program traits_protocol_operators_01
