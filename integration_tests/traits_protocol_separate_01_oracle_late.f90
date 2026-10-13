! Standard-Fortran oracle for traits_protocol_separate_01, part 4 of 5,
! compiled after the provider is archived and hidden: a new Rational type and
! real(real32) join the open protocol without recompiling the provider.
module traits_protocol_separate_01_oracle_late_m
    use, intrinsic :: iso_fortran_env, only: real32
    use traits_protocol_separate_01_oracle_contracts_m, only: INumeric, IAverager
    implicit none
    private
    public :: RationalNumeric, Real32Numeric, rational_of, real32_of, two_way_mean

    type, extends(INumeric) :: RationalNumeric
        integer :: num = 0
        integer :: den = 1
    contains
        procedure, nopass :: initial => rational_initial
        procedure         :: add     => rational_add
        procedure         :: divide  => rational_divide
    end type RationalNumeric

    type, extends(INumeric) :: Real32Numeric
        real(real32) :: value
    contains
        procedure, nopass :: initial => real32_initial
        procedure         :: add     => real32_add
        procedure         :: divide  => real32_divide
    end type Real32Numeric

contains

    function rational_initial(n) result(res)
        integer,         intent(in)  :: n
        class(INumeric), allocatable :: res
        res = RationalNumeric(n, 1)
    end function rational_initial

    function rational_add(lhs, rhs) result(res)
        class(RationalNumeric), intent(in)  :: lhs
        class(INumeric),        intent(in)  :: rhs
        class(INumeric),        allocatable :: res
        select type (rhs)
        type is (RationalNumeric)
            res = reduced(lhs%num * rhs%den + rhs%num * lhs%den, lhs%den * rhs%den)
        class default
            error stop 'operands of + differ in type'
        end select
    end function rational_add

    function rational_divide(lhs, rhs) result(res)
        class(RationalNumeric), intent(in)  :: lhs
        class(INumeric),        intent(in)  :: rhs
        class(INumeric),        allocatable :: res
        select type (rhs)
        type is (RationalNumeric)
            res = reduced(lhs%num * rhs%den, lhs%den * rhs%num)
        class default
            error stop 'operands of / differ in type'
        end select
    end function rational_divide

    function reduced(num, den) result(res)
        integer, intent(in)   :: num, den
        type(RationalNumeric) :: res
        integer               :: a, b, t
        a = abs(num)
        b = abs(den)
        do while (b /= 0)
            t = mod(a, b)
            a = b
            b = t
        end do
        res%num = sign(1, den) * num / a
        res%den = abs(den) / a
    end function reduced

    function real32_initial(n) result(res)
        integer,         intent(in)  :: n
        class(INumeric), allocatable :: res
        res = Real32Numeric(real(n, real32))
    end function real32_initial

    function real32_add(lhs, rhs) result(res)
        class(Real32Numeric), intent(in)  :: lhs
        class(INumeric),      intent(in)  :: rhs
        class(INumeric),      allocatable :: res
        select type (rhs)
        type is (Real32Numeric)
            res = Real32Numeric(lhs%value + rhs%value)
        class default
            error stop 'operands of + differ in type'
        end select
    end function real32_add

    function real32_divide(lhs, rhs) result(res)
        class(Real32Numeric), intent(in)  :: lhs
        class(INumeric),      intent(in)  :: rhs
        class(INumeric),      allocatable :: res
        select type (rhs)
        type is (Real32Numeric)
            res = Real32Numeric(lhs%value / rhs%value)
        class default
            error stop 'operands of / differ in type'
        end select
    end function real32_divide

    function rational_of(x) result(value)
        class(INumeric), intent(in) :: x
        type(RationalNumeric)       :: value
        select type (x)
        type is (RationalNumeric)
            value = x
        class default
            error stop 'Rational result expected'
        end select
    end function rational_of

    function real32_of(x) result(value)
        class(INumeric), intent(in) :: x
        real(real32)                :: value
        select type (x)
        type is (Real32Numeric)
            value = x%value
        class default
            error stop 'real(real32) result expected'
        end select
    end function real32_of

    function two_way_mean(averager, x) result(r)
        class(IAverager), intent(in)  :: averager
        class(INumeric),  intent(in)  :: x(:)
        class(INumeric),  allocatable :: r
        class(INumeric),  allocatable :: forward, backward
        forward = averager%average(x)
        backward = averager%average(x(size(x):1:-1))
        r = forward + backward
    end function two_way_mean
end module traits_protocol_separate_01_oracle_late_m
