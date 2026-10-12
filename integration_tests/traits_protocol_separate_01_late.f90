! Compiled after the provider is frozen and hidden: a new derived numeric
! type and a further intrinsic kind join the open INumeric protocol without
! recompiling the provider (paper sect:ocp_conformance). two_way_mean
! forwards its type parameter through two runtime generic calls.
module traits_protocol_separate_01_late_m
    use, intrinsic :: iso_fortran_env, only: real32
    use traits_protocol_separate_01_contracts_m, only: INumeric, IAverager
    implicit none
    private
    public :: Rational, two_way_mean

    type, sealed :: Rational
        integer :: num = 0
        integer :: den = 1
    end type Rational

    implements INumeric :: Rational
        initial :: from_integer
        procedure, static :: operator(+) => add
        procedure, static :: operator(/) => divide
    end implements Rational

    implements INumeric :: real(real32)
    end implements real(real32)

contains

    function from_integer(n) result(res)
        integer, intent(in) :: n
        type(Rational)      :: res
        res%num = n
        res%den = 1
    end function from_integer

    function add(lhs, rhs) result(res)
        type(Rational), intent(in) :: lhs, rhs
        type(Rational)             :: res
        res = reduced(lhs%num * rhs%den + rhs%num * lhs%den, lhs%den * rhs%den)
    end function add

    function divide(lhs, rhs) result(res)
        type(Rational), intent(in) :: lhs, rhs
        type(Rational)             :: res
        res = reduced(lhs%num * rhs%den, lhs%den * rhs%num)
    end function divide

    function reduced(num, den) result(res)
        integer, intent(in) :: num, den
        type(Rational)      :: res
        integer             :: a, b, t
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

    function two_way_mean{INumeric :: T}(averager, x) result(r)
        class(IAverager), intent(in) :: averager
        type(T),          intent(in) :: x(:)
        type(T)                      :: r
        r = averager%average(x) + averager%average(x(size(x):1:-1))
    end function two_way_mean
end module traits_protocol_separate_01_late_m
