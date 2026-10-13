! Operator requirements in a trait hierarchy: a child trait adds an overload
! of its parent's operator(*) and repeats its operator(+). The two
! operator(*) requirements are distinct, the two operator(+) requirements
! one. A derived type binds both overloads with static procedures; integer
! and real(real64) acknowledge them with their intrinsic operators.
module traits_protocol_operators_02_contracts
    implicit none
    private
    public :: IBase, IChild, scaled_square, double

    abstract interface :: IBase
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(*)
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
    end interface IBase

    abstract interface, extends(IBase) :: IChild
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs
            integer,      intent(in) :: rhs
            type(itself)             :: res
        end function operator(*)
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
    end interface IChild

contains

    ! Both overloads of operator(*) and the repeated operator(+).
    function scaled_square{IChild :: T}(x, k) result(r)
        type(T), intent(in) :: x
        integer, intent(in) :: k
        type(T)             :: r
        r = (x * x) * k + x
    end function scaled_square

    function double{IBase :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T)             :: r
        r = x + x
    end function double
end module traits_protocol_operators_02_contracts

module traits_protocol_operators_02_point
    use traits_protocol_operators_02_contracts, only: IChild
    implicit none
    private

    type, public, sealed :: Pt
        integer :: v = 0
    end type Pt

    implements IChild :: Pt
        procedure, static :: operator(*) => mul_pp, mul_pi
        procedure, static :: operator(+) => add
    end implements Pt

contains

    function mul_pp(lhs, rhs) result(res)
        type(Pt), intent(in) :: lhs, rhs
        type(Pt)             :: res
        res%v = lhs%v * rhs%v
    end function mul_pp

    function mul_pi(lhs, rhs) result(res)
        type(Pt), intent(in) :: lhs
        integer,  intent(in) :: rhs
        type(Pt)             :: res
        res%v = 100 * lhs%v * rhs
    end function mul_pi

    function add(lhs, rhs) result(res)
        type(Pt), intent(in) :: lhs, rhs
        type(Pt)             :: res
        res%v = lhs%v + rhs%v
    end function add
end module traits_protocol_operators_02_point

module traits_protocol_operators_02_intrinsic
    use traits_protocol_operators_02_contracts, only: IChild
    use iso_fortran_env, only: real64
    implicit none
    private

    implements IChild :: integer
    end implements integer

    implements IChild :: real(real64)
    end implements real(real64)
end module traits_protocol_operators_02_intrinsic

program traits_protocol_operators_02
    use traits_protocol_operators_02_contracts, only: scaled_square, double
    use traits_protocol_operators_02_point, only: Pt
    use traits_protocol_operators_02_intrinsic
    use iso_fortran_env, only: real64
    implicit none
    type(Pt) :: p, q

    p%v = 3
    q = scaled_square(p, 2)
    if (q%v /= 1803) error stop 1
    q = double(p)
    if (q%v /= 6) error stop 2
    ! The static bindings also apply to concrete values here.
    q = (p * p) * 2 + p
    if (q%v /= 1803) error stop 3

    if (scaled_square(3, 2) /= 21) error stop 4
    if (double(7) /= 14) error stop 5
    if (abs(scaled_square(1.5_real64, 2) - 6.0_real64) > 1e-12_real64) error stop 6
    if (abs(double(2.5_real64) - 5.0_real64) > 1e-12_real64) error stop 7
    print '(a)', 'traits_protocol_operators_02 ok'
end program traits_protocol_operators_02
