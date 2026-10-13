! Standard-Fortran oracle for traits_protocol_separate_01, part 2 of 5:
! integer and real(real64) acknowledged through wrapper types that forward
! to the intrinsic operations; SELECT TYPE enforces type(itself) operands.
module traits_protocol_separate_01_oracle_intrinsics_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_protocol_separate_01_oracle_contracts_m, only: INumeric
    implicit none
    private
    public :: IntegerNumeric, Real64Numeric, integer_of, real64_of

    type, extends(INumeric) :: IntegerNumeric
        integer :: value
    contains
        procedure, nopass :: initial => integer_initial
        procedure         :: add     => integer_add
        procedure         :: divide  => integer_divide
    end type IntegerNumeric

    type, extends(INumeric) :: Real64Numeric
        real(real64) :: value
    contains
        procedure, nopass :: initial => real64_initial
        procedure         :: add     => real64_add
        procedure         :: divide  => real64_divide
    end type Real64Numeric

contains

    function integer_initial(n) result(res)
        integer,         intent(in)  :: n
        class(INumeric), allocatable :: res
        res = IntegerNumeric(n)
    end function integer_initial

    function integer_add(lhs, rhs) result(res)
        class(IntegerNumeric), intent(in)  :: lhs
        class(INumeric),       intent(in)  :: rhs
        class(INumeric),       allocatable :: res
        select type (rhs)
        type is (IntegerNumeric)
            res = IntegerNumeric(lhs%value + rhs%value)
        class default
            error stop 'operands of + differ in type'
        end select
    end function integer_add

    function integer_divide(lhs, rhs) result(res)
        class(IntegerNumeric), intent(in)  :: lhs
        class(INumeric),       intent(in)  :: rhs
        class(INumeric),       allocatable :: res
        select type (rhs)
        type is (IntegerNumeric)
            res = IntegerNumeric(lhs%value / rhs%value)
        class default
            error stop 'operands of / differ in type'
        end select
    end function integer_divide

    function real64_initial(n) result(res)
        integer,         intent(in)  :: n
        class(INumeric), allocatable :: res
        res = Real64Numeric(real(n, real64))
    end function real64_initial

    function real64_add(lhs, rhs) result(res)
        class(Real64Numeric), intent(in)  :: lhs
        class(INumeric),      intent(in)  :: rhs
        class(INumeric),      allocatable :: res
        select type (rhs)
        type is (Real64Numeric)
            res = Real64Numeric(lhs%value + rhs%value)
        class default
            error stop 'operands of + differ in type'
        end select
    end function real64_add

    function real64_divide(lhs, rhs) result(res)
        class(Real64Numeric), intent(in)  :: lhs
        class(INumeric),      intent(in)  :: rhs
        class(INumeric),      allocatable :: res
        select type (rhs)
        type is (Real64Numeric)
            res = Real64Numeric(lhs%value / rhs%value)
        class default
            error stop 'operands of / differ in type'
        end select
    end function real64_divide

    function integer_of(x) result(value)
        class(INumeric), intent(in) :: x
        integer                     :: value
        select type (x)
        type is (IntegerNumeric)
            value = x%value
        class default
            error stop 'integer result expected'
        end select
    end function integer_of

    function real64_of(x) result(value)
        class(INumeric), intent(in) :: x
        real(real64)                :: value
        select type (x)
        type is (Real64Numeric)
            value = x%value
        class default
            error stop 'real(real64) result expected'
        end select
    end function real64_of
end module traits_protocol_separate_01_oracle_intrinsics_m
