module elemental_28_m
implicit none
integer(4), save :: array_calls = 0
integer(4), save :: element_calls = 0
integer(4), save :: scalar_calls = 0

contains

function array_value() result(r)
    real(4), dimension(4) :: r
    array_calls = array_calls + 1
    r(1) = 1.00000000e+00
    r(2) = 2.00000000e+00
    r(3) = 3.00000000e+00
    r(4) = 4.00000000e+00
end function array_value

subroutine e_exp(x, y)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), intent(in) :: x
    real(4), dimension(:), intent(out) :: y
    ! CheckBounds LHS = y, RHS = x, 
    __libasr_index_0_1 = lbound(x, 1)
    do __libasr_index_0_ = lbound(y, 1), ubound(y, 1)
        y(__libasr_index_0_) = Exp(-x(__libasr_index_0_1))
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine e_exp

subroutine e_max(x, y, alpha)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    real(4), intent(in) :: alpha
    real(4), dimension(:), intent(in) :: x
    real(4), dimension(:), intent(out) :: y
    ! CheckBounds LHS = y, RHS = x, x, 
    __libasr_index_0_1 = lbound(x, 1)
    __libasr_index_0_2 = lbound(x, 1)
    do __libasr_index_0_ = lbound(y, 1), ubound(y, 1)
        y(__libasr_index_0_) = Max(alpha*x(__libasr_index_0_1), x(__libasr_index_0_2))
        __libasr_index_0_1 = __libasr_index_0_1 + 1
        __libasr_index_0_2 = __libasr_index_0_2 + 1
    end do
end subroutine e_max

subroutine e_nested(x, y)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), intent(in) :: x
    real(4), dimension(:), intent(out) :: y
    ! CheckBounds LHS = y, RHS = x, 
    __libasr_index_0_1 = lbound(x, 1)
    do __libasr_index_0_ = lbound(y, 1), ubound(y, 1)
        y(__libasr_index_0_) = Log(Exp(x(__libasr_index_0_1)) + real(1, kind=4))
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine e_nested

subroutine e_rank2(x, y)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_1_
    integer(4) :: __libasr_index_1_1
    integer(4), dimension(:, :), intent(in) :: x
    real(4), dimension(:, :), intent(out) :: y
    ! CheckBounds LHS = y, RHS = x, 
    __libasr_index_1_1 = lbound(x, 2)
    do __libasr_index_1_ = lbound(y, 2), ubound(y, 2)
        __libasr_index_0_1 = lbound(x, 1)
        do __libasr_index_0_ = lbound(y, 1), ubound(y, 1)
            y(__libasr_index_0_, __libasr_index_1_) = Sqrt(Real(x(__libasr_index_0_1,&
         __libasr_index_1_1)) + 1.00000000e+00)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        __libasr_index_1_1 = __libasr_index_1_1 + 1
    end do
end subroutine e_rank2

subroutine e_shift(x, y)
    real(4) :: __libasr_created__intrinsic_array_function_MaxVal
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), intent(in) :: x
    real(4), dimension(:), intent(out) :: y
    __libasr_created__intrinsic_array_function_MaxVal = MaxVal(x)
    ! CheckBounds LHS = y, RHS = x, 
    __libasr_index_0_1 = lbound(x, 1)
    do __libasr_index_0_ = lbound(y, 1), ubound(y, 1)
        y(__libasr_index_0_) = Exp(x(__libasr_index_0_1) - __libasr_created__intrinsic_array_function_MaxVal)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine e_shift

subroutine e_sin(x, y)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), intent(in) :: x
    real(4), dimension(:), intent(out) :: y
    ! CheckBounds LHS = y, RHS = x, 
    __libasr_index_0_1 = lbound(x, 1)
    do __libasr_index_0_ = lbound(y, 1), ubound(y, 1)
        y(__libasr_index_0_) = Exp(Sin(x(__libasr_index_0_1)))
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine e_sin

subroutine e_sqrt(x, y, eps)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), intent(in) :: eps
    real(4), dimension(:), intent(in) :: x
    real(4), dimension(:), intent(out) :: y
    ! CheckBounds LHS = y, RHS = x, 
    __libasr_index_0_1 = lbound(x, 1)
    do __libasr_index_0_ = lbound(y, 1), ubound(y, 1)
        y(__libasr_index_0_) = Sqrt(x(__libasr_index_0_1) + eps)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine e_sqrt

impure elemental real(4) function element_value(x) result(r)
    real(4), intent(in) :: x
    element_calls = element_calls + 1
    r = x + 1.00000000e+00
end function element_value

real(4) function scalar_value()
    scalar_calls = scalar_calls + 1
    scalar_value = 1.00000000e+00
end function scalar_value

end module elemental_28_m

program elemental_28
use elemental_28_m, only: array_calls
use elemental_28_m, only: array_value
use elemental_28_m, only: e_exp
use elemental_28_m, only: e_max
use elemental_28_m, only: e_nested
use elemental_28_m, only: e_rank2
use elemental_28_m, only: e_shift
use elemental_28_m, only: e_sin
use elemental_28_m, only: e_sqrt
use elemental_28_m, only: element_calls
use elemental_28_m, only: element_value
use elemental_28_m, only: scalar_calls
use elemental_28_m, only: scalar_value
implicit none
integer(4), dimension(4), parameter :: __libasr_created__array_reshape_array = [0, 3, 8, 15]
real(4), dimension(4), parameter :: __libasr_created__array_reshape_array1 = [1.00000000e+00, 2.00000000e+00,&
         3.00000000e+00, 4.00000000e+00]
integer(4), dimension(2) :: __libasr_created__array_reshape_shape
integer(4), dimension(2) :: __libasr_created__array_reshape_shape1
real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
logical(4), dimension(4) :: __libasr_created__intrinsic_array_function_Any
logical(4), dimension(4) :: __libasr_created__intrinsic_array_function_Any1
logical(4) :: __libasr_created__intrinsic_array_function_Any10
logical(4) :: __libasr_created__intrinsic_array_function_Any11
logical(4) :: __libasr_created__intrinsic_array_function_Any12
logical(4) :: __libasr_created__intrinsic_array_function_Any13
logical(4) :: __libasr_created__intrinsic_array_function_Any14
logical(4) :: __libasr_created__intrinsic_array_function_Any15
logical(4) :: __libasr_created__intrinsic_array_function_Any16
logical(4) :: __libasr_created__intrinsic_array_function_Any17
logical(4), dimension(4) :: __libasr_created__intrinsic_array_function_Any2
logical(4), dimension(4) :: __libasr_created__intrinsic_array_function_Any3
logical(4), dimension(4) :: __libasr_created__intrinsic_array_function_Any4
logical(4), dimension(6) :: __libasr_created__intrinsic_array_function_Any5
logical(4), dimension(4) :: __libasr_created__intrinsic_array_function_Any6
logical(4), dimension(2, 2) :: __libasr_created__intrinsic_array_function_Any7
logical(4), dimension(4) :: __libasr_created__intrinsic_array_function_Any8
logical(4) :: __libasr_created__intrinsic_array_function_Any9
real(4), dimension(4) :: __libasr_created__intrinsic_elemental_function_Abs
real(4), dimension(4) :: __libasr_created__intrinsic_elemental_function_Abs1
real(4), dimension(4) :: __libasr_created__intrinsic_elemental_function_Abs2
real(4), dimension(4) :: __libasr_created__intrinsic_elemental_function_Abs3
real(4), dimension(4) :: __libasr_created__intrinsic_elemental_function_Abs4
real(4), dimension(6) :: __libasr_created__intrinsic_elemental_function_Abs5
real(4), dimension(4) :: __libasr_created__intrinsic_elemental_function_Abs6
real(4), dimension(2, 2) :: __libasr_created__intrinsic_elemental_function_Abs7
real(4), dimension(4) :: __libasr_created__intrinsic_elemental_function_Abs8
real(4), dimension(4) :: __libasr_created__intrinsic_elemental_function_Sqrt
real(4), dimension(4) :: __libasr_created__intrinsic_elemental_function_Sqrt1
real(4), dimension(4) :: __libasr_created__intrinsic_elemental_function_Sqrt2
real(4) :: __libasr_created__scalar_operand_
real(4), dimension(4) :: __libasr_created__subroutine_call_e_max
integer(4), dimension(2, 2), parameter :: __libasr_created__subroutine_call_e_rank2 = reshape([0, 3, 8, 15], [2, 2])
real(4), dimension(:), pointer :: __libasr_created__subroutine_call_e_sqrt
real(4), dimension(4) :: __libasr_created_real_binop_left_
real(4), dimension(:), allocatable :: __libasr_created_real_binop_left_1
real(4), dimension(4), parameter :: __libasr_created_real_binop_right_ = [-1.00000001e-01, 0.00000000e+00,&
         1.00000000e+00, 2.00000000e+00]
real(4), dimension(4), parameter :: __libasr_created_real_binop_right_1 = [1.00000000e+00, 1.41421354e+00,&
         1.73205078e+00, 2.00000000e+00]
real(4), dimension(4), parameter :: __libasr_created_real_binop_right_2 = [1.00000000e+00, 1.41421354e+00,&
         1.73205078e+00, 2.00000000e+00]
real(4), dimension(4), parameter :: __libasr_created_real_binop_right_3 = [2.00000000e+00, 2.23606801e+00,&
         2.44948983e+00, 2.64575124e+00]
real(4), dimension(4), parameter :: __libasr_created_real_binop_right_4 = [2.00000000e+00, 2.23606801e+00,&
         2.44948983e+00, 2.64575124e+00]
real(4), dimension(6), parameter :: __libasr_created_real_binop_right_5 = [0.00000000e+00, 1.00000000e+00,&
         1.41421354e+00, 1.73205078e+00, 2.00000000e+00, 2.23606801e+00]
real(4), dimension(4), parameter :: __libasr_created_real_binop_right_6 = [2.00000000e+00, 1.73205078e+00,&
         1.41421354e+00, 1.00000000e+00]
real(4), dimension(2, 2), parameter :: __libasr_created_real_binop_right_7 = reshape([1.00000000e+00, 2.00000000e+00,&
         3.00000000e+00, 4.00000000e+00], [2, 2])
real(4), dimension(4), parameter :: __libasr_created_real_binop_right_8 = [-1.00000000e+00, 0.00000000e+00,&
         1.00000000e+00, 1.41421354e+00]
integer(4) :: __libasr_index_0_
integer(4) :: __libasr_index_0_1
integer(4) :: __libasr_index_0_10
integer(4) :: __libasr_index_0_11
integer(4) :: __libasr_index_0_12
integer(4) :: __libasr_index_0_13
integer(4) :: __libasr_index_0_14
integer(4) :: __libasr_index_0_15
integer(4) :: __libasr_index_0_16
integer(4) :: __libasr_index_0_17
integer(4) :: __libasr_index_0_18
integer(4) :: __libasr_index_0_19
integer(4) :: __libasr_index_0_2
integer(4) :: __libasr_index_0_20
integer(4) :: __libasr_index_0_21
integer(4) :: __libasr_index_0_22
integer(4) :: __libasr_index_0_23
integer(4) :: __libasr_index_0_24
integer(4) :: __libasr_index_0_25
integer(4) :: __libasr_index_0_26
integer(4) :: __libasr_index_0_27
integer(4) :: __libasr_index_0_28
integer(4) :: __libasr_index_0_29
integer(4) :: __libasr_index_0_3
integer(4) :: __libasr_index_0_30
integer(4) :: __libasr_index_0_31
integer(4) :: __libasr_index_0_32
integer(4) :: __libasr_index_0_33
integer(4) :: __libasr_index_0_34
integer(4) :: __libasr_index_0_35
integer(4) :: __libasr_index_0_36
integer(4) :: __libasr_index_0_37
integer(4) :: __libasr_index_0_38
integer(4) :: __libasr_index_0_39
integer(4) :: __libasr_index_0_4
integer(4) :: __libasr_index_0_40
integer(4) :: __libasr_index_0_41
integer(4) :: __libasr_index_0_42
integer(4) :: __libasr_index_0_43
integer(4) :: __libasr_index_0_44
integer(4) :: __libasr_index_0_45
integer(4) :: __libasr_index_0_46
integer(4) :: __libasr_index_0_47
integer(4) :: __libasr_index_0_48
integer(4) :: __libasr_index_0_49
integer(4) :: __libasr_index_0_5
integer(4) :: __libasr_index_0_50
integer(4) :: __libasr_index_0_51
integer(4) :: __libasr_index_0_52
integer(4) :: __libasr_index_0_53
integer(4) :: __libasr_index_0_54
integer(4) :: __libasr_index_0_55
integer(4) :: __libasr_index_0_56
integer(4) :: __libasr_index_0_57
integer(4) :: __libasr_index_0_58
integer(4) :: __libasr_index_0_59
integer(4) :: __libasr_index_0_6
integer(4) :: __libasr_index_0_60
integer(4) :: __libasr_index_0_61
integer(4) :: __libasr_index_0_62
integer(4) :: __libasr_index_0_63
integer(4) :: __libasr_index_0_64
integer(4) :: __libasr_index_0_65
integer(4) :: __libasr_index_0_7
integer(4) :: __libasr_index_0_8
integer(4) :: __libasr_index_0_9
integer(4) :: __libasr_index_1_
integer(4) :: __libasr_index_1_1
integer(4) :: __libasr_index_1_2
integer(4) :: __libasr_index_1_3
integer(4) :: __libasr_index_1_4
real(4), dimension(6) :: a
real(4), dimension(2, 2) :: b
real(4), dimension(:), allocatable :: empty
integer(4) :: i
real(4), dimension(:), allocatable :: result
real(4), dimension(4) :: x
real(4), dimension(4) :: y
x(1) = 0.00000000e+00
x(2) = 1.00000000e+00
x(3) = 2.00000000e+00
x(4) = 3.00000000e+00
call e_exp(x, y)
do i = 1, 4
    if (Abs(y(i) - (Exp(-x(i)))) > 9.99999997e-07) then
        error stop
    end if
end do
__libasr_index_0_1 = lbound(x, 1)
do __libasr_index_0_ = lbound(__libasr_created__subroutine_call_e_max, 1),&
         ubound(__libasr_created__subroutine_call_e_max, 1)
    __libasr_created__subroutine_call_e_max(__libasr_index_0_) = x(__libasr_index_0_1) - 1.00000000e+00
    __libasr_index_0_1 = __libasr_index_0_1 + 1
end do
call e_max(__libasr_created__subroutine_call_e_max, y, 1.00000001e-01)
__libasr_index_0_3 = lbound(y, 1)
__libasr_index_0_4 = lbound(__libasr_created_real_binop_right_, 1)
do __libasr_index_0_2 = lbound(__libasr_created__intrinsic_elemental_function_Abs, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs, 1)
    __libasr_created__intrinsic_elemental_function_Abs(__libasr_index_0_2) = y(__libasr_index_0_3) - __libasr_created_re&
        al_binop_right_(__libasr_index_0_4)
    __libasr_index_0_3 = __libasr_index_0_3 + 1
    __libasr_index_0_4 = __libasr_index_0_4 + 1
end do
__libasr_index_0_6 = lbound(__libasr_created__intrinsic_elemental_function_Abs, 1)
do __libasr_index_0_5 = lbound(__libasr_created__intrinsic_array_function_Any, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any, 1)
    __libasr_created__intrinsic_array_function_Any(__libasr_index_0_5) = Abs(__libasr_created__intrinsic_elemental_funct&
        ion_Abs(__libasr_index_0_6)) > 9.99999997e-07
    __libasr_index_0_6 = __libasr_index_0_6 + 1
end do
__libasr_created__intrinsic_array_function_Any9 = Any(__libasr_created__intrinsic_array_function_Any)
if (__libasr_created__intrinsic_array_function_Any9) then
    error stop
end if
call e_sqrt(x, y, 1.00000000e+00)
__libasr_index_0_8 = lbound(y, 1)
__libasr_index_0_9 = lbound(__libasr_created_real_binop_right_1, 1)
do __libasr_index_0_7 = lbound(__libasr_created__intrinsic_elemental_function_Abs1, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs1, 1)
    __libasr_created__intrinsic_elemental_function_Abs1(__libasr_index_0_7) = y(__libasr_index_0_8) - __libasr_created_r&
        eal_binop_right_1(__libasr_index_0_9)
    __libasr_index_0_8 = __libasr_index_0_8 + 1
    __libasr_index_0_9 = __libasr_index_0_9 + 1
end do
__libasr_index_0_11 = lbound(__libasr_created__intrinsic_elemental_function_Abs1, 1)
do __libasr_index_0_10 = lbound(__libasr_created__intrinsic_array_function_Any1, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any1, 1)
    __libasr_created__intrinsic_array_function_Any1(__libasr_index_0_10) = Abs(__libasr_created__intrinsic_elemental_fun&
        ction_Abs1(__libasr_index_0_11)) > 9.99999997e-07
    __libasr_index_0_11 = __libasr_index_0_11 + 1
end do
__libasr_created__intrinsic_array_function_Any10 = Any(__libasr_created__intrinsic_array_function_Any1)
if (__libasr_created__intrinsic_array_function_Any10) then
    error stop
end if
call e_shift(x, y)
do i = 1, 4
    if (Abs(y(i) - Exp(x(i) - 3.00000000e+00)) > 9.99999997e-07) then
        error stop
    end if
end do
call e_nested(x, y)
do i = 1, 4
    if (Abs(y(i) - Log(Exp(x(i)) + 1.00000000e+00)) > 9.99999997e-07) then
        error stop
    end if
end do
call e_sin(x, y)
do i = 1, 4
    if (Abs(y(i) - Exp(Sin(x(i)))) > 9.99999997e-07) then
        error stop
    end if
end do
__libasr_created__scalar_operand_ = scalar_value()
__libasr_index_0_13 = lbound(x, 1)
do __libasr_index_0_12 = lbound(y, 1), ubound(y, 1)
    y(__libasr_index_0_12) = Sqrt(x(__libasr_index_0_13) + __libasr_created__scalar_operand_)
    __libasr_index_0_13 = __libasr_index_0_13 + 1
end do
if (scalar_calls /= 1) then
    error stop
end if
__libasr_index_0_15 = lbound(y, 1)
__libasr_index_0_16 = lbound(__libasr_created_real_binop_right_2, 1)
do __libasr_index_0_14 = lbound(__libasr_created__intrinsic_elemental_function_Abs2, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs2, 1)
    __libasr_created__intrinsic_elemental_function_Abs2(__libasr_index_0_14) = y(__libasr_index_0_15) - __libasr_created&
        _real_binop_right_2(__libasr_index_0_16)
    __libasr_index_0_15 = __libasr_index_0_15 + 1
    __libasr_index_0_16 = __libasr_index_0_16 + 1
end do
__libasr_index_0_18 = lbound(__libasr_created__intrinsic_elemental_function_Abs2, 1)
do __libasr_index_0_17 = lbound(__libasr_created__intrinsic_array_function_Any2, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any2, 1)
    __libasr_created__intrinsic_array_function_Any2(__libasr_index_0_17) = Abs(__libasr_created__intrinsic_elemental_fun&
        ction_Abs2(__libasr_index_0_18)) > 9.99999997e-07
    __libasr_index_0_18 = __libasr_index_0_18 + 1
end do
__libasr_created__intrinsic_array_function_Any11 = Any(__libasr_created__intrinsic_array_function_Any2)
if (__libasr_created__intrinsic_array_function_Any11) then
    error stop
end if
__libasr_created_real_binop_left_ = array_value()
__libasr_index_0_20 = lbound(__libasr_created_real_binop_left_, 1)
do __libasr_index_0_19 = lbound(__libasr_created__intrinsic_elemental_function_Sqrt, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Sqrt, 1)
    __libasr_created__intrinsic_elemental_function_Sqrt(__libasr_index_0_19) = __libasr_created_real_binop_left_(__libas&
        r_index_0_20) + 3.00000000e+00
    __libasr_index_0_20 = __libasr_index_0_20 + 1
end do
__libasr_index_0_22 = lbound(__libasr_created__intrinsic_elemental_function_Sqrt, 1)
do __libasr_index_0_21 = lbound(y, 1), ubound(y, 1)
    y(__libasr_index_0_21) = Sqrt(__libasr_created__intrinsic_elemental_function_Sqrt(__libasr_index_0_22))
    __libasr_index_0_22 = __libasr_index_0_22 + 1
end do
if (array_calls /= 1) then
    error stop
end if
__libasr_index_0_24 = lbound(y, 1)
__libasr_index_0_25 = lbound(__libasr_created_real_binop_right_3, 1)
do __libasr_index_0_23 = lbound(__libasr_created__intrinsic_elemental_function_Abs3, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs3, 1)
    __libasr_created__intrinsic_elemental_function_Abs3(__libasr_index_0_23) = y(__libasr_index_0_24) - __libasr_created&
        _real_binop_right_3(__libasr_index_0_25)
    __libasr_index_0_24 = __libasr_index_0_24 + 1
    __libasr_index_0_25 = __libasr_index_0_25 + 1
end do
__libasr_index_0_27 = lbound(__libasr_created__intrinsic_elemental_function_Abs3, 1)
do __libasr_index_0_26 = lbound(__libasr_created__intrinsic_array_function_Any3, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any3, 1)
    __libasr_created__intrinsic_array_function_Any3(__libasr_index_0_26) = Abs(__libasr_created__intrinsic_elemental_fun&
        ction_Abs3(__libasr_index_0_27)) > 9.99999997e-07
    __libasr_index_0_27 = __libasr_index_0_27 + 1
end do
__libasr_created__intrinsic_array_function_Any12 = Any(__libasr_created__intrinsic_array_function_Any3)
if (__libasr_created__intrinsic_array_function_Any12) then
    error stop
end if
__libasr_index_0_29 = lbound(x, 1)
do __libasr_index_0_28 = lbound(__libasr_created__intrinsic_elemental_function_Sqrt1, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Sqrt1, 1)
    __libasr_created__intrinsic_elemental_function_Sqrt1(__libasr_index_0_28) = element_value(x(__libasr_index_0_29)) + &
        3.00000000e+00
    __libasr_index_0_29 = __libasr_index_0_29 + 1
end do
__libasr_index_0_31 = lbound(__libasr_created__intrinsic_elemental_function_Sqrt1, 1)
do __libasr_index_0_30 = lbound(y, 1), ubound(y, 1)
    y(__libasr_index_0_30) = Sqrt(__libasr_created__intrinsic_elemental_function_Sqrt1(__libasr_index_0_31))
    __libasr_index_0_31 = __libasr_index_0_31 + 1
end do
if (element_calls /= 4) then
    error stop
end if
__libasr_index_0_33 = lbound(y, 1)
__libasr_index_0_34 = lbound(__libasr_created_real_binop_right_4, 1)
do __libasr_index_0_32 = lbound(__libasr_created__intrinsic_elemental_function_Abs4, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs4, 1)
    __libasr_created__intrinsic_elemental_function_Abs4(__libasr_index_0_32) = y(__libasr_index_0_33) - __libasr_created&
        _real_binop_right_4(__libasr_index_0_34)
    __libasr_index_0_33 = __libasr_index_0_33 + 1
    __libasr_index_0_34 = __libasr_index_0_34 + 1
end do
__libasr_index_0_36 = lbound(__libasr_created__intrinsic_elemental_function_Abs4, 1)
do __libasr_index_0_35 = lbound(__libasr_created__intrinsic_array_function_Any4, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any4, 1)
    __libasr_created__intrinsic_array_function_Any4(__libasr_index_0_35) = Abs(__libasr_created__intrinsic_elemental_fun&
        ction_Abs4(__libasr_index_0_36)) > 9.99999997e-07
    __libasr_index_0_36 = __libasr_index_0_36 + 1
end do
__libasr_created__intrinsic_array_function_Any13 = Any(__libasr_created__intrinsic_array_function_Any4)
if (__libasr_created__intrinsic_array_function_Any13) then
    error stop
end if
a(1) = 0.00000000e+00
a(2) = 1.00000000e+00
a(3) = 2.00000000e+00
a(4) = 3.00000000e+00
a(5) = 4.00000000e+00
a(6) = 5.00000000e+00
deallocate(__libasr_created_real_binop_left_1)
allocate(__libasr_created_real_binop_left_1((5 - 1)/1 + 1))
__libasr_created__array_section_pointer_ => a(1:5)
! CheckBounds LHS = __libasr_created_real_binop_left_1, RHS = __libasr_created__array_section_pointer_, 
__libasr_index_0_38 = lbound(__libasr_created__array_section_pointer_, 1)
do __libasr_index_0_37 = lbound(__libasr_created_real_binop_left_1, 1), ubound(__libasr_created_real_binop_left_1, 1)
    __libasr_created_real_binop_left_1(__libasr_index_0_37) = __libasr_created__array_section_pointer_(__libasr_index_0_&
        38)
    __libasr_index_0_38 = __libasr_index_0_38 + 1
end do
! CheckBounds LHS = a(2:6), RHS = __libasr_created_real_binop_left_1, 
__libasr_index_0_40 = lbound(__libasr_created_real_binop_left_1, 1)
do __libasr_index_0_39 = 2, 6
    a(__libasr_index_0_39) = Sqrt(__libasr_created_real_binop_left_1(__libasr_index_0_40) + 1.00000000e+00)
    __libasr_index_0_40 = __libasr_index_0_40 + 1
end do
__libasr_index_0_42 = lbound(a, 1)
__libasr_index_0_43 = lbound(__libasr_created_real_binop_right_5, 1)
do __libasr_index_0_41 = lbound(__libasr_created__intrinsic_elemental_function_Abs5, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs5, 1)
    __libasr_created__intrinsic_elemental_function_Abs5(__libasr_index_0_41) = a(__libasr_index_0_42) - __libasr_created&
        _real_binop_right_5(__libasr_index_0_43)
    __libasr_index_0_42 = __libasr_index_0_42 + 1
    __libasr_index_0_43 = __libasr_index_0_43 + 1
end do
__libasr_index_0_45 = lbound(__libasr_created__intrinsic_elemental_function_Abs5, 1)
do __libasr_index_0_44 = lbound(__libasr_created__intrinsic_array_function_Any5, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any5, 1)
    __libasr_created__intrinsic_array_function_Any5(__libasr_index_0_44) = Abs(__libasr_created__intrinsic_elemental_fun&
        ction_Abs5(__libasr_index_0_45)) > 9.99999997e-07
    __libasr_index_0_45 = __libasr_index_0_45 + 1
end do
__libasr_created__intrinsic_array_function_Any14 = Any(__libasr_created__intrinsic_array_function_Any5)
if (__libasr_created__intrinsic_array_function_Any14) then
    error stop
end if
__libasr_created__subroutine_call_e_sqrt => x(4:1:-1)
call e_sqrt(__libasr_created__subroutine_call_e_sqrt, y, 1.00000000e+00)
__libasr_index_0_47 = lbound(y, 1)
__libasr_index_0_48 = lbound(__libasr_created_real_binop_right_6, 1)
do __libasr_index_0_46 = lbound(__libasr_created__intrinsic_elemental_function_Abs6, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs6, 1)
    __libasr_created__intrinsic_elemental_function_Abs6(__libasr_index_0_46) = y(__libasr_index_0_47) - __libasr_created&
        _real_binop_right_6(__libasr_index_0_48)
    __libasr_index_0_47 = __libasr_index_0_47 + 1
    __libasr_index_0_48 = __libasr_index_0_48 + 1
end do
__libasr_index_0_50 = lbound(__libasr_created__intrinsic_elemental_function_Abs6, 1)
do __libasr_index_0_49 = lbound(__libasr_created__intrinsic_array_function_Any6, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any6, 1)
    __libasr_created__intrinsic_array_function_Any6(__libasr_index_0_49) = Abs(__libasr_created__intrinsic_elemental_fun&
        ction_Abs6(__libasr_index_0_50)) > 9.99999997e-07
    __libasr_index_0_50 = __libasr_index_0_50 + 1
end do
__libasr_created__intrinsic_array_function_Any15 = Any(__libasr_created__intrinsic_array_function_Any6)
if (__libasr_created__intrinsic_array_function_Any15) then
    error stop
end if
__libasr_created__array_reshape_shape(1) = 2
__libasr_created__array_reshape_shape(2) = 2
call e_rank2(__libasr_created__subroutine_call_e_rank2, b)
__libasr_created__array_reshape_shape1(1) = 2
__libasr_created__array_reshape_shape1(2) = 2
__libasr_index_1_1 = lbound(b, 2)
__libasr_index_1_2 = lbound(__libasr_created_real_binop_right_7, 2)
do __libasr_index_1_ = lbound(__libasr_created__intrinsic_elemental_function_Abs7, 2),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs7, 2)
    __libasr_index_0_52 = lbound(b, 1)
    __libasr_index_0_53 = lbound(__libasr_created_real_binop_right_7, 1)
    do __libasr_index_0_51 = lbound(__libasr_created__intrinsic_elemental_function_Abs7, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs7, 1)
        __libasr_created__intrinsic_elemental_function_Abs7(__libasr_index_0_51,&
         __libasr_index_1_) = b(__libasr_index_0_52,&
         __libasr_index_1_1) - __libasr_created_real_binop_right_7(__libasr_index_0_53, __libasr_index_1_2)
        __libasr_index_0_52 = __libasr_index_0_52 + 1
        __libasr_index_0_53 = __libasr_index_0_53 + 1
    end do
    __libasr_index_1_1 = __libasr_index_1_1 + 1
    __libasr_index_1_2 = __libasr_index_1_2 + 1
end do
__libasr_index_1_4 = lbound(__libasr_created__intrinsic_elemental_function_Abs7, 2)
do __libasr_index_1_3 = lbound(__libasr_created__intrinsic_array_function_Any7, 2),&
         ubound(__libasr_created__intrinsic_array_function_Any7, 2)
    __libasr_index_0_55 = lbound(__libasr_created__intrinsic_elemental_function_Abs7, 1)
    do __libasr_index_0_54 = lbound(__libasr_created__intrinsic_array_function_Any7, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any7, 1)
        __libasr_created__intrinsic_array_function_Any7(__libasr_index_0_54,&
         __libasr_index_1_3) = Abs(__libasr_created__intrinsic_elemental_function_Abs7(__libasr_index_0_55,&
         __libasr_index_1_4)) > 9.99999997e-07
        __libasr_index_0_55 = __libasr_index_0_55 + 1
    end do
    __libasr_index_1_4 = __libasr_index_1_4 + 1
end do
__libasr_created__intrinsic_array_function_Any16 = Any(__libasr_created__intrinsic_array_function_Any7)
if (__libasr_created__intrinsic_array_function_Any16) then
    error stop
end if
do __libasr_index_0_56 = lbound(y, 1), ubound(y, 1)
    y(__libasr_index_0_56) = -1.00000000e+00
end do
__libasr_index_0_58 = lbound(x, 1)
do __libasr_index_0_57 = lbound(__libasr_created__intrinsic_elemental_function_Sqrt2, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Sqrt2, 1)
    __libasr_created__intrinsic_elemental_function_Sqrt2(__libasr_index_0_57) = x(__libasr_index_0_58) - 1.00000000e+00
    __libasr_index_0_58 = __libasr_index_0_58 + 1
end do
where (x > 0.00000000e+00)
    __libasr_index_0_60 = lbound(__libasr_created__intrinsic_elemental_function_Sqrt2, 1)
    do __libasr_index_0_59 = lbound(y, 1), ubound(y, 1)
        y(__libasr_index_0_59) = Sqrt(__libasr_created__intrinsic_elemental_function_Sqrt2(__libasr_index_0_60))
        __libasr_index_0_60 = __libasr_index_0_60 + 1
    end do
end where
__libasr_index_0_62 = lbound(y, 1)
__libasr_index_0_63 = lbound(__libasr_created_real_binop_right_8, 1)
do __libasr_index_0_61 = lbound(__libasr_created__intrinsic_elemental_function_Abs8, 1),&
         ubound(__libasr_created__intrinsic_elemental_function_Abs8, 1)
    __libasr_created__intrinsic_elemental_function_Abs8(__libasr_index_0_61) = y(__libasr_index_0_62) - __libasr_created&
        _real_binop_right_8(__libasr_index_0_63)
    __libasr_index_0_62 = __libasr_index_0_62 + 1
    __libasr_index_0_63 = __libasr_index_0_63 + 1
end do
__libasr_index_0_65 = lbound(__libasr_created__intrinsic_elemental_function_Abs8, 1)
do __libasr_index_0_64 = lbound(__libasr_created__intrinsic_array_function_Any8, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any8, 1)
    __libasr_created__intrinsic_array_function_Any8(__libasr_index_0_64) = Abs(__libasr_created__intrinsic_elemental_fun&
        ction_Abs8(__libasr_index_0_65)) > 9.99999997e-07
    __libasr_index_0_65 = __libasr_index_0_65 + 1
end do
__libasr_created__intrinsic_array_function_Any17 = Any(__libasr_created__intrinsic_array_function_Any8)
if (__libasr_created__intrinsic_array_function_Any17) then
    error stop
end if
allocate(empty(0), result(0))
call e_exp(empty, result)
if (size(result) /= 0) then
    error stop
end if
end program elemental_28
