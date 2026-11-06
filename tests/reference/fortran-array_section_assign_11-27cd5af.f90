module array_section_assign_11_mod
implicit none

contains

subroutine shift(a, x, i, m)
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    integer(4) :: __libasr_created__array_section_subscript_
    integer(4) :: __libasr_created__array_section_subscript_1
    real(4), dimension(:), allocatable :: __libasr_created_real_binop_left_
    real(4), dimension(:), pointer :: __libasr_created_real_binop_right_
    real(4), dimension(:, :), intent(inout) :: a
    integer(4), intent(in) :: i
    integer(4), intent(in) :: m
    real(4), dimension(:), intent(in) :: x
    deallocate(__libasr_created_real_binop_left_)
    allocate(__libasr_created_real_binop_left_((m - 1 - 1)/1 + 1))
    __libasr_created__array_section_pointer_ => a(i, 1:m - 1)
    __libasr_created_real_binop_left_ = __libasr_created__array_section_pointer_
    __libasr_created_real_binop_right_ => x(1:m - 1)
    __libasr_created__array_section_subscript_ = i
    __libasr_created__array_section_subscript_1 = m
    a(__libasr_created__array_section_subscript_,&
         2:__libasr_created__array_section_subscript_1) = __libasr_created_real_binop_left_ + __libasr_created_real_binop_right&
        _
end subroutine shift

subroutine upd(a, x, i, m)
    integer(4) :: __libasr_created__array_section_subscript_
    integer(4) :: __libasr_created__array_section_subscript_1
    real(4), dimension(:), pointer :: __libasr_created_real_binop_left_
    real(4), dimension(:), pointer :: __libasr_created_real_binop_left_1
    real(4), dimension(:, :), intent(inout) :: a
    integer(4), intent(in) :: i
    integer(4), intent(in) :: m
    real(4), dimension(:), intent(in) :: x
    __libasr_created_real_binop_left_ => a(i, 1:m)
    __libasr_created_real_binop_left_1 => x(1:m)
    __libasr_created__array_section_subscript_ = i
    __libasr_created__array_section_subscript_1 = m
    a(__libasr_created__array_section_subscript_,&
         1:__libasr_created__array_section_subscript_1) = __libasr_created_real_binop_left_ + __libasr_created_real_binop_left_&
        1*2.00000000e+00
    a = a + 1.00000000e+00
end subroutine upd

end module array_section_assign_11_mod

program array_section_assign_11
use array_section_assign_11_mod, only: shift
use array_section_assign_11_mod, only: upd
implicit none
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any
logical(4) :: __libasr_created__intrinsic_array_function_Any1
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any2
logical(4) :: __libasr_created__intrinsic_array_function_Any3
logical(4) :: __libasr_created__intrinsic_array_function_Any4
real(4), dimension(:), pointer :: __libasr_created_real_compare_left_
real(4), dimension(:), pointer :: __libasr_created_real_compare_left_1
real(4), dimension(4), parameter :: __libasr_created_real_compare_right_ = [4.00000000e+00, 6.00000000e+00,&
         8.00000000e+00, 2.00000000e+00]
real(4), dimension(4), parameter :: __libasr_created_real_compare_right_1 = [4.00000000e+00, 5.00000000e+00,&
         8.00000000e+00, 1.10000000e+01]
real(4), dimension(2, 4) :: a
real(4), dimension(4) :: x
a = 1.00000000e+00
x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00, 4.00000000e+00]
call upd(a, x, 1, 3)
__libasr_created_real_compare_left_ => a(1, lbound(a, 2):ubound(a, 2))
deallocate(__libasr_created__intrinsic_array_function_Any)
allocate(__libasr_created__intrinsic_array_function_Any(size(__libasr_created_real_compare_left_, 1)))
__libasr_created__intrinsic_array_function_Any = __libasr_created_real_compare_left_ /= __libasr_created_real_compare_ri&
        ght_
__libasr_created__intrinsic_array_function_Any3 = Any(__libasr_created__intrinsic_array_function_Any)
if (__libasr_created__intrinsic_array_function_Any3) then
    error stop
end if
__libasr_created__intrinsic_array_function_Any1 = Any(a(2, lbound(a, 2):ubound(a, 2)) /= 2.00000000e+00)
if (__libasr_created__intrinsic_array_function_Any1) then
    error stop
end if
call shift(a, x, 1, 4)
__libasr_created_real_compare_left_1 => a(1, lbound(a, 2):ubound(a, 2))
deallocate(__libasr_created__intrinsic_array_function_Any2)
allocate(__libasr_created__intrinsic_array_function_Any2(size(__libasr_created_real_compare_left_1, 1)))
__libasr_created__intrinsic_array_function_Any2 = __libasr_created_real_compare_left_1 /= __libasr_created_real_compare_&
        right_1
__libasr_created__intrinsic_array_function_Any4 = Any(__libasr_created__intrinsic_array_function_Any2)
if (__libasr_created__intrinsic_array_function_Any4) then
    error stop
end if
print *, a
end program array_section_assign_11
