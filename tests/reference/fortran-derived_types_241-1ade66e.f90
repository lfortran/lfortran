module derived_types_241_m
implicit none
type :: t
    real(4), dimension(3) :: w
    real(4), dimension(:), allocatable :: x
    real(4), dimension(:, :), allocatable :: z
end type t
type :: outer
    type(t) :: in
end type outer
type(t) :: g
type(t), target :: gt

contains

subroutine elements(u, v, i, j)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created_real_binop_left_
    integer(4), intent(in) :: i
    integer(4), intent(in) :: j
    type(t), dimension(:), intent(in) :: u
    type(t), dimension(:), intent(inout) :: v
    __libasr_created_real_binop_left_ => u(j)%x(1:2)
    __libasr_created__array_section_ => v(i)%x(2:3)
    __libasr_created__array_section_ = __libasr_created_real_binop_left_ - real(1, kind=4)
end subroutine elements

subroutine fixed_add(a, c)
    type(t), intent(in) :: a
    type(t), intent(out) :: c
    c%w = a%w + real(1, kind=4)
end subroutine fixed_add

subroutine fixed_sec(a, c)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created_real_binop_left_
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    __libasr_created_real_binop_left_ => a%w(2:3)
    __libasr_created__array_section_ => c%w(1:2)
    __libasr_created__array_section_ = __libasr_created_real_binop_left_ + real(1, kind=4)
end subroutine fixed_sec

subroutine from_module(c)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    type(t), intent(inout) :: c
    __libasr_created__array_section_ => c%x(1:2)
    __libasr_created__array_section_pointer_ => g%x(2:3)
    __libasr_created__array_section_ = __libasr_created__array_section_pointer_
end subroutine from_module

subroutine module_pointer()
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    real(4), dimension(:), allocatable :: __libasr_created_real_binop_left_
    type(t), pointer :: p
    p => gt
    deallocate(__libasr_created_real_binop_left_)
    allocate(__libasr_created_real_binop_left_((2 - 1)/1 + 1))
    __libasr_created__array_section_pointer_ => p%x(1:2)
    __libasr_created_real_binop_left_ = __libasr_created__array_section_pointer_
    __libasr_created__array_section_ => gt%x(2:3)
    __libasr_created__array_section_ = __libasr_created_real_binop_left_ + real(1, kind=4)
end subroutine module_pointer

subroutine nested(a, c)
    type(outer), intent(in) :: a
    type(outer), intent(inout) :: c
    c%in%x = a%in%x*real(2, kind=4)
end subroutine nested

subroutine same_array(v, i, j)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    real(4), dimension(:), allocatable :: __libasr_created_real_binop_left_
    integer(4), intent(in) :: i
    integer(4), intent(in) :: j
    type(t), dimension(:), intent(inout) :: v
    deallocate(__libasr_created_real_binop_left_)
    allocate(__libasr_created_real_binop_left_((2 - 1)/1 + 1))
    __libasr_created__array_section_pointer_ => v(j)%x(1:2)
    __libasr_created_real_binop_left_ = __libasr_created__array_section_pointer_
    __libasr_created__array_section_ => v(i)%x(2:3)
    __libasr_created__array_section_ = __libasr_created_real_binop_left_*real(10, kind=4)
end subroutine same_array

subroutine same_array_fixed(v, i, j)
    real(4), dimension(:), allocatable :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    integer(4), intent(in) :: i
    integer(4), intent(in) :: j
    type(t), dimension(:), intent(inout) :: v
    deallocate(__libasr_created__array_section_)
    allocate(__libasr_created__array_section_(3))
    __libasr_created__array_section_pointer_ => v(j)%w(3:1:-1)
    __libasr_created__array_section_ = __libasr_created__array_section_pointer_
    v(i)%w = __libasr_created__array_section_
end subroutine same_array_fixed

subroutine same_object(c)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), allocatable :: __libasr_created__array_section_1
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    type(t), intent(inout) :: c
    __libasr_created__array_section_ => c%x(2:3)
    deallocate(__libasr_created__array_section_1)
    allocate(__libasr_created__array_section_1((2 - 1)/1 + 1))
    __libasr_created__array_section_pointer_ => c%x(1:2)
    __libasr_created__array_section_1 = __libasr_created__array_section_pointer_
    __libasr_created__array_section_ = __libasr_created__array_section_1
end subroutine same_object

subroutine sec_add(a, c, n)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created_real_binop_left_
    real(4), dimension(:), pointer :: __libasr_created_real_binop_right_
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    integer(4), intent(in) :: n
    __libasr_created_real_binop_left_ => a%x(1:n)
    __libasr_created_real_binop_right_ => a%z(1:n, 1)
    __libasr_created__array_section_ => c%x(1:n)
    __libasr_created__array_section_ = __libasr_created_real_binop_left_ + __libasr_created_real_binop_right_
end subroutine sec_add

subroutine sec_copy(a, c, n)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    integer(4), intent(in) :: n
    __libasr_created__array_section_ => c%x(1:n)
    __libasr_created__array_section_pointer_ => a%x(1:n)
    __libasr_created__array_section_ = __libasr_created__array_section_pointer_
end subroutine sec_copy

subroutine sec_shift(a, c)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    __libasr_created__array_section_ => c%x(2:3)
    __libasr_created__array_section_pointer_ => a%x(1:2)
    __libasr_created__array_section_ = __libasr_created__array_section_pointer_
end subroutine sec_shift

subroutine targets(a, c)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    real(4), dimension(:), allocatable :: __libasr_created_real_binop_left_
    type(t), intent(inout), target :: a
    type(t), intent(inout), target :: c
    deallocate(__libasr_created_real_binop_left_)
    allocate(__libasr_created_real_binop_left_((2 - 1)/1 + 1))
    __libasr_created__array_section_pointer_ => a%x(1:2)
    __libasr_created_real_binop_left_ = __libasr_created__array_section_pointer_
    __libasr_created__array_section_ => c%x(2:3)
    __libasr_created__array_section_ = __libasr_created_real_binop_left_ + real(1, kind=4)
end subroutine targets

subroutine targets_fixed(a, c)
    real(4), dimension(:), allocatable :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    type(t), intent(inout), target :: a
    type(t), intent(inout), target :: c
    deallocate(__libasr_created__array_section_)
    allocate(__libasr_created__array_section_(3))
    __libasr_created__array_section_pointer_ => a%w(3:1:-1)
    __libasr_created__array_section_ = __libasr_created__array_section_pointer_
    c%w = __libasr_created__array_section_
end subroutine targets_fixed

subroutine through_pointer(p, c)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), allocatable :: __libasr_created__array_section_1
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    type(t), intent(inout), target :: c
    type(t), pointer, intent(in) :: p
    __libasr_created__array_section_ => c%x(2:3)
    deallocate(__libasr_created__array_section_1)
    allocate(__libasr_created__array_section_1((2 - 1)/1 + 1))
    __libasr_created__array_section_pointer_ => p%x(1:2)
    __libasr_created__array_section_1 = __libasr_created__array_section_pointer_
    __libasr_created__array_section_ = __libasr_created__array_section_1
end subroutine through_pointer

subroutine to_module(c)
    type(t), intent(in) :: c
    g%x = c%x + real(1, kind=4)
end subroutine to_module

subroutine whole_copy(a, c)
    type(t), intent(in) :: a
    type(t), intent(inout) :: c
    c%x = a%x
end subroutine whole_copy

end module derived_types_241_m

program derived_types_241
use derived_types_241_m, only: elements
use derived_types_241_m, only: fixed_add
use derived_types_241_m, only: fixed_sec
use derived_types_241_m, only: from_module
use derived_types_241_m, only: g
use derived_types_241_m, only: gt
use derived_types_241_m, only: module_pointer
use derived_types_241_m, only: nested
use derived_types_241_m, only: outer
use derived_types_241_m, only: same_array
use derived_types_241_m, only: same_array_fixed
use derived_types_241_m, only: same_object
use derived_types_241_m, only: sec_add
use derived_types_241_m, only: sec_copy
use derived_types_241_m, only: sec_shift
use derived_types_241_m, only: t
use derived_types_241_m, only: targets
use derived_types_241_m, only: targets_fixed
use derived_types_241_m, only: through_pointer
use derived_types_241_m, only: to_module
use derived_types_241_m, only: whole_copy
implicit none
real(4), dimension(3), parameter :: __libasr_created__array_reshape_array = [1.00000000e+01, 2.00000000e+01,&
         3.00000000e+01]
integer(4), dimension(2) :: __libasr_created__array_reshape_shape
real(4), dimension(:), pointer :: __libasr_created__array_section_
real(4), dimension(:), allocatable :: __libasr_created__array_section_1
real(4), dimension(:), allocatable :: __libasr_created__array_section_2
real(4), dimension(:), allocatable :: __libasr_created__array_section_3
real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_1
real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_2
real(4), dimension(:), allocatable :: __libasr_created__assignment_value_
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any1
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any10
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any11
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any12
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any13
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any14
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any15
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any16
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any17
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any18
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any19
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any2
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any20
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any21
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any22
logical(4) :: __libasr_created__intrinsic_array_function_Any23
logical(4) :: __libasr_created__intrinsic_array_function_Any24
logical(4) :: __libasr_created__intrinsic_array_function_Any25
logical(4) :: __libasr_created__intrinsic_array_function_Any26
logical(4) :: __libasr_created__intrinsic_array_function_Any27
logical(4) :: __libasr_created__intrinsic_array_function_Any28
logical(4) :: __libasr_created__intrinsic_array_function_Any29
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any3
logical(4) :: __libasr_created__intrinsic_array_function_Any30
logical(4) :: __libasr_created__intrinsic_array_function_Any31
logical(4) :: __libasr_created__intrinsic_array_function_Any32
logical(4) :: __libasr_created__intrinsic_array_function_Any33
logical(4) :: __libasr_created__intrinsic_array_function_Any34
logical(4) :: __libasr_created__intrinsic_array_function_Any35
logical(4) :: __libasr_created__intrinsic_array_function_Any36
logical(4) :: __libasr_created__intrinsic_array_function_Any37
logical(4) :: __libasr_created__intrinsic_array_function_Any38
logical(4) :: __libasr_created__intrinsic_array_function_Any39
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any4
logical(4) :: __libasr_created__intrinsic_array_function_Any40
logical(4) :: __libasr_created__intrinsic_array_function_Any41
logical(4) :: __libasr_created__intrinsic_array_function_Any42
logical(4) :: __libasr_created__intrinsic_array_function_Any43
logical(4) :: __libasr_created__intrinsic_array_function_Any44
logical(4) :: __libasr_created__intrinsic_array_function_Any45
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any5
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any6
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any7
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any8
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any9
real(4), dimension(:), allocatable :: __libasr_created_real_compare_left_
real(4), dimension(3) :: __libasr_created_real_compare_left_1
real(4), dimension(:), allocatable :: __libasr_created_real_compare_left_2
real(4), dimension(:), allocatable :: __libasr_created_real_compare_left_3
real(4), dimension(3) :: __libasr_created_real_compare_left_4
real(4), dimension(:), allocatable :: __libasr_created_real_compare_left_5
real(4), dimension(:), allocatable :: __libasr_created_real_compare_left_6
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_ = [5.00000000e+00, 6.00000000e+00,&
         7.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_1 = [6.00000000e+00, 7.00000000e+00,&
         7.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_10 = [0.00000000e+00, 3.00000000e+00,&
         4.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_11 = [1.00000000e+00, 1.00000000e+00,&
         2.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_12 = [1.00000000e+00, 1.00000000e+01,&
         2.00000000e+01]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_13 = [4.00000000e+00, 1.00000000e+01,&
         1.00000000e+02]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_14 = [3.00000000e+00, 2.00000000e+00,&
         1.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_15 = [1.00000000e+00, 2.00000000e+00,&
         3.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_16 = [1.00000000e+00, 2.00000000e+00,&
         1.10000000e+01]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_17 = [3.00000000e+00, 2.00000000e+00,&
         1.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_18 = [1.00000000e+00, 1.00000000e+00,&
         2.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_19 = [3.00000000e+00, 2.00000000e+00,&
         1.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_2 = [1.10000000e+01, 2.20000000e+01,&
         0.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_20 = [3.00000000e+00, 2.00000000e+00,&
         1.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_21 = [1.00000000e+00, 1.00000000e+00,&
         2.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_22 = [1.00000000e+00, 2.00000000e+00,&
         1.10000000e+01]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_3 = [1.00000000e+00, 2.20000000e+01,&
         0.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_4 = [1.00000000e+00, 1.00000000e+00,&
         2.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_5 = [1.00000000e+00, 2.00000000e+00,&
         3.00000000e+00]
real(4), dimension(2), parameter :: __libasr_created_real_compare_right_6 = [6.00000000e+00, 8.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_7 = [2.00000000e+00, 3.00000000e+00,&
         4.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_8 = [3.00000000e+00, 4.00000000e+00,&
         3.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_9 = [0.00000000e+00, 3.00000000e+00,&
         4.00000000e+00]
type(t) :: a
type(t) :: c
type(outer) :: oa
type(outer) :: oc
type(t), pointer :: p
type(t), target :: tg
type(t), dimension(2) :: u
type(t), dimension(2) :: v
allocate(a%x(3), a%z(3, 1))
a%x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
__libasr_created__array_reshape_shape = [3, 1]
a%z = reshape(__libasr_created__array_reshape_array, __libasr_created__array_reshape_shape)
a%w = [4.00000000e+00, 5.00000000e+00, 6.00000000e+00]
call fixed_add(a, c)
__libasr_created__intrinsic_array_function_Any = c%w /= __libasr_created_real_compare_right_
__libasr_created__intrinsic_array_function_Any23 = Any(__libasr_created__intrinsic_array_function_Any)
if (__libasr_created__intrinsic_array_function_Any23) then
    error stop
end if
call fixed_sec(a, c)
__libasr_created__intrinsic_array_function_Any1 = c%w /= __libasr_created_real_compare_right_1
__libasr_created__intrinsic_array_function_Any24 = Any(__libasr_created__intrinsic_array_function_Any1)
if (__libasr_created__intrinsic_array_function_Any24) then
    error stop
end if
allocate(c%x(3))
c%x = real(0, kind=4)
call sec_add(a, c, 2)
deallocate(__libasr_created__intrinsic_array_function_Any2)
allocate(__libasr_created__intrinsic_array_function_Any2(size(c%x, 1)))
__libasr_created__intrinsic_array_function_Any2 = c%x /= __libasr_created_real_compare_right_2
__libasr_created__intrinsic_array_function_Any25 = Any(__libasr_created__intrinsic_array_function_Any2)
if (__libasr_created__intrinsic_array_function_Any25) then
    error stop
end if
call sec_copy(a, c, 1)
deallocate(__libasr_created__intrinsic_array_function_Any3)
allocate(__libasr_created__intrinsic_array_function_Any3(size(c%x, 1)))
__libasr_created__intrinsic_array_function_Any3 = c%x /= __libasr_created_real_compare_right_3
__libasr_created__intrinsic_array_function_Any26 = Any(__libasr_created__intrinsic_array_function_Any3)
if (__libasr_created__intrinsic_array_function_Any26) then
    error stop
end if
call sec_shift(a, c)
deallocate(__libasr_created__intrinsic_array_function_Any4)
allocate(__libasr_created__intrinsic_array_function_Any4(size(c%x, 1)))
__libasr_created__intrinsic_array_function_Any4 = c%x /= __libasr_created_real_compare_right_4
__libasr_created__intrinsic_array_function_Any27 = Any(__libasr_created__intrinsic_array_function_Any4)
if (__libasr_created__intrinsic_array_function_Any27) then
    error stop
end if
call whole_copy(a, c)
deallocate(__libasr_created__intrinsic_array_function_Any5)
allocate(__libasr_created__intrinsic_array_function_Any5(size(c%x, 1)))
__libasr_created__intrinsic_array_function_Any5 = c%x /= __libasr_created_real_compare_right_5
__libasr_created__intrinsic_array_function_Any28 = Any(__libasr_created__intrinsic_array_function_Any5)
if (__libasr_created__intrinsic_array_function_Any28) then
    error stop
end if
allocate(oa%in%x(2), oc%in%x(2))
oa%in%x = [3.00000000e+00, 4.00000000e+00]
call nested(oa, oc)
deallocate(__libasr_created__intrinsic_array_function_Any6)
allocate(__libasr_created__intrinsic_array_function_Any6(size(oc%in%x, 1)))
__libasr_created__intrinsic_array_function_Any6 = oc%in%x /= __libasr_created_real_compare_right_6
__libasr_created__intrinsic_array_function_Any29 = Any(__libasr_created__intrinsic_array_function_Any6)
if (__libasr_created__intrinsic_array_function_Any29) then
    error stop
end if
allocate(g%x(3))
call to_module(c)
deallocate(__libasr_created__intrinsic_array_function_Any7)
allocate(__libasr_created__intrinsic_array_function_Any7(size(g%x, 1)))
__libasr_created__intrinsic_array_function_Any7 = g%x /= __libasr_created_real_compare_right_7
__libasr_created__intrinsic_array_function_Any30 = Any(__libasr_created__intrinsic_array_function_Any7)
if (__libasr_created__intrinsic_array_function_Any30) then
    error stop
end if
call from_module(c)
deallocate(__libasr_created__intrinsic_array_function_Any8)
allocate(__libasr_created__intrinsic_array_function_Any8(size(c%x, 1)))
__libasr_created__intrinsic_array_function_Any8 = c%x /= __libasr_created_real_compare_right_8
__libasr_created__intrinsic_array_function_Any31 = Any(__libasr_created__intrinsic_array_function_Any8)
if (__libasr_created__intrinsic_array_function_Any31) then
    error stop
end if
allocate(u(1)%x(3), u(2)%x(3), v(1)%x(3), v(2)%x(3))
u(1)%x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
u(2)%x = [4.00000000e+00, 5.00000000e+00, 6.00000000e+00]
v(1)%x = real(0, kind=4)
v(2)%x = real(0, kind=4)
call elements(u, v, 2, 2)
deallocate(__libasr_created__intrinsic_array_function_Any9)
allocate(__libasr_created__intrinsic_array_function_Any9(size(v(2)%x, 1)))
__libasr_created__intrinsic_array_function_Any9 = v(2)%x /= __libasr_created_real_compare_right_9
__libasr_created__intrinsic_array_function_Any32 = Any(__libasr_created__intrinsic_array_function_Any9)
if (__libasr_created__intrinsic_array_function_Any32) then
    error stop
end if
call elements(u, v, 1, 2)
deallocate(__libasr_created__intrinsic_array_function_Any10)
allocate(__libasr_created__intrinsic_array_function_Any10(size(v(1)%x, 1)))
__libasr_created__intrinsic_array_function_Any10 = v(1)%x /= __libasr_created_real_compare_right_10
__libasr_created__intrinsic_array_function_Any33 = Any(__libasr_created__intrinsic_array_function_Any10)
if (__libasr_created__intrinsic_array_function_Any33) then
    error stop
end if
c%x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call same_object(c)
deallocate(__libasr_created__intrinsic_array_function_Any11)
allocate(__libasr_created__intrinsic_array_function_Any11(size(c%x, 1)))
__libasr_created__intrinsic_array_function_Any11 = c%x /= __libasr_created_real_compare_right_11
__libasr_created__intrinsic_array_function_Any34 = Any(__libasr_created__intrinsic_array_function_Any11)
if (__libasr_created__intrinsic_array_function_Any34) then
    error stop
end if
v(1)%x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
v(2)%x = [4.00000000e+00, 5.00000000e+00, 6.00000000e+00]
call same_array(v, 1, 1)
deallocate(__libasr_created__intrinsic_array_function_Any12)
allocate(__libasr_created__intrinsic_array_function_Any12(size(v(1)%x, 1)))
__libasr_created__intrinsic_array_function_Any12 = v(1)%x /= __libasr_created_real_compare_right_12
__libasr_created__intrinsic_array_function_Any35 = Any(__libasr_created__intrinsic_array_function_Any12)
if (__libasr_created__intrinsic_array_function_Any35) then
    error stop
end if
call same_array(v, 2, 1)
deallocate(__libasr_created__intrinsic_array_function_Any13)
allocate(__libasr_created__intrinsic_array_function_Any13(size(v(2)%x, 1)))
__libasr_created__intrinsic_array_function_Any13 = v(2)%x /= __libasr_created_real_compare_right_13
__libasr_created__intrinsic_array_function_Any36 = Any(__libasr_created__intrinsic_array_function_Any13)
if (__libasr_created__intrinsic_array_function_Any36) then
    error stop
end if
v(1)%w = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
v(2)%w = [4.00000000e+00, 5.00000000e+00, 6.00000000e+00]
call same_array_fixed(v, 1, 1)
__libasr_created__intrinsic_array_function_Any14 = v(1)%w /= __libasr_created_real_compare_right_14
__libasr_created__intrinsic_array_function_Any37 = Any(__libasr_created__intrinsic_array_function_Any14)
if (__libasr_created__intrinsic_array_function_Any37) then
    error stop
end if
call same_array_fixed(v, 2, 1)
__libasr_created__intrinsic_array_function_Any15 = v(2)%w /= __libasr_created_real_compare_right_15
__libasr_created__intrinsic_array_function_Any38 = Any(__libasr_created__intrinsic_array_function_Any15)
if (__libasr_created__intrinsic_array_function_Any38) then
    error stop
end if
allocate(tg%x(3))
tg%x = [1.00000000e+00, 1.00000000e+01, 1.00000000e+02]
call targets(tg, tg)
deallocate(__libasr_created_real_compare_left_)
allocate(__libasr_created_real_compare_left_(size(tg%x, 1)))
__libasr_created_real_compare_left_ = tg%x
deallocate(__libasr_created__intrinsic_array_function_Any16)
allocate(__libasr_created__intrinsic_array_function_Any16(size(__libasr_created_real_compare_left_, 1)))
__libasr_created__intrinsic_array_function_Any16 = __libasr_created_real_compare_left_ /= __libasr_created_real_compare_&
        right_16
__libasr_created__intrinsic_array_function_Any39 = Any(__libasr_created__intrinsic_array_function_Any16)
if (__libasr_created__intrinsic_array_function_Any39) then
    error stop
end if
tg%w = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call targets_fixed(tg, tg)
__libasr_created_real_compare_left_1 = tg%w
__libasr_created__intrinsic_array_function_Any17 = __libasr_created_real_compare_left_1 /= __libasr_created_real_compare&
        _right_17
__libasr_created__intrinsic_array_function_Any40 = Any(__libasr_created__intrinsic_array_function_Any17)
if (__libasr_created__intrinsic_array_function_Any40) then
    error stop
end if
p => tg
tg%x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
__libasr_created__array_section_ => tg%x(2:3)
deallocate(__libasr_created__array_section_1)
allocate(__libasr_created__array_section_1((2 - 1)/1 + 1))
__libasr_created__array_section_pointer_ => p%x(1:2)
__libasr_created__array_section_1 = __libasr_created__array_section_pointer_
__libasr_created__array_section_ = __libasr_created__array_section_1
deallocate(__libasr_created_real_compare_left_2)
allocate(__libasr_created_real_compare_left_2(size(tg%x, 1)))
__libasr_created_real_compare_left_2 = tg%x
deallocate(__libasr_created__intrinsic_array_function_Any18)
allocate(__libasr_created__intrinsic_array_function_Any18(size(__libasr_created_real_compare_left_2, 1)))
__libasr_created__intrinsic_array_function_Any18 = __libasr_created_real_compare_left_2 /= __libasr_created_real_compare&
        _right_18
__libasr_created__intrinsic_array_function_Any41 = Any(__libasr_created__intrinsic_array_function_Any18)
if (__libasr_created__intrinsic_array_function_Any41) then
    error stop
end if
tg%x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
deallocate(__libasr_created__array_section_2)
allocate(__libasr_created__array_section_2(3))
__libasr_created__array_section_pointer_1 => p%x(3:1:-1)
__libasr_created__array_section_2 = __libasr_created__array_section_pointer_1
deallocate(__libasr_created__assignment_value_)
allocate(__libasr_created__assignment_value_(size(__libasr_created__array_section_2)))
__libasr_created__assignment_value_ = __libasr_created__array_section_2
tg%x = __libasr_created__assignment_value_
deallocate(__libasr_created_real_compare_left_3)
allocate(__libasr_created_real_compare_left_3(size(tg%x, 1)))
__libasr_created_real_compare_left_3 = tg%x
deallocate(__libasr_created__intrinsic_array_function_Any19)
allocate(__libasr_created__intrinsic_array_function_Any19(size(__libasr_created_real_compare_left_3, 1)))
__libasr_created__intrinsic_array_function_Any19 = __libasr_created_real_compare_left_3 /= __libasr_created_real_compare&
        _right_19
__libasr_created__intrinsic_array_function_Any42 = Any(__libasr_created__intrinsic_array_function_Any19)
if (__libasr_created__intrinsic_array_function_Any42) then
    error stop
end if
tg%w = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
deallocate(__libasr_created__array_section_3)
allocate(__libasr_created__array_section_3(3))
__libasr_created__array_section_pointer_2 => p%w(3:1:-1)
__libasr_created__array_section_3 = __libasr_created__array_section_pointer_2
tg%w = __libasr_created__array_section_3
__libasr_created_real_compare_left_4 = tg%w
__libasr_created__intrinsic_array_function_Any20 = __libasr_created_real_compare_left_4 /= __libasr_created_real_compare&
        _right_20
__libasr_created__intrinsic_array_function_Any43 = Any(__libasr_created__intrinsic_array_function_Any20)
if (__libasr_created__intrinsic_array_function_Any43) then
    error stop
end if
tg%x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call through_pointer(p, tg)
deallocate(__libasr_created_real_compare_left_5)
allocate(__libasr_created_real_compare_left_5(size(tg%x, 1)))
__libasr_created_real_compare_left_5 = tg%x
deallocate(__libasr_created__intrinsic_array_function_Any21)
allocate(__libasr_created__intrinsic_array_function_Any21(size(__libasr_created_real_compare_left_5, 1)))
__libasr_created__intrinsic_array_function_Any21 = __libasr_created_real_compare_left_5 /= __libasr_created_real_compare&
        _right_21
__libasr_created__intrinsic_array_function_Any44 = Any(__libasr_created__intrinsic_array_function_Any21)
if (__libasr_created__intrinsic_array_function_Any44) then
    error stop
end if
allocate(gt%x(3))
gt%x = [1.00000000e+00, 1.00000000e+01, 1.00000000e+02]
call module_pointer()
deallocate(__libasr_created_real_compare_left_6)
allocate(__libasr_created_real_compare_left_6(size(gt%x, 1)))
__libasr_created_real_compare_left_6 = gt%x
deallocate(__libasr_created__intrinsic_array_function_Any22)
allocate(__libasr_created__intrinsic_array_function_Any22(size(__libasr_created_real_compare_left_6, 1)))
__libasr_created__intrinsic_array_function_Any22 = __libasr_created_real_compare_left_6 /= __libasr_created_real_compare&
        _right_22
__libasr_created__intrinsic_array_function_Any45 = Any(__libasr_created__intrinsic_array_function_Any22)
if (__libasr_created__intrinsic_array_function_Any45) then
    error stop
end if
print *, "ok"
end program derived_types_241
