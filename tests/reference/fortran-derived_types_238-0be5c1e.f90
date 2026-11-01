module derived_types_238_g
implicit none
type :: tw
    real(4), dimension(3) :: w
end type tw
type(tw) :: g

contains

function f() result(rr)
    real(4), dimension(3) :: rr
    rr(3:1:-1) = g%w + real(1, kind=4)
end function f

function f1() result(rr)
    real(4), dimension(3) :: rr
    call fill1(rr)
end function f1

subroutine fill1(d)
    real(4), dimension(3), intent(out) :: d
    d(3:1:-1) = g%w + real(1, kind=4)
end subroutine fill1

end module derived_types_238_g

module derived_types_238_m
implicit none
type :: inner
    real(4), dimension(:), allocatable :: x
end type inner
type :: t
    real(4), dimension(:), allocatable :: b
    type(inner) :: in
    real(4), dimension(3) :: w
    real(4), dimension(:), allocatable :: x
    real(4), dimension(:), allocatable :: y
    real(4), dimension(:, :), allocatable :: z
end type t

contains

subroutine accumulate(self, dw)
    real(4), dimension(:), intent(in) :: dw
    type(t), intent(inout) :: self
    self%b = self%b + dw
end subroutine accumulate

subroutine add(a, c)
    type(t), intent(in) :: a
    real(4), dimension(:), intent(out) :: c
    c = a%x + a%y
end subroutine add

subroutine add_col(a, j)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    type(t), intent(inout) :: a
    integer(4), intent(in) :: j
    __libasr_created__array_section_ => a%z(lbound(a%z, 1):ubound(a%z, 1), j)
    __libasr_created__array_section_ = a%x + a%y
end subroutine add_col

subroutine add_poly(a, c)
    type(t), intent(in) :: a
    real(4), dimension(:), intent(out) :: c
    c = a%x*a%y
end subroutine add_poly

subroutine cmp(a, m)
    type(t), intent(in) :: a
    logical(4), dimension(:), intent(out) :: m
    m = a%x > 2.00000000e+00
end subroutine cmp

subroutine element(arr, i, c)
    type(t), dimension(:), intent(in) :: arr
    real(4), dimension(:), intent(out) :: c
    integer(4), intent(in) :: i
    c = arr(i)%x*arr(2)%y
end subroutine element

subroutine fixed(a, c)
    type(t), intent(in) :: a
    real(4), dimension(:), intent(out) :: c
    c = a%w + real(1, kind=4)
end subroutine fixed

subroutine neg(a, c)
    type(t), intent(in) :: a
    real(4), dimension(:), intent(out) :: c
    c = -a%x
end subroutine neg

subroutine nested(a, c)
    type(t), intent(in) :: a
    real(4), dimension(:), intent(out) :: c
    c = a%in%x + real(1, kind=4)
end subroutine nested

subroutine reversed(a, n)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), allocatable :: __libasr_created__assignment_value_
    type(t), intent(inout) :: a
    integer(4), intent(in) :: n
    __libasr_created__array_section_ => a%x(n:1:-1)
    deallocate(__libasr_created__assignment_value_)
    allocate(__libasr_created__assignment_value_(size(a%x, 1)))
    __libasr_created__assignment_value_ = a%x + real(1, kind=4)
    __libasr_created__array_section_ = __libasr_created__assignment_value_
end subroutine reversed

subroutine shifted(a, n)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    real(4), dimension(:), allocatable :: __libasr_created_real_binop_left_
    real(4), dimension(:), pointer :: __libasr_created_real_binop_right_
    type(t), intent(inout) :: a
    integer(4), intent(in) :: n
    deallocate(__libasr_created_real_binop_left_)
    allocate(__libasr_created_real_binop_left_((n - 1 - 1)/1 + 1))
    __libasr_created__array_section_pointer_ => a%x(1:n - 1)
    __libasr_created_real_binop_left_ = __libasr_created__array_section_pointer_
    __libasr_created_real_binop_right_ => a%y(1:n - 1)
    __libasr_created__array_section_ => a%x(2:n)
    __libasr_created__array_section_ = __libasr_created_real_binop_left_ + __libasr_created_real_binop_right_
end subroutine shifted

subroutine where_mask(a, c)
    type(t), intent(inout) :: a
    real(4), dimension(:), intent(inout) :: c
    where (a%x > 2.00000000e+00)
        a%x = -a%x
        c = a%x
    end where
end subroutine where_mask

end module derived_types_238_m

program derived_types_238
use derived_types_238_m, only: accumulate
use derived_types_238_m, only: add
use derived_types_238_m, only: add_col
use derived_types_238_m, only: add_poly
use derived_types_238_m, only: cmp
use derived_types_238_m, only: element
use derived_types_238_g, only: f
use derived_types_238_g, only: f1
use derived_types_238_g, only: fill1
use derived_types_238_m, only: fixed
use derived_types_238_g, only: g
use derived_types_238_m, only: inner
use derived_types_238_m, only: neg
use derived_types_238_m, only: nested
use derived_types_238_m, only: reversed
use derived_types_238_m, only: shifted
use derived_types_238_m, only: t
use derived_types_238_g, only: tw
use derived_types_238_m, only: where_mask
implicit none
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any1
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any10
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any11
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any12
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any13
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any14
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any15
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any16
logical(4) :: __libasr_created__intrinsic_array_function_Any17
logical(4) :: __libasr_created__intrinsic_array_function_Any18
logical(4) :: __libasr_created__intrinsic_array_function_Any19
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any2
logical(4) :: __libasr_created__intrinsic_array_function_Any20
logical(4) :: __libasr_created__intrinsic_array_function_Any21
logical(4) :: __libasr_created__intrinsic_array_function_Any22
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
logical(4) :: __libasr_created__intrinsic_array_function_Any4
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any5
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any6
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any7
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any8
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any9
real(4), dimension(3), parameter :: __libasr_created__subroutine_call_accumulate = [1.00000000e+00, 2.00000000e+00,&
         3.00000000e+00]
logical(4), dimension(3), parameter :: __libasr_created_logical_binop_right_ = [.false., .false., .true.]
real(4), dimension(:), pointer :: __libasr_created_real_compare_left_
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_ = [1.10000000e+01, 2.20000000e+01,&
         3.30000000e+01]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_1 = [1.00000000e+01, 4.00000000e+01,&
         9.00000000e+01]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_10 = [4.00000000e+00, 3.00000000e+00,&
         2.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_11 = [1.00000000e+00, 2.00000000e+00,&
         -3.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_12 = [0.00000000e+00, 0.00000000e+00,&
         -3.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_13 = [4.00000000e+00, 3.00000000e+00,&
         2.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_14 = [4.00000000e+00, 3.00000000e+00,&
         2.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_2 = [2.00000000e+00, 3.00000000e+00,&
         4.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_3 = [1.10000000e+01, 2.20000000e+01,&
         3.30000000e+01]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_4 = [-1.00000000e+00, -2.00000000e+00,&
         -3.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_5 = [8.00000000e+00, 9.00000000e+00,&
         1.00000000e+01]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_6 = [5.00000000e+00, 6.00000000e+00,&
         7.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_7 = [3.00000000e+00, 6.00000000e+00,&
         9.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_8 = [3.00000000e+00, 4.00000000e+00,&
         5.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_9 = [1.00000000e+00, 1.10000000e+01,&
         2.20000000e+01]
type(t) :: a
type(t), dimension(2) :: arr
real(4), dimension(3) :: c
logical(4), dimension(3) :: m
allocate(a%x(3), a%y(3), a%b(3), a%z(3, 2), a%in%x(3))
a%x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
a%y = [1.00000000e+01, 2.00000000e+01, 3.00000000e+01]
a%b = [1.00000000e+00, 1.00000000e+00, 1.00000000e+00]
a%z = real(0, kind=4)
a%w = [4.00000000e+00, 5.00000000e+00, 6.00000000e+00]
a%in%x = [7.00000000e+00, 8.00000000e+00, 9.00000000e+00]
call add(a, c)
__libasr_created__intrinsic_array_function_Any = c /= __libasr_created_real_compare_right_
__libasr_created__intrinsic_array_function_Any17 = Any(__libasr_created__intrinsic_array_function_Any)
if (__libasr_created__intrinsic_array_function_Any17) then
    error stop
end if
call add_poly(a, c)
__libasr_created__intrinsic_array_function_Any1 = c /= __libasr_created_real_compare_right_1
__libasr_created__intrinsic_array_function_Any18 = Any(__libasr_created__intrinsic_array_function_Any1)
if (__libasr_created__intrinsic_array_function_Any18) then
    error stop
end if
call accumulate(a, __libasr_created__subroutine_call_accumulate)
deallocate(__libasr_created__intrinsic_array_function_Any2)
allocate(__libasr_created__intrinsic_array_function_Any2(size(a%b, 1)))
__libasr_created__intrinsic_array_function_Any2 = a%b /= __libasr_created_real_compare_right_2
__libasr_created__intrinsic_array_function_Any19 = Any(__libasr_created__intrinsic_array_function_Any2)
if (__libasr_created__intrinsic_array_function_Any19) then
    error stop
end if
call add_col(a, 2)
__libasr_created_real_compare_left_ => a%z(lbound(a%z, 1):ubound(a%z, 1), 2)
deallocate(__libasr_created__intrinsic_array_function_Any3)
allocate(__libasr_created__intrinsic_array_function_Any3(size(__libasr_created_real_compare_left_, 1)))
__libasr_created__intrinsic_array_function_Any3 = __libasr_created_real_compare_left_ /= __libasr_created_real_compare_r&
        ight_3
__libasr_created__intrinsic_array_function_Any20 = Any(__libasr_created__intrinsic_array_function_Any3)
if (__libasr_created__intrinsic_array_function_Any20) then
    error stop
end if
__libasr_created__intrinsic_array_function_Any4 = Any(a%z(lbound(a%z, 1):ubound(a%z, 1), 1) /= real(0, kind=4))
if (__libasr_created__intrinsic_array_function_Any4) then
    error stop
end if
call neg(a, c)
__libasr_created__intrinsic_array_function_Any5 = c /= __libasr_created_real_compare_right_4
__libasr_created__intrinsic_array_function_Any21 = Any(__libasr_created__intrinsic_array_function_Any5)
if (__libasr_created__intrinsic_array_function_Any21) then
    error stop
end if
call cmp(a, m)
__libasr_created__intrinsic_array_function_Any6 = m .neqv. __libasr_created_logical_binop_right_
__libasr_created__intrinsic_array_function_Any22 = Any(__libasr_created__intrinsic_array_function_Any6)
if (__libasr_created__intrinsic_array_function_Any22) then
    error stop
end if
call nested(a, c)
__libasr_created__intrinsic_array_function_Any7 = c /= __libasr_created_real_compare_right_5
__libasr_created__intrinsic_array_function_Any23 = Any(__libasr_created__intrinsic_array_function_Any7)
if (__libasr_created__intrinsic_array_function_Any23) then
    error stop
end if
call fixed(a, c)
__libasr_created__intrinsic_array_function_Any8 = c /= __libasr_created_real_compare_right_6
__libasr_created__intrinsic_array_function_Any24 = Any(__libasr_created__intrinsic_array_function_Any8)
if (__libasr_created__intrinsic_array_function_Any24) then
    error stop
end if
allocate(arr(1)%x(3), arr(1)%y(3), arr(2)%x(3), arr(2)%y(3))
arr(1)%x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
arr(1)%y = [4.00000000e+00, 5.00000000e+00, 6.00000000e+00]
arr(2)%x = [2.00000000e+00, 2.00000000e+00, 2.00000000e+00]
arr(2)%y = [3.00000000e+00, 3.00000000e+00, 3.00000000e+00]
call element(arr, 1, c)
__libasr_created__intrinsic_array_function_Any9 = c /= __libasr_created_real_compare_right_7
__libasr_created__intrinsic_array_function_Any25 = Any(__libasr_created__intrinsic_array_function_Any9)
if (__libasr_created__intrinsic_array_function_Any25) then
    error stop
end if
arr(2)%x = arr(1)%x + arr(2)%x
deallocate(__libasr_created__intrinsic_array_function_Any10)
allocate(__libasr_created__intrinsic_array_function_Any10(size(arr(2)%x, 1)))
__libasr_created__intrinsic_array_function_Any10 = arr(2)%x /= __libasr_created_real_compare_right_8
__libasr_created__intrinsic_array_function_Any26 = Any(__libasr_created__intrinsic_array_function_Any10)
if (__libasr_created__intrinsic_array_function_Any26) then
    error stop
end if
call shifted(a, 3)
deallocate(__libasr_created__intrinsic_array_function_Any11)
allocate(__libasr_created__intrinsic_array_function_Any11(size(a%x, 1)))
__libasr_created__intrinsic_array_function_Any11 = a%x /= __libasr_created_real_compare_right_9
__libasr_created__intrinsic_array_function_Any27 = Any(__libasr_created__intrinsic_array_function_Any11)
if (__libasr_created__intrinsic_array_function_Any27) then
    error stop
end if
a%x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call reversed(a, 3)
deallocate(__libasr_created__intrinsic_array_function_Any12)
allocate(__libasr_created__intrinsic_array_function_Any12(size(a%x, 1)))
__libasr_created__intrinsic_array_function_Any12 = a%x /= __libasr_created_real_compare_right_10
__libasr_created__intrinsic_array_function_Any28 = Any(__libasr_created__intrinsic_array_function_Any12)
if (__libasr_created__intrinsic_array_function_Any28) then
    error stop
end if
a%x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
c = real(0, kind=4)
call where_mask(a, c)
deallocate(__libasr_created__intrinsic_array_function_Any13)
allocate(__libasr_created__intrinsic_array_function_Any13(size(a%x, 1)))
__libasr_created__intrinsic_array_function_Any13 = a%x /= __libasr_created_real_compare_right_11
__libasr_created__intrinsic_array_function_Any29 = Any(__libasr_created__intrinsic_array_function_Any13)
if (__libasr_created__intrinsic_array_function_Any29) then
    error stop
end if
__libasr_created__intrinsic_array_function_Any14 = c /= __libasr_created_real_compare_right_12
__libasr_created__intrinsic_array_function_Any30 = Any(__libasr_created__intrinsic_array_function_Any14)
if (__libasr_created__intrinsic_array_function_Any30) then
    error stop
end if
g%w = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
g%w = f()
__libasr_created__intrinsic_array_function_Any15 = g%w /= __libasr_created_real_compare_right_13
__libasr_created__intrinsic_array_function_Any31 = Any(__libasr_created__intrinsic_array_function_Any15)
if (__libasr_created__intrinsic_array_function_Any31) then
    error stop
end if
g%w = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
g%w = f1()
__libasr_created__intrinsic_array_function_Any16 = g%w /= __libasr_created_real_compare_right_14
__libasr_created__intrinsic_array_function_Any32 = Any(__libasr_created__intrinsic_array_function_Any16)
if (__libasr_created__intrinsic_array_function_Any32) then
    error stop
end if
print *, "ok"
end program derived_types_238
