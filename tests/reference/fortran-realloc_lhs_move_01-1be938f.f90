module realloc_lhs_move_01_m
implicit none
type :: tensor_t
    real(4), dimension(:), allocatable :: values_
end type tensor_t

contains

pure subroutine numbers(n, r)
    integer(4) :: __1__libasr_index_
    real(4), dimension(:), allocatable :: __libasr__created__var__7__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: i
    integer(4) :: lfortran_implied_do_i
    integer(4), intent(in) :: n
    real(4), dimension(:), allocatable, intent(out) :: r
    if (Allocated(r)) then
        deallocate(r)
    end if
    deallocate(__libasr__created__var__7__array_constructor_)
    allocate(__libasr__created__var__7__array_constructor_(1*n))
    __1__libasr_index_ = lbound(__libasr__created__var__7__array_constructor_, 1)
    do lfortran_implied_do_i = 1, n
        __libasr__created__var__7__array_constructor_(__1__libasr_index_) = _lcompilers_real_i32(lfortran_implied_do_i)
        __1__libasr_index_ = __1__libasr_index_ + 1
    end do
    allocate(r(size(__libasr__created__var__7__array_constructor_, 1)))
    ! CheckBounds LHS = r, RHS = __libasr__created__var__7__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr__created__var__7__array_constructor_, 1)
    do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_) = __libasr__created__var__7__array_constructor_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine numbers

pure subroutine tensors(n, t)
    integer(4) :: __1__libasr_index_
    real(4), dimension(2) :: __libasr__created__var__8__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: i
    integer(4), intent(in) :: n
    type(tensor_t), dimension(:), allocatable, intent(out) :: t
    if (Allocated(t)) then
        deallocate(t)
    end if
    allocate(t(n))
    do i = 1, n
        __1__libasr_index_ = lbound(__libasr__created__var__8__array_constructor_, 1)
        __libasr__created__var__8__array_constructor_(__1__libasr_index_) = _lcompilers_real_i321(i)
        __1__libasr_index_ = __1__libasr_index_ + 1
        __libasr__created__var__8__array_constructor_(__1__libasr_index_) = _lcompilers_real_i322(2*i)
        __1__libasr_index_ = __1__libasr_index_ + 1
        reallocate(t(i)%values_(size(__libasr__created__var__8__array_constructor_, 1)))
        ! CheckBounds LHS = t(i)%values_, RHS = __libasr__created__var__8__array_constructor_, 
        __libasr_index_0_1 = lbound(__libasr__created__var__8__array_constructor_, 1)
        do __libasr_index_0_ = lbound(t(i)%values_, 1), ubound(t(i)%values_, 1)
            t(i)%values_(__libasr_index_0_) = __libasr__created__var__8__array_constructor_(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    end do
end subroutine tensors

end module realloc_lhs_move_01_m

program realloc_lhs_move_01
use realloc_lhs_move_01_m, only: numbers
use realloc_lhs_move_01_m, only: tensor_t
use realloc_lhs_move_01_m, only: tensors
implicit none
integer(4) :: __1_k
integer(4) :: __1_k1
integer(4) :: __1_k2
integer(4) :: __1_k3
integer(4) :: __1_k4
real(4), dimension(2) :: __libasr__created__var__0__array_constant_
real(4), dimension(2) :: __libasr__created__var__1__array_constant_
real(4), dimension(2) :: __libasr__created__var__2__array_constant_
real(4), dimension(2) :: __libasr__created__var__5__array_constant_
real(4), dimension(3) :: __libasr__created__var__6__array_constant_
real(4), dimension(1), parameter :: __libasr_array_constant_ = [0.00000000e+00]
real(4), dimension(1), parameter :: __libasr_array_constant_1 = [0.00000000e+00]
logical(4) :: __libasr_created__intrinsic_array_function_Any
logical(4) :: __libasr_created__intrinsic_array_function_Any1
logical(4) :: __libasr_created__intrinsic_array_function_Any2
logical(4) :: __libasr_created__intrinsic_array_function_Any3
logical(4) :: __libasr_created__intrinsic_array_function_Any4
type(tensor_t), dimension(:), allocatable :: __libasr_created__subroutine_from_function_
type(tensor_t), dimension(:), allocatable :: __libasr_created__subroutine_from_function_1
type(tensor_t), dimension(:), allocatable :: __libasr_created__subroutine_from_function_2
type(tensor_t), dimension(:), allocatable :: __libasr_created__subroutine_from_function_3
real(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_4
real(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_5
real(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_6
real(4), dimension(:), allocatable :: __libasr_created_real_compare_left_
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
integer(4) :: __libasr_index_0_4
integer(4) :: __libasr_index_0_5
integer(4) :: __libasr_index_0_6
integer(4) :: __libasr_index_0_7
integer(4) :: __libasr_index_0_8
integer(4) :: __libasr_index_0_9
logical(4) :: __libasr_reduction_acc
logical(4) :: __libasr_reduction_acc1
logical(4) :: __libasr_reduction_acc2
logical(4) :: __libasr_reduction_acc3
logical(4) :: __libasr_reduction_acc4
type(tensor_t), dimension(:), allocatable :: a
type(tensor_t), dimension(:), allocatable, target :: b
integer(4) :: k
type(tensor_t), pointer :: p
real(4) :: s
real(4), dimension(:), allocatable :: x
deallocate(__libasr_created__subroutine_from_function_) ! Implicit deallocate
call tensors(3, __libasr_created__subroutine_from_function_)
if (Allocated(a)) then
    if (size(a, 1) == size(__libasr_created__subroutine_from_function_, 1) .and. lbound(a, 1) /= 1) then
        reallocate(a(size(__libasr_created__subroutine_from_function_, 1)))
        ! CheckBounds LHS = a, RHS = __libasr_created__subroutine_from_function_, 
        __libasr_index_0_17 = lbound(__libasr_created__subroutine_from_function_, 1)
        do __libasr_index_0_16 = lbound(a, 1), ubound(a, 1)
            a(__libasr_index_0_16) = __libasr_created__subroutine_from_function_(__libasr_index_0_17)
            __libasr_index_0_17 = __libasr_index_0_17 + 1
        end do
    else
        deallocate(a) ! Implicit deallocate
        a = __libasr_created__subroutine_from_function_
    end if
else
    deallocate(a) ! Implicit deallocate
    a = __libasr_created__subroutine_from_function_
end if
if (size(a) /= 3 .or. lbound(a, 1) /= 1) then
    error stop
end if
__1_k = 1
__libasr__created__var__0__array_constant_(__1_k) = 3.00000000e+00
__1_k = 2
__libasr__created__var__0__array_constant_(__1_k) = 6.00000000e+00
__libasr_reduction_acc = .false.
! CheckBounds LHS = a(3)%values_, RHS = a(3)%values_, __libasr__created__var__0__array_constant_, 
__libasr_index_0_1 = lbound(__libasr__created__var__0__array_constant_, 1)
do __libasr_index_0_ = lbound(a(3)%values_, 1), ubound(a(3)%values_, 1)
    __libasr_reduction_acc = __libasr_reduction_acc .or. a(3)%values_(__libasr_index_0_) /= __libasr__created__var__0__a&
        rray_constant_(__libasr_index_0_1)
    __libasr_index_0_1 = __libasr_index_0_1 + 1
end do
__libasr_created__intrinsic_array_function_Any = __libasr_reduction_acc
if (__libasr_created__intrinsic_array_function_Any) then
    error stop
end if
deallocate(__libasr_created__subroutine_from_function_1) ! Implicit deallocate
call tensors(2, __libasr_created__subroutine_from_function_1)
if (Allocated(a)) then
    if (size(a, 1) == size(__libasr_created__subroutine_from_function_1, 1) .and. lbound(a, 1) /= 1) then
        reallocate(a(size(__libasr_created__subroutine_from_function_1, 1)))
        ! CheckBounds LHS = a, RHS = __libasr_created__subroutine_from_function_1, 
        __libasr_index_0_19 = lbound(__libasr_created__subroutine_from_function_1, 1)
        do __libasr_index_0_18 = lbound(a, 1), ubound(a, 1)
            a(__libasr_index_0_18) = __libasr_created__subroutine_from_function_1(__libasr_index_0_19)
            __libasr_index_0_19 = __libasr_index_0_19 + 1
        end do
    else
        deallocate(a) ! Implicit deallocate
        a = __libasr_created__subroutine_from_function_1
    end if
else
    deallocate(a) ! Implicit deallocate
    a = __libasr_created__subroutine_from_function_1
end if
__1_k1 = 1
__libasr__created__var__1__array_constant_(__1_k1) = 2.00000000e+00
__1_k1 = 2
__libasr__created__var__1__array_constant_(__1_k1) = 4.00000000e+00
__libasr_reduction_acc1 = .false.
! CheckBounds LHS = a(2)%values_, RHS = a(2)%values_, __libasr__created__var__1__array_constant_, 
__libasr_index_0_3 = lbound(__libasr__created__var__1__array_constant_, 1)
do __libasr_index_0_2 = lbound(a(2)%values_, 1), ubound(a(2)%values_, 1)
    __libasr_reduction_acc1 = __libasr_reduction_acc1 .or. a(2)%values_(__libasr_index_0_2) /= __libasr__created__var__1&
        __array_constant_(__libasr_index_0_3)
    __libasr_index_0_3 = __libasr_index_0_3 + 1
end do
__libasr_created__intrinsic_array_function_Any1 = __libasr_reduction_acc1
if (size(a) /= 2 .or. __libasr_created__intrinsic_array_function_Any1) then
    error stop
end if
deallocate(a)
allocate(a(2))
deallocate(__libasr_created__subroutine_from_function_2) ! Implicit deallocate
call tensors(2, __libasr_created__subroutine_from_function_2)
if (Allocated(a)) then
    if (size(a, 1) == size(__libasr_created__subroutine_from_function_2, 1) .and. lbound(a, 1) /= 1) then
        reallocate(a(size(__libasr_created__subroutine_from_function_2, 1)))
        ! CheckBounds LHS = a, RHS = __libasr_created__subroutine_from_function_2, 
        __libasr_index_0_21 = lbound(__libasr_created__subroutine_from_function_2, 1)
        do __libasr_index_0_20 = lbound(a, 1), ubound(a, 1)
            a(__libasr_index_0_20) = __libasr_created__subroutine_from_function_2(__libasr_index_0_21)
            __libasr_index_0_21 = __libasr_index_0_21 + 1
        end do
    else
        deallocate(a) ! Implicit deallocate
        a = __libasr_created__subroutine_from_function_2
    end if
else
    deallocate(a) ! Implicit deallocate
    a = __libasr_created__subroutine_from_function_2
end if
__1_k2 = 1
__libasr__created__var__2__array_constant_(__1_k2) = 2.00000000e+00
__1_k2 = 2
__libasr__created__var__2__array_constant_(__1_k2) = 4.00000000e+00
__libasr_reduction_acc2 = .false.
! CheckBounds LHS = a(1)%values_, RHS = a(1)%values_, __libasr__created__var__2__array_constant_, 
__libasr_index_0_5 = lbound(__libasr__created__var__2__array_constant_, 1)
do __libasr_index_0_4 = lbound(a(1)%values_, 1), ubound(a(1)%values_, 1)
    __libasr_reduction_acc2 = __libasr_reduction_acc2 .or. a(1)%values_(__libasr_index_0_4) /= __libasr__created__var__2&
        __array_constant_(__libasr_index_0_5)
    __libasr_index_0_5 = __libasr_index_0_5 + 1
end do
__libasr_created__intrinsic_array_function_Any2 = __libasr_reduction_acc2
if (lbound(a, 1) /= 0 .or. __libasr_created__intrinsic_array_function_Any2) then
    error stop
end if
allocate(b(2))
reallocate(b(1)%values_(size(__libasr_array_constant_, 1)))
! CheckBounds LHS = b(1)%values_, RHS = __libasr_array_constant_, 
__libasr_index_0_7 = lbound(__libasr_array_constant_, 1)
do __libasr_index_0_6 = lbound(b(1)%values_, 1), ubound(b(1)%values_, 1)
    b(1)%values_(__libasr_index_0_6) = __libasr_array_constant_(__libasr_index_0_7)
    __libasr_index_0_7 = __libasr_index_0_7 + 1
end do
reallocate(b(2)%values_(size(__libasr_array_constant_1, 1)))
! CheckBounds LHS = b(2)%values_, RHS = __libasr_array_constant_1, 
__libasr_index_0_9 = lbound(__libasr_array_constant_1, 1)
do __libasr_index_0_8 = lbound(b(2)%values_, 1), ubound(b(2)%values_, 1)
    b(2)%values_(__libasr_index_0_8) = __libasr_array_constant_1(__libasr_index_0_9)
    __libasr_index_0_9 = __libasr_index_0_9 + 1
end do
p => b(2)
deallocate(__libasr_created__subroutine_from_function_3) ! Implicit deallocate
call tensors(2, __libasr_created__subroutine_from_function_3)
if (Allocated(b)) then
    if (size(b, 1) == size(__libasr_created__subroutine_from_function_3, 1)) then
        reallocate(b(size(__libasr_created__subroutine_from_function_3, 1)))
        ! CheckBounds LHS = b, RHS = __libasr_created__subroutine_from_function_3, 
        __libasr_index_0_23 = lbound(__libasr_created__subroutine_from_function_3, 1)
        do __libasr_index_0_22 = lbound(b, 1), ubound(b, 1)
            b(__libasr_index_0_22) = __libasr_created__subroutine_from_function_3(__libasr_index_0_23)
            __libasr_index_0_23 = __libasr_index_0_23 + 1
        end do
    else
        deallocate(b) ! Implicit deallocate
        b = __libasr_created__subroutine_from_function_3
    end if
else
    deallocate(b) ! Implicit deallocate
    b = __libasr_created__subroutine_from_function_3
end if
__1_k3 = 1
__libasr__created__var__5__array_constant_(__1_k3) = 2.00000000e+00
__1_k3 = 2
__libasr__created__var__5__array_constant_(__1_k3) = 4.00000000e+00
deallocate(__libasr_created_real_compare_left_)
allocate(__libasr_created_real_compare_left_(size(p%values_, 1)))
reallocate(__libasr_created_real_compare_left_(size(p%values_, 1)))
! CheckBounds LHS = __libasr_created_real_compare_left_, RHS = p%values_, 
__libasr_index_0_11 = lbound(p%values_, 1)
do __libasr_index_0_10 = lbound(__libasr_created_real_compare_left_, 1), ubound(__libasr_created_real_compare_left_, 1)
    __libasr_created_real_compare_left_(__libasr_index_0_10) = p%values_(__libasr_index_0_11)
    __libasr_index_0_11 = __libasr_index_0_11 + 1
end do
__libasr_reduction_acc3 = .false.
! CheckBounds LHS = __libasr_created_real_compare_left_, RHS = __libasr_created_real_compare_left_,&
         __libasr__created__var__5__array_constant_, 
__libasr_index_0_13 = lbound(__libasr__created__var__5__array_constant_, 1)
do __libasr_index_0_12 = lbound(__libasr_created_real_compare_left_, 1), ubound(__libasr_created_real_compare_left_, 1)
    __libasr_reduction_acc3 = __libasr_reduction_acc3 .or. __libasr_created_real_compare_left_(__libasr_index_0_12) /= _&
        _libasr__created__var__5__array_constant_(__libasr_index_0_13)
    __libasr_index_0_13 = __libasr_index_0_13 + 1
end do
__libasr_created__intrinsic_array_function_Any3 = __libasr_reduction_acc3
if (__libasr_created__intrinsic_array_function_Any3) then
    error stop
end if
s = real(0, kind=4)
do k = 1, 4
    associate_block : associate ()
        type(tensor_t), dimension(:), allocatable :: __libasr_created__subroutine_from_function_
        integer(4) :: __libasr_index_0_
        integer(4) :: __libasr_index_0_1
        type(tensor_t), dimension(:), allocatable :: t
        deallocate(__libasr_created__subroutine_from_function_) ! Implicit deallocate
        call tensors(k, __libasr_created__subroutine_from_function_)
        if (Allocated(t)) then
            if (size(t, 1) == size(__libasr_created__subroutine_from_function_, 1) .and. lbound(t, 1) /= 1) then
                reallocate(t(size(__libasr_created__subroutine_from_function_, 1)))
                ! CheckBounds LHS = t, RHS = __libasr_created__subroutine_from_function_, 
                __libasr_index_0_1 = lbound(__libasr_created__subroutine_from_function_, 1)
                do __libasr_index_0_ = lbound(t, 1), ubound(t, 1)
                    t(__libasr_index_0_) = __libasr_created__subroutine_from_function_(__libasr_index_0_1)
                    __libasr_index_0_1 = __libasr_index_0_1 + 1
                end do
            else
                deallocate(t) ! Implicit deallocate
                t = __libasr_created__subroutine_from_function_
            end if
        else
            deallocate(t) ! Implicit deallocate
            t = __libasr_created__subroutine_from_function_
        end if
        s = s + t(k)%values_(2)
    end associate associate_block
end do
if (Abs(s - 2.00000000e+01) > 9.99999997e-07) then
    error stop
end if
deallocate(__libasr_created__subroutine_from_function_4) ! Implicit deallocate
call numbers(4, __libasr_created__subroutine_from_function_4)
if (Allocated(x)) then
    if (size(x, 1) == size(__libasr_created__subroutine_from_function_4, 1) .and. lbound(x, 1) /= 1) then
        reallocate(x(size(__libasr_created__subroutine_from_function_4, 1)))
        ! CheckBounds LHS = x, RHS = __libasr_created__subroutine_from_function_4, 
        __libasr_index_0_25 = lbound(__libasr_created__subroutine_from_function_4, 1)
        do __libasr_index_0_24 = lbound(x, 1), ubound(x, 1)
            x(__libasr_index_0_24) = __libasr_created__subroutine_from_function_4(__libasr_index_0_25)
            __libasr_index_0_25 = __libasr_index_0_25 + 1
        end do
    else
        deallocate(x) ! Implicit deallocate
        x = __libasr_created__subroutine_from_function_4
    end if
else
    deallocate(x) ! Implicit deallocate
    x = __libasr_created__subroutine_from_function_4
end if
deallocate(__libasr_created__subroutine_from_function_5) ! Implicit deallocate
call numbers(2, __libasr_created__subroutine_from_function_5)
if (Allocated(x)) then
    if (size(x, 1) == size(__libasr_created__subroutine_from_function_5, 1) .and. lbound(x, 1) /= 1) then
        reallocate(x(size(__libasr_created__subroutine_from_function_5, 1)))
        ! CheckBounds LHS = x, RHS = __libasr_created__subroutine_from_function_5, 
        __libasr_index_0_27 = lbound(__libasr_created__subroutine_from_function_5, 1)
        do __libasr_index_0_26 = lbound(x, 1), ubound(x, 1)
            x(__libasr_index_0_26) = __libasr_created__subroutine_from_function_5(__libasr_index_0_27)
            __libasr_index_0_27 = __libasr_index_0_27 + 1
        end do
    else
        deallocate(x) ! Implicit deallocate
        x = __libasr_created__subroutine_from_function_5
    end if
else
    deallocate(x) ! Implicit deallocate
    x = __libasr_created__subroutine_from_function_5
end if
if (size(x) /= 2 .or. lbound(x, 1) /= 1) then
    error stop
end if
deallocate(x)
allocate(x(3))
deallocate(__libasr_created__subroutine_from_function_6) ! Implicit deallocate
call numbers(3, __libasr_created__subroutine_from_function_6)
if (Allocated(x)) then
    if (size(x, 1) == size(__libasr_created__subroutine_from_function_6, 1) .and. lbound(x, 1) /= 1) then
        reallocate(x(size(__libasr_created__subroutine_from_function_6, 1)))
        ! CheckBounds LHS = x, RHS = __libasr_created__subroutine_from_function_6, 
        __libasr_index_0_29 = lbound(__libasr_created__subroutine_from_function_6, 1)
        do __libasr_index_0_28 = lbound(x, 1), ubound(x, 1)
            x(__libasr_index_0_28) = __libasr_created__subroutine_from_function_6(__libasr_index_0_29)
            __libasr_index_0_29 = __libasr_index_0_29 + 1
        end do
    else
        deallocate(x) ! Implicit deallocate
        x = __libasr_created__subroutine_from_function_6
    end if
else
    deallocate(x) ! Implicit deallocate
    x = __libasr_created__subroutine_from_function_6
end if
__1_k4 = 1
__libasr__created__var__6__array_constant_(__1_k4) = 1.00000000e+00
__1_k4 = 2
__libasr__created__var__6__array_constant_(__1_k4) = 2.00000000e+00
__1_k4 = 3
__libasr__created__var__6__array_constant_(__1_k4) = 3.00000000e+00
__libasr_reduction_acc4 = .false.
! CheckBounds LHS = x, RHS = x, __libasr__created__var__6__array_constant_, 
__libasr_index_0_15 = lbound(__libasr__created__var__6__array_constant_, 1)
do __libasr_index_0_14 = lbound(x, 1), ubound(x, 1)
    __libasr_reduction_acc4 = __libasr_reduction_acc4 .or. x(__libasr_index_0_14) /= __libasr__created__var__6__array_co&
        nstant_(__libasr_index_0_15)
    __libasr_index_0_15 = __libasr_index_0_15 + 1
end do
__libasr_created__intrinsic_array_function_Any4 = __libasr_reduction_acc4
if (lbound(x, 1) /= 0 .or. __libasr_created__intrinsic_array_function_Any4) then
    error stop
end if
print *, "ok"

contains

real(4) function _lcompilers_real_i32(x)
    integer(4), intent(in) :: x
    _lcompilers_real_i32 = real(x, kind=4)
end function _lcompilers_real_i32

real(4) function _lcompilers_real_i321(x)
    integer(4), intent(in) :: x
    _lcompilers_real_i321 = real(x, kind=4)
end function _lcompilers_real_i321

real(4) function _lcompilers_real_i322(x)
    integer(4), intent(in) :: x
    _lcompilers_real_i322 = real(x, kind=4)
end function _lcompilers_real_i322

end program realloc_lhs_move_01
