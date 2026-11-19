module realloc_lhs_result_inplace_01_m
implicit none
type :: box_t
    real(4), dimension(:), allocatable :: v
contains
    procedure :: __libasr_intent_out_reset => __libasr_intent_out_reset_box_t
end type box_t
type :: pair_t
    integer(4) :: k = 5
    real(4), dimension(:), allocatable :: w
contains
    procedure :: __libasr_intent_out_reset => __libasr_intent_out_reset_pair_t
end type pair_t

contains

pure subroutine c3(a, r)
    character(len=:, kind=1), allocatable :: __libasr__created__var__0_return_slot
    real(4), dimension(:), intent(in) :: a
    integer(4) :: __libasr_extent_r_1 = size(a)
    integer(4) :: i
    character(len=2, kind=1), dimension(__libasr_extent_r_1), intent(out) :: r
    do i = 1, size(a)
        deallocate(__libasr__created__var__0_return_slot)
        call _lcompilers_merge_str_l32("hi", "lo", a(i) > real(1, kind=4), __libasr__created__var__0_return_slot)
        r(i) = __libasr__created__var__0_return_slot
    end do
end subroutine c3

pure subroutine f3(a, r)
    real(4), dimension(:), intent(in) :: a
    integer(4) :: __libasr_extent_r_1 = size(a)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(__libasr_extent_r_1), intent(out) :: r
    ! CheckBounds LHS = r, RHS = a, 
    __libasr_index_0_1 = lbound(a, 1)
    do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_) = real(2, kind=4)*a(__libasr_index_0_1) + real(1, kind=4)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine f3

pure subroutine g3(a, r)
    real(4), dimension(:), intent(in) :: a
    integer(4) :: __libasr_extent_r_1 = size(a)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    logical(4), dimension(__libasr_extent_r_1), intent(out) :: r
    ! CheckBounds LHS = r, RHS = a, 
    __libasr_index_0_1 = lbound(a, 1)
    do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_) = a(__libasr_index_0_1) > real(1, kind=4)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine g3

subroutine pairs(n, r)
    integer(4) :: __1__libasr_index_
    integer(4) :: __1_intent_out_dealloc_idx_
    real(4), dimension(1) :: __libasr__created__var__11__array_constructor_
    integer(4), intent(in) :: n
    integer(4) :: __libasr_extent_r_1 = Max(0, n)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: i
    type(pair_t), dimension(__libasr_extent_r_1), intent(out) :: r
    do __1_intent_out_dealloc_idx_ = lbound(r, 1), ubound(r, 1)
        if (Allocated(r(__1_intent_out_dealloc_idx_)%w)) then
            deallocate(r(__1_intent_out_dealloc_idx_)%w)
        end if
        r(__1_intent_out_dealloc_idx_)%k = 5
    end do
    do i = 1, n
        if (Allocated(r(i)%w)) then
            error stop
        end if
        if (r(i)%k /= 5) then
            error stop
        end if
        __1__libasr_index_ = lbound(__libasr__created__var__11__array_constructor_, 1)
        __libasr__created__var__11__array_constructor_(__1__libasr_index_) = _lcompilers_real_i32(i)
        __1__libasr_index_ = __1__libasr_index_ + 1
        reallocate(r(i)%w(size(__libasr__created__var__11__array_constructor_, 1)))
        ! CheckBounds LHS = r(i)%w, RHS = __libasr__created__var__11__array_constructor_, 
        __libasr_index_0_1 = lbound(__libasr__created__var__11__array_constructor_, 1)
        do __libasr_index_0_ = lbound(r(i)%w, 1), ubound(r(i)%w, 1)
            r(i)%w(__libasr_index_0_) = __libasr__created__var__11__array_constructor_(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    end do
end subroutine pairs

pure subroutine rev(a, r)
    real(4), dimension(:), intent(in) :: a
    integer(4) :: __libasr_extent_r_1 = size(a)
    integer(4) :: i
    real(4), dimension(__libasr_extent_r_1), intent(out) :: r
    do i = 1, size(a)
        r(i) = a(size(a) + 1 - i)
    end do
end subroutine rev

subroutine with_internal(x)
    type :: __lcompilers_frame_with_internal
        real(4), dimension(:), allocatable, reference :: d
    end type __lcompilers_frame_with_internal
    type(__lcompilers_frame_with_internal) :: __lcompilers_frame
    real(4), dimension(:), pointer :: __libasr_created__function_call_f3
    integer(4) :: __libasr_created__result_metadata_
    integer(4) :: __libasr_created__result_metadata_1
    real(4), dimension(:), allocatable :: d
    real(4), dimension(:), intent(in) :: x
    bind_reference(__lcompilers_frame%d, d)
    __libasr_created__result_metadata_ = size(x)
    reallocate(d(__libasr_created__result_metadata_))
    call f3(x, d)
    call check(d)
    __libasr_created__result_metadata_1 = 1*(0 > (3 - 2 + 1)/1 ? 0 : (3 - 2 + 1)/1)
    __libasr_created__function_call_f3 => x(2:3)
    reallocate(d(__libasr_created__result_metadata_1))
    call f3(__libasr_created__function_call_f3, d)
    call check2(__lcompilers_frame)
    contains
    subroutine check(y)
        integer(4) :: __1_k
        real(4), dimension(3) :: __libasr__created__var__12__array_constant_
        logical(4) :: __libasr_created__intrinsic_array_function_Any
        integer(4) :: __libasr_index_0_
        integer(4) :: __libasr_index_0_1
        logical(4) :: __libasr_reduction_acc
        real(4), dimension(:), intent(in) :: y
        __1_k = 1
        __libasr__created__var__12__array_constant_(__1_k) = 3.00000000e+00
        __1_k = 2
        __libasr__created__var__12__array_constant_(__1_k) = 5.00000000e+00
        __1_k = 3
        __libasr__created__var__12__array_constant_(__1_k) = 7.00000000e+00
        __libasr_reduction_acc = .false.
        ! CheckBounds LHS = y, RHS = y, __libasr__created__var__12__array_constant_, 
        __libasr_index_0_1 = lbound(__libasr__created__var__12__array_constant_, 1)
        do __libasr_index_0_ = lbound(y, 1), ubound(y, 1)
            __libasr_reduction_acc = __libasr_reduction_acc .or. y(__libasr_index_0_) /= __libasr__created__var__12__arr&
        ay_constant_(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        __libasr_created__intrinsic_array_function_Any = __libasr_reduction_acc
        if (__libasr_created__intrinsic_array_function_Any) then
            error stop
        end if
    end subroutine check

    subroutine check2(__lcompilers_frame)
        integer(4) :: __1_k
        type(__lcompilers_frame_with_internal), intent(in) :: __lcompilers_frame
        real(4), dimension(2) :: __libasr__created__var__13__array_constant_
        logical(4) :: __libasr_created__intrinsic_array_function_Any
        integer(4) :: __libasr_index_0_
        integer(4) :: __libasr_index_0_1
        logical(4) :: __libasr_reduction_acc
        real(4), dimension(:), allocatable, host_associated :: d => __lcompilers_frame%d
        __1_k = 1
        __libasr__created__var__13__array_constant_(__1_k) = 5.00000000e+00
        __1_k = 2
        __libasr__created__var__13__array_constant_(__1_k) = 7.00000000e+00
        __libasr_reduction_acc = .false.
        ! CheckBounds LHS = d, RHS = d, __libasr__created__var__13__array_constant_, 
        __libasr_index_0_1 = lbound(__libasr__created__var__13__array_constant_, 1)
        do __libasr_index_0_ = lbound(d, 1), ubound(d, 1)
            __libasr_reduction_acc = __libasr_reduction_acc .or. d(__libasr_index_0_) /= __libasr__created__var__13__arr&
        ay_constant_(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        __libasr_created__intrinsic_array_function_Any = __libasr_reduction_acc
        if (size(d) /= 2 .or. __libasr_created__intrinsic_array_function_Any) then
            error stop
        end if
    end subroutine check2

end subroutine with_internal

end module realloc_lhs_result_inplace_01_m

program realloc_lhs_result_inplace_01
use realloc_lhs_result_inplace_01_m, only: box_t
use realloc_lhs_result_inplace_01_m, only: c3
use realloc_lhs_result_inplace_01_m, only: f3
use realloc_lhs_result_inplace_01_m, only: g3
use realloc_lhs_result_inplace_01_m, only: pair_t
use realloc_lhs_result_inplace_01_m, only: pairs
use realloc_lhs_result_inplace_01_m, only: rev
use realloc_lhs_result_inplace_01_m, only: with_internal
implicit none
integer(4) :: __1_k
integer(4) :: __1_k1
integer(4) :: __1_k2
integer(4) :: __1_k3
integer(4) :: __1_k4
integer(4) :: __1_k5
integer(4) :: __1_k6
real(4), dimension(2) :: __libasr__created__var__10__array_constant_
real(4), dimension(3) :: __libasr__created__var__1__array_constant_
real(4), dimension(2) :: __libasr__created__var__2__array_constant_
real(4), dimension(3) :: __libasr__created__var__3__array_constant_
logical(4), dimension(3) :: __libasr__created__var__4__array_constant_
character(len=2, kind=1), dimension(3) :: __libasr__created__var__5__array_constant_
real(4), dimension(3) :: __libasr__created__var__8__array_constant_
real(4), dimension(3), parameter :: __libasr_array_constant_ = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
real(4), dimension(2), parameter :: __libasr_array_constant_1 = [7.00000000e+00, 7.00000000e+00]
real(4), dimension(3), parameter :: __libasr_array_constant_2 = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
real(4), dimension(2), parameter :: __libasr_array_constant_3 = [1.00000000e+00, 2.00000000e+00]
real(4), dimension(:), pointer :: __libasr_created__function_call_f3
real(4), dimension(:), pointer :: __libasr_created__function_call_f31
real(4), dimension(:), pointer :: __libasr_created__function_call_g3
type(pair_t), dimension(2) :: __libasr_created__function_call_pairs
real(4), dimension(:), allocatable :: __libasr_created__function_call_rev
real(4), dimension(:), allocatable :: __libasr_created__function_call_rev1
logical(4) :: __libasr_created__intrinsic_array_function_Any
logical(4) :: __libasr_created__intrinsic_array_function_Any1
logical(4) :: __libasr_created__intrinsic_array_function_Any2
logical(4) :: __libasr_created__intrinsic_array_function_Any3
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any4
logical(4) :: __libasr_created__intrinsic_array_function_Any5
logical(4) :: __libasr_created__intrinsic_array_function_Any6
logical(4) :: __libasr_created__intrinsic_array_function_Any7
integer(4) :: __libasr_created__result_metadata_
integer(4) :: __libasr_created__result_metadata_1
integer(4) :: __libasr_created__result_metadata_2
integer(4) :: __libasr_created__result_metadata_3
integer(4) :: __libasr_created__result_metadata_4
integer(4) :: __libasr_created__result_metadata_5
integer(4) :: __libasr_created__result_metadata_6
integer(4) :: __libasr_created__result_metadata_7
integer(4) :: __libasr_created__result_metadata_8
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
logical(4) :: __libasr_reduction_acc5
real(4), dimension(:), allocatable :: a
type(box_t) :: b
real(4), dimension(:), allocatable :: e
logical(4), dimension(:), allocatable :: m
real(4), dimension(:), pointer :: p
type(pair_t), dimension(:), allocatable :: q
character(len=2, kind=1), dimension(:), allocatable :: s
real(4), dimension(:), allocatable, target :: t
allocate(a(size(__libasr_array_constant_, 1)))
! CheckBounds LHS = a, RHS = __libasr_array_constant_, 
__libasr_index_0_1 = lbound(__libasr_array_constant_, 1)
do __libasr_index_0_ = lbound(a, 1), ubound(a, 1)
    a(__libasr_index_0_) = __libasr_array_constant_(__libasr_index_0_1)
    __libasr_index_0_1 = __libasr_index_0_1 + 1
end do
__libasr_created__result_metadata_ = size(a)
reallocate(b%v(__libasr_created__result_metadata_))
call f3(a, b%v)
if (size(b%v) /= 3 .or. lbound(b%v, 1) /= 1) then
    error stop
end if
__1_k = 1
__libasr__created__var__1__array_constant_(__1_k) = 3.00000000e+00
__1_k = 2
__libasr__created__var__1__array_constant_(__1_k) = 5.00000000e+00
__1_k = 3
__libasr__created__var__1__array_constant_(__1_k) = 7.00000000e+00
__libasr_reduction_acc = .false.
! CheckBounds LHS = b%v, RHS = b%v, __libasr__created__var__1__array_constant_, 
__libasr_index_0_3 = lbound(__libasr__created__var__1__array_constant_, 1)
do __libasr_index_0_2 = lbound(b%v, 1), ubound(b%v, 1)
    __libasr_reduction_acc = __libasr_reduction_acc .or. b%v(__libasr_index_0_2) /= __libasr__created__var__1__array_con&
        stant_(__libasr_index_0_3)
    __libasr_index_0_3 = __libasr_index_0_3 + 1
end do
__libasr_created__intrinsic_array_function_Any = __libasr_reduction_acc
if (__libasr_created__intrinsic_array_function_Any) then
    error stop
end if
__libasr_created__result_metadata_1 = 1*(0 > (2 - 1 + 1)/1 ? 0 : (2 - 1 + 1)/1)
__libasr_created__function_call_f3 => a(1:2)
reallocate(b%v(__libasr_created__result_metadata_1))
call f3(__libasr_created__function_call_f3, b%v)
__1_k1 = 1
__libasr__created__var__2__array_constant_(__1_k1) = 3.00000000e+00
__1_k1 = 2
__libasr__created__var__2__array_constant_(__1_k1) = 5.00000000e+00
__libasr_reduction_acc1 = .false.
! CheckBounds LHS = b%v, RHS = b%v, __libasr__created__var__2__array_constant_, 
__libasr_index_0_5 = lbound(__libasr__created__var__2__array_constant_, 1)
do __libasr_index_0_4 = lbound(b%v, 1), ubound(b%v, 1)
    __libasr_reduction_acc1 = __libasr_reduction_acc1 .or. b%v(__libasr_index_0_4) /= __libasr__created__var__2__array_c&
        onstant_(__libasr_index_0_5)
    __libasr_index_0_5 = __libasr_index_0_5 + 1
end do
__libasr_created__intrinsic_array_function_Any1 = __libasr_reduction_acc1
if (size(b%v) /= 2 .or. __libasr_created__intrinsic_array_function_Any1) then
    error stop
end if
deallocate(b%v)
allocate(b%v(3))
__libasr_created__result_metadata_2 = size(a)
reallocate(b%v(__libasr_created__result_metadata_2))
call f3(a, b%v)
__1_k2 = 1
__libasr__created__var__3__array_constant_(__1_k2) = 3.00000000e+00
__1_k2 = 2
__libasr__created__var__3__array_constant_(__1_k2) = 5.00000000e+00
__1_k2 = 3
__libasr__created__var__3__array_constant_(__1_k2) = 7.00000000e+00
__libasr_reduction_acc2 = .false.
! CheckBounds LHS = b%v, RHS = b%v, __libasr__created__var__3__array_constant_, 
__libasr_index_0_7 = lbound(__libasr__created__var__3__array_constant_, 1)
do __libasr_index_0_6 = lbound(b%v, 1), ubound(b%v, 1)
    __libasr_reduction_acc2 = __libasr_reduction_acc2 .or. b%v(__libasr_index_0_6) /= __libasr__created__var__3__array_c&
        onstant_(__libasr_index_0_7)
    __libasr_index_0_7 = __libasr_index_0_7 + 1
end do
__libasr_created__intrinsic_array_function_Any2 = __libasr_reduction_acc2
if (lbound(b%v, 1) /= 0 .or. __libasr_created__intrinsic_array_function_Any2) then
    error stop
end if
__libasr_created__result_metadata_3 = 1*(0 > (0 - 1 + 1)/1 ? 0 : (0 - 1 + 1)/1)
__libasr_created__function_call_f31 => a(1:0)
reallocate(b%v(__libasr_created__result_metadata_3))
call f3(__libasr_created__function_call_f31, b%v)
if (size(b%v) /= 0) then
    error stop
end if
__libasr_created__result_metadata_4 = size(a)
allocate(m(__libasr_created__result_metadata_4))
call g3(a, m)
__1_k3 = 1
__libasr__created__var__4__array_constant_(__1_k3) = .false.
__1_k3 = 2
__libasr__created__var__4__array_constant_(__1_k3) = .true.
__1_k3 = 3
__libasr__created__var__4__array_constant_(__1_k3) = .true.
__libasr_reduction_acc3 = .false.
! CheckBounds LHS = m, RHS = m, __libasr__created__var__4__array_constant_, 
__libasr_index_0_9 = lbound(__libasr__created__var__4__array_constant_, 1)
do __libasr_index_0_8 = lbound(m, 1), ubound(m, 1)
    __libasr_reduction_acc3 = __libasr_reduction_acc3 .or. (m(__libasr_index_0_8) .neqv. __libasr__created__var__4__arra&
        y_constant_(__libasr_index_0_9))
    __libasr_index_0_9 = __libasr_index_0_9 + 1
end do
__libasr_created__intrinsic_array_function_Any3 = __libasr_reduction_acc3
if (size(m) /= 3 .or. __libasr_created__intrinsic_array_function_Any3) then
    error stop
end if
__libasr_created__result_metadata_5 = 1*(0 > (1 - 1 + 1)/1 ? 0 : (1 - 1 + 1)/1)
__libasr_created__function_call_g3 => a(1:1)
reallocate(m(__libasr_created__result_metadata_5))
call g3(__libasr_created__function_call_g3, m)
if (size(m) /= 1 .or. m(1)) then
    error stop
end if
__libasr_created__result_metadata_6 = size(a)
allocate(character(len=2) :: s(__libasr_created__result_metadata_6))
call c3(a, s)
__1_k4 = 1
__libasr__created__var__5__array_constant_(__1_k4) = "lo"
__1_k4 = 2
__libasr__created__var__5__array_constant_(__1_k4) = "hi"
__1_k4 = 3
__libasr__created__var__5__array_constant_(__1_k4) = "hi"
deallocate(__libasr_created__intrinsic_array_function_Any4)
allocate(__libasr_created__intrinsic_array_function_Any4(size(s, 1)))
! CheckBounds LHS = __libasr_created__intrinsic_array_function_Any4, RHS = s, __libasr__created__var__5__array_constant_,&
         
__libasr_index_0_11 = lbound(s, 1)
__libasr_index_0_12 = lbound(__libasr__created__var__5__array_constant_, 1)
do __libasr_index_0_10 = lbound(__libasr_created__intrinsic_array_function_Any4, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any4, 1)
    __libasr_created__intrinsic_array_function_Any4(__libasr_index_0_10) = s(__libasr_index_0_11) /= __libasr__created__&
        var__5__array_constant_(__libasr_index_0_12)
    __libasr_index_0_11 = __libasr_index_0_11 + 1
    __libasr_index_0_12 = __libasr_index_0_12 + 1
end do
__libasr_created__intrinsic_array_function_Any7 = _lcompilers_Any_4_1_0(__libasr_created__intrinsic_array_function_Any4)
if (size(s) /= 3 .or. __libasr_created__intrinsic_array_function_Any7) then
    error stop
end if
allocate(q(2))
q(1)%k = 9
reallocate(q(1)%w(size(__libasr_array_constant_1, 1)))
! CheckBounds LHS = q(1)%w, RHS = __libasr_array_constant_1, 
__libasr_index_0_14 = lbound(__libasr_array_constant_1, 1)
do __libasr_index_0_13 = lbound(q(1)%w, 1), ubound(q(1)%w, 1)
    q(1)%w(__libasr_index_0_13) = __libasr_array_constant_1(__libasr_index_0_14)
    __libasr_index_0_14 = __libasr_index_0_14 + 1
end do
call pairs(2, __libasr_created__function_call_pairs)
reallocate(q(size(__libasr_created__function_call_pairs, 1)))
! CheckBounds LHS = q, RHS = __libasr_created__function_call_pairs, 
__libasr_index_0_16 = lbound(__libasr_created__function_call_pairs, 1)
do __libasr_index_0_15 = lbound(q, 1), ubound(q, 1)
    q(__libasr_index_0_15) = __libasr_created__function_call_pairs(__libasr_index_0_16)
    __libasr_index_0_16 = __libasr_index_0_16 + 1
end do
if (q(1)%k /= 5 .or. size(q(1)%w) /= 1 .or. q(2)%w(1) /= real(2, kind=4)) then
    error stop
end if
call with_internal(a)
reallocate(t(size(__libasr_array_constant_2, 1)))
! CheckBounds LHS = t, RHS = __libasr_array_constant_2, 
__libasr_index_0_18 = lbound(__libasr_array_constant_2, 1)
do __libasr_index_0_17 = lbound(t, 1), ubound(t, 1)
    t(__libasr_index_0_17) = __libasr_array_constant_2(__libasr_index_0_18)
    __libasr_index_0_18 = __libasr_index_0_18 + 1
end do
p => t
__libasr_created__result_metadata_7 = size(p)
deallocate(__libasr_created__function_call_rev)
allocate(__libasr_created__function_call_rev(__libasr_created__result_metadata_7))
call rev(p, __libasr_created__function_call_rev)
reallocate(t(size(__libasr_created__function_call_rev, 1)))
! CheckBounds LHS = t, RHS = __libasr_created__function_call_rev, 
__libasr_index_0_20 = lbound(__libasr_created__function_call_rev, 1)
do __libasr_index_0_19 = lbound(t, 1), ubound(t, 1)
    t(__libasr_index_0_19) = __libasr_created__function_call_rev(__libasr_index_0_20)
    __libasr_index_0_20 = __libasr_index_0_20 + 1
end do
__1_k5 = 1
__libasr__created__var__8__array_constant_(__1_k5) = 3.00000000e+00
__1_k5 = 2
__libasr__created__var__8__array_constant_(__1_k5) = 2.00000000e+00
__1_k5 = 3
__libasr__created__var__8__array_constant_(__1_k5) = 1.00000000e+00
__libasr_reduction_acc4 = .false.
! CheckBounds LHS = t, RHS = t, __libasr__created__var__8__array_constant_, 
__libasr_index_0_22 = lbound(__libasr__created__var__8__array_constant_, 1)
do __libasr_index_0_21 = lbound(t, 1), ubound(t, 1)
    __libasr_reduction_acc4 = __libasr_reduction_acc4 .or. t(__libasr_index_0_21) /= __libasr__created__var__8__array_co&
        nstant_(__libasr_index_0_22)
    __libasr_index_0_22 = __libasr_index_0_22 + 1
end do
__libasr_created__intrinsic_array_function_Any5 = __libasr_reduction_acc4
if (__libasr_created__intrinsic_array_function_Any5) then
    error stop
end if
allocate(e(size(__libasr_array_constant_3, 1)))
! CheckBounds LHS = e, RHS = __libasr_array_constant_3, 
__libasr_index_0_24 = lbound(__libasr_array_constant_3, 1)
do __libasr_index_0_23 = lbound(e, 1), ubound(e, 1)
    e(__libasr_index_0_23) = __libasr_array_constant_3(__libasr_index_0_24)
    __libasr_index_0_24 = __libasr_index_0_24 + 1
end do
__libasr_created__result_metadata_8 = size(e)
deallocate(__libasr_created__function_call_rev1)
allocate(__libasr_created__function_call_rev1(__libasr_created__result_metadata_8))
call rev(e, __libasr_created__function_call_rev1)
reallocate(e(size(__libasr_created__function_call_rev1, 1)))
! CheckBounds LHS = e, RHS = __libasr_created__function_call_rev1, 
__libasr_index_0_26 = lbound(__libasr_created__function_call_rev1, 1)
do __libasr_index_0_25 = lbound(e, 1), ubound(e, 1)
    e(__libasr_index_0_25) = __libasr_created__function_call_rev1(__libasr_index_0_26)
    __libasr_index_0_26 = __libasr_index_0_26 + 1
end do
__1_k6 = 1
__libasr__created__var__10__array_constant_(__1_k6) = 2.00000000e+00
__1_k6 = 2
__libasr__created__var__10__array_constant_(__1_k6) = 1.00000000e+00
__libasr_reduction_acc5 = .false.
! CheckBounds LHS = e, RHS = e, __libasr__created__var__10__array_constant_, 
__libasr_index_0_28 = lbound(__libasr__created__var__10__array_constant_, 1)
do __libasr_index_0_27 = lbound(e, 1), ubound(e, 1)
    __libasr_reduction_acc5 = __libasr_reduction_acc5 .or. e(__libasr_index_0_27) /= __libasr__created__var__10__array_c&
        onstant_(__libasr_index_0_28)
    __libasr_index_0_28 = __libasr_index_0_28 + 1
end do
__libasr_created__intrinsic_array_function_Any6 = __libasr_reduction_acc5
if (__libasr_created__intrinsic_array_function_Any6) then
    error stop
end if
print *, "ok"

contains

subroutine __libasr_intent_out_reset_box_t(self)
    use realloc_lhs_result_inplace_01_m, only: 1_box_t => box_t
    type(1_box_t), intent(inout) :: self
    if (Allocated(self%v)) then
        deallocate(self%v)
    end if
end subroutine __libasr_intent_out_reset_box_t

subroutine __libasr_intent_out_reset_pair_t(self)
    use realloc_lhs_result_inplace_01_m, only: 1_pair_t => pair_t
    type(1_pair_t), intent(inout) :: self
    if (Allocated(self%w)) then
        deallocate(self%w)
    end if
    self%k = 5
end subroutine __libasr_intent_out_reset_pair_t

logical(4) function _lcompilers_Any_4_1_0(mask)
    integer(4) :: __1_i
    logical(4), dimension(:), intent(in) :: mask
    _lcompilers_Any_4_1_0 = .false.
    do __1_i = lbound(mask, 1), ubound(mask, 1)
        _lcompilers_Any_4_1_0 = _lcompilers_Any_4_1_0 .or. mask(__1_i)
    end do
end function _lcompilers_Any_4_1_0

subroutine _lcompilers_merge_str_l32(tsource, fsource, mask, merge)
    character(len=*, kind=1), intent(in) :: fsource
    logical(4), intent(in) :: mask
    character(len=:, kind=1), allocatable, intent(out) :: merge
    character(len=*, kind=1), intent(in) :: tsource
    if (mask) then
        merge = tsource
    else
        merge = fsource
    end if
end subroutine _lcompilers_merge_str_l32

real(4) function _lcompilers_real_i32(x)
    integer(4), intent(in) :: x
    _lcompilers_real_i32 = real(x, kind=4)
end function _lcompilers_real_i32

end program realloc_lhs_result_inplace_01
