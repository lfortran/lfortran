module realloc_lhs_unallocated_02_m
implicit none
type :: tensor_t
    real(4), dimension(:), allocatable :: values_
contains
    procedure :: __libasr_intent_out_reset => __libasr_intent_out_reset_tensor_t
end type tensor_t

contains

subroutine allocate_it(x)
    integer(4) :: __libasr_index_0_
    integer(4), dimension(:), allocatable, intent(inout) :: x
    allocate(x(5))
    do __libasr_index_0_ = lbound(x, 1), ubound(x, 1)
        x(__libasr_index_0_) = 7
    end do
end subroutine allocate_it

pure subroutine assigned_on_some_path(n, r)
    integer(4) :: __1__libasr_index_
    integer(4) :: __1__libasr_index_1
    integer(4), dimension(1) :: __libasr__created__var__6__array_constructor_
    integer(4), dimension(2) :: __libasr__created__var__7__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4), intent(in) :: n
    integer(4), dimension(:), allocatable, intent(out) :: r
    if (Allocated(r)) then
        deallocate(r)
    end if
    if (n > 0) then
        __1__libasr_index_ = lbound(__libasr__created__var__6__array_constructor_, 1)
        __libasr__created__var__6__array_constructor_(__1__libasr_index_) = n
        __1__libasr_index_ = __1__libasr_index_ + 1
        allocate(r(size(__libasr__created__var__6__array_constructor_, 1)))
        ! CheckBounds LHS = r, RHS = __libasr__created__var__6__array_constructor_, 
        __libasr_index_0_1 = lbound(__libasr__created__var__6__array_constructor_, 1)
        do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
            r(__libasr_index_0_) = __libasr__created__var__6__array_constructor_(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    end if
    __1__libasr_index_1 = lbound(__libasr__created__var__7__array_constructor_, 1)
    __libasr__created__var__7__array_constructor_(__1__libasr_index_1) = n
    __1__libasr_index_1 = __1__libasr_index_1 + 1
    __libasr__created__var__7__array_constructor_(__1__libasr_index_1) = n
    __1__libasr_index_1 = __1__libasr_index_1 + 1
    reallocate(r(size(__libasr__created__var__7__array_constructor_, 1)))
    ! CheckBounds LHS = r, RHS = __libasr__created__var__7__array_constructor_, 
    __libasr_index_0_3 = lbound(__libasr__created__var__7__array_constructor_, 1)
    do __libasr_index_0_2 = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_2) = __libasr__created__var__7__array_constructor_(__libasr_index_0_3)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
end subroutine assigned_on_some_path

pure subroutine assigned_twice(n, r)
    integer(4) :: __1__libasr_index_
    integer(4) :: __1__libasr_index_1
    integer(4), dimension(1) :: __libasr__created__var__8__array_constructor_
    integer(4), dimension(3) :: __libasr__created__var__9__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4), intent(in) :: n
    integer(4), dimension(:), allocatable, intent(out) :: r
    if (Allocated(r)) then
        deallocate(r)
    end if
    __1__libasr_index_ = lbound(__libasr__created__var__8__array_constructor_, 1)
    __libasr__created__var__8__array_constructor_(__1__libasr_index_) = n
    __1__libasr_index_ = __1__libasr_index_ + 1
    allocate(r(size(__libasr__created__var__8__array_constructor_, 1)))
    ! CheckBounds LHS = r, RHS = __libasr__created__var__8__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr__created__var__8__array_constructor_, 1)
    do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_) = __libasr__created__var__8__array_constructor_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    __1__libasr_index_1 = lbound(__libasr__created__var__9__array_constructor_, 1)
    __libasr__created__var__9__array_constructor_(__1__libasr_index_1) = n
    __1__libasr_index_1 = __1__libasr_index_1 + 1
    __libasr__created__var__9__array_constructor_(__1__libasr_index_1) = n
    __1__libasr_index_1 = __1__libasr_index_1 + 1
    __libasr__created__var__9__array_constructor_(__1__libasr_index_1) = n
    __1__libasr_index_1 = __1__libasr_index_1 + 1
    reallocate(r(size(__libasr__created__var__9__array_constructor_, 1)))
    ! CheckBounds LHS = r, RHS = __libasr__created__var__9__array_constructor_, 
    __libasr_index_0_3 = lbound(__libasr__created__var__9__array_constructor_, 1)
    do __libasr_index_0_2 = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_2) = __libasr__created__var__9__array_constructor_(__libasr_index_0_3)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
end subroutine assigned_twice

subroutine by_internal_procedure(n, r)
    type :: __lcompilers_frame_by_internal_procedure
        integer(4), dimension(:), allocatable, reference :: r
    end type __lcompilers_frame_by_internal_procedure
    integer(4) :: __1__libasr_index_
    type(__lcompilers_frame_by_internal_procedure) :: __lcompilers_frame
    integer(4), dimension(3) :: __libasr__created__var__11__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4), intent(in) :: n
    integer(4), dimension(:), allocatable, intent(out) :: r
    if (Allocated(r)) then
        deallocate(r)
    end if
    bind_reference(__lcompilers_frame%r, r)
    call set(__lcompilers_frame)
    __1__libasr_index_ = lbound(__libasr__created__var__11__array_constructor_, 1)
    __libasr__created__var__11__array_constructor_(__1__libasr_index_) = n
    __1__libasr_index_ = __1__libasr_index_ + 1
    __libasr__created__var__11__array_constructor_(__1__libasr_index_) = n
    __1__libasr_index_ = __1__libasr_index_ + 1
    __libasr__created__var__11__array_constructor_(__1__libasr_index_) = n
    __1__libasr_index_ = __1__libasr_index_ + 1
    reallocate(r(size(__libasr__created__var__11__array_constructor_, 1)))
    ! CheckBounds LHS = r, RHS = __libasr__created__var__11__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr__created__var__11__array_constructor_, 1)
    do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_) = __libasr__created__var__11__array_constructor_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    contains
    subroutine set(__lcompilers_frame)
        type(__lcompilers_frame_by_internal_procedure), intent(in) :: __lcompilers_frame
        integer(4), dimension(1), parameter :: __libasr_array_constant_ = [1]
        integer(4) :: __libasr_index_0_
        integer(4) :: __libasr_index_0_1
        integer(4), dimension(:), allocatable, host_associated :: r => __lcompilers_frame%r
        reallocate(r(size(__libasr_array_constant_, 1)))
        ! CheckBounds LHS = r, RHS = __libasr_array_constant_, 
        __libasr_index_0_1 = lbound(__libasr_array_constant_, 1)
        do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
            r(__libasr_index_0_) = __libasr_array_constant_(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    end subroutine set

end subroutine by_internal_procedure

pure subroutine in_a_loop(n, r)
    integer(4) :: __1__libasr_index_
    integer(4), dimension(:), allocatable :: __libasr__created__var__12__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: i
    integer(4) :: j
    integer(4) :: lfortran_implied_do_j
    integer(4), intent(in) :: n
    integer(4), dimension(:), allocatable, intent(out) :: r
    if (Allocated(r)) then
        deallocate(r)
    end if
    do i = 1, n
        deallocate(__libasr__created__var__12__array_constructor_)
        allocate(__libasr__created__var__12__array_constructor_(1*i))
        __1__libasr_index_ = lbound(__libasr__created__var__12__array_constructor_, 1)
        do lfortran_implied_do_j = 1, i
            __libasr__created__var__12__array_constructor_(__1__libasr_index_) = lfortran_implied_do_j
            __1__libasr_index_ = __1__libasr_index_ + 1
        end do
        reallocate(r(size(__libasr__created__var__12__array_constructor_, 1)))
        ! CheckBounds LHS = r, RHS = __libasr__created__var__12__array_constructor_, 
        __libasr_index_0_1 = lbound(__libasr__created__var__12__array_constructor_, 1)
        do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
            r(__libasr_index_0_) = __libasr__created__var__12__array_constructor_(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    end do
end subroutine in_a_loop

subroutine moved_into(n, r)
    integer(4) :: __1__libasr_index_
    integer(4), dimension(1) :: __libasr__created__var__13__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4), intent(in) :: n
    integer(4), dimension(:), allocatable, intent(out) :: r
    integer(4), dimension(:), allocatable :: t
    if (Allocated(r)) then
        deallocate(r)
    end if
    allocate(t(4))
    do __libasr_index_0_ = lbound(t, 1), ubound(t, 1)
        t(__libasr_index_0_) = n
    end do
    call _lcompilers_move_alloc_integer[:] allocatable(t, r)
    __1__libasr_index_ = lbound(__libasr__created__var__13__array_constructor_, 1)
    __libasr__created__var__13__array_constructor_(__1__libasr_index_) = n
    __1__libasr_index_ = __1__libasr_index_ + 1
    reallocate(r(size(__libasr__created__var__13__array_constructor_, 1)))
    ! CheckBounds LHS = r, RHS = __libasr__created__var__13__array_constructor_, 
    __libasr_index_0_2 = lbound(__libasr__created__var__13__array_constructor_, 1)
    do __libasr_index_0_1 = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_1) = __libasr__created__var__13__array_constructor_(__libasr_index_0_2)
        __libasr_index_0_2 = __libasr_index_0_2 + 1
    end do
end subroutine moved_into

subroutine optional_out(x, __libasr_is_present_x, n)
    integer(4) :: __1__libasr_index_
    integer(4), dimension(2) :: __libasr__created__var__14__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    logical(4), intent(in) :: __libasr_is_present_x
    integer(4), intent(in) :: n
    integer(4), dimension(:), allocatable, intent(out) :: x
    if (__libasr_is_present_x) then
        if (Allocated(x)) then
            deallocate(x)
        end if
    end if
    if (__libasr_is_present_x) then
        __1__libasr_index_ = lbound(__libasr__created__var__14__array_constructor_, 1)
        __libasr__created__var__14__array_constructor_(__1__libasr_index_) = n
        __1__libasr_index_ = __1__libasr_index_ + 1
        __libasr__created__var__14__array_constructor_(__1__libasr_index_) = n
        __1__libasr_index_ = __1__libasr_index_ + 1
        reallocate(x(size(__libasr__created__var__14__array_constructor_, 1)))
        ! CheckBounds LHS = x, RHS = __libasr__created__var__14__array_constructor_, 
        __libasr_index_0_1 = lbound(__libasr__created__var__14__array_constructor_, 1)
        do __libasr_index_0_ = lbound(x, 1), ubound(x, 1)
            x(__libasr_index_0_) = __libasr__created__var__14__array_constructor_(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    end if
end subroutine optional_out

subroutine passed_down(n, r)
    integer(4) :: __1__libasr_index_
    integer(4), dimension(2) :: __libasr__created__var__15__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4), intent(in) :: n
    integer(4), dimension(:), allocatable, intent(out) :: r
    if (Allocated(r)) then
        deallocate(r)
    end if
    call allocate_it(r)
    __1__libasr_index_ = lbound(__libasr__created__var__15__array_constructor_, 1)
    __libasr__created__var__15__array_constructor_(__1__libasr_index_) = n
    __1__libasr_index_ = __1__libasr_index_ + 1
    __libasr__created__var__15__array_constructor_(__1__libasr_index_) = n
    __1__libasr_index_ = __1__libasr_index_ + 1
    reallocate(r(size(__libasr__created__var__15__array_constructor_, 1)))
    ! CheckBounds LHS = r, RHS = __libasr__created__var__15__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr__created__var__15__array_constructor_, 1)
    do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_) = __libasr__created__var__15__array_constructor_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine passed_down

subroutine saved(n, total)
    integer(4) :: __1__libasr_index_
    integer(4), dimension(:), allocatable :: __libasr__created__var__16__array_constructor_
    integer(4) :: __libasr_created__intrinsic_array_function_Sum
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: i
    integer(4) :: lfortran_implied_do_i
    integer(4), intent(in) :: n
    integer(4), intent(inout) :: total
    integer(4), dimension(:), allocatable, save :: w
    deallocate(__libasr__created__var__16__array_constructor_)
    allocate(__libasr__created__var__16__array_constructor_(1*n))
    __1__libasr_index_ = lbound(__libasr__created__var__16__array_constructor_, 1)
    do lfortran_implied_do_i = 1, n
        __libasr__created__var__16__array_constructor_(__1__libasr_index_) = n
        __1__libasr_index_ = __1__libasr_index_ + 1
    end do
    reallocate(w(size(__libasr__created__var__16__array_constructor_, 1)))
    ! CheckBounds LHS = w, RHS = __libasr__created__var__16__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr__created__var__16__array_constructor_, 1)
    do __libasr_index_0_ = lbound(w, 1), ubound(w, 1)
        w(__libasr_index_0_) = __libasr__created__var__16__array_constructor_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    __libasr_created__intrinsic_array_function_Sum = _lcompilers_Sum_4_1_0_idx4(w)
    total = total + __libasr_created__intrinsic_array_function_Sum
end subroutine saved

subroutine through_associate(n, t)
    integer(4) :: __1__libasr_index_
    real(4), dimension(2) :: __libasr__created__var__18__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4), intent(in) :: n
    type(tensor_t), intent(out) :: t
    if (Allocated(t%values_)) then
        deallocate(t%values_)
    end if
    associate_block : associate (q => t)
        integer(4) :: __1__libasr_index_
        real(4), dimension(1) :: __libasr__created__var__17__array_constructor_
        integer(4) :: __libasr_index_0_
        integer(4) :: __libasr_index_0_1
        __1__libasr_index_ = lbound(__libasr__created__var__17__array_constructor_, 1)
        __libasr__created__var__17__array_constructor_(__1__libasr_index_) = _lcompilers_real_i32(n)
        __1__libasr_index_ = __1__libasr_index_ + 1
        reallocate(q%values_(size(__libasr__created__var__17__array_constructor_, 1)))
        ! CheckBounds LHS = q%values_, RHS = __libasr__created__var__17__array_constructor_, 
        __libasr_index_0_1 = lbound(__libasr__created__var__17__array_constructor_, 1)
        do __libasr_index_0_ = lbound(q%values_, 1), ubound(q%values_, 1)
            q%values_(__libasr_index_0_) = __libasr__created__var__17__array_constructor_(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    end associate associate_block
    __1__libasr_index_ = lbound(__libasr__created__var__18__array_constructor_, 1)
    __libasr__created__var__18__array_constructor_(__1__libasr_index_) = _lcompilers_real_i321(n)
    __1__libasr_index_ = __1__libasr_index_ + 1
    __libasr__created__var__18__array_constructor_(__1__libasr_index_) = _lcompilers_real_i322(n)
    __1__libasr_index_ = __1__libasr_index_ + 1
    reallocate(t%values_(size(__libasr__created__var__18__array_constructor_, 1)))
    ! CheckBounds LHS = t%values_, RHS = __libasr__created__var__18__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr__created__var__18__array_constructor_, 1)
    do __libasr_index_0_ = lbound(t%values_, 1), ubound(t%values_, 1)
        t%values_(__libasr_index_0_) = __libasr__created__var__18__array_constructor_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine through_associate

end module realloc_lhs_unallocated_02_m

program realloc_lhs_unallocated_02
use realloc_lhs_unallocated_02_m, only: allocate_it
use realloc_lhs_unallocated_02_m, only: assigned_on_some_path
use realloc_lhs_unallocated_02_m, only: assigned_twice
use realloc_lhs_unallocated_02_m, only: by_internal_procedure
use realloc_lhs_unallocated_02_m, only: in_a_loop
use realloc_lhs_unallocated_02_m, only: moved_into
use realloc_lhs_unallocated_02_m, only: optional_out
use realloc_lhs_unallocated_02_m, only: passed_down
use realloc_lhs_unallocated_02_m, only: saved
use realloc_lhs_unallocated_02_m, only: tensor_t
use realloc_lhs_unallocated_02_m, only: through_associate
implicit none
integer(4) :: __1_k
integer(4) :: __1_k1
integer(4) :: __1_k2
integer(4) :: __1_k3
integer(4) :: __1_k4
integer(4), dimension(2) :: __libasr__created__var__0__array_constant_
integer(4), dimension(1) :: __libasr__created__var__1__array_constant_
integer(4), dimension(3) :: __libasr__created__var__2__array_constant_
real(4), dimension(2) :: __libasr__created__var__3__array_constant_
integer(4), dimension(2) :: __libasr__created__var__5__array_constant_
integer(4), dimension(3), parameter :: __libasr_array_constant_ = [1, 2, 3]
integer(4), dimension(:), allocatable :: __libasr_created__array_size_v
integer(4), dimension(:), allocatable :: __libasr_created__array_size_v1
integer(4), dimension(:), allocatable :: __libasr_created__array_size_v2
logical(4), dimension(2) :: __libasr_created__intrinsic_array_function_Any
logical(4), dimension(1) :: __libasr_created__intrinsic_array_function_Any1
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any2
logical(4) :: __libasr_created__intrinsic_array_function_Any3
logical(4) :: __libasr_created__intrinsic_array_function_Any4
logical(4) :: __libasr_created__intrinsic_array_function_Any5
logical(4) :: __libasr_created__intrinsic_array_function_Any6
logical(4) :: __libasr_created__intrinsic_array_function_Any7
integer(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_
integer(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_1
integer(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_2
integer(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_3
integer(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_4
integer(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_5
integer(4), dimension(:), allocatable :: __libasr_created_integer_compare_left_
integer(4), dimension(:), allocatable :: __libasr_created_integer_compare_left_1
integer(4), dimension(:), allocatable :: __libasr_created_integer_compare_left_2
integer(4), dimension(:), allocatable :: __libasr_created_variable_
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
integer(4) :: __libasr_index_0_3
integer(4) :: __libasr_index_0_4
integer(4) :: __libasr_index_0_5
integer(4) :: __libasr_index_0_6
integer(4) :: __libasr_index_0_7
integer(4) :: __libasr_index_0_8
integer(4) :: __libasr_index_0_9
logical(4) :: __libasr_reduction_acc
logical(4) :: __libasr_reduction_acc1
type(tensor_t) :: t
integer(4) :: total
integer(4), dimension(:), allocatable :: x
deallocate(__libasr_created__subroutine_from_function_) ! Implicit deallocate
call assigned_twice(4, __libasr_created__subroutine_from_function_)
if (.false.) then
    if (size(__libasr_created__array_size_v, 1) == size(__libasr_created__subroutine_from_function_,&
         1) .and. lbound(__libasr_created__array_size_v, 1) /= 1) then
        ! CheckBounds LHS = __libasr_created__array_size_v, RHS = __libasr_created__subroutine_from_function_, 
        __libasr_index_0_16 = lbound(__libasr_created__subroutine_from_function_, 1)
        do __libasr_index_0_15 = lbound(__libasr_created__array_size_v, 1), ubound(__libasr_created__array_size_v, 1)
            __libasr_created__array_size_v(__libasr_index_0_15) = __libasr_created__subroutine_from_function_(__libasr_i&
        ndex_0_16)
            __libasr_index_0_16 = __libasr_index_0_16 + 1
        end do
    else
        deallocate(__libasr_created__array_size_v) ! Implicit deallocate
        __libasr_created__array_size_v = __libasr_created__subroutine_from_function_
    end if
else
    deallocate(__libasr_created__array_size_v) ! Implicit deallocate
    __libasr_created__array_size_v = __libasr_created__subroutine_from_function_
end if
if (size(__libasr_created__array_size_v) /= 3) then
    error stop
end if
deallocate(__libasr_created__subroutine_from_function_1) ! Implicit deallocate
call assigned_on_some_path(4, __libasr_created__subroutine_from_function_1)
if (.false.) then
    if (size(__libasr_created__array_size_v1, 1) == size(__libasr_created__subroutine_from_function_1,&
         1) .and. lbound(__libasr_created__array_size_v1, 1) /= 1) then
        ! CheckBounds LHS = __libasr_created__array_size_v1, RHS = __libasr_created__subroutine_from_function_1, 
        __libasr_index_0_18 = lbound(__libasr_created__subroutine_from_function_1, 1)
        do __libasr_index_0_17 = lbound(__libasr_created__array_size_v1, 1), ubound(__libasr_created__array_size_v1, 1)
            __libasr_created__array_size_v1(__libasr_index_0_17) = __libasr_created__subroutine_from_function_1(__libasr&
        _index_0_18)
            __libasr_index_0_18 = __libasr_index_0_18 + 1
        end do
    else
        deallocate(__libasr_created__array_size_v1) ! Implicit deallocate
        __libasr_created__array_size_v1 = __libasr_created__subroutine_from_function_1
    end if
else
    deallocate(__libasr_created__array_size_v1) ! Implicit deallocate
    __libasr_created__array_size_v1 = __libasr_created__subroutine_from_function_1
end if
if (size(__libasr_created__array_size_v1) /= 2) then
    error stop
end if
deallocate(__libasr_created__subroutine_from_function_2) ! Implicit deallocate
call in_a_loop(4, __libasr_created__subroutine_from_function_2)
if (.false.) then
    if (size(__libasr_created__array_size_v2, 1) == size(__libasr_created__subroutine_from_function_2,&
         1) .and. lbound(__libasr_created__array_size_v2, 1) /= 1) then
        ! CheckBounds LHS = __libasr_created__array_size_v2, RHS = __libasr_created__subroutine_from_function_2, 
        __libasr_index_0_20 = lbound(__libasr_created__subroutine_from_function_2, 1)
        do __libasr_index_0_19 = lbound(__libasr_created__array_size_v2, 1), ubound(__libasr_created__array_size_v2, 1)
            __libasr_created__array_size_v2(__libasr_index_0_19) = __libasr_created__subroutine_from_function_2(__libasr&
        _index_0_20)
            __libasr_index_0_20 = __libasr_index_0_20 + 1
        end do
    else
        deallocate(__libasr_created__array_size_v2) ! Implicit deallocate
        __libasr_created__array_size_v2 = __libasr_created__subroutine_from_function_2
    end if
else
    deallocate(__libasr_created__array_size_v2) ! Implicit deallocate
    __libasr_created__array_size_v2 = __libasr_created__subroutine_from_function_2
end if
if (size(__libasr_created__array_size_v2) /= 4) then
    error stop
end if
__1_k = 1
__libasr__created__var__0__array_constant_(__1_k) = 5
__1_k = 2
__libasr__created__var__0__array_constant_(__1_k) = 5
deallocate(__libasr_created__subroutine_from_function_3) ! Implicit deallocate
call passed_down(5, __libasr_created__subroutine_from_function_3)
if (.false.) then
    if (size(__libasr_created_integer_compare_left_, 1) == size(__libasr_created__subroutine_from_function_3,&
         1) .and. lbound(__libasr_created_integer_compare_left_, 1) /= 1) then
        ! CheckBounds LHS = __libasr_created_integer_compare_left_, RHS = __libasr_created__subroutine_from_function_3, 
        __libasr_index_0_22 = lbound(__libasr_created__subroutine_from_function_3, 1)
        do __libasr_index_0_21 = lbound(__libasr_created_integer_compare_left_, 1),&
         ubound(__libasr_created_integer_compare_left_, 1)
            __libasr_created_integer_compare_left_(__libasr_index_0_21) = __libasr_created__subroutine_from_function_3(_&
        _libasr_index_0_22)
            __libasr_index_0_22 = __libasr_index_0_22 + 1
        end do
    else
        deallocate(__libasr_created_integer_compare_left_) ! Implicit deallocate
        __libasr_created_integer_compare_left_ = __libasr_created__subroutine_from_function_3
    end if
else
    deallocate(__libasr_created_integer_compare_left_) ! Implicit deallocate
    __libasr_created_integer_compare_left_ = __libasr_created__subroutine_from_function_3
end if
! CheckBounds LHS = __libasr_created__intrinsic_array_function_Any, RHS = __libasr_created_integer_compare_left_,&
         __libasr__created__var__0__array_constant_, 
__libasr_index_0_1 = lbound(__libasr_created_integer_compare_left_, 1)
__libasr_index_0_2 = lbound(__libasr__created__var__0__array_constant_, 1)
do __libasr_index_0_ = lbound(__libasr_created__intrinsic_array_function_Any, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any, 1)
    __libasr_created__intrinsic_array_function_Any(__libasr_index_0_) = __libasr_created_integer_compare_left_(__libasr_&
        index_0_1) /= __libasr__created__var__0__array_constant_(__libasr_index_0_2)
    __libasr_index_0_1 = __libasr_index_0_1 + 1
    __libasr_index_0_2 = __libasr_index_0_2 + 1
end do
__libasr_created__intrinsic_array_function_Any5 = _lcompilers_Any_4_1_0(__libasr_created__intrinsic_array_function_Any)
if (__libasr_created__intrinsic_array_function_Any5) then
    error stop
end if
__1_k1 = 1
__libasr__created__var__1__array_constant_(__1_k1) = 6
deallocate(__libasr_created__subroutine_from_function_4) ! Implicit deallocate
call moved_into(6, __libasr_created__subroutine_from_function_4)
if (.false.) then
    if (size(__libasr_created_integer_compare_left_1, 1) == size(__libasr_created__subroutine_from_function_4,&
         1) .and. lbound(__libasr_created_integer_compare_left_1, 1) /= 1) then
        ! CheckBounds LHS = __libasr_created_integer_compare_left_1, RHS = __libasr_created__subroutine_from_function_4,&
         
        __libasr_index_0_24 = lbound(__libasr_created__subroutine_from_function_4, 1)
        do __libasr_index_0_23 = lbound(__libasr_created_integer_compare_left_1, 1),&
         ubound(__libasr_created_integer_compare_left_1, 1)
            __libasr_created_integer_compare_left_1(__libasr_index_0_23) = __libasr_created__subroutine_from_function_4(&
        __libasr_index_0_24)
            __libasr_index_0_24 = __libasr_index_0_24 + 1
        end do
    else
        deallocate(__libasr_created_integer_compare_left_1) ! Implicit deallocate
        __libasr_created_integer_compare_left_1 = __libasr_created__subroutine_from_function_4
    end if
else
    deallocate(__libasr_created_integer_compare_left_1) ! Implicit deallocate
    __libasr_created_integer_compare_left_1 = __libasr_created__subroutine_from_function_4
end if
! CheckBounds LHS = __libasr_created__intrinsic_array_function_Any1, RHS = __libasr_created_integer_compare_left_1,&
         __libasr__created__var__1__array_constant_, 
__libasr_index_0_4 = lbound(__libasr_created_integer_compare_left_1, 1)
__libasr_index_0_5 = lbound(__libasr__created__var__1__array_constant_, 1)
do __libasr_index_0_3 = lbound(__libasr_created__intrinsic_array_function_Any1, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any1, 1)
    __libasr_created__intrinsic_array_function_Any1(__libasr_index_0_3) = __libasr_created_integer_compare_left_1(__liba&
        sr_index_0_4) /= __libasr__created__var__1__array_constant_(__libasr_index_0_5)
    __libasr_index_0_4 = __libasr_index_0_4 + 1
    __libasr_index_0_5 = __libasr_index_0_5 + 1
end do
__libasr_created__intrinsic_array_function_Any6 = _lcompilers_Any_4_1_0(__libasr_created__intrinsic_array_function_Any1)
if (__libasr_created__intrinsic_array_function_Any6) then
    error stop
end if
__1_k2 = 1
__libasr__created__var__2__array_constant_(__1_k2) = 2
__1_k2 = 2
__libasr__created__var__2__array_constant_(__1_k2) = 2
__1_k2 = 3
__libasr__created__var__2__array_constant_(__1_k2) = 2
deallocate(__libasr_created__subroutine_from_function_5) ! Implicit deallocate
call by_internal_procedure(2, __libasr_created__subroutine_from_function_5)
if (.false.) then
    if (size(__libasr_created_integer_compare_left_2, 1) == size(__libasr_created__subroutine_from_function_5,&
         1) .and. lbound(__libasr_created_integer_compare_left_2, 1) /= 1) then
        ! CheckBounds LHS = __libasr_created_integer_compare_left_2, RHS = __libasr_created__subroutine_from_function_5,&
         
        __libasr_index_0_26 = lbound(__libasr_created__subroutine_from_function_5, 1)
        do __libasr_index_0_25 = lbound(__libasr_created_integer_compare_left_2, 1),&
         ubound(__libasr_created_integer_compare_left_2, 1)
            __libasr_created_integer_compare_left_2(__libasr_index_0_25) = __libasr_created__subroutine_from_function_5(&
        __libasr_index_0_26)
            __libasr_index_0_26 = __libasr_index_0_26 + 1
        end do
    else
        deallocate(__libasr_created_integer_compare_left_2) ! Implicit deallocate
        __libasr_created_integer_compare_left_2 = __libasr_created__subroutine_from_function_5
    end if
else
    deallocate(__libasr_created_integer_compare_left_2) ! Implicit deallocate
    __libasr_created_integer_compare_left_2 = __libasr_created__subroutine_from_function_5
end if
! CheckBounds LHS = __libasr_created__intrinsic_array_function_Any2, RHS = __libasr_created_integer_compare_left_2,&
         __libasr__created__var__2__array_constant_, 
__libasr_index_0_7 = lbound(__libasr_created_integer_compare_left_2, 1)
__libasr_index_0_8 = lbound(__libasr__created__var__2__array_constant_, 1)
do __libasr_index_0_6 = lbound(__libasr_created__intrinsic_array_function_Any2, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any2, 1)
    __libasr_created__intrinsic_array_function_Any2(__libasr_index_0_6) = __libasr_created_integer_compare_left_2(__liba&
        sr_index_0_7) /= __libasr__created__var__2__array_constant_(__libasr_index_0_8)
    __libasr_index_0_7 = __libasr_index_0_7 + 1
    __libasr_index_0_8 = __libasr_index_0_8 + 1
end do
__libasr_created__intrinsic_array_function_Any7 = _lcompilers_Any_4_1_0(__libasr_created__intrinsic_array_function_Any2)
if (__libasr_created__intrinsic_array_function_Any7) then
    error stop
end if
call through_associate(3, t)
__1_k3 = 1
__libasr__created__var__3__array_constant_(__1_k3) = 3.00000000e+00
__1_k3 = 2
__libasr__created__var__3__array_constant_(__1_k3) = 3.00000000e+00
__libasr_reduction_acc = .false.
! CheckBounds LHS = t%values_, RHS = t%values_, __libasr__created__var__3__array_constant_, 
__libasr_index_0_10 = lbound(__libasr__created__var__3__array_constant_, 1)
do __libasr_index_0_9 = lbound(t%values_, 1), ubound(t%values_, 1)
    __libasr_reduction_acc = __libasr_reduction_acc .or. t%values_(__libasr_index_0_9) /= __libasr__created__var__3__arr&
        ay_constant_(__libasr_index_0_10)
    __libasr_index_0_10 = __libasr_index_0_10 + 1
end do
__libasr_created__intrinsic_array_function_Any3 = __libasr_reduction_acc
if (__libasr_created__intrinsic_array_function_Any3) then
    error stop
end if
total = 0
call saved(2, total)
call saved(3, total)
if (total /= 4 + 9) then
    error stop
end if
allocate(x(size(__libasr_array_constant_, 1)))
! CheckBounds LHS = x, RHS = __libasr_array_constant_, 
__libasr_index_0_12 = lbound(__libasr_array_constant_, 1)
do __libasr_index_0_11 = lbound(x, 1), ubound(x, 1)
    x(__libasr_index_0_11) = __libasr_array_constant_(__libasr_index_0_12)
    __libasr_index_0_12 = __libasr_index_0_12 + 1
end do
call optional_out(x, .true., 4)
__1_k4 = 1
__libasr__created__var__5__array_constant_(__1_k4) = 4
__1_k4 = 2
__libasr__created__var__5__array_constant_(__1_k4) = 4
__libasr_reduction_acc1 = .false.
! CheckBounds LHS = x, RHS = x, __libasr__created__var__5__array_constant_, 
__libasr_index_0_14 = lbound(__libasr__created__var__5__array_constant_, 1)
do __libasr_index_0_13 = lbound(x, 1), ubound(x, 1)
    __libasr_reduction_acc1 = __libasr_reduction_acc1 .or. x(__libasr_index_0_13) /= __libasr__created__var__5__array_co&
        nstant_(__libasr_index_0_14)
    __libasr_index_0_14 = __libasr_index_0_14 + 1
end do
__libasr_created__intrinsic_array_function_Any4 = __libasr_reduction_acc1
if (__libasr_created__intrinsic_array_function_Any4) then
    error stop
end if
call optional_out(__libasr_created_variable_, .false., 4)
print *, "ok"

contains

subroutine __libasr_intent_out_reset_tensor_t(self)
    use realloc_lhs_unallocated_02_m, only: 1_tensor_t => tensor_t
    type(1_tensor_t), intent(inout) :: self
    if (Allocated(self%values_)) then
        deallocate(self%values_)
    end if
end subroutine __libasr_intent_out_reset_tensor_t

logical(4) function _lcompilers_Any_4_1_0(mask)
    integer(4) :: __1_i
    logical(4), dimension(:), intent(in) :: mask
    _lcompilers_Any_4_1_0 = .false.
    do __1_i = lbound(mask, 1), ubound(mask, 1)
        _lcompilers_Any_4_1_0 = _lcompilers_Any_4_1_0 .or. mask(__1_i)
    end do
end function _lcompilers_Any_4_1_0

integer(4) function _lcompilers_Sum_4_1_0_idx4(array) result(result)
    integer(4) :: __1_i
    integer(4), dimension(:), intent(in) :: array
    result = 0
    do __1_i = lbound(array, 1), ubound(array, 1)
        result = result + array(__1_i)
    end do
end function _lcompilers_Sum_4_1_0_idx4

subroutine _lcompilers_move_alloc_integer[:] allocatable(from, to)
    integer(4), dimension(:), allocatable, intent(in) :: from
    integer(4), dimension(:), allocatable, intent(inout) :: to
    if (Allocated(to)) then
        deallocate(to)
    end if
    if (Allocated(from)) then
        deallocate(to) ! Implicit deallocate
        to = from
    end if
end subroutine _lcompilers_move_alloc_integer[:] allocatable

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

end program realloc_lhs_unallocated_02
