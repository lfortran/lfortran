module realloc_lhs_unallocated_01_m
implicit none
type :: tensor_t
    real(4), dimension(:), allocatable :: values_
contains
    procedure :: __libasr_intent_out_reset => __libasr_intent_out_reset_tensor_t
end type tensor_t

contains

pure subroutine branches(n, r)
    integer(4) :: __1__libasr_index_
    integer(4) :: __1__libasr_index_1
    integer(4), dimension(2) :: __libasr__created__var__12__array_constructor_
    integer(4), dimension(1) :: __libasr__created__var__13__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: m
    integer(4), intent(in) :: n
    integer(4), dimension(:), allocatable, intent(out) :: r
    if (Allocated(r)) then
        deallocate(r)
    end if
    m = 2*n
    if (n > 2) then
        __1__libasr_index_ = lbound(__libasr__created__var__12__array_constructor_, 1)
        __libasr__created__var__12__array_constructor_(__1__libasr_index_) = n
        __1__libasr_index_ = __1__libasr_index_ + 1
        __libasr__created__var__12__array_constructor_(__1__libasr_index_) = m
        __1__libasr_index_ = __1__libasr_index_ + 1
        allocate(r(size(__libasr__created__var__12__array_constructor_, 1)))
        ! CheckBounds LHS = r, RHS = __libasr__created__var__12__array_constructor_, 
        __libasr_index_0_1 = lbound(__libasr__created__var__12__array_constructor_, 1)
        do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
            r(__libasr_index_0_) = __libasr__created__var__12__array_constructor_(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    else
        __1__libasr_index_1 = lbound(__libasr__created__var__13__array_constructor_, 1)
        __libasr__created__var__13__array_constructor_(__1__libasr_index_1) = m
        __1__libasr_index_1 = __1__libasr_index_1 + 1
        allocate(r(size(__libasr__created__var__13__array_constructor_, 1)))
        ! CheckBounds LHS = r, RHS = __libasr__created__var__13__array_constructor_, 
        __libasr_index_0_3 = lbound(__libasr__created__var__13__array_constructor_, 1)
        do __libasr_index_0_2 = lbound(r, 1), ubound(r, 1)
            r(__libasr_index_0_2) = __libasr__created__var__13__array_constructor_(__libasr_index_0_3)
            __libasr_index_0_3 = __libasr_index_0_3 + 1
        end do
    end if
end subroutine branches

subroutine countdown(n, r)
    integer(4) :: __1__libasr_index_
    integer(4) :: __1_t
    integer(4), dimension(1), parameter :: __libasr_array_constant_ = [0]
    integer(4), dimension(:), allocatable :: __libasr_created__array_constructor_
    integer(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4), intent(in) :: n
    integer(4), dimension(:), allocatable, intent(out) :: r
    if (Allocated(r)) then
        deallocate(r)
    end if
    if (n == 0) then
        allocate(r(size(__libasr_array_constant_, 1)))
        ! CheckBounds LHS = r, RHS = __libasr_array_constant_, 
        __libasr_index_0_1 = lbound(__libasr_array_constant_, 1)
        do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
            r(__libasr_index_0_) = __libasr_array_constant_(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    else
        deallocate(__libasr_created__subroutine_from_function_) ! Implicit deallocate
        call countdown(n - 1, __libasr_created__subroutine_from_function_)
        deallocate(__libasr_created__array_constructor_) ! Implicit deallocate
        __libasr_created__array_constructor_ = __libasr_created__subroutine_from_function_
        allocate(r(size(__libasr_created__array_constructor_) + 1))
        __1__libasr_index_ = lbound(r, 1)
        r(__1__libasr_index_) = n
        __1__libasr_index_ = __1__libasr_index_ + 1
        do __1_t = lbound(__libasr_created__array_constructor_, 1), ubound(__libasr_created__array_constructor_, 1)
            r(__1__libasr_index_) = __libasr_created__array_constructor_(__1_t)
            __1__libasr_index_ = __1__libasr_index_ + 1
        end do
    end if
end subroutine countdown

subroutine fill(x, t, n)
    integer(4) :: __1__libasr_index_
    integer(4) :: __1__libasr_index_1
    integer(4), dimension(:), allocatable :: __libasr__created__var__15__array_constructor_
    real(4), dimension(1) :: __libasr__created__var__16__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: i
    integer(4) :: lfortran_implied_do_i
    integer(4), intent(in) :: n
    type(tensor_t), intent(out) :: t
    integer(4), dimension(:), allocatable, intent(out) :: x
    if (Allocated(x)) then
        deallocate(x)
    end if
    if (Allocated(t%values_)) then
        deallocate(t%values_)
    end if
    deallocate(__libasr__created__var__15__array_constructor_)
    allocate(__libasr__created__var__15__array_constructor_(1*n))
    __1__libasr_index_ = lbound(__libasr__created__var__15__array_constructor_, 1)
    do lfortran_implied_do_i = 1, n
        __libasr__created__var__15__array_constructor_(__1__libasr_index_) = n
        __1__libasr_index_ = __1__libasr_index_ + 1
    end do
    allocate(x(size(__libasr__created__var__15__array_constructor_, 1)))
    ! CheckBounds LHS = x, RHS = __libasr__created__var__15__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr__created__var__15__array_constructor_, 1)
    do __libasr_index_0_ = lbound(x, 1), ubound(x, 1)
        x(__libasr_index_0_) = __libasr__created__var__15__array_constructor_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    __1__libasr_index_1 = lbound(__libasr__created__var__16__array_constructor_, 1)
    __libasr__created__var__16__array_constructor_(__1__libasr_index_1) = _lcompilers_real_i322(n)
    __1__libasr_index_1 = __1__libasr_index_1 + 1
    allocate(t%values_(size(__libasr__created__var__16__array_constructor_, 1)))
    ! CheckBounds LHS = t%values_, RHS = __libasr__created__var__16__array_constructor_, 
    __libasr_index_0_3 = lbound(__libasr__created__var__16__array_constructor_, 1)
    do __libasr_index_0_2 = lbound(t%values_, 1), ubound(t%values_, 1)
        t%values_(__libasr_index_0_2) = __libasr__created__var__16__array_constructor_(__libasr_index_0_3)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
end subroutine fill

subroutine locals(n, s)
    integer(4) :: __1__libasr_index_
    integer(4), dimension(:), allocatable :: __libasr__created__var__18__array_constructor_
    integer(4), dimension(5), parameter :: __libasr_array_constant_ = [1, 2, 3, 4, 5]
    integer(4), dimension(1), parameter :: __libasr_array_constant_1 = [10]
    integer(4) :: __libasr_created__intrinsic_array_function_Sum
    integer(4) :: __libasr_created__intrinsic_array_function_Sum1
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    integer(4), dimension(:), allocatable :: a
    integer(4) :: i
    integer(4) :: lfortran_implied_do_i
    integer(4), intent(in) :: n
    integer(4), intent(out) :: s
    deallocate(__libasr__created__var__18__array_constructor_)
    allocate(__libasr__created__var__18__array_constructor_(1*n))
    __1__libasr_index_ = lbound(__libasr__created__var__18__array_constructor_, 1)
    do lfortran_implied_do_i = 1, n
        __libasr__created__var__18__array_constructor_(__1__libasr_index_) = lfortran_implied_do_i
        __1__libasr_index_ = __1__libasr_index_ + 1
    end do
    allocate(a(size(__libasr__created__var__18__array_constructor_, 1)))
    ! CheckBounds LHS = a, RHS = __libasr__created__var__18__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr__created__var__18__array_constructor_, 1)
    do __libasr_index_0_ = lbound(a, 1), ubound(a, 1)
        a(__libasr_index_0_) = __libasr__created__var__18__array_constructor_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    s = _lcompilers_Sum_4_1_0_idx4(a)
    do i = 1, 3
        block : block
            integer(4) :: __1__libasr_index_
            integer(4), dimension(:), allocatable :: __libasr__created__var__17__array_constructor_
            integer(4) :: __libasr_created__intrinsic_array_function_Sum
            integer(4) :: __libasr_index_0_
            integer(4) :: __libasr_index_0_1
            integer(4), dimension(:), allocatable :: b
            integer(4) :: j
            integer(4) :: lfortran_implied_do_j
            deallocate(__libasr__created__var__17__array_constructor_)
            allocate(__libasr__created__var__17__array_constructor_(1*n))
            __1__libasr_index_ = lbound(__libasr__created__var__17__array_constructor_, 1)
            do lfortran_implied_do_j = 1, n
                __libasr__created__var__17__array_constructor_(__1__libasr_index_) = i
                __1__libasr_index_ = __1__libasr_index_ + 1
            end do
            allocate(b(size(__libasr__created__var__17__array_constructor_, 1)))
            ! CheckBounds LHS = b, RHS = __libasr__created__var__17__array_constructor_, 
            __libasr_index_0_1 = lbound(__libasr__created__var__17__array_constructor_, 1)
            do __libasr_index_0_ = lbound(b, 1), ubound(b, 1)
                b(__libasr_index_0_) = __libasr__created__var__17__array_constructor_(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
            __libasr_created__intrinsic_array_function_Sum = _lcompilers_Sum_4_1_0_idx4(b)
            s = s + __libasr_created__intrinsic_array_function_Sum
        end block block
    end do
    deallocate(a)
    allocate(a(size(__libasr_array_constant_, 1)))
    ! CheckBounds LHS = a, RHS = __libasr_array_constant_, 
    __libasr_index_0_3 = lbound(__libasr_array_constant_, 1)
    do __libasr_index_0_2 = lbound(a, 1), ubound(a, 1)
        a(__libasr_index_0_2) = __libasr_array_constant_(__libasr_index_0_3)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
    __libasr_created__intrinsic_array_function_Sum = _lcompilers_Sum_4_1_0_idx4(a)
    s = s + __libasr_created__intrinsic_array_function_Sum
    if (Allocated(a)) then
        deallocate(a)
    end if
    allocate(a(size(__libasr_array_constant_1, 1)))
    ! CheckBounds LHS = a, RHS = __libasr_array_constant_1, 
    __libasr_index_0_5 = lbound(__libasr_array_constant_1, 1)
    do __libasr_index_0_4 = lbound(a, 1), ubound(a, 1)
        a(__libasr_index_0_4) = __libasr_array_constant_1(__libasr_index_0_5)
        __libasr_index_0_5 = __libasr_index_0_5 + 1
    end do
    __libasr_created__intrinsic_array_function_Sum1 = _lcompilers_Sum_4_1_0_idx4(a)
    s = s + __libasr_created__intrinsic_array_function_Sum1
end subroutine locals

elemental subroutine scaled(x, k, t)
    integer(4) :: __1__libasr_index_
    real(4), dimension(:), allocatable :: __libasr__created__var__21__array_constructor_
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: i
    integer(4), intent(in) :: k
    integer(4) :: lfortran_implied_do_i
    type(tensor_t), intent(out) :: t
    real(4), intent(in) :: x
    if (Allocated(t%values_)) then
        deallocate(t%values_)
    end if
    deallocate(__libasr__created__var__21__array_constructor_)
    allocate(__libasr__created__var__21__array_constructor_(1*k))
    __1__libasr_index_ = lbound(__libasr__created__var__21__array_constructor_, 1)
    do lfortran_implied_do_i = 1, k
        __libasr__created__var__21__array_constructor_(__1__libasr_index_) = x*real(lfortran_implied_do_i, kind=4)
        __1__libasr_index_ = __1__libasr_index_ + 1
    end do
    allocate(t%values_(size(__libasr__created__var__21__array_constructor_, 1)))
    ! CheckBounds LHS = t%values_, RHS = __libasr__created__var__21__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr__created__var__21__array_constructor_, 1)
    do __libasr_index_0_ = lbound(t%values_, 1), ubound(t%values_, 1)
        t%values_(__libasr_index_0_) = __libasr__created__var__21__array_constructor_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine scaled

pure subroutine tensor(v, t)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    type(tensor_t), intent(out) :: t
    real(4), dimension(:), intent(in) :: v
    if (Allocated(t%values_)) then
        deallocate(t%values_)
    end if
    allocate(t%values_(size(v, 1)))
    ! CheckBounds LHS = t%values_, RHS = v, 
    __libasr_index_0_1 = lbound(v, 1)
    do __libasr_index_0_ = lbound(t%values_, 1), ubound(t%values_, 1)
        t%values_(__libasr_index_0_) = v(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine tensor

pure subroutine values(self, r)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), allocatable, intent(out) :: r
    type(tensor_t), intent(in) :: self
    if (Allocated(r)) then
        deallocate(r)
    end if
    allocate(r(size(self%values_, 1)))
    ! CheckBounds LHS = r, RHS = self%values_, 
    __libasr_index_0_1 = lbound(self%values_, 1)
    do __libasr_index_0_ = lbound(r, 1), ubound(r, 1)
        r(__libasr_index_0_) = self%values_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
end subroutine values

end module realloc_lhs_unallocated_01_m

program realloc_lhs_unallocated_01
use realloc_lhs_unallocated_01_m, only: branches
use realloc_lhs_unallocated_01_m, only: countdown
use realloc_lhs_unallocated_01_m, only: fill
use realloc_lhs_unallocated_01_m, only: locals
use realloc_lhs_unallocated_01_m, only: scaled
use realloc_lhs_unallocated_01_m, only: tensor
use realloc_lhs_unallocated_01_m, only: tensor_t
use realloc_lhs_unallocated_01_m, only: values
implicit none
integer(4) :: __1__libasr_index_
integer(4) :: __1__libasr_index_1
integer(4) :: __1_k
integer(4) :: __1_k1
integer(4) :: __1_k2
integer(4) :: __1_k3
integer(4) :: __1_k4
integer(4) :: __1_k5
integer(4) :: __1_k6
integer(4) :: __1_k7
integer(4) :: __1_k8
real(4), dimension(3) :: __libasr__created__var__0__array_constructor_
integer(4), dimension(3) :: __libasr__created__var__10__array_constant_
real(4), dimension(1) :: __libasr__created__var__11__array_constant_
real(4), dimension(3) :: __libasr__created__var__1__array_constructor_
real(4), dimension(3) :: __libasr__created__var__2__array_constant_
real(4), dimension(2) :: __libasr__created__var__3__array_constant_
integer(4), dimension(2) :: __libasr__created__var__4__array_constant_
integer(4), dimension(1) :: __libasr__created__var__5__array_constant_
integer(4), dimension(4) :: __libasr__created__var__6__array_constant_
integer(4), dimension(2) :: __libasr__created__var__8__array_constant_
real(4), dimension(1) :: __libasr__created__var__9__array_constant_
integer(4), dimension(7), parameter :: __libasr_array_constant_ = [1, 2, 3, 4, 5, 6, 7]
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any
logical(4) :: __libasr_created__intrinsic_array_function_Any1
logical(4) :: __libasr_created__intrinsic_array_function_Any10
logical(4) :: __libasr_created__intrinsic_array_function_Any11
logical(4) :: __libasr_created__intrinsic_array_function_Any12
logical(4), dimension(2) :: __libasr_created__intrinsic_array_function_Any2
logical(4), dimension(1) :: __libasr_created__intrinsic_array_function_Any3
logical(4), dimension(4) :: __libasr_created__intrinsic_array_function_Any4
logical(4) :: __libasr_created__intrinsic_array_function_Any5
logical(4) :: __libasr_created__intrinsic_array_function_Any6
logical(4) :: __libasr_created__intrinsic_array_function_Any7
logical(4) :: __libasr_created__intrinsic_array_function_Any8
logical(4) :: __libasr_created__intrinsic_array_function_Any9
real(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_
integer(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_1
integer(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_2
integer(4), dimension(:), allocatable :: __libasr_created__subroutine_from_function_3
integer(4), dimension(:), allocatable :: __libasr_created_integer_compare_left_
integer(4), dimension(:), allocatable :: __libasr_created_integer_compare_left_1
integer(4), dimension(:), allocatable :: __libasr_created_integer_compare_left_2
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
integer(4) :: k
integer(4) :: s
type(tensor_t) :: t
type(tensor_t), dimension(3) :: u
integer(4), dimension(:), allocatable :: x
do k = 1, 3
    __1__libasr_index_ = lbound(__libasr__created__var__0__array_constructor_, 1)
    __libasr__created__var__0__array_constructor_(__1__libasr_index_) = _lcompilers_real_i32(k)
    __1__libasr_index_ = __1__libasr_index_ + 1
    __libasr__created__var__0__array_constructor_(__1__libasr_index_) = 2.00000000e+00
    __1__libasr_index_ = __1__libasr_index_ + 1
    __libasr__created__var__0__array_constructor_(__1__libasr_index_) = 3.00000000e+00
    __1__libasr_index_ = __1__libasr_index_ + 1
    call tensor(__libasr__created__var__0__array_constructor_, t)
    __1__libasr_index_1 = lbound(__libasr__created__var__1__array_constructor_, 1)
    __libasr__created__var__1__array_constructor_(__1__libasr_index_1) = _lcompilers_real_i321(k)
    __1__libasr_index_1 = __1__libasr_index_1 + 1
    __libasr__created__var__1__array_constructor_(__1__libasr_index_1) = 2.00000000e+00
    __1__libasr_index_1 = __1__libasr_index_1 + 1
    __libasr__created__var__1__array_constructor_(__1__libasr_index_1) = 3.00000000e+00
    __1__libasr_index_1 = __1__libasr_index_1 + 1
    deallocate(__libasr_created__subroutine_from_function_) ! Implicit deallocate
    call values(t, __libasr_created__subroutine_from_function_)
    deallocate(__libasr_created_real_compare_left_) ! Implicit deallocate
    __libasr_created_real_compare_left_ = __libasr_created__subroutine_from_function_
    deallocate(__libasr_created__intrinsic_array_function_Any)
    allocate(__libasr_created__intrinsic_array_function_Any(size(__libasr_created_real_compare_left_, 1)))
    ! CheckBounds LHS = __libasr_created__intrinsic_array_function_Any, RHS = __libasr_created_real_compare_left_,&
         __libasr__created__var__1__array_constructor_, 
    __libasr_index_0_1 = lbound(__libasr_created_real_compare_left_, 1)
    __libasr_index_0_2 = lbound(__libasr__created__var__1__array_constructor_, 1)
    do __libasr_index_0_ = lbound(__libasr_created__intrinsic_array_function_Any, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any, 1)
        __libasr_created__intrinsic_array_function_Any(__libasr_index_0_) = __libasr_created_real_compare_left_(__libasr&
        _index_0_1) /= __libasr__created__var__1__array_constructor_(__libasr_index_0_2)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
        __libasr_index_0_2 = __libasr_index_0_2 + 1
    end do
    __libasr_created__intrinsic_array_function_Any9 = _lcompilers_Any_4_1_0(__libasr_created__intrinsic_array_function_A&
        ny)
    if (__libasr_created__intrinsic_array_function_Any9) then
        error stop
    end if
end do
__1_k = 1
__libasr__created__var__2__array_constant_(__1_k) = 1.00000000e+00
__1_k = 2
__libasr__created__var__2__array_constant_(__1_k) = 2.00000000e+00
__1_k = 3
__libasr__created__var__2__array_constant_(__1_k) = 3.00000000e+00
__libasr_index_0_4 = lbound(__libasr__created__var__2__array_constant_, 1)
do __libasr_index_0_3 = lbound(u, 1), ubound(u, 1)
    call scaled(__libasr__created__var__2__array_constant_(__libasr_index_0_4), 2, u(__libasr_index_0_3))
    __libasr_index_0_4 = __libasr_index_0_4 + 1
end do
__1_k1 = 1
__libasr__created__var__3__array_constant_(__1_k1) = 3.00000000e+00
__1_k1 = 2
__libasr__created__var__3__array_constant_(__1_k1) = 6.00000000e+00
__libasr_reduction_acc = .false.
! CheckBounds LHS = u(3)%values_, RHS = u(3)%values_, __libasr__created__var__3__array_constant_, 
__libasr_index_0_6 = lbound(__libasr__created__var__3__array_constant_, 1)
do __libasr_index_0_5 = lbound(u(3)%values_, 1), ubound(u(3)%values_, 1)
    __libasr_reduction_acc = __libasr_reduction_acc .or. u(3)%values_(__libasr_index_0_5) /= __libasr__created__var__3__&
        array_constant_(__libasr_index_0_6)
    __libasr_index_0_6 = __libasr_index_0_6 + 1
end do
__libasr_created__intrinsic_array_function_Any1 = __libasr_reduction_acc
if (__libasr_created__intrinsic_array_function_Any1) then
    error stop
end if
__1_k2 = 1
__libasr__created__var__4__array_constant_(__1_k2) = 3
__1_k2 = 2
__libasr__created__var__4__array_constant_(__1_k2) = 6
deallocate(__libasr_created__subroutine_from_function_1) ! Implicit deallocate
call branches(3, __libasr_created__subroutine_from_function_1)
deallocate(__libasr_created_integer_compare_left_) ! Implicit deallocate
__libasr_created_integer_compare_left_ = __libasr_created__subroutine_from_function_1
! CheckBounds LHS = __libasr_created__intrinsic_array_function_Any2, RHS = __libasr_created_integer_compare_left_,&
         __libasr__created__var__4__array_constant_, 
__libasr_index_0_8 = lbound(__libasr_created_integer_compare_left_, 1)
__libasr_index_0_9 = lbound(__libasr__created__var__4__array_constant_, 1)
do __libasr_index_0_7 = lbound(__libasr_created__intrinsic_array_function_Any2, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any2, 1)
    __libasr_created__intrinsic_array_function_Any2(__libasr_index_0_7) = __libasr_created_integer_compare_left_(__libas&
        r_index_0_8) /= __libasr__created__var__4__array_constant_(__libasr_index_0_9)
    __libasr_index_0_8 = __libasr_index_0_8 + 1
    __libasr_index_0_9 = __libasr_index_0_9 + 1
end do
__libasr_created__intrinsic_array_function_Any10 = _lcompilers_Any_4_1_0(__libasr_created__intrinsic_array_function_Any2&
        )
if (__libasr_created__intrinsic_array_function_Any10) then
    error stop
end if
__1_k3 = 1
__libasr__created__var__5__array_constant_(__1_k3) = 2
deallocate(__libasr_created__subroutine_from_function_2) ! Implicit deallocate
call branches(1, __libasr_created__subroutine_from_function_2)
deallocate(__libasr_created_integer_compare_left_1) ! Implicit deallocate
__libasr_created_integer_compare_left_1 = __libasr_created__subroutine_from_function_2
! CheckBounds LHS = __libasr_created__intrinsic_array_function_Any3, RHS = __libasr_created_integer_compare_left_1,&
         __libasr__created__var__5__array_constant_, 
__libasr_index_0_11 = lbound(__libasr_created_integer_compare_left_1, 1)
__libasr_index_0_12 = lbound(__libasr__created__var__5__array_constant_, 1)
do __libasr_index_0_10 = lbound(__libasr_created__intrinsic_array_function_Any3, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any3, 1)
    __libasr_created__intrinsic_array_function_Any3(__libasr_index_0_10) = __libasr_created_integer_compare_left_1(__lib&
        asr_index_0_11) /= __libasr__created__var__5__array_constant_(__libasr_index_0_12)
    __libasr_index_0_11 = __libasr_index_0_11 + 1
    __libasr_index_0_12 = __libasr_index_0_12 + 1
end do
__libasr_created__intrinsic_array_function_Any11 = _lcompilers_Any_4_1_0(__libasr_created__intrinsic_array_function_Any3&
        )
if (__libasr_created__intrinsic_array_function_Any11) then
    error stop
end if
__1_k4 = 1
__libasr__created__var__6__array_constant_(__1_k4) = 3
__1_k4 = 2
__libasr__created__var__6__array_constant_(__1_k4) = 2
__1_k4 = 3
__libasr__created__var__6__array_constant_(__1_k4) = 1
__1_k4 = 4
__libasr__created__var__6__array_constant_(__1_k4) = 0
deallocate(__libasr_created__subroutine_from_function_3) ! Implicit deallocate
call countdown(3, __libasr_created__subroutine_from_function_3)
deallocate(__libasr_created_integer_compare_left_2) ! Implicit deallocate
__libasr_created_integer_compare_left_2 = __libasr_created__subroutine_from_function_3
! CheckBounds LHS = __libasr_created__intrinsic_array_function_Any4, RHS = __libasr_created_integer_compare_left_2,&
         __libasr__created__var__6__array_constant_, 
__libasr_index_0_14 = lbound(__libasr_created_integer_compare_left_2, 1)
__libasr_index_0_15 = lbound(__libasr__created__var__6__array_constant_, 1)
do __libasr_index_0_13 = lbound(__libasr_created__intrinsic_array_function_Any4, 1),&
         ubound(__libasr_created__intrinsic_array_function_Any4, 1)
    __libasr_created__intrinsic_array_function_Any4(__libasr_index_0_13) = __libasr_created_integer_compare_left_2(__lib&
        asr_index_0_14) /= __libasr__created__var__6__array_constant_(__libasr_index_0_15)
    __libasr_index_0_14 = __libasr_index_0_14 + 1
    __libasr_index_0_15 = __libasr_index_0_15 + 1
end do
__libasr_created__intrinsic_array_function_Any12 = _lcompilers_Any_4_1_0(__libasr_created__intrinsic_array_function_Any4&
        )
if (__libasr_created__intrinsic_array_function_Any12) then
    error stop
end if
allocate(x(size(__libasr_array_constant_, 1)))
! CheckBounds LHS = x, RHS = __libasr_array_constant_, 
__libasr_index_0_17 = lbound(__libasr_array_constant_, 1)
do __libasr_index_0_16 = lbound(x, 1), ubound(x, 1)
    x(__libasr_index_0_16) = __libasr_array_constant_(__libasr_index_0_17)
    __libasr_index_0_17 = __libasr_index_0_17 + 1
end do
call fill(x, t, 2)
__1_k5 = 1
__libasr__created__var__8__array_constant_(__1_k5) = 2
__1_k5 = 2
__libasr__created__var__8__array_constant_(__1_k5) = 2
__1_k6 = 1
__libasr__created__var__9__array_constant_(__1_k6) = 2.00000000e+00
__libasr_reduction_acc1 = .false.
! CheckBounds LHS = x, RHS = x, __libasr__created__var__8__array_constant_, 
__libasr_index_0_19 = lbound(__libasr__created__var__8__array_constant_, 1)
do __libasr_index_0_18 = lbound(x, 1), ubound(x, 1)
    __libasr_reduction_acc1 = __libasr_reduction_acc1 .or. x(__libasr_index_0_18) /= __libasr__created__var__8__array_co&
        nstant_(__libasr_index_0_19)
    __libasr_index_0_19 = __libasr_index_0_19 + 1
end do
__libasr_created__intrinsic_array_function_Any5 = __libasr_reduction_acc1
__libasr_reduction_acc2 = .false.
! CheckBounds LHS = t%values_, RHS = t%values_, __libasr__created__var__9__array_constant_, 
__libasr_index_0_21 = lbound(__libasr__created__var__9__array_constant_, 1)
do __libasr_index_0_20 = lbound(t%values_, 1), ubound(t%values_, 1)
    __libasr_reduction_acc2 = __libasr_reduction_acc2 .or. t%values_(__libasr_index_0_20) /= __libasr__created__var__9__&
        array_constant_(__libasr_index_0_21)
    __libasr_index_0_21 = __libasr_index_0_21 + 1
end do
__libasr_created__intrinsic_array_function_Any6 = __libasr_reduction_acc2
if (__libasr_created__intrinsic_array_function_Any5 .or. __libasr_created__intrinsic_array_function_Any6) then
    error stop
end if
call fill(x, t, 3)
__1_k7 = 1
__libasr__created__var__10__array_constant_(__1_k7) = 3
__1_k7 = 2
__libasr__created__var__10__array_constant_(__1_k7) = 3
__1_k7 = 3
__libasr__created__var__10__array_constant_(__1_k7) = 3
__1_k8 = 1
__libasr__created__var__11__array_constant_(__1_k8) = 3.00000000e+00
__libasr_reduction_acc3 = .false.
! CheckBounds LHS = x, RHS = x, __libasr__created__var__10__array_constant_, 
__libasr_index_0_23 = lbound(__libasr__created__var__10__array_constant_, 1)
do __libasr_index_0_22 = lbound(x, 1), ubound(x, 1)
    __libasr_reduction_acc3 = __libasr_reduction_acc3 .or. x(__libasr_index_0_22) /= __libasr__created__var__10__array_c&
        onstant_(__libasr_index_0_23)
    __libasr_index_0_23 = __libasr_index_0_23 + 1
end do
__libasr_created__intrinsic_array_function_Any7 = __libasr_reduction_acc3
__libasr_reduction_acc4 = .false.
! CheckBounds LHS = t%values_, RHS = t%values_, __libasr__created__var__11__array_constant_, 
__libasr_index_0_25 = lbound(__libasr__created__var__11__array_constant_, 1)
do __libasr_index_0_24 = lbound(t%values_, 1), ubound(t%values_, 1)
    __libasr_reduction_acc4 = __libasr_reduction_acc4 .or. t%values_(__libasr_index_0_24) /= __libasr__created__var__11_&
        _array_constant_(__libasr_index_0_25)
    __libasr_index_0_25 = __libasr_index_0_25 + 1
end do
__libasr_created__intrinsic_array_function_Any8 = __libasr_reduction_acc4
if (__libasr_created__intrinsic_array_function_Any7 .or. __libasr_created__intrinsic_array_function_Any8) then
    error stop
end if
call locals(4, s)
if (s /= 10 + 4 + 8 + 12 + 15 + 10) then
    error stop
end if
print *, "ok"

contains

subroutine __libasr_intent_out_reset_tensor_t(self)
    use realloc_lhs_unallocated_01_m, only: 1_tensor_t => tensor_t
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

end program realloc_lhs_unallocated_01
