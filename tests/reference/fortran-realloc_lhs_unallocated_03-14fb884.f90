module realloc_lhs_unallocated_03_m
implicit none
integer(4), save :: calls = 0

contains

subroutine run(p)
    interface
        subroutine p()
        end subroutine p

    end interface
    call p()
end subroutine run

subroutine tick()
    calls = calls + 1
end subroutine tick

end module realloc_lhs_unallocated_03_m

program realloc_lhs_unallocated_03
use realloc_lhs_unallocated_03_m, only: calls
use realloc_lhs_unallocated_03_m, only: run
use realloc_lhs_unallocated_03_m, only: tick
implicit none
call untouched()
call allocated_by_internal()
call allocated_through_argument()
if (calls /= 3) then
    error stop
end if
print *, "ok"

contains

subroutine allocated_by_internal()
    type :: __lcompilers_frame_allocated_by_internal
        real(4), dimension(:), allocatable, reference :: t
    end type __lcompilers_frame_allocated_by_internal
    integer(4) :: __1_k
    type(__lcompilers_frame_allocated_by_internal) :: __lcompilers_frame
    real(4), dimension(2) :: __libasr__created__var__1__array_constant_
    real(4), dimension(2), parameter :: __libasr_array_constant_ = [1.00000000e+00, 2.00000000e+00]
    logical(4) :: __libasr_created__intrinsic_array_function_Any
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    logical(4) :: __libasr_reduction_acc
    real(4), dimension(:), allocatable :: t
    bind_reference(__lcompilers_frame%t, t)
    call tick()
    call make(__lcompilers_frame)
    if (size(t) /= 5) then
        error stop
    end if
    reallocate(t(size(__libasr_array_constant_, 1)))
    ! CheckBounds LHS = t, RHS = __libasr_array_constant_, 
    __libasr_index_0_1 = lbound(__libasr_array_constant_, 1)
    do __libasr_index_0_ = lbound(t, 1), ubound(t, 1)
        t(__libasr_index_0_) = __libasr_array_constant_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    __1_k = 1
    __libasr__created__var__1__array_constant_(__1_k) = 1.00000000e+00
    __1_k = 2
    __libasr__created__var__1__array_constant_(__1_k) = 2.00000000e+00
    __libasr_reduction_acc = .false.
    ! CheckBounds LHS = t, RHS = t, __libasr__created__var__1__array_constant_, 
    __libasr_index_0_3 = lbound(__libasr__created__var__1__array_constant_, 1)
    do __libasr_index_0_2 = lbound(t, 1), ubound(t, 1)
        __libasr_reduction_acc = __libasr_reduction_acc .or. t(__libasr_index_0_2) /= __libasr__created__var__1__array_c&
        onstant_(__libasr_index_0_3)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
    __libasr_created__intrinsic_array_function_Any = __libasr_reduction_acc
    if (size(t) /= 2 .or. __libasr_created__intrinsic_array_function_Any) then
        error stop
    end if
    contains
    subroutine make(__lcompilers_frame)
        type(__lcompilers_frame_allocated_by_internal), intent(in) :: __lcompilers_frame
        integer(4) :: __libasr_index_0_
        real(4), dimension(:), allocatable, host_associated :: t => __lcompilers_frame%t
        allocate(t(5))
        do __libasr_index_0_ = lbound(t, 1), ubound(t, 1)
            t(__libasr_index_0_) = real(0, kind=4)
        end do
    end subroutine make

end subroutine allocated_by_internal

subroutine allocated_through_argument()
    type :: __lcompilers_frame_allocated_through_argument
        integer(4), reference :: __lcompilers_held_frame_slot
        real(4), dimension(:), allocatable, reference :: t
    end type __lcompilers_frame_allocated_through_argument
    type :: __lcompilers_frame_slot_allocated_through_argument
        type(__lcompilers_frame_allocated_through_argument), pointer :: frame
    end type __lcompilers_frame_slot_allocated_through_argument
    integer(4) :: __1_k
    type(__lcompilers_frame_allocated_through_argument), pointer, save :: __lcompilers_current_frame
    type(__lcompilers_frame_allocated_through_argument) :: __lcompilers_frame
    integer(4) :: __lcompilers_frame_slot
    type(__lcompilers_frame_slot_allocated_through_argument), dimension(8), save :: __lcompilers_frame_slots
    integer(4) :: __lcompilers_held_frame_slot
    real(4), dimension(3) :: __libasr__created__var__3__array_constant_
    real(4), dimension(3), parameter :: __libasr_array_constant_ = [7.00000000e+00, 8.00000000e+00, 9.00000000e+00]
    logical(4) :: __libasr_created__intrinsic_array_function_Any
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    logical(4) :: __libasr_reduction_acc
    procedure(make__lcompilers_value_1), pointer :: make__lcompilers_value_pointer
    real(4), dimension(:), allocatable :: t
    bind_reference(__lcompilers_frame%__lcompilers_held_frame_slot, __lcompilers_held_frame_slot)
    bind_reference(__lcompilers_frame%t, t)
    __lcompilers_held_frame_slot = 0
    call tick()
    call __lcompilers_acquire_frame_slot(__lcompilers_frame, __lcompilers_frame_slot)
    call make__lcompilers_value_select(__lcompilers_frame_slot, make__lcompilers_value_pointer)
    call run(make__lcompilers_value_pointer)
    if (.not. Allocated(t)) then
        error stop
    end if
    reallocate(t(size(__libasr_array_constant_, 1)))
    ! CheckBounds LHS = t, RHS = __libasr_array_constant_, 
    __libasr_index_0_1 = lbound(__libasr_array_constant_, 1)
    do __libasr_index_0_ = lbound(t, 1), ubound(t, 1)
        t(__libasr_index_0_) = __libasr_array_constant_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    __1_k = 1
    __libasr__created__var__3__array_constant_(__1_k) = 7.00000000e+00
    __1_k = 2
    __libasr__created__var__3__array_constant_(__1_k) = 8.00000000e+00
    __1_k = 3
    __libasr__created__var__3__array_constant_(__1_k) = 9.00000000e+00
    __libasr_reduction_acc = .false.
    ! CheckBounds LHS = t, RHS = t, __libasr__created__var__3__array_constant_, 
    __libasr_index_0_3 = lbound(__libasr__created__var__3__array_constant_, 1)
    do __libasr_index_0_2 = lbound(t, 1), ubound(t, 1)
        __libasr_reduction_acc = __libasr_reduction_acc .or. t(__libasr_index_0_2) /= __libasr__created__var__3__array_c&
        onstant_(__libasr_index_0_3)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
    __libasr_created__intrinsic_array_function_Any = __libasr_reduction_acc
    if (size(t) /= 3 .or. lbound(t, 1) /= 1 .or. __libasr_created__intrinsic_array_function_Any) then
        error stop
    end if
    if (__lcompilers_held_frame_slot /= 0) then
        call __lcompilers_release_frame_slot(__lcompilers_frame)
    end if
    contains
    subroutine __lcompilers_acquire_frame_slot(__lcompilers_frame, index)
        type(__lcompilers_frame_allocated_through_argument), intent(in), target :: __lcompilers_frame
        integer(4), host_associated :: __lcompilers_held_frame_slot => __lcompilers_frame%__lcompilers_held_frame_slot
        integer(4) :: i
        integer(4), intent(out) :: index
        if (__lcompilers_held_frame_slot == 0) then
            i = 1
            do while (i <= 8 .and. __lcompilers_held_frame_slot == 0)
                if (.not. associated(__lcompilers_frame_slots(i)%frame)) then
                    __lcompilers_frame_slots(i)%frame => __lcompilers_frame
                    __lcompilers_held_frame_slot = i
                end if
                i = i + 1
            end do
            if (__lcompilers_held_frame_slot == 0) then
                error stop
            end if
        end if
        index = __lcompilers_held_frame_slot
    end subroutine __lcompilers_acquire_frame_slot

    subroutine __lcompilers_release_frame_slot(__lcompilers_frame)
        type(__lcompilers_frame_allocated_through_argument), intent(in), target :: __lcompilers_frame
        integer(4), host_associated :: __lcompilers_held_frame_slot => __lcompilers_frame%__lcompilers_held_frame_slot
        nullify (__lcompilers_frame_slots(__lcompilers_held_frame_slot)%frame)
        __lcompilers_held_frame_slot = 0
    end subroutine __lcompilers_release_frame_slot

    subroutine make(__lcompilers_frame)
        type(__lcompilers_frame_allocated_through_argument), intent(in) :: __lcompilers_frame
        integer(4) :: __libasr_index_0_
        real(4), dimension(:), allocatable, host_associated :: t => __lcompilers_frame%t
        allocate(t(2))
        do __libasr_index_0_ = lbound(t, 1), ubound(t, 1)
            t(__libasr_index_0_) = real(1, kind=4)
        end do
    end subroutine make

    subroutine make__lcompilers_value()
        call make(__lcompilers_current_frame)
    end subroutine make__lcompilers_value

    subroutine make__lcompilers_value_1()
        call make(__lcompilers_frame_slots(1)%frame)
    end subroutine make__lcompilers_value_1

    subroutine make__lcompilers_value_2()
        call make(__lcompilers_frame_slots(2)%frame)
    end subroutine make__lcompilers_value_2

    subroutine make__lcompilers_value_3()
        call make(__lcompilers_frame_slots(3)%frame)
    end subroutine make__lcompilers_value_3

    subroutine make__lcompilers_value_4()
        call make(__lcompilers_frame_slots(4)%frame)
    end subroutine make__lcompilers_value_4

    subroutine make__lcompilers_value_5()
        call make(__lcompilers_frame_slots(5)%frame)
    end subroutine make__lcompilers_value_5

    subroutine make__lcompilers_value_6()
        call make(__lcompilers_frame_slots(6)%frame)
    end subroutine make__lcompilers_value_6

    subroutine make__lcompilers_value_7()
        call make(__lcompilers_frame_slots(7)%frame)
    end subroutine make__lcompilers_value_7

    subroutine make__lcompilers_value_8()
        call make(__lcompilers_frame_slots(8)%frame)
    end subroutine make__lcompilers_value_8

    subroutine make__lcompilers_value_select(index, pointer)
        integer(4), intent(in) :: index
        procedure(make__lcompilers_value_1), pointer, intent(out) :: pointer
        if (index == 1) then
            pointer => make__lcompilers_value_1
        end if
        if (index == 2) then
            pointer => make__lcompilers_value_2
        end if
        if (index == 3) then
            pointer => make__lcompilers_value_3
        end if
        if (index == 4) then
            pointer => make__lcompilers_value_4
        end if
        if (index == 5) then
            pointer => make__lcompilers_value_5
        end if
        if (index == 6) then
            pointer => make__lcompilers_value_6
        end if
        if (index == 7) then
            pointer => make__lcompilers_value_7
        end if
        if (index == 8) then
            pointer => make__lcompilers_value_8
        end if
    end subroutine make__lcompilers_value_select

end subroutine allocated_through_argument

subroutine untouched()
    type :: __lcompilers_frame_untouched
        integer(4), reference :: n
    end type __lcompilers_frame_untouched
    integer(4) :: __1_k
    type(__lcompilers_frame_untouched) :: __lcompilers_frame
    real(4), dimension(3) :: __libasr__created__var__5__array_constant_
    real(4), dimension(3), parameter :: __libasr_array_constant_ = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
    logical(4) :: __libasr_created__intrinsic_array_function_Any
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    logical(4) :: __libasr_reduction_acc
    integer(4) :: n
    real(4), dimension(:), allocatable :: t
    bind_reference(__lcompilers_frame%n, n)
    n = 0
    call tick()
    call count_up(__lcompilers_frame)
    allocate(t(size(__libasr_array_constant_, 1)))
    ! CheckBounds LHS = t, RHS = __libasr_array_constant_, 
    __libasr_index_0_1 = lbound(__libasr_array_constant_, 1)
    do __libasr_index_0_ = lbound(t, 1), ubound(t, 1)
        t(__libasr_index_0_) = __libasr_array_constant_(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    __1_k = 1
    __libasr__created__var__5__array_constant_(__1_k) = 1.00000000e+00
    __1_k = 2
    __libasr__created__var__5__array_constant_(__1_k) = 2.00000000e+00
    __1_k = 3
    __libasr__created__var__5__array_constant_(__1_k) = 3.00000000e+00
    __libasr_reduction_acc = .false.
    ! CheckBounds LHS = t, RHS = t, __libasr__created__var__5__array_constant_, 
    __libasr_index_0_3 = lbound(__libasr__created__var__5__array_constant_, 1)
    do __libasr_index_0_2 = lbound(t, 1), ubound(t, 1)
        __libasr_reduction_acc = __libasr_reduction_acc .or. t(__libasr_index_0_2) /= __libasr__created__var__5__array_c&
        onstant_(__libasr_index_0_3)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
    end do
    __libasr_created__intrinsic_array_function_Any = __libasr_reduction_acc
    if (size(t) /= 3 .or. lbound(t, 1) /= 1 .or. __libasr_created__intrinsic_array_function_Any) then
        error stop
    end if
    if (n /= 1) then
        error stop
    end if
    contains
    subroutine count_up(__lcompilers_frame)
        type(__lcompilers_frame_untouched), intent(in) :: __lcompilers_frame
        integer(4), host_associated :: n => __lcompilers_frame%n
        n = n + 1
    end subroutine count_up

end subroutine untouched

end program realloc_lhs_unallocated_03
