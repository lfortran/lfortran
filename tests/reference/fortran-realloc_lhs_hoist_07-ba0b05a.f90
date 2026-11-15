module realloc_lhs_hoist_07_mod
implicit none
type :: state_t
    real(4), dimension(:), allocatable :: v
    real(4), dimension(:), allocatable :: x
end type state_t
type :: outer_t
    type(state_t) :: s
end type outer_t

contains

subroutine component(p, q, reps)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: i
    type(state_t), intent(inout) :: p
    type(state_t), intent(inout) :: q
    integer(4), intent(in) :: reps
    if (1 <= reps) then
        reallocate(q%x(size(p%x, 1)))
    end if
    do i = 1, reps
        p%x(1) = p%x(1) + real(1, kind=4)
        ! CheckBounds LHS = q%x, RHS = p%x, p%v, p%v, 
        __libasr_index_0_1 = lbound(p%x, 1)
        __libasr_index_0_2 = lbound(p%v, 1)
        do __libasr_index_0_ = lbound(q%x, 1), ubound(q%x, 1)
            q%x(__libasr_index_0_) = p%x(__libasr_index_0_1) + real(2, kind=4)*p%v(__libasr_index_0_2)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
            __libasr_index_0_2 = __libasr_index_0_2 + 1
        end do
    end do
end subroutine component

subroutine nested_component(p, o, reps)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: i
    type(outer_t), intent(inout) :: o
    type(state_t), intent(inout) :: p
    integer(4), intent(in) :: reps
    if (1 <= reps) then
        reallocate(o%s%v(size(p%v, 1)))
    end if
    do i = 1, reps
        ! CheckBounds LHS = o%s%v, RHS = p%v, p%v, 
        __libasr_index_0_1 = lbound(p%v, 1)
        do __libasr_index_0_ = lbound(o%s%v, 1), ubound(o%s%v, 1)
            o%s%v(__libasr_index_0_) = real(3, kind=4)*p%v(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    end do
end subroutine nested_component

subroutine source_changes(p, q, sizes)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: i
    type(state_t), intent(inout) :: p
    type(state_t), intent(inout) :: q
    integer(4), dimension(2), intent(out) :: sizes
    do i = 1, 2
        reallocate(q%x(size(p%v, 1)))
        ! CheckBounds LHS = q%x, RHS = p%v, 
        __libasr_index_0_1 = lbound(p%v, 1)
        do __libasr_index_0_ = lbound(q%x, 1), ubound(q%x, 1)
            q%x(__libasr_index_0_) = p%v(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        sizes(i) = size(q%x)
        deallocate(p%v)
        allocate(p%v(5))
        do __libasr_index_0_2 = lbound(p%v, 1), ubound(p%v, 1)
            p%v(__libasr_index_0_2) = real(1, kind=4)
        end do
    end do
end subroutine source_changes

subroutine target_component(p, q, reps)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    logical(4) :: __libasr_reallocations_do_nothing
    integer(4) :: i
    type(state_t), intent(inout) :: p
    type(state_t), intent(inout), target :: q
    integer(4), intent(in) :: reps
    __libasr_reallocations_do_nothing = .true.
    if (__libasr_reallocations_do_nothing) then
        __libasr_reallocations_do_nothing = .false.
        if (Allocated(q%x)) then
            if (Allocated(p%x)) then
                __libasr_reallocations_do_nothing = .not. size(q%x, 1) /= size(p%x, 1)
            end if
        end if
    end if
    if (__libasr_reallocations_do_nothing) then
        do i = 1, reps
            ! CheckBounds LHS = q%x, RHS = p%x, 
            __libasr_index_0_1 = lbound(p%x, 1)
            do __libasr_index_0_ = lbound(q%x, 1), ubound(q%x, 1)
                q%x(__libasr_index_0_) = p%x(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end do
    else
        do i = 1, reps
            reallocate(q%x(size(p%x, 1)))
            ! CheckBounds LHS = q%x, RHS = p%x, 
            __libasr_index_0_1 = lbound(p%x, 1)
            do __libasr_index_0_ = lbound(q%x, 1), ubound(q%x, 1)
                q%x(__libasr_index_0_) = p%x(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end do
    end if
end subroutine target_component

subroutine two_components(p, q, reps)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: i
    type(state_t), intent(inout) :: p
    type(state_t), intent(inout) :: q
    integer(4), intent(in) :: reps
    if (1 <= reps) then
        reallocate(q%x(size(p%x, 1)))
    end if
    do i = 1, reps
        ! CheckBounds LHS = q%x, RHS = p%x, 
        __libasr_index_0_1 = lbound(p%x, 1)
        do __libasr_index_0_ = lbound(q%x, 1), ubound(q%x, 1)
            q%x(__libasr_index_0_) = p%x(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        __libasr_index_0_3 = 1
        do __libasr_index_0_2 = lbound(q%v, 1), ubound(q%v, 1)
            q%v(__libasr_index_0_2) = q%x(__libasr_index_0_3)
            __libasr_index_0_3 = __libasr_index_0_3 + 1
        end do
    end do
end subroutine two_components

end module realloc_lhs_hoist_07_mod

program realloc_lhs_hoist_07
use realloc_lhs_hoist_07_mod, only: component
use realloc_lhs_hoist_07_mod, only: nested_component
use realloc_lhs_hoist_07_mod, only: outer_t
use realloc_lhs_hoist_07_mod, only: source_changes
use realloc_lhs_hoist_07_mod, only: state_t
use realloc_lhs_hoist_07_mod, only: target_component
use realloc_lhs_hoist_07_mod, only: two_components
implicit none
integer(4) :: __libasr_lbound_
integer(4) :: __libasr_lbound_1
type(outer_t) :: o
type(state_t) :: p
type(state_t) :: q
integer(4), dimension(2) :: sizes
allocate(p%x(3), p%v(3))
__libasr_lbound_ = lbound(p%x, 1)
p%x(__libasr_lbound_ + 0) = 1.00000000e+00
p%x(__libasr_lbound_ + 1) = 2.00000000e+00
p%x(__libasr_lbound_ + 2) = 3.00000000e+00
__libasr_lbound_1 = lbound(p%v, 1)
p%v(__libasr_lbound_1 + 0) = 1.00000000e+01
p%v(__libasr_lbound_1 + 1) = 2.00000000e+01
p%v(__libasr_lbound_1 + 2) = 3.00000000e+01
call component(p, q, 2)
if (.not. Allocated(q%x)) then
    error stop
end if
if (size(q%x) /= 3) then
    error stop
end if
if (Any(q%x /= [2.30000000e+01, 4.20000000e+01, 6.30000000e+01])) then
    error stop
end if
deallocate(q%x)
call component(p, q, 0)
if (Allocated(q%x)) then
    error stop
end if
allocate(q%x(7))
call component(p, q, 1)
if (size(q%x) /= 3) then
    error stop
end if
call nested_component(p, o, 2)
if (size(o%s%v) /= 3) then
    error stop
end if
if (Any(o%s%v /= [3.00000000e+01, 6.00000000e+01, 9.00000000e+01])) then
    error stop
end if
call two_components(p, q, 2)
if (size(q%v) /= 2) then
    error stop
end if
call source_changes(p, q, sizes)
if (Any(sizes /= [3, 5])) then
    error stop
end if
deallocate(q%x)
call target_component(p, q, 2)
if (size(q%x) /= 3) then
    error stop
end if
print *, "ok"
end program realloc_lhs_hoist_07
