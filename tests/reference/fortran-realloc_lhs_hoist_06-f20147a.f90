module realloc_lhs_hoist_06_mod
implicit none
integer(4), save :: calls = 0

contains

logical(4) function counted_below(i, n)
    integer(4), intent(in) :: i
    integer(4), intent(in) :: n
    calls = calls + 1
    counted_below = i < n
end function counted_below

subroutine while_add(a, b, c, reps)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: b
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: i
    integer(4), intent(in) :: reps
    i = 0
    if (i < reps) then
        reallocate(c(size(a, 1)))
    end if
    do while (i < reps)
        i = i + 1
        a(1) = a(1) + real(1, kind=4)
        ! CheckBounds LHS = c, RHS = a, b, 
        __libasr_index_0_1 = lbound(a, 1)
        __libasr_index_0_2 = lbound(b, 1)
        do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
            c(__libasr_index_0_) = a(__libasr_index_0_1) + b(__libasr_index_0_2)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
            __libasr_index_0_2 = __libasr_index_0_2 + 1
        end do
    end do
end subroutine while_add

subroutine while_calls(a, c, reps)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: i
    integer(4), intent(in) :: reps
    i = 0
    do while (counted_below(i, reps))
        i = i + 1
        reallocate(c(size(a, 1)))
        ! CheckBounds LHS = c, RHS = a, 
        __libasr_index_0_1 = lbound(a, 1)
        do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
            c(__libasr_index_0_) = a(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    end do
end subroutine while_calls

subroutine while_exit_first(a, c, k)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    logical(4) :: __libasr_reallocations_do_nothing
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: i
    integer(4), intent(in) :: k
    i = 0
    __libasr_reallocations_do_nothing = .true.
    if (__libasr_reallocations_do_nothing) then
        __libasr_reallocations_do_nothing = .false.
        if (Allocated(c)) then
            if (Allocated(a)) then
                __libasr_reallocations_do_nothing = .not. size(c, 1) /= size(a, 1)
            end if
        end if
    end if
    if (__libasr_reallocations_do_nothing) then
        do while (i < 3)
            i = i + 1
            if (i == k) then
                exit
            end if
            ! CheckBounds LHS = c, RHS = a, 
            __libasr_index_0_1 = lbound(a, 1)
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end do
    else
        do while (i < 3)
            i = i + 1
            if (i == k) then
                exit
            end if
            reallocate(c(size(a, 1)))
            ! CheckBounds LHS = c, RHS = a, 
            __libasr_index_0_1 = lbound(a, 1)
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end do
    end if
end subroutine while_exit_first

subroutine while_reads_target(a, c, trips)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    logical(4) :: __libasr_reallocations_do_nothing
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4), intent(out) :: trips
    trips = 0
    __libasr_reallocations_do_nothing = .true.
    if (__libasr_reallocations_do_nothing) then
        __libasr_reallocations_do_nothing = .false.
        if (Allocated(c)) then
            __libasr_reallocations_do_nothing = .not. size(c, 1) /= (size(c) + 1 - 1)/1 + 1
        end if
    end if
    if (__libasr_reallocations_do_nothing) then
        do while (size(c) < 3)
            trips = trips + 1
            __libasr_index_0_1 = 1
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end do
    else
        do while (size(c) < 3)
            trips = trips + 1
            reallocate(c((size(c) + 1 - 1)/1 + 1))
            __libasr_index_0_1 = 1
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end do
    end if
end subroutine while_reads_target

end module realloc_lhs_hoist_06_mod

program realloc_lhs_hoist_06
use realloc_lhs_hoist_06_mod, only: calls
use realloc_lhs_hoist_06_mod, only: counted_below
use realloc_lhs_hoist_06_mod, only: while_add
use realloc_lhs_hoist_06_mod, only: while_calls
use realloc_lhs_hoist_06_mod, only: while_exit_first
use realloc_lhs_hoist_06_mod, only: while_reads_target
implicit none
integer(4) :: __libasr_index_0_
integer(4) :: __libasr_lbound_
integer(4) :: __libasr_lbound_1
real(4), dimension(:), allocatable :: a
real(4), dimension(:), allocatable :: b
real(4), dimension(:), allocatable :: c
integer(4) :: trips
__libasr_lbound_ = lbound(a, 1)
a(__libasr_lbound_ + 0) = 1.00000000e+00
a(__libasr_lbound_ + 1) = 2.00000000e+00
a(__libasr_lbound_ + 2) = 3.00000000e+00
__libasr_lbound_1 = lbound(b, 1)
b(__libasr_lbound_1 + 0) = 1.00000000e+01
b(__libasr_lbound_1 + 1) = 2.00000000e+01
b(__libasr_lbound_1 + 2) = 3.00000000e+01
call while_add(a, b, c, 3)
if (.not. Allocated(c)) then
    error stop
end if
if (size(c) /= 3) then
    error stop
end if
if (Any(c /= [1.40000000e+01, 2.20000000e+01, 3.30000000e+01])) then
    error stop
end if
deallocate(c)
allocate(c(2))
do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
    c(__libasr_index_0_) = real(5, kind=4)
end do
call while_add(a, b, c, 0)
if (size(c) /= 2) then
    error stop
end if
if (Any(c /= real(5, kind=4))) then
    error stop
end if
call while_add(a, b, c, 1)
if (size(c) /= 3) then
    error stop
end if
deallocate(c)
allocate(c(1))
call while_reads_target(a, c, trips)
if (trips /= 2 .or. size(c) /= 3) then
    error stop
end if
calls = 0
deallocate(c)
call while_calls(a, c, 2)
if (calls /= 3) then
    error stop
end if
if (size(c) /= 3) then
    error stop
end if
calls = 0
deallocate(c)
call while_calls(a, c, 0)
if (calls /= 1) then
    error stop
end if
if (Allocated(c)) then
    error stop
end if
call while_exit_first(a, c, 1)
if (Allocated(c)) then
    error stop
end if
call while_exit_first(a, c, 2)
if (size(c) /= 3) then
    error stop
end if
print *, "ok"
end program realloc_lhs_hoist_06
