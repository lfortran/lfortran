module realloc_lhs_version_01_mod
implicit none
real(4), dimension(:), allocatable :: mc
integer(4), save :: ticks = 0

contains

subroutine call_first(a, c, reps)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    logical(4) :: __libasr_reallocations_do_nothing
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: i
    integer(4), intent(in) :: reps
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
        do i = 1, reps
            call tick()
            ! CheckBounds LHS = c, RHS = a, 
            __libasr_index_0_1 = lbound(a, 1)
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end do
    else
        do i = 1, reps
            call tick()
            reallocate(c(size(a, 1)))
            ! CheckBounds LHS = c, RHS = a, 
            __libasr_index_0_1 = lbound(a, 1)
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end do
    end if
end subroutine call_first

subroutine changing(a, c, sizes)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: i
    integer(4) :: k
    integer(4), dimension(3), intent(out) :: sizes
    k = 1
    do i = 1, 3
        if (k > 0) then
            reallocate(c((k - 1)/1 + 1))
            __libasr_index_0_1 = 1
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end if
        sizes(i) = size(c)
        k = k + 1
    end do
end subroutine changing

subroutine conditional(a, b, c, reps)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    logical(4) :: __libasr_reallocations_do_nothing
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: b
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: i
    integer(4), intent(in) :: reps
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
        __libasr_reallocations_do_nothing = .false.
        if (Allocated(c)) then
            if (Allocated(a)) then
                __libasr_reallocations_do_nothing = .not. size(c, 1) /= size(a, 1)
            end if
        end if
    end if
    if (__libasr_reallocations_do_nothing) then
        do i = 1, reps
            if (Mod(i, 2) == 0) then
                ! CheckBounds LHS = c, RHS = a, b, 
                __libasr_index_0_1 = lbound(a, 1)
                __libasr_index_0_2 = lbound(b, 1)
                do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                    c(__libasr_index_0_) = a(__libasr_index_0_1) + b(__libasr_index_0_2)
                    __libasr_index_0_1 = __libasr_index_0_1 + 1
                    __libasr_index_0_2 = __libasr_index_0_2 + 1
                end do
            else
                ! CheckBounds LHS = c, RHS = a, b, 
                __libasr_index_0_4 = lbound(a, 1)
                __libasr_index_0_5 = lbound(b, 1)
                do __libasr_index_0_3 = lbound(c, 1), ubound(c, 1)
                    c(__libasr_index_0_3) = a(__libasr_index_0_4) - b(__libasr_index_0_5)
                    __libasr_index_0_4 = __libasr_index_0_4 + 1
                    __libasr_index_0_5 = __libasr_index_0_5 + 1
                end do
            end if
        end do
    else
        do i = 1, reps
            if (Mod(i, 2) == 0) then
                reallocate(c(size(a, 1)))
                ! CheckBounds LHS = c, RHS = a, b, 
                __libasr_index_0_1 = lbound(a, 1)
                __libasr_index_0_2 = lbound(b, 1)
                do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                    c(__libasr_index_0_) = a(__libasr_index_0_1) + b(__libasr_index_0_2)
                    __libasr_index_0_1 = __libasr_index_0_1 + 1
                    __libasr_index_0_2 = __libasr_index_0_2 + 1
                end do
            else
                reallocate(c(size(a, 1)))
                ! CheckBounds LHS = c, RHS = a, b, 
                __libasr_index_0_4 = lbound(a, 1)
                __libasr_index_0_5 = lbound(b, 1)
                do __libasr_index_0_3 = lbound(c, 1), ubound(c, 1)
                    c(__libasr_index_0_3) = a(__libasr_index_0_4) - b(__libasr_index_0_5)
                    __libasr_index_0_4 = __libasr_index_0_4 + 1
                    __libasr_index_0_5 = __libasr_index_0_5 + 1
                end do
            end if
        end do
    end if
end subroutine conditional

subroutine deallocates(a, c, n)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: i
    integer(4), intent(out) :: n
    n = 0
    do i = 1, 3
        if (i > 0) then
            reallocate(c(size(a, 1)))
            ! CheckBounds LHS = c, RHS = a, 
            __libasr_index_0_1 = lbound(a, 1)
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end if
        n = n + size(c)
        deallocate(c)
        allocate(c(1))
    end do
end subroutine deallocates

subroutine exit_first(a, c, k)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    logical(4) :: __libasr_reallocations_do_nothing
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: i
    integer(4), intent(in) :: k
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
        do i = 1, 3
            if (i == k) then
                exit
            end if
            ! CheckBounds LHS = c, RHS = a, a, 
            __libasr_index_0_1 = lbound(a, 1)
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = real(2, kind=4)*a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end do
    else
        do i = 1, 3
            if (i == k) then
                exit
            end if
            reallocate(c(size(a, 1)))
            ! CheckBounds LHS = c, RHS = a, a, 
            __libasr_index_0_1 = lbound(a, 1)
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = real(2, kind=4)*a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end do
    end if
end subroutine exit_first

subroutine maybe_absent(c, o)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: i
    real(4), dimension(:), intent(in), optional :: o
    do i = 1, 3
        if (Present(o)) then
            reallocate(c(size(o, 1)))
            ! CheckBounds LHS = c, RHS = o, 
            __libasr_index_0_1 = lbound(o, 1)
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = o(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end if
    end do
end subroutine maybe_absent

subroutine maybe_unallocated(flag, u, c)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    logical(4) :: __libasr_reallocations_do_nothing
    real(4), dimension(:), allocatable, intent(inout) :: c
    logical(4), intent(in) :: flag
    integer(4) :: i
    real(4), dimension(:), allocatable, intent(inout) :: u
    __libasr_reallocations_do_nothing = .true.
    if (__libasr_reallocations_do_nothing) then
        __libasr_reallocations_do_nothing = .false.
        if (Allocated(c)) then
            if (Allocated(u)) then
                __libasr_reallocations_do_nothing = .not. size(c, 1) /= size(u, 1)
            end if
        end if
    end if
    if (__libasr_reallocations_do_nothing) then
        do i = 1, 3
            if (flag) then
                ! CheckBounds LHS = c, RHS = u, 
                __libasr_index_0_1 = lbound(u, 1)
                do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                    c(__libasr_index_0_) = u(__libasr_index_0_1)
                    __libasr_index_0_1 = __libasr_index_0_1 + 1
                end do
            end if
        end do
    else
        do i = 1, 3
            if (flag) then
                reallocate(c(size(u, 1)))
                ! CheckBounds LHS = c, RHS = u, 
                __libasr_index_0_1 = lbound(u, 1)
                do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                    c(__libasr_index_0_) = u(__libasr_index_0_1)
                    __libasr_index_0_1 = __libasr_index_0_1 + 1
                end do
            end if
        end do
    end if
end subroutine maybe_unallocated

subroutine module_target(a, sizes)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), allocatable, intent(inout) :: a
    integer(4) :: i
    integer(4), dimension(2), intent(out) :: sizes
    do i = 1, 2
        if (i > 1) then
            call reset_mc()
        end if
        if (i > 0) then
            reallocate(mc(size(a, 1)))
            ! CheckBounds LHS = mc, RHS = a, 
            __libasr_index_0_1 = lbound(a, 1)
            do __libasr_index_0_ = lbound(mc, 1), ubound(mc, 1)
                mc(__libasr_index_0_) = a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
        end if
        sizes(i) = size(mc)
        call reset_mc()
    end do
end subroutine module_target

subroutine never(flag, a, c)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    logical(4) :: __libasr_reallocations_do_nothing
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout) :: c
    logical(4), intent(in) :: flag
    integer(4) :: i
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
        do i = 1, 3
            if (flag) then
                ! CheckBounds LHS = c, RHS = a, 
                __libasr_index_0_1 = lbound(a, 1)
                do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                    c(__libasr_index_0_) = a(__libasr_index_0_1)
                    __libasr_index_0_1 = __libasr_index_0_1 + 1
                end do
            end if
        end do
    else
        do i = 1, 3
            if (flag) then
                reallocate(c(size(a, 1)))
                ! CheckBounds LHS = c, RHS = a, 
                __libasr_index_0_1 = lbound(a, 1)
                do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                    c(__libasr_index_0_) = a(__libasr_index_0_1)
                    __libasr_index_0_1 = __libasr_index_0_1 + 1
                end do
            end if
        end do
    end if
end subroutine never

subroutine reset_mc()
    if (Allocated(mc)) then
        deallocate(mc)
    end if
    allocate(mc(1))
end subroutine reset_mc

subroutine target_attr(a, c, reps)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    logical(4) :: __libasr_reallocations_do_nothing
    real(4), dimension(:), allocatable, intent(inout) :: a
    real(4), dimension(:), allocatable, intent(inout), target :: c
    integer(4) :: i
    real(4), dimension(:), pointer :: p
    integer(4), intent(in) :: reps
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
        do i = 1, reps
            a(1) = a(1) + real(1, kind=4)
            ! CheckBounds LHS = c, RHS = a, 
            __libasr_index_0_1 = lbound(a, 1)
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
            p => c
            p(2) = real(-1, kind=4)
        end do
    else
        do i = 1, reps
            a(1) = a(1) + real(1, kind=4)
            reallocate(c(size(a, 1)))
            ! CheckBounds LHS = c, RHS = a, 
            __libasr_index_0_1 = lbound(a, 1)
            do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
                c(__libasr_index_0_) = a(__libasr_index_0_1)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
            end do
            p => c
            p(2) = real(-1, kind=4)
        end do
    end if
end subroutine target_attr

subroutine tick()
    ticks = ticks + 1
end subroutine tick

end module realloc_lhs_version_01_mod

program realloc_lhs_version_01
use realloc_lhs_version_01_mod, only: call_first
use realloc_lhs_version_01_mod, only: changing
use realloc_lhs_version_01_mod, only: conditional
use realloc_lhs_version_01_mod, only: deallocates
use realloc_lhs_version_01_mod, only: exit_first
use realloc_lhs_version_01_mod, only: maybe_absent
use realloc_lhs_version_01_mod, only: maybe_unallocated
use realloc_lhs_version_01_mod, only: mc
use realloc_lhs_version_01_mod, only: module_target
use realloc_lhs_version_01_mod, only: never
use realloc_lhs_version_01_mod, only: reset_mc
use realloc_lhs_version_01_mod, only: target_attr
use realloc_lhs_version_01_mod, only: tick
use realloc_lhs_version_01_mod, only: ticks
implicit none
integer(4) :: __libasr_index_0_
integer(4) :: __libasr_index_0_1
integer(4) :: __libasr_lbound_
integer(4) :: __libasr_lbound_1
integer(4) :: __libasr_lbound_2
real(4), dimension(:), allocatable :: a
real(4), dimension(:), allocatable :: b
real(4), dimension(:), allocatable :: c
integer(4) :: n
integer(4), dimension(3) :: sizes
integer(4), dimension(2) :: sizes2
real(4), dimension(:), allocatable :: u
__libasr_lbound_ = lbound(a, 1)
a(__libasr_lbound_ + 0) = 1.00000000e+00
a(__libasr_lbound_ + 1) = 2.00000000e+00
a(__libasr_lbound_ + 2) = 3.00000000e+00
__libasr_lbound_1 = lbound(b, 1)
b(__libasr_lbound_1 + 0) = 1.00000000e+01
b(__libasr_lbound_1 + 1) = 2.00000000e+01
b(__libasr_lbound_1 + 2) = 3.00000000e+01
allocate(c(3))
call exit_first(a, c, 3)
if (Any(c /= [2.00000000e+00, 4.00000000e+00, 6.00000000e+00])) then
    error stop
end if
deallocate(c)
call exit_first(a, c, 1)
if (Allocated(c)) then
    error stop
end if
allocate(c(5))
do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
    c(__libasr_index_0_) = real(7, kind=4)
end do
call exit_first(a, c, 1)
if (size(c) /= 5 .or. Any(c /= real(7, kind=4))) then
    error stop
end if
call exit_first(a, c, 2)
if (size(c) /= 3 .or. Any(c /= [2.00000000e+00, 4.00000000e+00, 6.00000000e+00])) then
    error stop
end if
call conditional(a, b, c, 3)
if (size(c) /= 3 .or. Any(c /= [-9.00000000e+00, -1.80000000e+01, -2.70000000e+01])) then
    error stop
end if
deallocate(c)
call conditional(a, b, c, 2)
if (size(c) /= 3 .or. Any(c /= [1.10000000e+01, 2.20000000e+01, 3.30000000e+01])) then
    error stop
end if
deallocate(c)
allocate(c(2))
do __libasr_index_0_1 = lbound(c, 1), ubound(c, 1)
    c(__libasr_index_0_1) = real(4, kind=4)
end do
call never(.false., a, c)
if (size(c) /= 2 .or. Any(c /= real(4, kind=4))) then
    error stop
end if
call never(.true., a, c)
if (size(c) /= 3) then
    error stop
end if
call maybe_unallocated(.false., u, c)
if (size(c) /= 3) then
    error stop
end if
__libasr_lbound_2 = lbound(u, 1)
u(__libasr_lbound_2 + 0) = 5.00000000e+00
u(__libasr_lbound_2 + 1) = 6.00000000e+00
call maybe_unallocated(.true., u, c)
if (size(c) /= 2 .or. Any(c /= [5.00000000e+00, 6.00000000e+00])) then
    error stop
end if
call maybe_absent(c)
if (size(c) /= 2) then
    error stop
end if
call maybe_absent(c, [1.00000000e+00, 2.00000000e+00, 3.00000000e+00, 4.00000000e+00])
if (size(c) /= 4) then
    error stop
end if
deallocate(c)
call target_attr(a, c, 2)
if (size(c) /= 3) then
    error stop
end if
if (Any(c /= [3.00000000e+00, -1.00000000e+00, 3.00000000e+00])) then
    error stop
end if
call target_attr(a, c, 1)
if (Any(c /= [4.00000000e+00, -1.00000000e+00, 3.00000000e+00])) then
    error stop
end if
ticks = 0
deallocate(c)
call call_first(a, c, 2)
if (ticks /= 2 .or. size(c) /= 3) then
    error stop
end if
call call_first(a, c, 1)
if (ticks /= 3 .or. size(c) /= 3) then
    error stop
end if
allocate(mc(3))
call module_target(a, sizes2)
if (Any(sizes2 /= [3, 3])) then
    error stop
end if
deallocate(c)
allocate(c(1))
call changing(a, c, sizes)
if (Any(sizes /= [1, 2, 3])) then
    error stop
end if
call deallocates(a, c, n)
if (n /= 9) then
    error stop
end if
print *, "ok"
end program realloc_lhs_version_01
