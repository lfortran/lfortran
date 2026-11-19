module realloc_lhs_hoist_13_mod
implicit none
real(4), dimension(:), allocatable :: g

contains

subroutine asked_before(n, s)
    integer(4) :: i
    integer(4), intent(in) :: n
    integer(4), intent(out) :: s
    real(4), dimension(:), allocatable :: w
    s = 0
    do i = 1, n
        if (Allocated(w)) then
            s = s + 100
        end if
        if (.not. Allocated(w)) then
            allocate(w(3))
        end if
        s = s + size(w)
    end do
end subroutine asked_before

subroutine deallocated(n, s)
    integer(4) :: i
    integer(4), intent(in) :: n
    integer(4), intent(out) :: s
    real(4), dimension(:), allocatable :: w
    s = 0
    do i = 1, n
        if (.not. Allocated(w)) then
            allocate(w(i))
        end if
        s = s + size(w)
        deallocate(w)
    end do
end subroutine deallocated

subroutine extent_changes(n, s)
    integer(4) :: i
    integer(4) :: m
    integer(4), intent(in) :: n
    integer(4), intent(out) :: s
    real(4), dimension(:), allocatable :: w
    m = 1
    s = 0
    do i = 1, n
        m = m + 1
        if (.not. Allocated(w)) then
            allocate(w(m))
        end if
        s = s + size(w)
    end do
end subroutine extent_changes

subroutine prefixes(a, n, s)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), intent(in) :: a
    integer(4) :: i
    integer(4), intent(in) :: n
    real(4), intent(out) :: s
    real(4), dimension(:), allocatable :: w
    s = real(0, kind=4)
    if (1 <= n) then
        if (.not. Allocated(w)) then
            allocate(w(size(a)))
        end if
    end if
    do i = 1, n
        reallocate(w((i - 1)/1 + 1))
        __libasr_index_0_1 = 1
        do __libasr_index_0_ = lbound(w, 1), ubound(w, 1)
            w(__libasr_index_0_) = a(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        if (size(w) /= i) then
            error stop
        end if
        s = s + Sum(w)
    end do
end subroutine prefixes

subroutine release(x)
    real(4), dimension(:), allocatable, intent(inout) :: x
    deallocate(x)
end subroutine release

subroutine released(n, s)
    integer(4) :: __libasr_index_0_
    integer(4) :: i
    integer(4), intent(in) :: n
    integer(4), intent(out) :: s
    real(4), dimension(:), allocatable :: t
    real(4), dimension(:), allocatable :: w
    s = 0
    do i = 1, n
        if (.not. Allocated(w)) then
            allocate(w(i))
        end if
        s = s + size(w)
        call release(w)
    end do
    do i = 1, n
        if (.not. Allocated(w)) then
            allocate(w(i))
        end if
        do __libasr_index_0_ = lbound(w, 1), ubound(w, 1)
            w(__libasr_index_0_) = real(i, kind=4)
        end do
call move_alloc(w, t)
        s = s + Nint(Sum(t))
    end do
end subroutine released

subroutine work(a, n, s)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), intent(in) :: a
    integer(4) :: i
    integer(4), intent(in) :: n
    real(4), intent(out) :: s
    real(4), dimension(:), allocatable :: w
    s = real(0, kind=4)
    if (1 <= n) then
        if (.not. Allocated(w)) then
            allocate(w(size(a)))
        end if
        reallocate(w(size(a, 1)))
    end if
    do i = 1, n
        ! CheckBounds LHS = w, RHS = a, a, 
        __libasr_index_0_1 = lbound(a, 1)
        do __libasr_index_0_ = lbound(w, 1), ubound(w, 1)
            w(__libasr_index_0_) = real(2, kind=4)*a(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        s = s + w(i)
    end do
end subroutine work

subroutine zero_trip(a, n)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    real(4), dimension(:), intent(in) :: a
    integer(4) :: i
    integer(4), intent(in) :: n
    if (1 <= n) then
        if (.not. Allocated(g)) then
            allocate(g(size(a)))
        end if
        reallocate(g(size(a, 1)))
    end if
    do i = 1, n
        ! CheckBounds LHS = g, RHS = a, 
        __libasr_index_0_1 = lbound(a, 1)
        do __libasr_index_0_ = lbound(g, 1), ubound(g, 1)
            g(__libasr_index_0_) = a(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
    end do
end subroutine zero_trip

end module realloc_lhs_hoist_13_mod

program realloc_lhs_hoist_13
use realloc_lhs_hoist_13_mod, only: asked_before
use realloc_lhs_hoist_13_mod, only: deallocated
use realloc_lhs_hoist_13_mod, only: extent_changes
use realloc_lhs_hoist_13_mod, only: g
use realloc_lhs_hoist_13_mod, only: prefixes
use realloc_lhs_hoist_13_mod, only: release
use realloc_lhs_hoist_13_mod, only: released
use realloc_lhs_hoist_13_mod, only: work
use realloc_lhs_hoist_13_mod, only: zero_trip
implicit none
real(4), dimension(4) :: a
integer(4) :: k
real(4) :: s
a(1) = 1.00000000e+00
a(2) = 2.00000000e+00
a(3) = 3.00000000e+00
a(4) = 4.00000000e+00
call work(a, 4, s)
if (Abs(s - real(20, kind=4)) > 9.99999997e-07) then
    error stop
end if
call prefixes(a, 4, s)
if (Abs(s - real(20, kind=4)) > 9.99999997e-07) then
    error stop
end if
call zero_trip(a, 0)
if (Allocated(g)) then
    error stop
end if
call zero_trip(a, 2)
if (.not. Allocated(g)) then
    error stop
end if
if (size(g) /= 4) then
    error stop
end if
call deallocated(4, k)
if (k /= 10) then
    error stop
end if
call released(3, k)
if (k /= 6 + 1 + 4 + 9) then
    error stop
end if
call extent_changes(3, k)
if (k /= 6) then
    error stop
end if
call asked_before(3, k)
if (k /= 209) then
    error stop
end if
print *, "ok"
end program realloc_lhs_hoist_13
