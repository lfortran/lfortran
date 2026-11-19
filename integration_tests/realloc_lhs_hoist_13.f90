! The allocation of an unallocated array in a loop,
! `if (.not. allocated(w)) allocate(w(n))`, does nothing once the array is
! allocated, whatever shape an assignment gives it then: it moves before the
! loop unless something in the loop can deallocate the array, or changes what
! it reads before it.
module realloc_lhs_hoist_13_mod
implicit none
real, allocatable :: g(:)
contains

! Both the allocation and the reallocation move before the loop.
subroutine work(a, n, s)
    real, intent(in) :: a(:)
    integer, intent(in) :: n
    real, intent(out) :: s
    real, allocatable :: w(:)
    integer :: i
    s = 0
    do i = 1, n
        if (.not. allocated(w)) allocate(w(size(a)))
        w = 2*a
        s = s + w(i)
    end do
end subroutine

! The allocation moves; the reallocation, to a shape that changes, stays.
subroutine prefixes(a, n, s)
    real, intent(in) :: a(:)
    integer, intent(in) :: n
    real, intent(out) :: s
    real, allocatable :: w(:)
    integer :: i
    s = 0
    do i = 1, n
        if (.not. allocated(w)) allocate(w(size(a)))
        w = a(1:i)
        if (size(w) /= i) error stop 1
        s = s + sum(w)
    end do
end subroutine

! A loop that does not run allocates nothing.
subroutine zero_trip(a, n)
    real, intent(in) :: a(:)
    integer, intent(in) :: n
    integer :: i
    do i = 1, n
        if (.not. allocated(g)) allocate(g(size(a)))
        g = a
    end do
end subroutine

! The loop deallocates the array: the allocation stays.
subroutine deallocated(n, s)
    integer, intent(in) :: n
    integer, intent(out) :: s
    real, allocatable :: w(:)
    integer :: i
    s = 0
    do i = 1, n
        if (.not. allocated(w)) allocate(w(i))
        s = s + size(w)
        deallocate(w)
    end do
end subroutine

subroutine release(x)
    real, allocatable, intent(inout) :: x(:)
    deallocate(x)
end subroutine

! So does a call that can, and MOVE_ALLOC.
subroutine released(n, s)
    integer, intent(in) :: n
    integer, intent(out) :: s
    real, allocatable :: w(:), t(:)
    integer :: i
    s = 0
    do i = 1, n
        if (.not. allocated(w)) allocate(w(i))
        s = s + size(w)
        call release(w)
    end do
    do i = 1, n
        if (.not. allocated(w)) allocate(w(i))
        w = i
        call move_alloc(w, t)
        s = s + nint(sum(t))
    end do
end subroutine

! The statement before it changes the extent it allocates: it stays.
subroutine extent_changes(n, s)
    integer, intent(in) :: n
    integer, intent(out) :: s
    real, allocatable :: w(:)
    integer :: i, m
    m = 1
    s = 0
    do i = 1, n
        m = m + 1
        if (.not. allocated(w)) allocate(w(m))
        s = s + size(w)
    end do
end subroutine

! The statement before it asks whether the array is allocated: it stays.
subroutine asked_before(n, s)
    integer, intent(in) :: n
    integer, intent(out) :: s
    real, allocatable :: w(:)
    integer :: i
    s = 0
    do i = 1, n
        if (allocated(w)) s = s + 100
        if (.not. allocated(w)) allocate(w(3))
        s = s + size(w)
    end do
end subroutine

end module

program realloc_lhs_hoist_13
use realloc_lhs_hoist_13_mod
implicit none
real :: a(4), s
integer :: k

a = [1, 2, 3, 4]
call work(a, 4, s)
if (abs(s - 20) > 1e-6) error stop 2
call prefixes(a, 4, s)
if (abs(s - 20) > 1e-6) error stop 3
call zero_trip(a, 0)
if (allocated(g)) error stop 4
call zero_trip(a, 2)
if (.not. allocated(g)) error stop 5
if (size(g) /= 4) error stop 6
call deallocated(4, k)
if (k /= 10) error stop 7
call released(3, k)
if (k /= 6 + 1 + 4 + 9) error stop 8
call extent_changes(3, k)
if (k /= 6) error stop 9
call asked_before(3, k)
if (k /= 209) error stop 10
print *, "ok"
end program
