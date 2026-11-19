! Reallocation on assignment in loops: shapes computed by a function of its
! arguments alone (`pack`, which counts the elements of its mask), and calls
! that receive the source of the shape as an INTENT(IN) argument.
module realloc_lhs_hoist_08_mod
implicit none
integer :: offset = 0
contains

! The number of elements of a value, computed from the argument alone.
pure integer function half(n)
    integer, intent(in) :: n
    half = n / 2
end function

! The same, plus a module variable that the loop changes.
integer function half_plus_offset(n)
    integer, intent(in) :: n
    half_plus_offset = n / 2 + offset
end function

subroutine bump_offset()
    offset = offset + 1
end subroutine

real function total(x)
    real, intent(in) :: x(:)
    total = sum(x)
end function

subroutine copy_in(x, y)
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    y = x
end subroutine

subroutine grow(x)
    real, allocatable, intent(inout) :: x(:)
    real, allocatable :: y(:)
    allocate(y(size(x) + 1))
    y = 0
    call move_alloc(y, x)
end subroutine

subroutine clear_first(m)
    logical, intent(inout) :: m(:)
    m(1) = .false.
end subroutine

! c = pack(a, m) with a mask the loop does not change.
subroutine pack_fixed(a, m, c, reps)
    real, allocatable, intent(inout) :: a(:), c(:)
    logical, allocatable, intent(in) :: m(:)
    integer, intent(in) :: reps
    integer :: i
    do i = 1, reps
        a(1) = a(1) + 1
        c = pack(a, m)
    end do
end subroutine

! The mask changes: so does the shape.
subroutine pack_changing(a, m, c, sizes)
    real, allocatable, intent(inout) :: a(:), c(:)
    logical, allocatable, intent(inout) :: m(:)
    integer, intent(out) :: sizes(3)
    integer :: i
    do i = 1, 3
        m(i) = .false.
        c = pack(a, m)
        sizes(i) = size(c)
    end do
end subroutine

! A call changes the mask through an INTENT(INOUT) argument.
subroutine pack_call_changes(a, m, c, sizes)
    real, allocatable, intent(inout) :: a(:), c(:)
    logical, allocatable, intent(inout) :: m(:)
    integer, intent(out) :: sizes(2)
    integer :: i
    do i = 1, 2
        c = pack(a, m)
        sizes(i) = size(c)
        call clear_first(m)
    end do
end subroutine

subroutine function_shape(a, c, reps)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(in) :: reps
    integer :: i
    do i = 1, reps
        c = a(1:half(size(a)))
    end do
end subroutine

! The function reads a module variable that a call in the loop changes.
subroutine function_shape_global(a, c, sizes)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(out) :: sizes(2)
    integer :: i
    do i = 1, 2
        c = a(1:half_plus_offset(4))
        sizes(i) = size(c)
        call bump_offset()
    end do
end subroutine

! The loop passes the source of the shape to INTENT(IN) dummy arguments.
subroutine intent_in(a, c, w, s, reps)
    real, allocatable, intent(inout) :: a(:), c(:), w(:)
    real, intent(out) :: s
    integer, intent(in) :: reps
    integer :: i
    s = 0
    do i = 1, reps
        c = a + 1
        s = s + total(a)
        call copy_in(a, w)
    end do
end subroutine

! The source of the shape is passed to an INTENT(INOUT) allocatable dummy
! argument that changes its shape: the reallocation stays in the loop.
subroutine intent_inout(a, c, sizes)
    real, allocatable, intent(inout) :: a(:), c(:)
    integer, intent(out) :: sizes(2)
    integer :: i
    do i = 1, 2
        c = a
        sizes(i) = size(c)
        call grow(a)
    end do
end subroutine

end module

program realloc_lhs_hoist_08
use realloc_lhs_hoist_08_mod
implicit none
real, allocatable :: a(:), c(:), w(:)
logical, allocatable :: m(:)
integer :: sizes(3), sizes2(2)
real :: s

a = [1.0, 2.0, 3.0, 4.0]
m = [.true., .false., .true., .true.]
call pack_fixed(a, m, c, 2)
if (size(c) /= 3) error stop 1
if (any(c /= [3.0, 3.0, 4.0])) error stop 2
deallocate(c)
call pack_fixed(a, m, c, 0)
if (allocated(c)) error stop 3

call pack_changing(a, m, c, sizes)
if (any(sizes /= [2, 2, 1])) error stop 4

m = [.true., .false., .true., .true.]
call pack_call_changes(a, m, c, sizes2)
if (any(sizes2 /= [3, 2])) error stop 5

call function_shape(a, c, 2)
if (size(c) /= 2) error stop 6

offset = 0
call function_shape_global(a, c, sizes2)
if (any(sizes2 /= [2, 3])) error stop 7

allocate(w(4))
call intent_in(a, c, w, s, 2)
if (size(c) /= 4) error stop 8
if (abs(s - 2*sum(a)) > 1e-6) error stop 9

call intent_inout(a, c, sizes2)
if (any(sizes2 /= [4, 5])) error stop 10

print *, "ok"
end program
