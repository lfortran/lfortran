! The pointer to the descriptor of an allocatable or pointer array dummy
! argument does not change when an element of an array is stored, but does
! when the procedure, or one it calls, reallocates or reassociates the
! array: loops that store elements of TARGET and non-TARGET allocatable
! dummy arguments, also through pointers, and reallocate them through calls
! and assignments.
module tbaa_15_m
implicit none
contains

subroutine regrow(x, n)
    real, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: n
    real, allocatable :: y(:)
    allocate(y(n))
    y = 0
    y(1:min(n, size(x))) = x(1:min(n, size(x)))
    call move_alloc(y, x)
end subroutine

! Elements stored through the dummy argument and through a pointer to it,
! with the array reallocated through a call in the loop.
subroutine fill(c, a, sizes)
    real, allocatable, target, intent(inout) :: c(:)
    real, allocatable, intent(inout) :: a(:)
    integer, intent(out) :: sizes(3)
    real, pointer :: p(:)
    integer :: i, j
    do i = 1, 3
        do j = 1, size(c)
            c(j) = c(j) + a(min(j, size(a)))
        end do
        p => c
        p(1) = p(1) + 100
        sizes(i) = size(c)
        call regrow(c, size(c) + 1)
        a(1) = a(1) + 1
    end do
end subroutine

! A pointer dummy argument reassociated in the loop.
subroutine walk(q, a, b, s)
    real, pointer, intent(inout) :: q(:)
    real, target, intent(inout) :: a(:), b(:)
    real, intent(out) :: s
    integer :: i, j
    s = 0
    do i = 1, 4
        do j = 1, size(q)
            q(j) = q(j) + 1
            s = s + q(j)
        end do
        if (mod(i, 2) == 1) then
            q => b
        else
            q => a
        end if
    end do
end subroutine

! An allocatable dummy argument assigned with reallocation in the loop.
subroutine resize(c, n)
    real, allocatable, intent(inout) :: c(:)
    integer, intent(in) :: n
    integer :: i, j
    do i = 1, n
        c = [(real(j), j = 1, i)]
        c(i) = c(i) * 2
    end do
end subroutine

end module

program tbaa_15
use tbaa_15_m
implicit none
real, allocatable, target :: c(:)
real, allocatable :: a(:)
real, allocatable, target :: u(:), v(:)
real, pointer :: q(:)
integer :: sizes(3)
real :: s

allocate(c(2), a(2))
c = [1.0, 2.0]
a = [10.0, 20.0]
call fill(c, a, sizes)
if (any(sizes /= [2, 3, 4])) error stop 1
if (size(c) /= 5) error stop 2
! i = 1: c = [111, 22] -> [111, 22, 0]
! i = 2: c = [111 + 11 + 100, 22 + 20, 0 + 20] -> [222, 42, 20, 0]
! i = 3: c = [222 + 12 + 100, 42 + 20, 20 + 20, 0 + 20]
if (any(c /= [334.0, 62.0, 40.0, 20.0, 0.0])) error stop 3

allocate(u(3), v(2))
u = 0
v = 0
q => u
call walk(q, u, v, s)
! u and v are each visited twice: u = 2, v = 2
if (any(u /= 2) .or. any(v /= 2)) error stop 4
if (abs(s - (3 + 2 + 6 + 4)) > 1e-6) error stop 5

call resize(c, 4)
if (size(c) /= 4) error stop 6
if (any(c /= [1.0, 2.0, 3.0, 8.0])) error stop 7

print *, "ok"
end program
