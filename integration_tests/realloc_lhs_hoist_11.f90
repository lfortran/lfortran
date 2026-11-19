! Reallocation on assignment in loops: of the operands of an elemental
! expression, which all have its shape, the reallocation reads the shape of
! one that the innermost loops do not change, so that it can move out of
! them: `d = p(j)%r - p(i)%r` and `c = a(:, j) + b(:, i)` in loops over `j`.
module realloc_lhs_hoist_11_mod
implicit none
type :: particle_t
    real, allocatable :: r(:)
end type
contains

subroutine pairs(p, s)
    type(particle_t), intent(in) :: p(:)
    real, intent(out) :: s
    real, allocatable :: d(:)
    integer :: i, j
    s = 0
    do i = 1, size(p)
        do j = 1, size(p)
            d = p(j)%r - p(i)%r
            s = s + sum(d*d)
        end do
    end do
end subroutine

subroutine columns(a, b, c, i, lb)
    real, intent(in) :: a(:,:), b(:,:)
    real, allocatable, intent(inout) :: c(:)
    integer, intent(in) :: i
    integer, intent(out) :: lb
    integer :: j
    do j = 1, size(a, 2)
        c = a(:, j) + b(:, i)
    end do
    lb = lbound(c, 1)
end subroutine

! The operand that does not depend on the loop reads a function, which the
! reallocation does not evaluate again.
subroutine with_call(a, c, n)
    real, intent(in) :: a(:)
    real, allocatable, intent(inout) :: c(:)
    integer, intent(out) :: n
    integer :: j
    n = 0
    do j = 1, 2
        c = a(1:j+1) + a(first():first()+j)
    end do
    n = size(c)
end subroutine

integer function first()
    first = 1
end function

end module

program realloc_lhs_hoist_11
use realloc_lhs_hoist_11_mod
implicit none
type(particle_t) :: p(3)
real :: a(4, 3), b(4, 2), x(5)
real, allocatable :: c(:)
real :: s
integer :: i, lb, n

do i = 1, 3
    p(i)%r = [real(i), 0.0]
end do
call pairs(p, s)
! sum over i, j of (j - i)**2 = 2*(1 + 1 + 4)
if (abs(s - 12.0) > 1e-5) error stop 1

a = 1
b = 2
allocate(c(0:3))
call columns(a, b, c, 1, lb)
if (size(c) /= 4 .or. lb /= 0) error stop 2
if (any(c /= 3)) error stop 3
deallocate(c)
call columns(a, b, c, 2, lb)
if (size(c) /= 4 .or. lb /= 1) error stop 4

x = [1, 2, 3, 4, 5]
call with_call(x, c, n)
if (n /= 3) error stop 5
if (any(c /= [2.0, 4.0, 6.0])) error stop 6

print *, "ok"
end program
