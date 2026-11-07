! A module subroutine of another file (--separate-compilation) whose twin the
! caller knows from its declaration only: the scalars its launches are handed
! by value, such as the extents of its contiguous dummies, are not storage a
! pending launch may still read, so `project` returns with the launches of
! `matvec` pending and the host waits once, for the check.
module gpu_ending_locals_02_model
use gpu_ending_locals_02_mod
implicit none
type :: matrix
    real, allocatable :: w(:,:)
end type
contains
subroutine project(x, m, y)
    real, contiguous, intent(in) :: x(:)
    type(matrix), intent(in) :: m
    real, contiguous, intent(out) :: y(:)
    call matvec(x, m%w, y)
end subroutine
end module

program gpu_ending_locals_02
use gpu_ending_locals_02_model
implicit none
integer, parameter :: n = 64, layers = 2
type(matrix) :: m
real, allocatable :: x(:), y(:), z(:), expected(:), next(:)
integer :: i, j, li
allocate(m%w(n,n), x(n), y(n), z(n), expected(n), next(n))
do i = 1, n
    x(i) = real(modulo(i, 7))
    do j = 1, n
        m%w(j,i) = real(modulo(i + j, 5) - 2)
    end do
end do
expected = x
do li = 1, layers
    do i = 1, n
        next(i) = 0
        do j = 1, n
            next(i) = next(i) + m%w(j,i)*(2*expected(j))
        end do
    end do
    expected = next
end do
call project(x, m, y)
call project(y, m, z)
do i = 1, n
    if (z(i) /= expected(i)) error stop "project"
end do
print *, "ok"
end program
