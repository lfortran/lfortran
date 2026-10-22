! Automatic arrays of a host procedure, sized by the host's dummy
! arguments, used in array expressions inside an internal procedure
module nested_vars_25_mod
implicit none
contains

    subroutine scale_1d(a, r)
    real, intent(in) :: a(:)
    real, intent(out) :: r(:)
    real :: s(size(a))
    s = 1.0
    call inner()
    contains
        subroutine inner()
        r = 2*s
        end subroutine
    end subroutine

    subroutine update_2d(a, total)
    real, intent(inout) :: a(:,:)
    real, intent(out) :: total
    real :: w(size(a, 1), size(a, 2))
    w = 3.0
    call inner()
    contains
        subroutine inner()
        w = w + a
        a = 2*w - a
        total = sum(w*a)
        end subroutine
    end subroutine

    subroutine fill_n(n, x)
    integer, intent(in) :: n
    real, intent(out) :: x(n)
    real :: v(n)
    call inner()
    x = v
    contains
        subroutine inner()
        integer :: i
        v = [(real(i), i = 1, n)]
        v = v*v + 1
        end subroutine
    end subroutine

end module

subroutine outer(a, r)
real :: a(:)
real :: r(:)
real :: s(size(a))
s = 1.0
call inner()
contains
subroutine inner()
r = 2*s
print *, 2*s
end subroutine
end subroutine

program nested_vars_25
use nested_vars_25_mod
implicit none
interface
subroutine outer(a, r)
real :: a(:)
real :: r(:)
end subroutine
end interface
real :: a(3), r(3), b(2, 3), total, x(4)
integer :: i, j

a = 0.0
r = 0.0
call outer(a, r)
if (any(abs(r - 2.0) > 1e-6)) error stop

r = 0.0
call scale_1d(a, r)
if (any(abs(r - 2.0) > 1e-6)) error stop

do j = 1, 3
    do i = 1, 2
        b(i, j) = real(i + 10*j)
    end do
end do
call update_2d(b, total)
! w = 3 + b0, b = 2*w - b0 = 6 + b0, total = sum((3 + b0)*(6 + b0))
do j = 1, 3
    do i = 1, 2
        if (abs(b(i, j) - (6.0 + real(i + 10*j))) > 1e-5) error stop
    end do
end do
if (abs(total - 4444.0) > 1e-3) error stop

call fill_n(4, x)
if (any(abs(x - [2.0, 5.0, 10.0, 17.0]) > 1e-6)) error stop
print *, r, total, x
end program
