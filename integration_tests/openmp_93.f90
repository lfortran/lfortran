! Explicit-shape dummies in nested OpenMP regions, with a non-default
! lower bound and in a reduction: the procedures live in openmp_93b.f90
! and are compiled separately.
program openmp_93
implicit none
interface
    subroutine task_in_parallel(a, n)
    integer, intent(in) :: n
    real, intent(inout) :: a(n)
    end subroutine
    subroutine nested_parallel_do(a, n)
    integer, intent(in) :: n
    real, intent(inout) :: a(n)
    end subroutine
    subroutine fill_lbound(a, n)
    integer, intent(in) :: n
    real, intent(inout) :: a(0:n-1)
    end subroutine
    subroutine sum_lbound(a, n, s)
    integer, intent(in) :: n
    real, intent(in) :: a(0:n-1)
    real, intent(out) :: s
    end subroutine
    subroutine task_pointers(tc, ye, ts, tz, tp)
    integer, intent(out) :: tc, ye, ts, tz, tp
    end subroutine
end interface
real :: x(20), s
integer :: i
integer :: tc, ye, ts, tz, tp

x = 1.0
call task_in_parallel(x, 20)
if (abs(x(1) - 11.0) > 1e-6) error stop
if (abs(x(2) - 6.0) > 1e-6) error stop
do i = 3, 20
    if (abs(x(i) - 1.0) > 1e-6) error stop
end do

x = 1.0
call nested_parallel_do(x, 20)
do i = 1, 20
    if (abs(x(i) - (1.0 + i)) > 1e-6) error stop
end do

x = -1.0
call fill_lbound(x, 10)
do i = 1, 10
    if (abs(x(i) - 2.0*(i - 1)) > 1e-6) error stop
end do
if (abs(x(11) + 1.0) > 1e-6) error stop

call sum_lbound(x, 10, s)
if (abs(s - 90.0) > 1e-6) error stop

call task_pointers(tc, ye, ts, tz, tp)
if (tc /= 380) error stop
if (ye /= 953) error stop
if (ts /= 55) error stop
if (tz /= 0) error stop
if (tp /= 55) error stop

print *, "ok"
end program
