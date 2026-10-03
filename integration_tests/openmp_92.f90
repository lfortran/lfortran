! Array dummies used in an OpenMP parallel do keep their interface:
! the procedures live in openmp_92b.f90 and are compiled separately.
! host_arrays has contained procedures that write and read its arrays in a
! parallel do, including a pointer associated with a section that has strides,
! which is also used in parallel sections and in single.
program openmp_92
implicit none
interface
    subroutine double_explicit(a, n)
    integer, intent(in) :: n
    real, intent(inout) :: a(n)
    end subroutine
    subroutine double_constant(a)
    real, intent(inout) :: a(100)
    end subroutine
    subroutine fill_2d(a, n, m)
    integer, intent(in) :: n, m
    real, intent(inout) :: a(n, 0:m)
    end subroutine
    subroutine add_local(a, n)
    integer, intent(in) :: n
    real, intent(inout) :: a(n)
    end subroutine
    subroutine host_arrays(hs, gs, g43, g01, ws, ps, pt, pst)
    integer, intent(out) :: hs, gs, g43, g01, ws, ps, pt, pst
    end subroutine
end interface
real :: x(100), y(3, 0:4)
integer :: i, j
integer :: hs, gs, g43, g01, ws, ps, pt, pst

x = 1.5
call double_explicit(x, 100)
do i = 1, 100
    if (abs(x(i) - 3.0) > 1e-6) error stop
end do

call double_constant(x)
do i = 1, 100
    if (abs(x(i) - 6.0) > 1e-6) error stop
end do

y = 0.0
call fill_2d(y, 3, 4)
do j = 0, 4
    do i = 1, 3
        if (abs(y(i, j) - (10.0*i + j)) > 1e-6) error stop
    end do
end do

x = 0.0
call add_local(x, 10)
do i = 1, 10
    if (abs(x(i) - 1.0*i) > 1e-6) error stop
end do
if (abs(x(11)) > 1e-6) error stop

call host_arrays(hs, gs, g43, g01, ws, ps, pt, pst)
if (hs /= 55) error stop
if (gs /= 110) error stop
if (g43 /= 20) error stop
if (g01 /= 2) error stop
if (ws /= 30) error stop
if (ps /= 210) error stop
if (pt /= 55000) error stop
if (pst /= 18576) error stop

print *, "ok"
end program
