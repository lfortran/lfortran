program gpu_metal_196
! Test: a kernel argument that is an assumed shape array. Its extent is not
! known until run time, so it has to reach the kernel as an argument of its
! own, interleaved with the ordinary scalar arguments. The extent is read
! back inside the loop through size(), so a wrong one shows up in the result.
implicit none
integer, parameter :: n = 4
real :: a(n), b(n), out(n)
integer :: i
do i = 1, n
    a(i) = real(i)
    b(i) = real(i) * 10.0
end do
out = 0.0
call go(a, b, out, n, 3.0)
do i = 1, n
    if (abs(out(i) - (3.0 * a(i) + b(i) + real(n))) > 1e-5) error stop
end do
print *, "PASSED"
contains
subroutine go(x, y, r, m, s)
    real, intent(in) :: x(:), y(:)
    real, intent(out) :: r(:)
    integer, intent(in) :: m
    real, intent(in) :: s
    integer :: j
    do concurrent (j = 1:m)
        r(j) = s * x(j) + y(j) + real(size(x))
    end do
end subroutine
end program
