program gpu_local_preservation_01
! This is intentionally limited to GPU lowering: cuda_cpu must exercise the
! same privatization path, while ordinary LLVM still has the pre-existing
! #14622 gap where LOCAL does not preserve the outer actuals.
implicit none
integer, parameter :: n = 100000
real :: x(n), g(n), t(2), s
integer :: i

do i = 1, n
    x(i) = real(mod(i, 997))
end do
t = [-11.0, -13.0]
s = -17.0

do concurrent (i = 1:n) local(t, s)
    t(1) = x(i)
    t(2) = 2.0 * x(i)
    s = 3.0 * x(i)
    g(i) = t(1) + t(2) + s
end do

do i = 1, n
    if (abs(g(i) - 6.0 * x(i)) > 1.0e-3) error stop "local result"
end do
if (any(t /= [-11.0, -13.0])) error stop "local array escaped"
if (s /= -17.0) error stop "local scalar escaped"
print *, "ok"
end program
