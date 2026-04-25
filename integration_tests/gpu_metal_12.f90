program gpu_metal_12
! Min and max intrinsics inside do concurrent
implicit none
integer, parameter :: n = 10000
real :: a(n), b(n), c_min(n), c_max(n)
real :: c_min_expected(n), c_max_expected(n)
integer :: i

do i = 1, n
    a(i) = sin(real(i))
    b(i) = cos(real(i))
    c_min_expected(i) = min(a(i), b(i))
    c_max_expected(i) = max(a(i), b(i))
end do

do concurrent (i = 1:n)
    c_min(i) = min(a(i), b(i))
    c_max(i) = max(a(i), b(i))
end do

do i = 1, n
    if (abs(c_min(i) - c_min_expected(i)) > 1.0e-5) error stop
    if (abs(c_max(i) - c_max_expected(i)) > 1.0e-5) error stop
end do

print *, "PASSED"
end program
