program gpu_metal_17
! Vector copy
implicit none
integer, parameter :: n = 100000
real :: src(n), dst(n)
integer :: i

do i = 1, n
    src(i) = real(i) * 1.5
    dst(i) = 0.0
end do

do concurrent (i = 1:n)
    dst(i) = src(i)
end do

do i = 1, n
    if (abs(dst(i) - src(i)) > 1.0e-5) error stop
end do

print *, "PASSED"
end program
