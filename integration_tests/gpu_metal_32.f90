program gpu_metal_32
implicit none
real, allocatable :: x(:)
integer :: i, n
n = 10

allocate(x(n))
do i = 1, n
    x(i) = real(i)
end do

do concurrent (i = 1:n)
    x(i) = x(i) * 2.0
end do

do i = 1, n
    if (abs(x(i) - real(i) * 2.0) > 1.0e-5) error stop
end do

print *, "PASSED"
end program
