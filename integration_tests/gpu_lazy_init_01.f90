! A program linked with --gpu-lazy-init creates the CUDA context at its first
! CUDA call (here, the managed allocation of `x`) instead of when it starts.
program gpu_lazy_init_01
implicit none
integer, parameter :: n = 1000000
real(8), allocatable :: x(:)
integer :: i
allocate(x(n))
do concurrent (i = 1:n)
    x(i) = 2 * i
end do
if (abs(sum(x) - real(n, 8) * (n + 1)) > 1d-6) error stop
print *, x(1), x(n)
end program
