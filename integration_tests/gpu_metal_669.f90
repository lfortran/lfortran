! The kernels of launches with reductions that may run in chunks of at most
! 50 iterations (lfortran/lfortran#16569), as this test is compiled with
! --gpu-chunk-limit=50. Such a kernel takes the bounds of the dimensions a
! chunk splits as parameters, and a flag that is 1 for the first chunk
! only (or when the launch runs as one): only the threads of the first
! chunk start their accumulators at the identity, and the threads of
! the later chunks keep combining into them. A launch that never runs in
! chunks, as the 10 iterations of the last loop, takes no flag.
program gpu_metal_669
implicit none
integer :: i, j, n, s
real :: a(10, 8), t
n = 100
s = 3
do concurrent (i = 1:n) reduce(+:s)
    s = s + i
end do
if (s /= 3 + 5050) error stop 1
do j = 1, 8
    do i = 1, 10
        a(i, j) = real(mod(3 * i + 7 * j, 23))
    end do
end do
t = -1
do concurrent (i = 1:10, j = 1:8) reduce(max:t)
    t = max(t, a(i, j))
end do
if (t /= maxval(a)) error stop 2
s = 0
do concurrent (i = 1:10) reduce(+:s)
    s = s + i
end do
if (s /= 55) error stop 3
print *, "ok"
end program
