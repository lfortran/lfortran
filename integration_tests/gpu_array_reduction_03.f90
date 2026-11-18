program gpu_array_reduction_03
! The kernel source of a REDUCE of two elements of an array and of one of
! another (asr_to_gpu_c.h, GpuReduceCombine): the threads accumulate in a
! grid-stride loop and hand their accumulators to the combination, which the
! block that finishes last applies to the elements.
implicit none
real :: x(1000), ss(3), mx(2)
integer :: i
do i = 1, 1000
    x(i) = real(mod(i, 7) - 3)
end do
ss = [0.0, 1.0, 2.0]
mx = 0
do concurrent (i = 1:1000) reduce(+: ss) reduce(max: mx)
    ss(1) = ss(1) + x(i)*x(i)
    ss(3) = ss(3) + x(i)
    mx(2) = max(mx(2), x(i))
end do
if (ss(1) /= 3995 .or. ss(2) /= 1 .or. ss(3) /= 5 .or. mx(1) /= 0 &
        .or. mx(2) /= 3) error stop 1
print *, ss, mx
end program
