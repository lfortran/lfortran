program gpu_metal_345
! Metal has no 64-bit floating point type, so a `do concurrent` over
! `real(8)` data genuinely cannot run in a Metal kernel. The loop runs on the
! CPU instead, with a warning naming the reason, and gives the full double
! precision answer; no flag asks for that. CUDA has `double`, so there the
! loop is offloaded.
implicit none
integer, parameter :: dp = kind(1.0d0), n = 16
real(dp) :: x(n), y(n)
integer :: i
do i = 1, n
    x(i) = real(i, dp) / 3.0_dp
end do
do concurrent (i = 1:n)
    y(i) = x(i) * x(i) + 1.0e-12_dp
end do
do i = 1, n
    if (abs(y(i) - (x(i) * x(i) + 1.0e-12_dp)) > 1.0e-13_dp * y(i)) error stop
end do
print *, sum(y)
end program
