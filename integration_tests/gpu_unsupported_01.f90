! A real(8) local of a BLOCK in a do concurrent body. Metal has no 64-bit
! floating point type, so the loop is on the unsupported list there: a compile
! error, or with --gpu-allow-cpu-fallback a loop that runs on the CPU.
program gpu_unsupported_01
implicit none
integer, parameter :: n = 8
real :: y(n)
integer :: i
do concurrent (i = 1:n)
    block
        real(8) :: t
        t = real(i, 8) / 4.0d0
        y(i) = real(t)
    end block
end do
do i = 1, n
    if (abs(y(i) - real(i) / 4.0) > 1.0e-6) error stop
end do
print *, "PASS"
end program
