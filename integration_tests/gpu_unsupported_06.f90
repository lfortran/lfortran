! A print in a do concurrent body. No device has Fortran units or formats,
! so the loop is on the unsupported list of every device: a compile error, or
! with --gpu-allow-cpu-fallback a loop that runs on the CPU.
program gpu_unsupported_06
implicit none
integer :: i
real :: a(4)
do concurrent (i = 1:4)
    a(i) = real(i)
    print *, i
end do
if (abs(sum(a) - 10.0) > 1.0e-5) error stop
print *, "PASS"
end program
