! A BLOCK nested in IF inside a loop the launch declines. The BLOCK
! locals are representable; the captured complex array is not, so
! CUDA rejects the launch after the kernel is drafted. The host
! BLOCK must still run.
program gpu_metal_301
implicit none
complex :: z(4)
integer :: a(4), i
logical :: take

take = .true.
z = (0.0, 0.0)
a = 0

do concurrent (i = 1:4)
    if (take) then
        block
            integer :: t
            t = i * 2
            a(i) = t
            z(i) = cmplx(real(i), 0.0)
        end block
    end if
end do

do i = 1, 4
    if (a(i) /= i * 2) error stop "if-block"
    if (abs(real(z(i)) - real(i)) > 1.0e-5) error stop "complex real"
    if (abs(aimag(z(i))) > 1.0e-5) error stop "complex imag"
end do

print *, "PASS"
end program
