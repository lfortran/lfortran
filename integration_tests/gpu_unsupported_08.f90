! A do concurrent that calls a pure function with an `error stop` on a branch
! the data never takes. A Metal shader cannot halt the program, so on Metal
! the loop is on the unsupported list: a compile error, or with
! --gpu-allow-cpu-fallback a loop that runs on the CPU. CUDA can halt, so
! there the loop is offloaded.
program gpu_unsupported_08
implicit none
integer :: i
real :: a(8)
do concurrent (i = 1:8)
    a(i) = checked_sqrt(real(i))
end do
do i = 1, 8
    if (abs(a(i) - sqrt(real(i))) > 1.0e-5) error stop
end do
print *, "PASS"
contains
    pure real function checked_sqrt(x)
        real, intent(in) :: x
        if (x < 0.0) error stop "negative"
        checked_sqrt = sqrt(x)
    end function
end program
