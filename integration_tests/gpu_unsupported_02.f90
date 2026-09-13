! A do concurrent over real(4) data that calls a pure function computing in
! real(8). The function runs on the device too, so on Metal the loop is on the
! unsupported list: a compile error, or with --gpu-allow-cpu-fallback a loop
! that runs on the CPU.
program gpu_unsupported_02
implicit none
integer, parameter :: n = 8
real :: x(n), y(n)
integer :: i
x = [(real(i), i = 1, n)]
do concurrent (i = 1:n)
    y(i) = half(x(i))
end do
do i = 1, n
    if (abs(y(i) - x(i) / 2.0) > 1.0e-6) error stop
end do
print *, "PASS"
contains
    pure real function half(a)
        real, intent(in) :: a
        real(8) :: t
        t = real(a, 8)
        half = real(t / 2.0d0)
    end function
end program
