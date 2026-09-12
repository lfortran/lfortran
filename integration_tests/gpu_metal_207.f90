! A strided `do concurrent` has no gpu index arithmetic yet, so it stays on the
! host. It must still produce the right answer.
program gpu_metal_207
implicit none
integer, parameter :: n = 100
real :: a(n), b(n)
integer :: i
a = 0.0
b = 3.0
do concurrent (i = 1:n:2)
    a(i) = 2.0 * b(i) + 1.0
end do
do i = 1, n
    if (mod(i, 2) == 1) then
        if (abs(a(i) - 7.0) > 1e-5) error stop
    else
        if (abs(a(i)) > 1e-5) error stop
    end if
end do
print *, "PASSED"
end program
