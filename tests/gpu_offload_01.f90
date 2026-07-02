program gpu_offload_01
implicit none

integer, parameter :: n = 1000
real :: x(n), y(n), a
integer :: i

a = 2.0
x = 3.0
y = 4.0

do concurrent (i = 1:n)
    y(i) = a * x(i) + y(i)
end do

if (abs(y(1) - 10.0) > 1e-5) error stop
print *, y(1)

end program
