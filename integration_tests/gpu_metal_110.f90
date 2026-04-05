program gpu_metal_110
implicit none
integer, parameter :: n = 2
real :: a(n, 2), z(n, 2)
integer :: i
z = reshape([1.0, 2.0, 3.0, 4.0], [n, 2])
do concurrent (i = 1:2)
    a(1:n, i) = f(z(1:n, i))
end do
print *, a(1,1), a(2,1), a(1,2), a(2,2)
if (abs(a(1,1) - 2.0) > 1e-6) error stop
if (abs(a(2,1) - 4.0) > 1e-6) error stop
if (abs(a(1,2) - 6.0) > 1e-6) error stop
if (abs(a(2,2) - 8.0) > 1e-6) error stop
contains
elemental function f(x) result(y)
    real, intent(in) :: x
    real :: y
    y = x * 2.0
end function
end program
