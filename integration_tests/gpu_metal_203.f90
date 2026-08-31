module gpu_metal_203_m
implicit none
contains
    ! Called from host code and from inside a do concurrent loop, so it has
    ! to be compiled for both.
    pure real function scale_it(x, f) result(r)
        real, intent(in) :: x
        real, intent(in) :: f
        r = x * f
    end function
end module

program gpu_metal_203
use gpu_metal_203_m
implicit none
integer :: i
real :: a(4), b(4), w(2,2), v(2), host_mat(2)

do i = 1, 4
    b(i) = scale_it(real(i), 2.0)
end do

w(1,1) = 1.0
w(2,1) = 2.0
w(1,2) = 3.0
w(2,2) = 4.0
v = [1.0, 2.0]
! matmul is lowered to a shared helper that the host and the device both use
host_mat = matmul(w, v)

a = 0.0
do concurrent (i = 1:4)
    a(i) = scale_it(real(i), 3.0) + sum(matmul(w, v))
end do

if (abs(b(3) - 6.0) > 1.0e-5) error stop
if (abs(host_mat(1) - 7.0) > 1.0e-5) error stop
if (abs(host_mat(2) - 10.0) > 1.0e-5) error stop
if (abs(a(1) - 20.0) > 1.0e-5) error stop
if (abs(a(4) - 29.0) > 1.0e-5) error stop
print *, "PASSED"
end program
