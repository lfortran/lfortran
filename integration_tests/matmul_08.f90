program matmul_08
implicit none
integer, parameter :: n = 4
real, parameter :: a(n,n) = reshape([1.,2.,0.,0., 2.,3.,4.,0., 0.,4.,5.,6., 0.,0.,6.,7.], [n,n])
real, parameter :: x(n) = [1.,2.,3.,4.]
real, parameter :: y(n) = matmul(a, x)
if (abs(y(1) - 5.) > 1e-6) error stop
if (abs(y(2) - 20.) > 1e-6) error stop
if (abs(y(3) - 47.) > 1e-6) error stop
if (abs(y(4) - 46.) > 1e-6) error stop
print *, y
end program
