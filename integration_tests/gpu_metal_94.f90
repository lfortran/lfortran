program gpu_metal_94
! matmul combined with array addition inside do concurrent
implicit none
integer :: i
real :: a(2), z(2), w(2,2), b(2)

! Matrix-vector matmul + addition: z = matmul(w, a) + b
w(1,1) = 1.0; w(2,1) = 0.0
w(1,2) = 0.0; w(2,2) = 1.0
a(1) = 1.0; a(2) = 2.0
b(1) = 10.0; b(2) = 20.0

do concurrent (i = 1:1)
    z = matmul(w, a) + b
end do
! z = [1,2] + [10,20] = [11,22]
if (abs(z(1) - 11.0) > 1.0e-5) error stop
if (abs(z(2) - 22.0) > 1.0e-5) error stop

! Matrix-vector matmul - subtraction
b(1) = 1.0; b(2) = 2.0
do concurrent (i = 1:1)
    z = matmul(w, a) - b
end do
! z = [1,2] - [1,2] = [0,0]
if (abs(z(1)) > 1.0e-5) error stop
if (abs(z(2)) > 1.0e-5) error stop

! Matrix-vector matmul + addition with non-identity matrix
w(1,1) = 1.0; w(2,1) = 3.0
w(1,2) = 2.0; w(2,2) = 4.0
a(1) = 5.0; a(2) = 6.0
b(1) = 0.5; b(2) = 0.5

do concurrent (i = 1:1)
    z = matmul(w, a) + b
end do
! matmul: z1 = 1*5+2*6=17, z2 = 3*5+4*6=39
! + b:    z1 = 17.5, z2 = 39.5
if (abs(z(1) - 17.5) > 1.0e-5) error stop
if (abs(z(2) - 39.5) > 1.0e-5) error stop

print *, "PASSED"
end program
