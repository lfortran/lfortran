program gpu_metal_81
! matmul inside do concurrent (matrix-vector and matrix-matrix)
implicit none
real :: a(2,2), b(2), c(2)
real :: m1(2,3), m2(3,2), m3(2,2)
real :: v(2), w(3)
integer :: i

! Matrix-vector: a(2,2) * b(2) = c(2)
a(1,1) = 1.0; a(2,1) = 3.0
a(1,2) = 2.0; a(2,2) = 4.0
b(1) = 5.0; b(2) = 6.0

do concurrent (i = 1:1)
    c = matmul(a, b)
end do
! c(1) = 1*5 + 2*6 = 17, c(2) = 3*5 + 4*6 = 39
if (abs(c(1) - 17.0) > 1.0e-5) error stop
if (abs(c(2) - 39.0) > 1.0e-5) error stop

! Vector-matrix: v(2) * m1(2,3) = w(3)
m1(1,1) = 1.0; m1(2,1) = 2.0
m1(1,2) = 3.0; m1(2,2) = 4.0
m1(1,3) = 5.0; m1(2,3) = 6.0
v(1) = 1.0; v(2) = 2.0

do concurrent (i = 1:1)
    w = matmul(v, m1)
end do
! w(1) = 1*1 + 2*2 = 5, w(2) = 1*3 + 2*4 = 11, w(3) = 1*5 + 2*6 = 17
if (abs(w(1) - 5.0) > 1.0e-5) error stop
if (abs(w(2) - 11.0) > 1.0e-5) error stop
if (abs(w(3) - 17.0) > 1.0e-5) error stop

! Matrix-matrix: m1(2,3) * m2(3,2) = m3(2,2)
m2(1,1) = 1.0; m2(2,1) = 3.0; m2(3,1) = 5.0
m2(1,2) = 2.0; m2(2,2) = 4.0; m2(3,2) = 6.0

do concurrent (i = 1:1)
    m3 = matmul(m1, m2)
end do
! m3(1,1) = 1*1+3*3+5*5 = 35, m3(2,1) = 2*1+4*3+6*5 = 44
! m3(1,2) = 1*2+3*4+5*6 = 44, m3(2,2) = 2*2+4*4+6*6 = 56
if (abs(m3(1,1) - 35.0) > 1.0e-5) error stop
if (abs(m3(2,1) - 44.0) > 1.0e-5) error stop
if (abs(m3(1,2) - 44.0) > 1.0e-5) error stop
if (abs(m3(2,2) - 56.0) > 1.0e-5) error stop

print *, "PASSED"
end program
