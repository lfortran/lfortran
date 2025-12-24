program matmul_04
implicit none
real, allocatable :: A(:,:), B(:,:), C(:,:)
real :: expected

allocate(A(3, 4), B(4, 2))
A = 1.0
B = 1.0

C = matmul(A, B)

expected = 4.0
if (abs(C(1,1) - expected) > 1e-6) error stop
if (abs(C(3,2) - expected) > 1e-6) error stop
if (size(C, 1) /= 3) error stop
if (size(C, 2) /= 2) error stop
print *, "OK: matmul with auto-allocated result"

end program
