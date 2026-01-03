program matmul_unallocated_01
implicit none
real, allocatable :: A(:,:), B(:,:), C(:,:)

allocate(A(3, 4), B(4, 2))
A = 1.0
B = 1.0

! C is not allocated - should fail without --realloc-lhs-arrays
C = matmul(A, B)

print *, C(1,1)
end program
