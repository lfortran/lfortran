program intrinsics_109
  integer, parameter :: N = 2
  real :: A(N, N), B(N, N), C(N, N)

  ! Initialize matrices A and B as 2x2 identity matrices
  A = reshape([1.0, 0.0, 0.0, 1.0], shape(A))
  B = reshape([1.0, 0.0, 0.0, 1.0], shape(B))

  ! Perform matrix multiplication: C = A * B
  C = matmul(matrix_a = A, matrix_b = B)

  ! Print the matrices
  print *, "Matrix A (2x2 identity matrix):"
  print *, A

  print *, "Matrix B (2x2 identity matrix):"
  print *, B

  print *, "Matrix C (result of A * B):"
  print *, C
end
