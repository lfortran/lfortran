program gpu_metal_455
! Every form of matmul an offloaded loop lowers inline, each element
! accumulated in a scalar: matrix-vector (also with a transposed
! matrix and a vector of another lower bound), vector-matrix and
! matrix-matrix, alone and combined with another operand on either
! side, including the target itself.
implicit none
integer, parameter :: n = 3, m = 4, np = 2
real :: w(n, m), x(m, np), x0(0:m - 1, np), b(n), z(n, np), y(m, np)
real :: v(m, np), c(n, n, np), d(n, np), e(n, np), r(n, np)
integer :: i, k, p, j

do k = 1, m
  do i = 1, n
    w(i, k) = real(i + 2*k)
  end do
  do p = 1, np
    x(k, p) = real(k*p)
    x0(k - 1, p) = real(k*p)
  end do
end do
b = [1.0, -2.0, 3.0]
do p = 1, np
  d(:, p) = real(p)
end do

do concurrent (p = 1:np)
  z(1:n, p) = matmul(w(1:n, 1:m), x(1:m, p)) + b(1:n)
  e(1:n, p) = b(1:n) - matmul(w(1:n, 1:m), x0(0:m - 1, p))
  y(1:m, p) = matmul(transpose(w(1:n, 1:m)), z(1:n, p))
  v(1:m, p) = matmul(z(1:n, p), w(1:n, 1:m))
  c(1:n, 1:n, p) = matmul(w(1:n, 1:m), transpose(w(1:n, 1:m))) * real(p)
  d(1:n, p) = matmul(w(1:n, 1:m), x(1:m, p)) + d(1:n, p)
end do

do p = 1, np
  do i = 1, n
    r(i, p) = 0
    do k = 1, m
      r(i, p) = r(i, p) + w(i, k) * x(k, p)
    end do
    if (abs(z(i, p) - (r(i, p) + b(i))) > 1e-3) error stop
    if (abs(e(i, p) - (b(i) - r(i, p))) > 1e-3) error stop
    if (abs(d(i, p) - (r(i, p) + real(p))) > 1e-3) error stop
    do j = 1, n
      if (abs(c(i, j, p) - real(p) * sum(w(i, :) * w(j, :))) > 1e-3) &
        error stop
    end do
  end do
  do k = 1, m
    if (abs(y(k, p) - sum(w(:, k) * z(:, p))) > 1e-2) error stop
    if (abs(v(k, p) - sum(z(:, p) * w(:, k))) > 1e-2) error stop
  end do
end do
print *, z(1, 1), e(1, 2), y(1, 1), v(m, np), c(1, 2, 2), d(n, np)
print *, "PASSED"
end program
