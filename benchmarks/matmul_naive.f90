! Dense matrix multiplication with explicit loops (no matmul intrinsic).
program matmul_naive
implicit none
integer, parameter :: dp = kind(0.d0)
integer, parameter :: n = 1024
real(dp), allocatable :: a(:,:), b(:,:), c(:,:)
real(dp) :: s, expected
integer :: i, j, k

allocate(a(n,n), b(n,n), c(n,n))
do j = 1, n
    do i = 1, n
        a(i,j) = real(i + j, dp) / n
        b(i,j) = real(i - j, dp) / n
    end do
end do

c = 0
do j = 1, n
    do k = 1, n
        do i = 1, n
            c(i,j) = c(i,j) + a(i,k) * b(k,j)
        end do
    end do
end do

! sum_ij c(i,j) = sum_k (sum_i a(i,k)) * (sum_j b(k,j))
expected = 0
do k = 1, n
    expected = expected + sum(a(:,k)) * sum(b(k,:))
end do
s = sum(c)
print *, "checksum:", s
if (abs(s - expected) > 1e-8_dp * max(1.0_dp, abs(expected))) error stop
end program
