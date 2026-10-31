! Conjugate gradient on the 2D Poisson matrix stored in CSR format.
module cg_sparse_mod
implicit none
integer, parameter :: dp = kind(0.d0)

contains

    subroutine spmv(rowptr, colind, val, x, y)
    integer, intent(in) :: rowptr(:), colind(:)
    real(dp), intent(in) :: val(:), x(:)
    real(dp), intent(out) :: y(:)
    integer :: i, k
    do i = 1, size(y)
        y(i) = 0
        do k = rowptr(i), rowptr(i+1) - 1
            y(i) = y(i) + val(k) * x(colind(k))
        end do
    end do
    end subroutine

end module

program cg_sparse
use cg_sparse_mod, only: dp, spmv
implicit none
integer, parameter :: m = 400, n = m*m, maxiter = 2000
integer, allocatable :: rowptr(:), colind(:)
real(dp), allocatable :: val(:), b(:), x(:), r(:), p(:), q(:)
real(dp) :: alpha, beta, rr, rr_new, bnorm
integer :: i, j, row, nnz, it

allocate(rowptr(n+1), colind(5*n), val(5*n))
nnz = 0
do j = 1, m
    do i = 1, m
        row = (j-1)*m + i
        rowptr(row) = nnz + 1
        if (j > 1) call add(row - m, -1.0_dp)
        if (i > 1) call add(row - 1, -1.0_dp)
        call add(row, 4.0_dp)
        if (i < m) call add(row + 1, -1.0_dp)
        if (j < m) call add(row + m, -1.0_dp)
    end do
end do
rowptr(n+1) = nnz + 1

allocate(b(n), x(n), r(n), p(n), q(n))
b = 1
x = 0
r = b
p = r
rr = dot_product(r, r)
bnorm = sqrt(rr)
do it = 1, maxiter
    call spmv(rowptr, colind, val, p, q)
    alpha = rr / dot_product(p, q)
    x = x + alpha * p
    r = r - alpha * q
    rr_new = dot_product(r, r)
    if (sqrt(rr_new) < 1e-8_dp * bnorm) exit
    beta = rr_new / rr
    p = r + beta * p
    rr = rr_new
end do

! Check the true residual, not just the recurrence
call spmv(rowptr, colind, val, x, q)
print *, "iterations:", it, "checksum:", sum(x)
if (it > maxiter) error stop
if (sqrt(sum((b - q)**2)) > 1e-6_dp * bnorm) error stop

contains

    subroutine add(col, v)
    integer, intent(in) :: col
    real(dp), intent(in) :: v
    nnz = nnz + 1
    colind(nnz) = col
    val(nnz) = v
    end subroutine

end program
