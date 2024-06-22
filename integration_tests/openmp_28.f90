subroutine csr_matvec(Ap, Aj, Ax, x, y)
! Compute y = A*x for CSR matrix A and dense vectors x, y
integer, intent(in) :: Ap(:), Aj(:)
real, intent(in) :: Ax(:), x(:)
real, intent(inout) :: y(size(Ap)-1)
integer :: i, j

! Initialize y to zero
y = 0.0

!$omp parallel shared(Ap, Aj, Ax, x, y) private(i, j)
!$omp do
do i = 1, size(Ap)-1
    do j = Ap(i), Ap(i+1)-1
        y(i) = y(i) + Ax(j)*x(Aj(j))
    end do
end do
!$omp end do
!$omp end parallel

print *, sum(y(1:10))
if (abs(sum(y(1:10)) - 950.00) > 1e-8) error stop
end subroutine

program openmp_28
use omp_lib
implicit none
interface
subroutine csr_matvec(Ap, Aj, Ax, x, y)
integer, intent(in) :: Ap(:), Aj(:)
real, intent(in) :: Ax(:), x(:)
real, intent(inout) :: y(size(Ap)-1)
end subroutine
end interface

integer, parameter :: n = 100
integer :: i
real :: x(n), y(n)
integer, allocatable :: Ap(:), Aj(:)
real, allocatable :: Ax(:)

! Initialize CSR matrix A
allocate(Ap(n+1), Aj(3*n), Ax(3*n))
Ap = [(3*(i-1)+1, i=1,n+1)]
Aj = [(mod(i-1, n)+1, i=1,3*n)]
Ax = [(1.0, 2.0, 3.0, i=1,n)]

! Initialize vector x
x = [(i, i=1,n)]

! Initialize y to zero
y = 0.0

! Compute y = A*x
call csr_matvec(Ap, Aj, Ax, x, y)

! Print the result
print *, sum(y)
if (abs(sum(y) - 30300.00) > 1e-8) error stop

end program
