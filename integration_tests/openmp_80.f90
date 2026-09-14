! An OpenMP directive continued onto following `!$omp` lines with `&`.
program openmp_80
implicit none
integer, parameter :: n = 100
real :: a(n)
integer :: i, j, s
a = 1.0
!$omp parallel do schedule(static) &
!$omp   private(i, j)
do i = 1, n
    j = i
    a(j) = a(j)*2.0
end do
!$omp end parallel do
s = 0
!$omp parallel do &
!$omp& private(j) &
!$omp&   reduction(+:s)
do i = 1, n
    j = int(a(i))
    s = s + j
end do
!$omp end parallel do
do i = 1, n
    if (abs(a(i) - 2.0) > 1e-5) error stop
end do
if (s /= 2*n) error stop
print *, "PASSED"
end program
