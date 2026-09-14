! The `default(shared)` and `default(none)` clauses on OpenMP directives.
program openmp_81
implicit none
integer, parameter :: n = 100
real :: a(n)
integer :: i, s
a = 1.0
!$omp parallel do default(shared) private(i) schedule(static)
do i = 1, n
    a(i) = a(i)*2.0
end do
!$omp end parallel do
s = 0
!$omp parallel do default(none) shared(a) private(i) reduction(+:s)
do i = 1, n
    s = s + int(a(i))
end do
!$omp end parallel do
do i = 1, n
    if (abs(a(i) - 2.0) > 1e-5) error stop
end do
if (s /= 2*n) error stop
print *, "PASSED"
end program
