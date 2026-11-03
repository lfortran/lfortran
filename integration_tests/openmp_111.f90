program openmp_111
! Array-valued expression arguments of array intrinsics (`any(a > 10.0)`,
! `sum(z + 1.0)`, ...) in a program with a parallel region
implicit none
real :: a(4), z(2)
integer :: i
a = 0.0
if (any(a /= 0.0)) error stop
!$omp parallel do
do i = 1, 4
    a(i) = real(i)
end do
!$omp end parallel do
!$omp parallel
z = 2.0
!$omp end parallel
if (any(a > 10.0)) error stop
if (.not. any(a > 3.5)) error stop
if (.not. all(a < 5.0)) error stop
if (all(a < 4.0)) error stop
if (count(a > 1.5) /= 3) error stop
if (any(z /= 2.0)) error stop
if (abs(sum(z + 1.0) - 6.0) > 1e-6) error stop
if (abs(maxval(a * 2.0) - 8.0) > 1e-6) error stop
if (abs(product(a + 1.0) - 120.0) > 1e-4) error stop
print *, a, sum(z + 1.0)
end program
