! The `default(shared)` and `default(none)` clauses on `parallel do`,
! `parallel` and `parallel sections`.
program openmp_80
implicit none
integer, parameter :: n = 100
integer :: a(n), b(n), i, s, u

!$omp parallel do default(shared)
do i = 1, n
    a(i) = 2*i
end do
!$omp end parallel do
do i = 1, n
    if (a(i) /= 2*i) error stop
end do

b = 0
!$omp parallel do default(none) shared(a, b) private(i)
do i = 1, n
    b(i) = a(i) + 1
end do
!$omp end parallel do
do i = 1, n
    if (b(i) /= 2*i + 1) error stop
end do

s = 0
!$omp parallel do default( none ) shared(a) private(i) reduction(+:s)
do i = 1, n
    s = s + a(i)
end do
!$omp end parallel do
if (s /= n*(n + 1)) error stop

!$omp parallel default(shared) private(i)
!$omp do
do i = 1, n
    b(i) = b(i) + i
end do
!$omp end do
!$omp end parallel
do i = 1, n
    if (b(i) /= 3*i + 1) error stop
end do

s = 0
u = 0
!$omp parallel sections default(none) shared(s, u)
!$omp section
s = 5
!$omp section
u = 7
!$omp end parallel sections
if (s /= 5) error stop
if (u /= 7) error stop
print *, "PASSED"
end program
