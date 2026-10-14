! A critical construct nested in a critical construct of another name, and
! a critical construct nested in a master construct in a loop body.
program openmp_103
implicit none
integer :: seq(3), n, a, b, c, d, i

n = 0
seq = 0
!$omp critical (outer)
n = n + 1
seq(n) = 1
!$omp critical (inner)
n = n + 1
seq(n) = 2
!$omp end critical (inner)
n = n + 1
seq(n) = 3
!$omp end critical (outer)
if (n /= 3) error stop
if (any(seq /= [1, 2, 3])) error stop

a = 0
b = 0
!$omp parallel do shared(a, b)
do i = 1, 1000
    !$omp critical (outer)
    a = a + 1
    !$omp critical (inner)
    b = b + 1
    !$omp end critical (inner)
    a = a + 1
    !$omp end critical (outer)
end do
!$omp end parallel do
if (a /= 2000) error stop
if (b /= 1000) error stop

! Only the master thread runs the master construct, so the result does not
! depend on the number of threads.
c = 0
!$omp parallel num_threads(4) shared(c) private(i)
do i = 1, 3
    !$omp master
    !$omp critical
    c = c + 1
    !$omp end critical
    c = c + 10
    !$omp end master
end do
!$omp end parallel
if (c /= 33) error stop

d = 0
!$omp parallel num_threads(4) shared(d) private(i)
do i = 1, 3
    !$omp master
    !$omp critical (inner)
    d = d + 1
    !$omp end critical (inner)
    d = d + 10
    !$omp end master
end do
!$omp end parallel
if (d /= 33) error stop
print *, seq, a, b, c, d
end program
