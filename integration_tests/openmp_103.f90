! A critical construct nested in a critical construct of another name.
program openmp_103
implicit none
integer :: seq(3), n, a, b, i

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
print *, seq, a, b
end program
