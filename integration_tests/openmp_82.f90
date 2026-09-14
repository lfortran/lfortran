program openmp_82
! do loops inside an !$omp task
implicit none
integer :: i, j, s
integer :: a(2), b(3,2), c(4), d(2)

a = 1
!$omp task shared(a)
do i = 1, 2
    a(i) = 2
end do
!$omp end task
if (a(1) /= 2 .or. a(2) /= 2) error stop

b = 0
!$omp task shared(b)
do j = 1, 2
    do i = 1, 3
        b(i, j) = i*10 + j
    end do
end do
!$omp end task
if (b(1, 1) /= 11 .or. b(3, 1) /= 31 .or. b(3, 2) /= 32) error stop

d = 0
!$omp task shared(d)
if (d(1) == 0) then
    do i = 1, 2
        d(i) = 5
    end do
end if
!$omp end task
if (d(1) /= 5 .or. d(2) /= 5) error stop

c = 0
!$omp parallel shared(c)
!$omp single
!$omp task shared(c)
do i = 1, 4
    c(i) = i
end do
!$omp end task
!$omp end single
!$omp end parallel
s = sum(c)
if (s /= 10) error stop

print *, a, b, d, s
end program
