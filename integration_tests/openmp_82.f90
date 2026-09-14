program openmp_82
! do loops inside an !$omp task
implicit none
integer :: i, j, k, s
integer :: a(2), b(3,2), c(4), d(2)
real :: x(2), y(10), w(5)
integer(1) :: i1
integer(8) :: k8
real(4) :: r4
real(8) :: r8
logical :: l
complex(8) :: z
character(len=3) :: str

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

! The task data holds three integer(4) values followed by a pointer, so
! the pointer is preceded by padding
x = 1
!$omp task shared(x)
do i = 1, 2
    x(i) = 8
end do
!$omp end task
if (abs(x(1) - 8) > 1e-6 .or. abs(x(2) - 8) > 1e-6) error stop

y = 1
k = 3
!$omp parallel shared(y) firstprivate(k)
!$omp single
!$omp task shared(y) firstprivate(k)
y(k) = y(k) + 7
!$omp end task
!$omp end single
!$omp end parallel
if (abs(y(3) - 8) > 1e-6 .or. abs(y(2) - 1) > 1e-6) error stop

i1 = 7
k8 = 3
r4 = 1.5
r8 = 2.5d0
l = .true.
z = (1d0, 2d0)
str = 'abc'
w = 0
!$omp task firstprivate(i1, k8, r4, r8, l, z, str) shared(w)
do i = 1, 5
    w(i) = real(k8) + r4 + real(r8) + merge(1.0, 0.0, l) &
        + real(aimag(z)) + real(i1) + len_trim(str) + i
end do
!$omp end task
if (abs(w(1) - 21) > 1e-5 .or. abs(w(5) - 25) > 1e-5) error stop

print *, x, y(3), w
end program
