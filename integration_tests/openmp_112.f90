module openmp_112_mod
implicit none
contains
subroutine fill(n, a)
    integer, intent(in) :: n
    integer, intent(out) :: a(n)
    integer :: i
!$OMP PARALLEL
!$OMP DO
    do i = 1, n
        a(i) = i
    end do
!$OMP ENDDO
!$OMP ENDPARALLEL
end subroutine
end module

program openmp_112
! OpenMP directive names with the optional blanks between their keywords
! omitted: `enddo`, `endparallel`, `paralleldo`, `endparalleldo`, ...
use openmp_112_mod
implicit none
integer :: a(10), b(10), c(10), d(10), i, s, t, u1, u2, nm, na
call fill(10, a)
if (sum(a) /= 55) error stop
!$omp paralleldo
do i = 1, 10
    b(i) = 2*i
end do
!$omp endparalleldo
if (sum(b) /= 110) error stop
!$omp parallel do
do i = 1, 10
    c(i) = 3*i
end do
!$omp end paralleldo
if (sum(c) /= 165) error stop
!$omp paralleldo
do i = 1, 10
    d(i) = 4*i
end do
!$omp endparallel do
if (sum(d) /= 220) error stop
s = 0
t = 0
u1 = 0
u2 = 0
nm = 0
na = 0
!$omp parallel shared(s, t, u1, u2, nm, na)
!$omp do
do i = 1, 10
!$omp critical
    s = s + i
!$omp endcritical
end do
!$omp enddo nowait
!$omp atomic
    na = na + 1
!$omp endatomic
!$omp single
    t = t + 1
!$omp endsingle
!$omp master
    nm = nm + 1
!$omp endmaster
!$omp sections
!$omp section
    u1 = 1
!$omp section
    u2 = 10
!$omp endsections
!$omp endparallel
if (s /= 55) error stop
if (t /= 1) error stop
if (nm /= 1) error stop
if (na < 1) error stop
if (u1 + u2 /= 11) error stop
!$omp parallelsections
!$omp section
    a(1) = -1
!$omp section
    a(2) = -2
!$omp endparallelsections
if (a(1) /= -1 .or. a(2) /= -2) error stop
!$omp parallel
!$omp single
!$omp task
    b(1) = 100
!$omp endtask
!$omp endsingle
!$omp endparallel
if (b(1) /= 100) error stop
c = 0
!$omp parallel num_threads(1) shared(c)
!$omp single
!$omp taskloop shared(c)
do i = 1, 10
    c(i) = i
end do
!$omp endtaskloop
!$omp endsingle
!$omp endparallel
if (sum(c) /= 55) error stop
d = 0
!$omp teamsdistribute num_teams(2) shared(d)
do i = 1, 10
    d(i) = 5*i
end do
!$omp endteamsdistribute
if (sum(d) /= 275) error stop
print *, "OK"
end program
