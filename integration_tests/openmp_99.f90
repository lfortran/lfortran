! The regions of openmp_99_mod, compiled separately with
! --separate-compilation, are outlined under names that the program's object
! also uses; the objects must link.
program openmp_99
use openmp_99_mod, only: openmp_99_work
implicit none
integer :: i, s, y, s2, y2
call openmp_99_work(s, y)
s2 = 0
!$omp parallel do reduction(+:s2)
do i = 1, 4
    s2 = s2 + i
end do
!$omp end parallel do
!$omp parallel do reduction(+:s2)
do i = 1, 4
    s2 = s2 + 10*i
end do
!$omp end parallel do
y2 = 0
!$omp teams num_teams(1)
y2 = y2 + 1
!$omp end teams
!$omp teams num_teams(1)
y2 = y2 + 2
!$omp end teams
if (s /= 165) error stop
if (y /= 7) error stop
if (s2 /= 110) error stop
if (y2 /= 3) error stop
print *, s, y, s2, y2
end program
