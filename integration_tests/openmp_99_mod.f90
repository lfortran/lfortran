module openmp_99_mod
implicit none
contains
subroutine openmp_99_work(s, y)
integer, intent(out) :: s, y
integer :: i, t
s = 0
!$omp parallel do reduction(+:s)
do i = 1, 10
    s = s + i
end do
!$omp end parallel do
!$omp parallel do reduction(+:s)
do i = 1, 10
    s = s + 2*i
end do
!$omp end parallel do
t = 0
!$omp teams num_teams(1)
t = t + 3
!$omp end teams
!$omp teams num_teams(1)
t = t + 4
!$omp end teams
y = t
end subroutine
end module
