subroutine openmp_98_regions(s, x, y)
implicit none
integer, intent(out) :: s, x, y
integer :: i, tx, ty

s = 0
!$omp parallel do reduction(+:s)
do i = 1, 10
    s = s + 2*i
end do
!$omp end parallel do

tx = 0
!$omp parallel
!$omp single
!$omp task shared(tx)
tx = 20
!$omp end task
!$omp end single
!$omp end parallel
x = tx

ty = 0
!$omp teams num_teams(1)
ty = 9
!$omp end teams
y = ty
end subroutine
