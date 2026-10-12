! The parallel, task and teams regions here and in openmp_98b.f90, compiled
! separately, are outlined under the same names; the objects must link.
program openmp_98
implicit none
interface
    subroutine openmp_98_regions(s, x, y)
    integer, intent(out) :: s, x, y
    end subroutine
end interface
integer :: i, s, x, y, s2, x2, y2

s = 0
!$omp parallel do reduction(+:s)
do i = 1, 10
    s = s + i
end do
!$omp end parallel do

x = 0
!$omp parallel
!$omp single
!$omp task shared(x)
x = 55
!$omp end task
!$omp end single
!$omp end parallel

y = 0
!$omp teams num_teams(1)
y = 7
!$omp end teams

call openmp_98_regions(s2, x2, y2)
if (s /= 55) error stop
if (x /= 55) error stop
if (y /= 7) error stop
if (s2 /= 110) error stop
if (x2 /= 20) error stop
if (y2 /= 9) error stop
print *, s, x, y, s2, x2, y2
end program
