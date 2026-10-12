subroutine openmp_102_add(total)
implicit none
integer, intent(inout) :: total
!$omp critical (acc)
total = total + 1
!$omp end critical (acc)
end subroutine
