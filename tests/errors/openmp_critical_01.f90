program openmp_critical_01
implicit none
integer :: n
n = 0
!$omp critical (1acc)
n = n + 1
!$omp end critical (1acc)
end program
