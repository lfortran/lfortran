! A critical construct outside any parallel region in the program, and
! one in a procedure that a parallel region calls.
module openmp_101_mod
implicit none
contains
    subroutine add_one(total)
    integer, intent(inout) :: total
    !$omp critical
    total = total + 1
    !$omp end critical
    end subroutine
end module

program openmp_101
use omp_lib
use openmp_101_mod, only: add_one
implicit none
integer :: total, n

total = 0
!$omp critical
total = total + 1
!$omp end critical
if (total /= 1) error stop

call add_one(total)
if (total /= 2) error stop

total = 0
n = 0
!$omp parallel shared(total, n)
call add_one(total)
!$omp atomic
n = n + 1
!$omp end atomic
!$omp end parallel
if (total /= n) error stop
print *, total, n
end program
