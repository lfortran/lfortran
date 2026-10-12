! Named critical constructs: each name is its own lock, which every critical
! construct of that name shares, here and in openmp_102b.f90.
program openmp_102
use omp_lib
implicit none
interface
    subroutine openmp_102_add(total)
    integer, intent(inout) :: total
    end subroutine
end interface
integer :: total, other, i

total = 0
!$omp critical (acc)
total = total + 1
!$omp end critical (acc)
if (total /= 1) error stop

total = 0
other = 0
!$omp parallel do shared(total, other)
do i = 1, 1000
    !$omp critical (ACC)
    total = total + 1
    !$omp end critical (ACC)
    !$omp critical(other_lock) hint(omp_sync_hint_contended)
    other = other + 2
    !$omp end critical(other_lock)
    call openmp_102_add(total)
end do
!$omp end parallel do
if (total /= 2000) error stop
if (other /= 2000) error stop
print *, total, other
end program
