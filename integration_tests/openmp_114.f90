module openmp_114_mod
!$ use omp_lib, only: omp_get_thread_num
implicit none
contains
    subroutine set_flag(flag)
    integer, intent(out) :: flag
    flag = 0
    !$ flag = 1
    !$ if (omp_get_thread_num() /= 0) flag = -1
    end subroutine
end module

program openmp_114
! OpenMP conditional compilation: free-form lines starting with the `!$`
! sentinel followed by a blank are compiled when OpenMP is enabled
!$ use omp_lib
use openmp_114_mod, only: set_flag
implicit none
integer :: n, i, m, flag, x, total
n = -1
!$ n = omp_get_max_threads()
if (n < 1) error stop

m = 0
!$ do 30 i = 1, 3
!$    m = m + i
!$ 30 continue
if (m /= 6) error stop

call set_flag(flag)
if (flag /= 1) error stop

! `!$` after a statement is an ordinary comment
x = 1 !$ x = 2
if (x /= 1) error stop

!$
total = 0
!$omp parallel do reduction(+:total)
do i = 1, 10
    total = total + i
end do
!$omp end parallel do
if (total /= 55) error stop
print *, n >= 1, m, flag, x, total
end program
