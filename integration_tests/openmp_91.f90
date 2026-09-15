program openmp_91
use openmp_91_m, only: work, scaled_sum
implicit none
integer :: x, i, b(4)
call work(x)
if (x /= 15150) error stop
!$omp parallel do
do i = 1, 4
    b(i) = scaled_sum(i)
end do
!$omp end parallel do
if (b(1) /= 10 .or. b(2) /= 20 .or. b(3) /= 30 .or. b(4) /= 40) error stop
print *, x, b
end program
