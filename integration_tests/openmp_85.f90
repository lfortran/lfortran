program openmp_85
! The result of a reduction on a worksharing `do` nested in a `parallel`
! region is final once the threads have passed the barrier of the loop,
! so it can be read later in the same region.
use omp_lib, only: omp_get_thread_num
implicit none
integer, parameter :: n = 64
integer :: i, isum, imax, single_sum, master_sum, master_max, count_ge
real :: rsum, single_rsum

isum = 0
rsum = 0.0
single_sum = -1
single_rsum = -1.0
!$omp parallel shared(isum, rsum, single_sum, single_rsum)
!$omp do reduction(+:isum, rsum) schedule(static)
do i = 1, n
    isum = isum + i
    rsum = rsum + 0.5
end do
!$omp end do
!$omp single
single_sum = isum
single_rsum = rsum
!$omp end single
!$omp end parallel
print *, isum, single_sum, rsum, single_rsum
if (isum /= n * (n + 1) / 2) error stop
if (single_sum /= n * (n + 1) / 2) error stop
if (abs(rsum - 32.0) > 1e-6) error stop
if (abs(single_rsum - 32.0) > 1e-6) error stop

imax = -1
isum = 0
master_sum = -1
master_max = -1
count_ge = -1
!$omp parallel shared(imax, isum, master_sum, master_max, count_ge)
!$omp do reduction(max:imax) schedule(dynamic, 5)
do i = 1, n
    imax = max(imax, mod(i * 37, 101))
end do
!$omp end do
!$omp do reduction(+:isum) schedule(guided, 3)
do i = 1, n
    if (mod(i * 37, 101) == imax) isum = isum + 1
end do
!$omp end do
!$omp master
master_sum = isum
master_max = imax
!$omp end master
!$omp end parallel
print *, imax, master_max, isum, master_sum
if (imax /= 100) error stop
if (master_max /= 100) error stop
if (isum /= 1) error stop
if (master_sum /= 1) error stop

isum = 0
single_sum = -1
!$omp parallel shared(isum, single_sum)
!$omp do reduction(+:isum)
do i = 1, n
    isum = isum + 2 * i
end do
!$omp end do
!$omp barrier
if (omp_get_thread_num() == 0) single_sum = isum
!$omp end parallel
print *, isum, single_sum
if (isum /= n * (n + 1)) error stop
if (single_sum /= n * (n + 1)) error stop
end program
