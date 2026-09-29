program openmp_84
! A reduction clause on a worksharing `do` nested in a `parallel` region,
! where the reduction variables are shared by the region.
implicit none
integer, parameter :: n = 100
integer :: a(n), i
real :: b(n)
integer :: isum, iprod, imax, imin
real :: rsum, rprod, rmax, rmin
integer :: isum_ref, iprod_ref, imax_ref, imin_ref
real :: rsum_ref, rprod_ref, rmax_ref, rmin_ref

do i = 1, n
    a(i) = mod(i * 37, 101) - 50
    b(i) = real(mod(i * 53, 97)) + 1.0
end do

isum_ref = 5
iprod_ref = 3
imax_ref = -1000
imin_ref = 1000
rsum_ref = 2.5
rprod_ref = 4.0
rmax_ref = 0.5
rmin_ref = 1000.0
do i = 1, n
    isum_ref = isum_ref + a(i)
    rsum_ref = rsum_ref + b(i)
    if (mod(i, 10) == 0) then
        iprod_ref = iprod_ref * 2
        rprod_ref = rprod_ref * 0.5
    end if
    imax_ref = max(imax_ref, a(i))
    imin_ref = min(imin_ref, a(i))
    rmax_ref = max(rmax_ref, b(i))
    rmin_ref = min(rmin_ref, b(i))
end do

isum = 5
iprod = 3
rsum = 2.5
rprod = 4.0
!$omp parallel
!$omp do reduction(+:isum, rsum)
do i = 1, n
    isum = isum + a(i)
    rsum = rsum + b(i)
end do
!$omp end do
!$omp do reduction(*:iprod) reduction(*:rprod)
do i = 1, n
    if (mod(i, 10) == 0) then
        iprod = iprod * 2
        rprod = rprod * 0.5
    end if
end do
!$omp end do
!$omp end parallel

imax = -1000
imin = 1000
rmax = 0.5
rmin = 1000.0
!$omp parallel shared(imax, imin, rmax, rmin)
!$omp do reduction(max:imax, rmax) schedule(dynamic, 7)
do i = 1, n
    imax = max(imax, a(i))
    rmax = max(rmax, b(i))
end do
!$omp end do
!$omp do reduction(min:imin, rmin)
do i = 1, n
    imin = min(imin, a(i))
    rmin = min(rmin, b(i))
end do
!$omp end do
!$omp end parallel

print *, isum, iprod, imax, imin
print *, rsum, rprod, rmax, rmin
if (isum /= isum_ref) error stop
if (iprod /= iprod_ref) error stop
if (imax /= imax_ref) error stop
if (imin /= imin_ref) error stop
if (abs(rsum - rsum_ref) > 1e-3) error stop
if (abs(rprod - rprod_ref) > 1e-6) error stop
if (abs(rmax - rmax_ref) > 1e-6) error stop
if (abs(rmin - rmin_ref) > 1e-6) error stop
end program
