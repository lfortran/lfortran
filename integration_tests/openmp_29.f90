program openmp_29
use omp_lib

implicit none

integer :: i
integer :: seed
integer, parameter :: n = 10000
double precision :: x(n)
double precision :: max1
double precision :: max2
double precision :: max3

seed = 1325

do i = 1, n
seed = mod ( ( 3125 * seed ), 65536 )
x(i) = ( seed - 32768.0 ) / 16384.0
end do

max1 = - huge(max1)
do i = 1, n
  if ( max1 < x(i) ) then
    max1 = x(i)
  end if
end do

!$omp parallel shared(x) private(i) reduction(max:max2)

!$omp do
do i = 1, n
max2 = max ( max2, x(i) )
end do
!$omp end do

!$omp end parallel

print *, "Done with parallel loop 1"
max3 = - huge(max3)

!$omp parallel shared(x) private(i) reduction(max:max3)
!$omp do
do i = 1, n
  if ( max3 < x(i) ) then
    max3 = x(i)
  end if
end do
!$omp end do

!$omp end parallel

print *, 'Done'
print *, '  X_MAX should be   ', max1
print *, '  Computed X_MAX2 = ', max2
print *, '  Computed X_MAX3 = ', max3
if (abs(max1 - max2) > 1e-8) error stop
if (abs(max1 - max3) > 1e-8) error stop
if (abs(max2 - max3) > 1e-8) error stop
end
