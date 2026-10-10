! Under separate compilation, a bind(c) module array of a type with
! component defaults links and takes the defaults.
program derived_types_212
use derived_types_212_m, only: dt212_arr, dt212_arr2, bump
implicit none
integer :: i
do i = 1, 4
    if (dt212_arr(i)%z /= 7) error stop 1
    if (abs(dt212_arr(i)%r - 1.5d0) > 1d-12) error stop 2
end do
do i = 1, 3
    if (dt212_arr2(i)%z /= 7) error stop 3
    if (abs(dt212_arr2(i)%r - 1.5d0) > 1d-12) error stop 4
end do
call bump()
dt212_arr2(3)%r = 4.0d0
if (dt212_arr(2)%z /= 8) error stop 5
if (dt212_arr(1)%z /= 7) error stop 6
if (abs(dt212_arr2(3)%r - 4.0d0) > 1d-12) error stop 7
print *, dt212_arr(4)%z, dt212_arr2(1)%z
end program
