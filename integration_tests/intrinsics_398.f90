program main
use, intrinsic :: iso_fortran_env, only: int64
implicit none
integer(int64) :: m, arr(10)
integer :: i
do i = 1, 10
    arr(i) = i**5
end do

m = findloc(arr(1:5), value=arr(3), dim=1)
if (m /= 3) error stop

m = findloc(arr(2) + arr(3) + arr(4) + arr(1:5), value=arr(6), dim=1)
if (m /= 0) error stop

m = findloc(arr(1) + arr(2:6), value=arr(1) + arr(3), dim=1)
if (m /= 2) error stop

end program main
