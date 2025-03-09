program intrinsics_238
implicit none
integer :: i
integer, parameter :: res(5) = [ (maskl(i), i=1, 5) ]
do i = 1, 5
print*, res(i), maskl(i)
if (maskl(i) /= res(i) ) error stop
end do
print *, maskl(1, 8)
end program