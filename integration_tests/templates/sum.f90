module sum_m

contains

integer function mysum(x) result(r)
integer, intent(in) :: x(:)
integer :: i
r = 0
do i = 1, size(x)
    r = r + x(i)
end do
end function

end module



program sum
use sum_m, only: mysum
integer :: x(10), r
x = 1
r = mysum(x)
print *, r
end program
