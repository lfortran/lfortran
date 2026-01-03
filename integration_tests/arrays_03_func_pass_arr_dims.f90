program arrays_03_func
implicit none
integer :: x(10), s, i
do i = 1, size(x)
    x(i) = i
end do
s = mysum(size(x, dim=1), x)
print *, s
if (s /= 55) error stop

contains

    integer function mysum(na1, a) result(r)
    integer, intent(in) :: na1
    integer, intent(in) :: a(na1)
    integer :: i
    r = 0
    do i = 1, size(a)
        r = r + a(i)
    end do
    end function
end
