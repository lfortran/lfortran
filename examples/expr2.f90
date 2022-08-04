program expr2
implicit none

integer :: x(2)
print *, mysum(x)

contains
function mysum(x) result(r)
    integer, intent(in) :: x(:)
    integer :: i
    integer :: r
    r = 0
    do i = 1, size(x)
        r = r + x(i)
    end do
end function
end program
