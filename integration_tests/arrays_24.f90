program arrays_24

real :: x(1), y(1), z(1)

z = f(x, y)

contains

function f(a, b) result(r)
real, intent(in) :: a(:), b(:)
real :: r(size(a))
integer :: i
do i = 1, size(a)
    r(i) = a(i) + b(i)
end do
end function

end program