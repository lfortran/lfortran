program array_section_44
! Array sections whose bounds differ only in an operand that the shape check
! does not compare (an integer(8) offset with a converted literal, an array
! element, or a function result) must keep their full extent (#16723).
implicit none
integer :: z(10), y(3), w(1), n(3), i
integer(8) :: k
real :: r(10), loc(3)
integer :: j

z = [(i, i = 1, 10)]
r = [(real(i), i = 1, 10)]

k = 1
y = z(k+1:k+3)
if (any(y /= [2, 3, 4])) error stop 1

k = 4
y = z(k-3:k-1)
if (any(y /= [1, 2, 3])) error stop 2

k = 2
y = z(k+1:k+5:2)
if (any(y /= [3, 5, 7])) error stop 3

w = z(k+2:k+2)
if (w(1) /= 4) error stop 4

j = 2
n = [1, 2, 3]
loc = r(j+n(1):j+n(3))
if (any(loc /= [3.0, 4.0, 5.0])) error stop 5

loc = r(j+f(1):j+f(3))
if (any(loc /= [3.0, 4.0, 5.0])) error stop 6

print *, "ok"

contains

integer function f(m)
integer, intent(in) :: m
f = m
end function

end program
