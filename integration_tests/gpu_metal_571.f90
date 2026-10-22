! Named constants of device procedures declared from constants of their
! module (#15765)
module gpu_metal_571_m
implicit none
integer, parameter :: n = 3
integer, parameter :: w(3) = [10, 20, 30]
contains
pure integer function f(i)
    integer, intent(in) :: i
    integer, parameter :: two = n - 1
    f = i + two
end function

pure integer function g(i)
    integer, intent(in) :: i
    integer, parameter :: v(n) = w + n
    integer :: a(n)
    a = v
    g = a(mod(i - 1, n) + 1) + h()
contains
    pure integer function h()
        integer, parameter :: k = 2*n
        h = k
    end function
end function
end module

program gpu_metal_571
use gpu_metal_571_m, only: f, g
implicit none
integer :: o(4), p(4), i
do concurrent (i = 1:4)
    o(i) = f(i)
    p(i) = g(i)
end do
print *, o
print *, p
if (any(o /= [3, 4, 5, 6])) error stop 1
if (any(p /= [19, 29, 39, 19])) error stop 2
end program
