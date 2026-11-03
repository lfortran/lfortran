! size() of a dummy whose extent kind differs from the size kind, used as a
! function-result extent (#15394)
module array_size_kind_01_mod
implicit none
contains

    function g(x) result(y)
        real, intent(in) :: x(:,:)
        real :: y(size(x, kind=8))
        y = 1.0
    end function

    function f8(a, n) result(r)
        integer(8), intent(in) :: n
        real, intent(in) :: a(n)
        real :: r(size(a))
        r = a + 1
    end function

    function f4(a, n) result(r)
        integer, intent(in) :: n
        real, intent(in) :: a(n)
        real :: r(size(a, kind=8))
        r = a + 1
    end function

end module

program array_size_kind_01
use array_size_kind_01_mod, only: g, f8, f4
implicit none
real :: a(2,3), r(6), x(3)

a = 1.0
r = 0.0
r = g(a + 1.0)
print *, sum(r)
if (abs(sum(r) - 6.0) > 1e-6) error stop

r = 0.0
r = g(transpose(a))
print *, sum(r)
if (abs(sum(r) - 6.0) > 1e-6) error stop

x = 1.0
x = f8(x, 3_8)
print *, x
if (any(abs(x - 2.0) > 1e-6)) error stop

x = 1.0
x = f4(x, 3)
print *, x
if (any(abs(x - 2.0) > 1e-6)) error stop
end program
