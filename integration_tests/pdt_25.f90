! A length type parameter with a constant value: arrays of a parameterized
! derived type whose array component has the length parameter as its extent
module pdt_25_mod
implicit none

type :: vec(n)
    integer, len :: n
    integer :: a(n)
end type

contains

    integer function total(v)
        type(vec(10)), intent(in) :: v
        total = sum(v%a) + v%n
    end function

end module

program pdt_25
use pdt_25_mod, only: vec, total
implicit none

type(vec(10)), target :: x(7)
type(vec(1000)) :: y
type(vec(0)) :: z
integer, pointer :: p(:)
integer :: i

do i = 1, size(x)
    x(i)%a(:) = i
end do
if (any(x(:)%a(1) /= [1, 2, 3, 4, 5, 6, 7])) error stop 1
p => x(4)%a
if (size(p) /= 10 .or. any(p /= 4)) error stop 2
if (x(1)%n /= 10 .or. storage_size(x(1)%a(1)) /= 32) error stop 3
if (total(x(3)) /= 40) error stop 4

y%a = 1
if (y%n /= 1000 .or. size(y%a) /= 1000 .or. sum(y%a) /= 1000) error stop 5
if (z%n /= 0 .or. size(z%a) /= 0) error stop 6
print *, x(:)%a(1)
print *, p
end program
