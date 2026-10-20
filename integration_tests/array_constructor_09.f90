! Reallocating an allocatable component to an array constructor that
! contains an array-valued function call.
program array_constructor_09
implicit none
type :: t
    integer, allocatable :: arr(:)
end type
type(t) :: s(3)
s(1)%arr = [1, g()]
if (size(s(1)%arr) /= 3) error stop 1
if (any(s(1)%arr /= [1, 2, 3])) error stop 2
s(1)%arr = [g(), s(1)%arr]
if (size(s(1)%arr) /= 5) error stop 3
if (any(s(1)%arr /= [2, 3, 1, 2, 3])) error stop 4
print *, "ok"
contains
    function g() result(r)
        integer :: r(2)
        r = [2, 3]
    end function
end program
