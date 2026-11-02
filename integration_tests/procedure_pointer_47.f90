! References through procedure pointer components with an implicit
! interface whose designator depends on an implied do variable (#16414)
program procedure_pointer_47
implicit none
type :: h
    procedure(integer), pointer, nopass :: f => null()
end type
type(h) :: a(3)
integer :: i, r(3)
character(len=40) :: line
a(1)%f => inc
a(2)%f => dbl
a(3)%f => inc
r = [(a(i)%f(10), i = 1, 3)]
if (any(r /= [11, 20, 11])) error stop 1
r = [(a(i)%f(i), i = 1, 3)]
if (any(r /= [2, 4, 4])) error stop 2
write (line, "(3i4)") (a(i)%f(i), i = 1, 3)
if (line /= "   2   4   4") error stop 3
r = 0
do i = 1, 3
    r(i) = a(i)%f(a(4 - i)%f(i))
end do
if (any(r /= [3, 8, 5])) error stop 4
print *, "ok"
contains
integer function inc(x)
    integer, intent(in) :: x
    inc = x + 1
end function
integer function dbl(x)
    integer, intent(in) :: x
    dbl = 2*x
end function
end program
