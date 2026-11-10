program implicit_interface_114
integer :: i, y
integer, external :: f
external g
type :: t
    procedure(), pointer, nopass :: p
end type
type(t) :: a
i = 1
y = f(1)
call g(i)
a%p => touch
call a%p(1)
call apply(incr, i)
if (i /= 2) error stop
if (y /= 1) error stop
contains
subroutine incr(x)
    integer, intent(inout) :: x
    x = x + 1
end subroutine
subroutine apply(f, x)
    external f
    integer, intent(inout) :: x
    call f(x)
end subroutine
subroutine touch(n)
    integer, intent(in) :: n
    if (n /= 1) error stop
end subroutine
end program

integer function f(n)
    integer, intent(in) :: n
    f = n
end function

subroutine g(i)
    integer, intent(inout) :: i
    if (i /= 1) error stop
end subroutine
