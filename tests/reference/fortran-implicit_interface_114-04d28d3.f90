program implicit_interface_114
implicit none
type :: t
    procedure(__p_iface_implicit), pointer :: p
end type t
type(t) :: a
integer(4) :: i
integer(4) :: y
external :: __p_iface_implicit
integer(4), external :: f
external :: g
i = 1
y = f(1)
call g(i)
a%p => touch
call a%p(1)
call apply(incr, i)
if (i /= 2) then
    error stop
end if
if (y /= 1) then
    error stop
end if

contains

subroutine apply(f, x)
    integer(4), intent(inout) :: x
    integer(4), external :: f
    call f(x)
end subroutine apply

subroutine incr(x)
    integer(4), intent(inout) :: x
    x = x + 1
end subroutine incr

subroutine touch(n)
    integer(4), intent(in) :: n
    if (n /= 1) then
        error stop
    end if
end subroutine touch

integer(4) function f(n)
    integer(4), intent(in) :: n
    f = n
end function f

subroutine g(i)
    integer(4), intent(inout) :: i
    if (i /= 1) then
        error stop
    end if
end subroutine g

end program implicit_interface_114
