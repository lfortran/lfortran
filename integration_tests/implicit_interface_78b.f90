! Companion to implicit_interface_78.
subroutine lib(p, a)
external p
real :: a(10)
call p(a)
end subroutine

subroutine u(x)
real :: x(10)
x(2) = 7
end subroutine
