subroutine separate_compilation_61_s(x, total)
use separate_compilation_61a
implicit none
type(t) :: x
real :: total
real :: a(2)
procedure(callback), pointer :: q
procedure(fcb), pointer :: g
call x%f(x, a)
total = a(1) + a(2)
q => impl
a = 0
call q(x, a)
total = total + a(1) + a(2)
g => fimpl
a = g(3)
total = total + a(1) + a(2) + size(g(1))
end subroutine
