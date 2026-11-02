module derived_types_245_m
implicit none
type payload
    real :: w(3)
end type
type link
    type(payload), pointer :: p
end type
type envelope
    type(link) :: links(2)
end type
type(payload), target :: g
contains
function f() result(r)
    real :: r(3)
    r(1) = 100.
    r(2) = g%w(1) + 10.
    r(3) = g%w(1) + 20.
end function
end module

program derived_types_245
use derived_types_245_m
implicit none
type(link) :: object
type(envelope) :: box

object%p => g
g%w = [1., 2., 3.]
object%p%w = f()
print *, g%w
if (any(g%w /= [100., 11., 21.])) error stop 1

box%links(2)%p => g
g%w = [1., 2., 3.]
box%links(2)%p%w = f()
print *, g%w
if (any(g%w /= [100., 11., 21.])) error stop 2
end program
