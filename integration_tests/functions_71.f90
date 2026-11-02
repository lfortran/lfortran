! A procedure pointer can carry a host-associated read of the assignment
! target. F2018 10.2.1.3 requires evaluating the result before defining v.
! GFortran 16.2 also returns the wrong [100,110,120]; it is not an oracle.
module functions_71_m
implicit none
abstract interface
    function scalar_callback() result(q)
        real :: q
    end function
end interface
type callbacks
    procedure(scalar_callback), pointer, nopass :: p
end type
contains
function f(p) result(r)
    procedure(scalar_callback) :: p
    real :: r(3)
    r(1) = 100
    r(2) = p() + 10
    r(3) = p() + 20
end function

function one() result(q)
    real :: q
    q = 1.
end function
end module

program functions_71
use functions_71_m
implicit none
procedure(scalar_callback), pointer :: pp
type(callbacks) :: object
real :: v(3)

pp => first
v = [1., 2., 3.]
if (v(1) /= 1.) error stop 1
v = f(pp)
print *, v
if (any(v /= [100., 11., 21.])) error stop 2

object%p => first
v = [1., 2., 3.]
if (v(1) /= 1.) error stop 3
v = f(object%p)
print *, v
if (any(v /= [100., 11., 21.])) error stop 4

! A known module procedure cannot capture this program's local v.
v = f(one)
print *, v
if (any(v /= [100., 11., 21.])) error stop 5
contains
function first() result(q)
    real :: q
    q = v(1)
end function
end program
