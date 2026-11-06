! Evaluating an array result must not define v while a callback reads it.
! GFortran 16.2 also writes v early; F2018 10.2.1.3 supplies the expected result.
module functions_72_m
implicit none
abstract interface
    function scalar_callback() result(q)
        real :: q
    end function
end interface
type callbacks
    type(callbacks), pointer :: next => null()
    procedure(scalar_callback), pointer, nopass :: p
contains
    procedure :: evaluate
end type
type wrapper
    type(callbacks) :: inner
end type
type, extends(callbacks) :: extended_callbacks
end type
type plain
    type(plain), pointer :: next => null()
    real :: value
end type
procedure(scalar_callback), pointer :: stored
contains
function f(object) result(r)
    type(callbacks), intent(in) :: object
    real :: r(3)
    r(1) = 100.
    r(2) = object%p() + 10.
    r(3) = object%p() + 20.
end function

function nested(object) result(r)
    type(wrapper), intent(in) :: object
    real :: r(3)
    r(1) = 100.
    r(2) = object%inner%p() + 10.
    r(3) = object%inner%p() + 20.
end function

function evaluate(self) result(r)
    class(callbacks), intent(in) :: self
    real :: r(3)
    r(1) = 100.
    r(2) = self%p() + 10.
    r(3) = self%p() + 20.
end function

function inherited(object) result(r)
    type(extended_callbacks), intent(in) :: object
    real :: r(3)
    r(1) = 100.
    r(2) = object%p() + 10.
    r(3) = object%p() + 20.
end function

function dispatch() result(q)
    real :: q
    q = stored()
end function

function indirect(p) result(r)
    procedure(scalar_callback) :: p
    real :: r(3)
    r(1) = 100.
    r(2) = p() + 10.
    r(3) = p() + 20.
end function

function one() result(q)
    real :: q
    q = 1.
end function

function forward(p) result(q)
    procedure(scalar_callback) :: p
    real :: q
    q = p()
end function

function safe_dispatch() result(q)
    real :: q
    q = forward(one)
end function

function values(object) result(r)
    type(plain), intent(in) :: object
    real :: r(3)
    r = [100., object%value + 10., object%value + 20.]
end function

subroutine assign_dummy(object, v)
    type(callbacks), intent(in) :: object
    real, intent(inout) :: v(3)
    v = object%evaluate()
end subroutine

recursive function saved_target(read_only) result(r)
    logical, intent(in) :: read_only
    real :: r(3)
    real, save :: v(3) = [1., 2., 3.]
    if (read_only) then
        r = v
    else
        v = read_saved()
        r = v
    end if
end function

recursive function read_saved() result(r)
    real :: r(3), previous(3)
    r(1) = 100.
    previous = saved_target(.true.)
    r(2) = previous(1) + 10.
    r(3) = previous(1) + 20.
end function
end module

program functions_72
use functions_72_m
implicit none
type(callbacks) :: object
type(wrapper) :: box
type(extended_callbacks) :: child
type(plain) :: data
real :: v(3)

object%p => first
v = [1., 2., 3.]
if (v(1) /= 1.) error stop 1
v = f(object)
print *, v
if (any(v /= [100., 11., 21.])) error stop 2

box%inner%p => first
v = [1., 2., 3.]
if (v(1) /= 1.) error stop 3
v = nested(box)
print *, v
if (any(v /= [100., 11., 21.])) error stop 4

stored => first
v = [1., 2., 3.]
if (v(1) /= 1.) error stop 5
v = indirect(dispatch)
print *, v
if (any(v /= [100., 11., 21.])) error stop 6

v = [1., 2., 3.]
if (v(1) /= 1.) error stop 7
v = object%evaluate()
print *, v
if (any(v /= [100., 11., 21.])) error stop 8

child%p => first
v = [1., 2., 3.]
if (v(1) /= 1.) error stop 9
v = inherited(child)
print *, v
if (any(v /= [100., 11., 21.])) error stop 10

! Recursive data types and proven noncapturing callbacks remain eligible.
data%value = 1.
v = values(data)
if (any(v /= [100., 11., 21.])) error stop 11
v = indirect(safe_dispatch)
if (any(v /= [100., 11., 21.])) error stop 12

v = [1., 2., 3.]
call assign_dummy(object, v)
if (any(v /= [100., 11., 21.])) error stop 13

v = saved_target(.false.)
if (any(v /= [100., 11., 21.])) error stop 14
contains
function first() result(q)
    real :: q
    q = v(1)
end function
end program
