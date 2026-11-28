module elemental_actual_target_element_01_m
implicit none

type :: point_t
    real :: x = 0.0
end type

type :: box_t
    real :: arr(4) = 0.0
    real, allocatable :: opt
end type

type :: scaler_t
    real :: k = 1.0
contains
    procedure :: scale
end type

contains

elemental real function blend(p, q)
real, intent(in) :: p, q
blend = p * q
end function

elemental real function value_blend(p, q)
real, intent(in) :: p
real, value :: q
value_blend = p * q
end function

elemental real function target_blend(p, q)
real, intent(in) :: p
real, intent(in), target :: q
target_blend = p * q
end function

elemental real function optional_blend(p, q)
real, intent(in) :: p
real, intent(in), optional :: q
optional_blend = -p
if (present(q)) optional_blend = p * q
end function

elemental type(point_t) function point_blend(p, q)
type(point_t), intent(in) :: p, q
point_blend%x = p%x * q%x
end function

elemental real function point_x_blend(p, q)
type(point_t), intent(in) :: p, q
point_x_blend = p%x * q%x
end function

elemental real function positive_blend(p, q)
real, intent(in) :: p
type(point_t), intent(in) :: q
if (p <= 0.0) error stop "called for an element whose mask is false"
positive_blend = p * q%x
end function

elemental real function box_blend(p, q)
real, intent(in) :: p
type(box_t), intent(in) :: q
box_blend = p * q%arr(1)
end function

elemental real function scale(self, p, q)
class(scaler_t), intent(in) :: self
real, intent(in) :: p, q
scale = self%k * p * q
end function

real function plain(p)
real, intent(in) :: p
plain = p
end function

end module

program elemental_actual_target_element_01
use elemental_actual_target_element_01_m
implicit none
real :: w(4), f(4), x(4)
logical :: m(4)
type(point_t) :: a(4), b(4)
type(box_t) :: s
type(scaler_t) :: oo(4)
class(scaler_t), allocatable :: po(:)

f = 2.0
b%x = 2.0

! The element is the second argument, then the first one
w = [3.0, 1.0, 1.0, 1.0]
w = blend(f, w(1))
if (any(w /= 6.0)) error stop
w = [1.0, 3.0, 1.0, 1.0]
w = blend(w(2), f)
if (any(w /= 6.0)) error stop

! A VALUE dummy takes a copy, a TARGET dummy does not
w = [3.0, 1.0, 1.0, 1.0]
w = value_blend(f, w(1))
if (any(w /= 6.0)) error stop
w = [3.0, 1.0, 1.0, 1.0]
w = target_blend(f, w(1))
if (any(w /= 6.0)) error stop

! An OPTIONAL dummy does not take a copy: an unallocated actual is absent
w = [3.0, 1.0, 1.0, 1.0]
w = optional_blend(f, w(1))
if (any(w /= 6.0)) error stop
s%arr = optional_blend(f, s%opt)
if (any(s%arr /= -2.0)) error stop

! Derived type arguments and result
a%x = [3.0, 1.0, 1.0, 1.0]
a = point_blend(b, a(1))
if (any(a%x /= 6.0)) error stop

! The target is a component of the array
a%x = [3.0, 1.0, 1.0, 1.0]
a%x = point_x_blend(b, a(1))
if (any(a%x /= 6.0)) error stop
a%x = [3.0, 1.0, 1.0, 1.0]
a%x = blend(f, a(1)%x)
if (any(a%x /= 6.0)) error stop

! The target is an array component of a scalar
s%arr = [3.0, 1.0, 1.0, 1.0]
s%arr = blend(f, s%arr(1))
if (any(s%arr /= 6.0)) error stop
s%arr = [3.0, 1.0, 1.0, 1.0]
s%arr = box_blend(f, s)
if (any(s%arr /= 6.0)) error stop

! The target is a section
w = [1.0, 1.0, 1.0, 3.0]
w(4:1:-1) = blend(f, w(4))
if (any(w /= 6.0)) error stop

! A type-bound function with an element as the passed object, also polymorphic
oo%k = [3.0, 1.0, 1.0, 1.0]
oo%k = oo(1)%scale(f, 1.0)
if (any(oo%k /= 6.0)) error stop
allocate(po(4))
po%k = [3.0, 1.0, 1.0, 1.0]
po%k = po(1)%scale(f, 1.0)
if (any(po%k /= 6.0)) error stop

! An associate name of the target
w = [3.0, 1.0, 1.0, 1.0]
associate (y => w)
    w = blend(f, y(1))
end associate
if (any(w /= 6.0)) error stop

! A WHERE statement, also with a scalar reference, which is evaluated for every element
m = [.true., .true., .false., .true.]
w = [3.0, 1.0, 1.0, 1.0]
where (m) w = blend(f, w(1))
if (any(w /= [6.0, 6.0, 1.0, 6.0])) error stop
w = [3.0, 1.0, 1.0, 1.0]
where (m) w = f + plain(w(1))
if (any(w /= [5.0, 5.0, 1.0, 5.0])) error stop

! A later statement of a WHERE construct reads the element that an earlier one assigned
w = [3.0, 1.0, 1.0, 1.0]
where (m)
    w = w + 1.0
elsewhere
    w = blend(f, w(1))
end where
if (any(w /= [4.0, 2.0, 8.0, 2.0])) error stop

! WHERE does not call the function for an element whose mask is false
x = [1.0, -1.0, 2.0, 0.0]
a%x = [1.0, 1.0, 1.0, 3.0]
where (x > 0.0) a%x = positive_blend(x, a(4))
if (any(a%x /= [3.0, 1.0, 6.0, 3.0])) error stop

print *, "ok"
end program
