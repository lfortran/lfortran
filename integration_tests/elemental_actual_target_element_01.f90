module elemental_actual_target_element_01_m
implicit none

type :: point_t
    real :: x = 0.0
end type

type :: box_t
    real :: k = 0.0
    real :: arr(4) = 0.0
end type

type :: scaler_t
    real :: k = 1.0
contains
    procedure :: scale
end type

interface operator(.mul.)
    module procedure blend
end interface

contains

elemental real function blend(p, q)
real, intent(in) :: p, q
blend = p * q
end function

elemental integer function iblend(p, q)
integer, intent(in) :: p, q
iblend = p * q
end function

elemental logical function lblend(p, q)
logical, intent(in) :: p, q
lblend = p .neqv. q
end function

elemental complex function zblend(p, q)
complex, intent(in) :: p
real, intent(in) :: q
zblend = p * q
end function

impure elemental real function impure_blend(p, q)
real, intent(in) :: p, q
impure_blend = p * q
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

elemental real function blend3(p, q, r)
real, intent(in) :: p, q, r
blend3 = p * q * r
end function

elemental real function optional_blend(p, q)
real, intent(in) :: p
real, intent(in), optional :: q
optional_blend = p
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
real :: w(4), f(4), m(2,2), n(2,2)
real, allocatable :: aw(:)
integer :: iw(4), jf(4), i
logical :: l(4), l0(4)
complex :: z(4)
type(point_t) :: a(4), b(4)
type(box_t) :: s
type(scaler_t) :: o, oo(4)
class(scaler_t), allocatable :: po(:)

f = 2.0
b%x = 2.0

! The element is the second argument
w = [3.0, 1.0, 1.0, 1.0]
w = blend(f, w(1))
if (any(w /= 6.0)) error stop

! The element is the first argument, and not the first element
w = [1.0, 3.0, 1.0, 1.0]
w = blend(w(2), f)
if (any(w /= 6.0)) error stop

! A subscript that is a variable
i = 3
w = [1.0, 1.0, 3.0, 1.0]
w = blend(f, w(i))
if (any(w /= 6.0)) error stop

! Derived type arguments and result
a%x = [3.0, 1.0, 1.0, 1.0]
a = point_blend(b, a(1))
if (any(a%x /= 6.0)) error stop
a%x = [3.0, 1.0, 1.0, 1.0]
a = point_blend(a(1), b)
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

! Two elements of the target
w = [3.0, 2.0, 1.0, 1.0]
w = blend3(w(1), f, w(2))
if (any(w /= 12.0)) error stop

! The target is a section
w = [3.0, 1.0, 1.0, 1.0]
w(1:3) = blend(f(1:3), w(1))
if (any(w /= [6.0, 6.0, 6.0, 1.0])) error stop
w = [1.0, 1.0, 1.0, 3.0]
w(4:1:-1) = blend(f, w(4))
if (any(w /= 6.0)) error stop

! The reference is an operand
w = [3.0, 1.0, 1.0, 1.0]
w = blend(f, w(1)) + 1.0
if (any(w /= 7.0)) error stop
w = [3.0, 2.0, 1.0, 1.0]
w = blend(blend(f, w(1)), w(2))
if (any(w /= 12.0)) error stop
w = [8.0, 1.0, 1.0, 1.0]
w = sqrt(blend(f, w(1)))
if (any(w /= 4.0)) error stop
w = [3.0, 1.0, 1.0, 1.0]
w = merge(blend(f, w(1)), 0.0, f > 1.0)
if (any(w /= 6.0)) error stop

! An array constructor as the array argument
w = [3.0, 1.0, 1.0, 1.0]
w = blend([f(1), f(2), f(3), f(4)], w(1))
if (any(w /= 6.0)) error stop

! Other types
iw = [3, 1, 1, 1]
jf = 2
iw = iblend(jf, iw(1))
if (any(iw /= 6)) error stop
l0 = .true.
l = [.true., .false., .false., .false.]
l = lblend(l0, l(1))
if (any(l)) error stop
z = [(3.0, 1.0), (1.0, 1.0), (1.0, 1.0), (1.0, 1.0)]
z = zblend(z(1), f)
if (any(z /= (6.0, 2.0))) error stop

! A defined operator, an impure function, a VALUE, a TARGET and an optional dummy
w = [3.0, 1.0, 1.0, 1.0]
w = f .mul. w(1)
if (any(w /= 6.0)) error stop
w = [3.0, 1.0, 1.0, 1.0]
w = impure_blend(f, w(1))
if (any(w /= 6.0)) error stop
w = [3.0, 1.0, 1.0, 1.0]
w = value_blend(f, w(1))
if (any(w /= 6.0)) error stop
w = [3.0, 1.0, 1.0, 1.0]
w = target_blend(f, w(1))
if (any(w /= 6.0)) error stop
w = [3.0, 1.0, 1.0, 1.0]
w = optional_blend(f, w(1))
if (any(w /= 6.0)) error stop
w = [3.0, 1.0, 1.0, 1.0]
w = optional_blend(w(1))
if (any(w /= 3.0)) error stop

! A type-bound function, also with an element as the passed object, also polymorphic
w = [3.0, 1.0, 1.0, 1.0]
w = o%scale(f, w(1))
if (any(w /= 6.0)) error stop
oo%k = [3.0, 1.0, 1.0, 1.0]
oo%k = oo(1)%scale(f, 1.0)
if (any(oo%k /= 6.0)) error stop

allocate(po(4))
po%k = [3.0, 1.0, 1.0, 1.0]
po%k = po(1)%scale(f, 1.0)
if (any(po%k /= 6.0)) error stop

! Rank 2 and an allocatable target
m = reshape([3.0, 1.0, 1.0, 1.0], [2, 2])
n = 2.0
m = blend(n, m(1,1))
if (any(m /= 6.0)) error stop
allocate(aw(4))
aw = [3.0, 1.0, 1.0, 1.0]
aw = blend(f, aw(1))
if (any(aw /= 6.0)) error stop

! An associate name of the target
w = [3.0, 1.0, 1.0, 1.0]
associate (y => w)
    y = blend(f, w(1))
end associate
if (any(w /= 6.0)) error stop
w = [3.0, 1.0, 1.0, 1.0]
associate (y => w)
    w = blend(f, y(1))
end associate
if (any(w /= 6.0)) error stop

! In WHERE, also a scalar reference, which is evaluated for every element
w = [3.0, 1.0, 1.0, 1.0]
where (f > 1.0) w = blend(f, w(1))
if (any(w /= 6.0)) error stop
w = [3.0, 1.0, 1.0, 1.0]
where (f > 1.0) w = f + blend(w(1), w(1))
if (any(w /= 11.0)) error stop
w = [3.0, 1.0, 1.0, 1.0]
where (f > 1.0) w = f + plain(w(1))
if (any(w /= 5.0)) error stop

print *, "ok"
end program
