program complex_allocatable_scalar_01
implicit none

type :: t
    complex, allocatable :: c
    complex(8), allocatable :: z
end type t

complex, allocatable :: c
complex(8), allocatable :: z
complex :: d
complex(8) :: w
type(t) :: a

c = (1.0, 2.0)
z = (1.0_8, 2.0_8)
a%c = (1.0, 2.0)
a%z = (1.0_8, 2.0_8)

! arithmetic result used as an I/O output item
print *, c + (1.0, 2.0)
print *, z + (1.0_8, 2.0_8)

! arithmetic result used as a function actual argument
if (abs(abs(c + (1.0, 2.0)) - sqrt(20.0)) > 1e-5) error stop
if (abs(abs(z + (1.0_8, 2.0_8)) - sqrt(20.0_8)) > 1e-10_8) error stop

! unary minus on an allocatable scalar
print *, -c
print *, -z
d = -c
if (abs(d - (-1.0, -2.0)) > 1e-5) error stop
w = -z
if (abs(w - (-1.0_8, -2.0_8)) > 1e-10_8) error stop

! allocatable component as an operand
d = a%c + (1.0, 2.0)
if (abs(d - (2.0, 4.0)) > 1e-5) error stop
w = a%z + (1.0_8, 2.0_8)
if (abs(w - (2.0_8, 4.0_8)) > 1e-10_8) error stop
print *, a%c + (1.0, 2.0)
print *, -a%c

! regression guards: cases that already worked
d = c + (1.0, 2.0)
if (abs(d - (2.0, 4.0)) > 1e-5) error stop
c = c + (1.0, 2.0)
if (abs(c - (2.0, 4.0)) > 1e-5) error stop
if (.not. (c == (2.0, 4.0))) error stop
print *, c

! other operators on the allocatable scalar
c = (1.0, 2.0)
d = c - (1.0, 1.0)
if (abs(d - (0.0, 1.0)) > 1e-5) error stop
print *, c * (2.0, 0.0)
print *, c / (2.0, 0.0)
d = c * (2.0, 0.0)
if (abs(d - (2.0, 4.0)) > 1e-5) error stop
d = c / (2.0, 0.0)
if (abs(d - (0.5, 1.0)) > 1e-5) error stop

end program complex_allocatable_scalar_01
