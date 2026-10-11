! VALUE dummy arguments passed through module procedures, procedure
! pointers, dummy procedures, type-bound procedures, internal procedures
! and external procedures, with various kinds of actual arguments.
module value_attribute_06_mod
implicit none

abstract interface
    integer(8) function value_iface(a, b, x, l)
    integer(4), value :: a
    integer(8), value :: b
    real(8), value :: x
    logical, value :: l
    end function
end interface

type :: t
    integer(4) :: a = 5
    integer(8) :: b = 6
    real(8) :: x = 0.5d0
    logical :: l = .true.
contains
    procedure :: bump
end type

contains

    integer(8) function combine(a, b, x, l) result(r)
    integer(4), value :: a
    integer(8), value :: b
    real(8), value :: x
    logical, value :: l
    r = a + b + int(10 * x, 8)
    if (l) r = -r
    a = 0
    b = 0
    x = 0
    l = .false.
    end function

    integer(8) function call_dummy(f, a, b, x, l) result(r)
    procedure(value_iface) :: f
    integer(4), value :: a
    integer(8), value :: b
    real(8), value :: x
    logical, value :: l
    r = f(a, b, x, l)
    end function

    integer(8) function forward(a, b, x, l) result(r)
    integer(4), value :: a
    integer(8), value :: b
    real(8), value :: x
    logical, value :: l
    a = a + 1
    r = combine(a, b, x, l)
    if (a /= 2 .and. a /= 6) error stop
    end function

    integer(8) function bump(self, n)
    class(t), intent(in) :: self
    integer(8), value :: n
    n = n + self%b
    bump = n
    end function

    recursive integer(4) function fact(n) result(r)
    integer(4), value :: n
    if (n <= 1) then
        r = 1
    else
        r = n * fact(n - 1)
    end if
    end function

    subroutine opt(a, b)
    integer(4), value :: a
    integer(4), value, optional :: b
    if (present(b)) then
        if (a /= 1 .or. b /= 2) error stop
    else
        if (a /= 3) error stop
    end if
    end subroutine

end module

integer(4) function ext_add(a, b)
integer(4), value :: a
real(4), value :: b
ext_add = a + int(b)
a = 0
end function

program value_attribute_06
use value_attribute_06_mod
implicit none
interface
    integer(4) function ext_add(a, b)
    integer(4), value :: a
    real(4), value :: b
    end function
end interface
procedure(value_iface), pointer :: p
type(t) :: s
integer(4) :: a, arr(3)
integer(8) :: b, n
integer(4), target :: tgt
integer(4), pointer :: ptr
real(8) :: x
logical :: l

a = 1
b = 2
x = 0.3d0
l = .false.

if (combine(a, b, x, l) /= 6) error stop
if (a /= 1 .or. b /= 2 .or. x /= 0.3d0 .or. l) error stop
if (combine(1, 2_8, 0.3d0, .true.) /= -6) error stop
if (combine(a + 1, b * 2, x + 0.1d0, a > b) /= 10) error stop

arr = [10, 20, 30]
if (combine(arr(2), b, x, l) /= 25) error stop
if (arr(2) /= 20) error stop

if (combine(s%a, s%b, s%x, s%l) /= -16) error stop
if (s%a /= 5 .or. s%b /= 6 .or. s%x /= 0.5d0 .or. .not. s%l) error stop

tgt = 4
ptr => tgt
if (combine(ptr, b, x, l) /= 9) error stop
if (tgt /= 4) error stop

p => combine
if (p(a, b, x, l) /= 6) error stop
if (a /= 1 .or. b /= 2 .or. x /= 0.3d0 .or. l) error stop

if (call_dummy(combine, a, b, x, l) /= 6) error stop
if (call_dummy(p, 5, 6_8, 0.5d0, .true.) /= -16) error stop
if (a /= 1) error stop

if (forward(a, b, x, l) /= 7) error stop
if (a /= 1) error stop

n = 3
if (s%bump(n) /= 9) error stop
if (n /= 3) error stop

if (fact(5) /= 120) error stop

call opt(1, 2)
call opt(3)

if (ext_add(a, 2.5) /= 3) error stop
if (a /= 1) error stop

if (inner(a) /= 101) error stop
if (a /= 1) error stop

print *, "PASS"

contains

    integer(4) function inner(k)
    integer(4), value :: k
    k = k + 100
    inner = k
    end function

end program
