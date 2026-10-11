! A private derived type of a module is not accessible by use association,
! so a local type of the same name does not conflict with it, while the
! public types of the module are still imported.
module derived_types_216_pm
implicit none
private
public :: z
integer :: z = 1
type :: q
    integer :: v
end type
end module

module derived_types_216_m1
implicit none
private
type :: base
    integer :: v = 3
end type
type, public :: pub
    private
    integer :: w = 7
contains
    procedure :: getw
end type
type, extends(base), public :: child
    integer :: c = 4
end type
type(base), public :: x
public :: getw
contains
integer function getw(self)
    class(pub), intent(in) :: self
    getw = self%w
end function
end module

module derived_types_216_m2
implicit none
type :: later
    integer :: k = 9
end type
type, private :: node
    integer :: a
end type
private
public :: later
type :: hidden
    integer :: h
end type
end module

module derived_types_216_m3
implicit none
type :: node
    integer :: a = 11
end type
type :: tail
    integer :: c = 13
end type
private
end module

module derived_types_216_m4
implicit none
type :: hidden
    real :: r = 2.5
end type
end module

program derived_types_216
use derived_types_216_pm
use derived_types_216_m1
use derived_types_216_m2
use derived_types_216_m3
use derived_types_216_m4
implicit none
type :: q
    real :: w
end type
type :: base
    integer :: u = 5
end type
type :: node
    integer :: a = 15
end type
type :: tail
    integer :: c = 17
end type
type(q) :: b
type(base) :: bs
type(pub) :: p
type(child) :: c
type(later) :: l
type(hidden) :: h
type(node) :: n
type(tail) :: tl
b%w = 1.5
print *, b%w, z
if (abs(b%w - 1.5) > 1e-6) error stop 1
if (z /= 1) error stop 2
if (bs%u /= 5) error stop 3
if (x%v /= 3) error stop 4
if (p%getw() /= 7) error stop 5
if (c%v /= 3 .or. c%c /= 4) error stop 6
if (l%k /= 9) error stop 7
if (abs(h%r - 2.5) > 1e-6) error stop 8
if (n%a /= 15) error stop 9
if (tl%c /= 17) error stop 10
end program
