module class_160_mod
implicit none
type, abstract :: base_t
contains
    procedure(iface), deferred :: specific
    generic :: method => specific
end type

abstract interface
    integer function iface(this)
        import base_t
        class(base_t), intent(in) :: this
    end function
end interface

type, extends(base_t) :: child_t
    integer :: val = 1
contains
    procedure :: specific
end type

type, extends(child_t) :: grandchild_t
contains
    procedure :: specific => specific_grandchild
end type

! Generic with two specifics; the child overrides only one of them
type :: two_base_t
contains
    procedure :: s_int
    procedure :: s_real
    generic :: method => s_int, s_real
end type

type, extends(two_base_t) :: two_child_t
contains
    procedure :: s_real => s_real_child
end type

! Generic whose specific is a nopass binding overridden in the child
type :: nopass_base_t
contains
    procedure, nopass :: np_specific
    generic :: method => np_specific
end type

type, extends(nopass_base_t) :: nopass_child_t
contains
    procedure, nopass :: np_specific => np_specific_child
end type

contains

integer function specific(this)
    class(child_t), intent(in) :: this
    specific = this%val
end function

integer function specific_grandchild(this)
    class(grandchild_t), intent(in) :: this
    specific_grandchild = 10 * this%val
end function

integer function call_child(this)
    class(child_t), intent(in) :: this
    call_child = this%method()
end function

integer function call_base(this)
    class(base_t), intent(in) :: this
    call_base = this%method()
end function

integer function s_int(this, x)
    class(two_base_t), intent(in) :: this
    integer, intent(in) :: x
    s_int = x
end function

integer function s_real(this, x)
    class(two_base_t), intent(in) :: this
    real, intent(in) :: x
    s_real = 1
end function

integer function s_real_child(this, x)
    class(two_child_t), intent(in) :: this
    real, intent(in) :: x
    s_real_child = 2
end function

integer function np_specific(x)
    integer, intent(in) :: x
    np_specific = x
end function

integer function np_specific_child(x)
    integer, intent(in) :: x
    np_specific_child = 10 * x
end function

end module

program class_160
use class_160_mod
implicit none
type(child_t) :: c
type(grandchild_t) :: g
type(two_child_t) :: tc
class(two_base_t), allocatable :: tb
type(nopass_child_t) :: nc
class(nopass_base_t), allocatable :: nb

c%val = 3
g%val = 4

if (call_child(c) /= 3) error stop
if (call_child(g) /= 40) error stop
if (call_base(c) /= 3) error stop
if (call_base(g) /= 40) error stop
if (c%method() /= 3) error stop
if (g%method() /= 40) error stop
print *, call_child(c), call_child(g), c%method(), g%method()

allocate(two_child_t :: tb)
if (tc%method(5) /= 5) error stop
if (tc%method(1.0) /= 2) error stop
if (tb%method(7) /= 7) error stop
if (tb%method(1.0) /= 2) error stop
print *, tc%method(5), tc%method(1.0), tb%method(7), tb%method(1.0)

allocate(nopass_child_t :: nb)
if (nc%method(2) /= 20) error stop
if (nb%method(2) /= 20) error stop
print *, nc%method(2), nb%method(2)
end program
