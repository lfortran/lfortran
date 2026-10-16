! Components of a named constant whose value holds a procedure pointer
! component associated with a module procedure of another module, referenced
! from other program units (lfortran/lfortran#14794).
module procedure_pointer_40_funcs
implicit none
abstract interface
    integer function ifunc()
    end function
end interface
contains
    integer function one()
        one = 1
    end function
    integer function two()
        two = 2
    end function
    integer function three()
        three = 3
    end function
end module

module procedure_pointer_40_mod
use procedure_pointer_40_funcs
implicit none
type :: pc
    procedure(ifunc), pointer, nopass :: f => null()
end type
type :: outer
    type(pc) :: inner
end type
type :: top
    type(outer) :: mid
end type
type(outer), parameter :: o = outer(pc(one))
type(outer), parameter :: os(2) = [outer(pc(two)), outer(pc(three))]
type(top), parameter :: t = top(outer(pc(three)))
end module

module procedure_pointer_40_use
use procedure_pointer_40_mod, only: o, pc
implicit none
contains
    integer function from_only()
        type(pc) :: v
        v = o%inner
        from_only = v%f()
    end function
end module

program procedure_pointer_40
use procedure_pointer_40_mod
use procedure_pointer_40_use
implicit none
type(pc) :: v
v = o%inner
if (v%f() /= 1) error stop
if (from_only() /= 1) error stop
if (call_it(o%inner) /= 1) error stop
associate (x => o%inner)
    v = x
end associate
if (v%f() /= 1) error stop
v = os(1)%inner
if (v%f() /= 2) error stop
v = os(2)%inner
if (v%f() /= 3) error stop
if (call_it(os(1)%inner) /= 2) error stop
v = t%mid%inner
if (v%f() /= 3) error stop
if (call_it(t%mid%inner) /= 3) error stop
print *, "ok"
contains
    integer function call_it(c)
        type(pc), intent(in) :: c
        call_it = c%f()
    end function
end program
