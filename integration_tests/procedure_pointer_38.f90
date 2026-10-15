module procedure_pointer_38_sizes
implicit none
contains
    pure integer function extent()
        extent = 2
    end function
end module

! The interface of a procedure pointer component or a dummy procedure can be
! a procedure defined later whose dummy has a bound using a function that
! only that procedure accesses, as can one of a procedure defined earlier.
module procedure_pointer_38_mod
implicit none
type :: mt
    procedure(later), pointer, nopass :: pf => null()
end type
contains
    integer function earlier(x)
        use procedure_pointer_38_sizes, only: extent
        integer, intent(in) :: x(extent())
        earlier = sum(x) + 1
    end function
    integer function invoke_local()
        type :: lt
            procedure(later), pointer, nopass :: pf => null()
        end type
        type :: et
            procedure(earlier), pointer, nopass :: pf => null()
        end type
        type(lt) :: a
        type(et) :: b
        a = lt(later)
        b = et(earlier)
        invoke_local = a%pf([40, 2]) + b%pf([1, 2])
    end function
    integer function invoke_dummy(f)
        procedure(later) :: f
        invoke_dummy = f([40, 2])
    end function
    integer function invoke_internal()
        invoke_internal = apply(inner)
    contains
        integer function apply(f)
            procedure(inner) :: f
            apply = f([3, 4])
        end function
        integer function inner(x)
            use procedure_pointer_38_sizes, only: extent
            integer, intent(in) :: x(extent())
            inner = product(x)
        end function
    end function
    integer function later(x)
        use procedure_pointer_38_sizes, only: extent
        integer, intent(in) :: x(extent())
        later = sum(x)
    end function
end module

program procedure_pointer_38
use procedure_pointer_38_mod
implicit none
type(mt) :: m
m%pf => later
if (m%pf([40, 2]) /= 42) error stop
if (invoke_local() /= 46) error stop
if (invoke_dummy(later) /= 42) error stop
if (invoke_internal() /= 12) error stop
print *, "ok"
end program
