module procedure_pointer_39_sizes
implicit none
contains
    pure integer function extent()
        extent = 2
    end function
end module

! The interface of a procedure pointer or dummy procedure can be a procedure
! whose dummy procedure has the interface of a procedure defined later, and
! whose array dummy has a bound using a function that only it accesses. The
! interface is complete whatever the names or order of the declarations.
module procedure_pointer_39_mod
implicit none
type :: aa
    procedure(zapply), pointer, nopass :: pf => null()
end type
type :: zz
    procedure(zapply), pointer, nopass :: pf => null()
end type
type :: mm
    procedure(bapply), pointer, nopass :: pf => null()
end type
type :: bb
    procedure(mid), pointer, nopass :: pf => null()
end type
contains
    integer function zapply(worker, x)
        use procedure_pointer_39_sizes, only: extent
        procedure(zworker) :: worker
        integer, intent(inout) :: x(extent())
        call worker(x)
        zapply = sum(x)
    end function
    integer function bapply(worker, x)
        use procedure_pointer_39_sizes, only: extent
        procedure(aworker) :: worker
        integer, intent(inout) :: x(extent())
        call worker(x)
        bapply = sum(x)
    end function
    integer function mid(apply, x)
        use procedure_pointer_39_sizes, only: extent
        procedure(zapply) :: apply
        integer, intent(inout) :: x(extent())
        mid = apply(zworker, x)
    end function
    integer function user(x)
        integer, intent(inout) :: x(2)
        procedure(zapply), pointer :: q
        q => zapply
        user = q(zworker, x)
    end function
    subroutine aworker(x)
        integer, intent(inout) :: x(2)
        x = x + 2
    end subroutine
    subroutine zworker(x)
        integer, intent(inout) :: x(2)
        x = x + 1
    end subroutine
end module

program procedure_pointer_39
use procedure_pointer_39_mod
implicit none
type(aa) :: a
type(zz) :: z
type(mm) :: m
type(bb) :: b
integer :: v(2)
a%pf => zapply
v = [39, 1]
if (a%pf(zworker, v) /= 42) error stop
if (any(v /= [40, 2])) error stop
z%pf => zapply
v = [39, 1]
if (z%pf(zworker, v) /= 42) error stop
if (any(v /= [40, 2])) error stop
m%pf => bapply
v = [38, 0]
if (m%pf(aworker, v) /= 42) error stop
if (any(v /= [40, 2])) error stop
b%pf => mid
v = [39, 1]
if (b%pf(zapply, v) /= 42) error stop
if (any(v /= [40, 2])) error stop
v = [39, 1]
if (user(v) /= 42) error stop
if (any(v /= [40, 2])) error stop
print *, "ok"
end program
