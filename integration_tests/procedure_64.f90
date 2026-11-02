module procedure_64_m
use iso_c_binding, only: c_int
implicit none
abstract interface
    integer function scalar_fn(x)
        integer, intent(in) :: x
    end function
    integer function apply_fn(f, x)
        import scalar_fn
        procedure(scalar_fn) :: f
        integer, intent(in) :: x
    end function
    integer(c_int) function c_scalar_fn(x) bind(c)
        import c_int
        integer(c_int), value :: x
    end function
    integer(c_int) function c_apply_fn(f, x) bind(c)
        import c_int, c_scalar_fn
        procedure(c_scalar_fn) :: f
        integer(c_int), value :: x
    end function
end interface
contains
integer function plus_one(x)
    integer, intent(in) :: x
    plus_one = x + 1
end function
integer function apply(f, x)
    procedure(scalar_fn) :: f
    integer, intent(in) :: x
    apply = f(x)
end function
integer function invoke(f, g)
    procedure(apply_fn) :: f
    procedure(scalar_fn) :: g
    invoke = f(g, 12)
end function
function select_apply() result(f)
    procedure(apply_fn), pointer :: f
    f => apply
end function
integer(c_int) function c_plus_one(x) bind(c)
    integer(c_int), value :: x
    c_plus_one = x + 1
end function
integer(c_int) function c_apply(f, x) bind(c)
    procedure(c_scalar_fn) :: f
    integer(c_int), value :: x
    c_apply = f(x)
end function
integer(c_int) function invoke_c(f, g)
    procedure(c_apply_fn) :: f
    procedure(c_scalar_fn) :: g
    invoke_c = f(g, 12_c_int)
end function
end module

program procedure_64
use procedure_64_m
implicit none
integer :: state
procedure(apply_fn), pointer :: selected
state = 30
if (invoke(apply, plus_one) /= 13) error stop 1
if (invoke(apply, inner) /= 42) error stop 2
if (invoke(captured_apply, inner) /= 72) error stop 3
selected => select_apply()
if (selected(plus_one, 12) /= 13) error stop 4
if (invoke_c(c_apply, c_plus_one) /= 13) error stop 5
contains
integer function inner(x)
    integer, intent(in) :: x
    inner = x + state
end function
integer function captured_apply(f, x)
    procedure(scalar_fn) :: f
    integer, intent(in) :: x
    captured_apply = f(x) + state
end function
end program
