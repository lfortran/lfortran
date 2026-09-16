! An external with a derived-type argument is passed to a procedure defined
! later in this file (#12827).
module implicit_interface_75_mod
type :: t
    real(8) :: v
end type
end module

program implicit_interface_75
use implicit_interface_75_mod, only: t
external set_three
type(t) :: z
z%v = 0
call apply(set_three, z)
if (abs(z%v - 3) > 1d-12) error stop
print *, "ok"
end program

subroutine apply(f, z)
use implicit_interface_75_mod, only: t
external f
type(t) :: z
call f(z)
end subroutine

subroutine set_three(x)
use implicit_interface_75_mod, only: t
type(t) :: x
x%v = 3
end subroutine
