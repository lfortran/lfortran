module implicit_interface_71_mod
implicit none
type :: t71
    real(8) :: v
end type
end module

subroutine apply_t71(f)
use implicit_interface_71_mod, only: t71
external f
type(t71) :: z
z%v = 0
call f(z)
if (abs(z%v - 3.0d0) > 1.0d-12) error stop
end subroutine
