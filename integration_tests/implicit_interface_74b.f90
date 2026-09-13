module implicit_interface_74_mod
implicit none
type :: t74
    real(8) :: v
end type
end module

subroutine apply_t74(f, z)
use implicit_interface_74_mod, only: t74
external f
type(t74) :: z
call f(z)
end subroutine
