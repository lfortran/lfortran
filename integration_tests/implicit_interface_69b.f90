module implicit_interface_69_mod
implicit none
type :: t69
    real(8) :: v
end type
end module

subroutine apply_t69(f, z)
use implicit_interface_69_mod, only: t69
external f
type(t69) :: z
call f(z)
end subroutine
