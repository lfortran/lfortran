module generic_interface_dummy_procedure_07_mod_a
implicit none
interface g
    module procedure gi
end interface
contains
integer function gi(x)
    integer, intent(in) :: x
    gi = x + 1
end function
end module

module generic_interface_dummy_procedure_07_mod_b
use generic_interface_dummy_procedure_07_mod_a, only: g
implicit none
integer :: gi = 7
contains
subroutine p()
    interface g
        module procedure dbl
    end interface
    if (g(1) /= 2) error stop 1
    if (abs(g(1.5) - 3.0) > 1e-6) error stop 2
    print *, g(1), g(1.5), gi
    if (gi /= 7) error stop 3
    gi = gi + 1
end subroutine
real function dbl(x)
    real, intent(in) :: x
    dbl = 2*x
end function
end module

program generic_interface_dummy_procedure_07
use generic_interface_dummy_procedure_07_mod_b
implicit none
call p()
if (gi /= 8) error stop 4
end program
