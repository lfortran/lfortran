module save_32_mod
implicit none
save
real :: module_constant
parameter (module_constant = 6.5)
real :: module_total = 0.0
end module

module save_32_shadow_mod
implicit none
real :: shadowed
save shadowed
contains
    real function shadow_constant()
        real :: shadowed
        parameter (shadowed = 8.5)
        shadow_constant = shadowed
    end function
end module

program save_32
use save_32_mod, only: module_constant, module_total
use save_32_shadow_mod, only: shadow_constant
implicit none

! A bare SAVE in a module does not apply to its named constants
if (abs(module_constant - 6.5) > 1e-6) error stop 1
module_total = module_total + module_constant
if (abs(module_total - 6.5) > 1e-6) error stop 2

! A named constant may hide a SAVE variable of the host module
if (abs(shadow_constant() - 8.5) > 1e-6) error stop 3

call bare_save(1)
call bare_save(2)
call save_other(1)
call save_other(2)
call data_other()
print *, "ok"

contains

    ! A bare SAVE does not apply to named constants
    subroutine bare_save(expected)
        integer, intent(in) :: expected
        save
        real :: limit
        integer :: calls = 0
        parameter (limit = 2.5)
        calls = calls + 1
        if (calls /= expected) error stop 4
        if (abs(limit - 2.5) > 1e-6) error stop 5
    end subroutine

    ! SAVE on one entity and PARAMETER on another
    subroutine save_other(expected)
        integer, intent(in) :: expected
        real :: step, total
        save total
        parameter (step = 4.5)
        if (expected == 1) total = 0.0
        total = total + step
        if (abs(total - step * expected) > 1e-6) error stop 6
    end subroutine

    ! An initialization in DATA (implicit SAVE) on one entity
    ! and PARAMETER on another
    subroutine data_other()
        real :: a, b
        data a /1.0/
        parameter (b = 7.5)
        if (abs(a - 1.0) > 1e-6) error stop 7
        if (abs(b - 7.5) > 1e-6) error stop 8
    end subroutine

end program
