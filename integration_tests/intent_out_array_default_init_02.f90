! The default of an `intent(out)` dummy array's components must be usable where
! the compiler places it: a constant named in the type's own module is not in
! scope in the procedure, and a structure constructor is no longer a node the
! backends lower by the time it is emitted. Kept free of intrinsic calls, so
! that it can take the `c` label once #13307 is fixed.
module intent_out_array_default_init_02_types
implicit none

integer, parameter :: kdef = 5

type :: cfg_t
    integer :: a = 1
    real :: b = 2.0
end type cfg_t

type :: p_t
    integer :: k = kdef
    type(cfg_t) :: cfg = cfg_t(7, 8.0)
end type p_t

end module intent_out_array_default_init_02_types

module intent_out_array_default_init_02_other
use intent_out_array_default_init_02_types
implicit none

contains

    subroutine reset(a)
        type(p_t), intent(out) :: a(:)
        if (a(1)%k /= 5) error stop 1
        if (a(2)%k /= 5) error stop 2
        if (a(1)%cfg%a /= 7) error stop 3
        if (a(1)%cfg%b /= 8.0) error stop 4
        if (a(2)%cfg%a /= 7) error stop 5
        if (a(2)%cfg%b /= 8.0) error stop 6
    end subroutine reset

end module intent_out_array_default_init_02_other

program intent_out_array_default_init_02
use intent_out_array_default_init_02_types
use intent_out_array_default_init_02_other
implicit none

type(p_t) :: q(2)

q(1)%k = 1
q(2)%k = 1
q(1)%cfg%a = 99
q(1)%cfg%b = 99.0
q(2)%cfg%a = 99
q(2)%cfg%b = 99.0
call reset(q)

print *, "ok"
end program intent_out_array_default_init_02
