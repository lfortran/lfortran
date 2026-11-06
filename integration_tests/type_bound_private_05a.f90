module type_bound_private_05_base_m
    ! A private specific binding of a public type-bound generic.
    implicit none
    private
    public :: base_t, run

    type :: base_t
        integer :: calls = 0
    contains
        generic :: resolve => resolve_impl
        procedure, private :: resolve_impl
    end type base_t

contains

    subroutine resolve_impl(self)
        class(base_t), intent(inout) :: self
        self%calls = self%calls + 1
    end subroutine resolve_impl

    subroutine run(obj)
        class(base_t), intent(inout) :: obj
        call obj%resolve()
        call obj%resolve_impl()
    end subroutine run

end module type_bound_private_05_base_m
