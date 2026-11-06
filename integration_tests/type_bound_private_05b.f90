module type_bound_private_05_child_m
    ! base_t's private binding resolve_impl is not accessible here, so the
    ! binding of the same name below does not override it (F2023 7.5.7.3);
    ! the inherited generic resolve keeps base_t's specific.
    use type_bound_private_05_base_m, only: base_t
    implicit none
    private
    public :: child_t

    type, extends(base_t) :: child_t
        integer :: child_calls = 0
    contains
        procedure, private :: resolve_impl => child_resolve
    end type child_t

contains

    subroutine child_resolve(self)
        class(child_t), intent(inout) :: self
        self%child_calls = self%child_calls + 1
    end subroutine child_resolve

end module type_bound_private_05_child_m
