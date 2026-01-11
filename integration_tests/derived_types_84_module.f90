module derived_types_84_module
    implicit none

    type :: Person
        integer :: age
    end type Person

contains

subroutine get_age(p)
    type(Person), intent(inout) :: p
    p%age = 30
end subroutine get_age

end module derived_types_84_module


