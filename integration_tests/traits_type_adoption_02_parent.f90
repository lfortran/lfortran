module traits_type_adoption_02_parent
    use traits_type_adoption_02_contracts, only: IAll
    implicit none
    private
    public :: Parent, Middle
    type, abstract, implements(IAll) :: Parent
        integer :: seed = 5
    contains
        procedure :: value => parent_value
        procedure(legacy_interface), deferred :: legacy
    end type
    type, abstract, extends(Parent) :: Middle
        real(8) :: padding(3) = [1.0_8, 2.0_8, 3.0_8]
    end type
    abstract interface
        integer function legacy_interface(self)
            import Parent
            class(Parent), intent(in) :: self
        end function
    end interface
contains
    integer function parent_value(self) result(n)
        class(Parent), intent(in) :: self
        n = self%seed
    end function
end module
