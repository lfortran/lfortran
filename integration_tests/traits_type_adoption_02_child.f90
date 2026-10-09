module traits_type_adoption_02_child
    use traits_type_adoption_02_parent, only: Middle
    implicit none
    private
    public :: Child
    type, extends(Middle) :: Child
        integer :: bonus = 11
    contains
        procedure, pass(self) :: extra => child_extra
        procedure :: measure => child_measure
        procedure :: legacy => child_legacy
    end type
contains
    integer function child_extra(n, self) result(v)
        integer, intent(in) :: n
        class(Child), intent(in) :: self
        v = n + self%seed + self%bonus
    end function
    real function child_measure(self) result(v)
        class(Child), intent(in) :: self
        v = real(self%seed + self%bonus)
    end function
    integer function child_legacy(self) result(v)
        class(Child), intent(in) :: self
        v = self%seed - 1
    end function
end module
