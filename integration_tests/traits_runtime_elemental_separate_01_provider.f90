! The provider: Floors keeps its floor and weight private.
module traits_runtime_elemental_separate_01_provider_m
    use traits_runtime_elemental_separate_01_contracts_m, only: IFloors
    implicit none
    private
    public :: Floors, make_floors
    type, implements(IFloors) :: Floors
        private
        real(8) :: smallp = 1.d-50
        real(8) :: weight = 1
    contains
        procedure :: pfloor => floors_pfloor
        procedure :: blend => floors_blend
        procedure :: raise => floors_raise
    end type Floors
contains
    function make_floors(smallp, weight) result(f)
        real(8), intent(in) :: smallp, weight
        type(Floors) :: f
        f%smallp = smallp
        f%weight = weight
    end function make_floors

    elemental function floors_pfloor(self, p) result(res)
        class(Floors), intent(in) :: self
        real(8), intent(in) :: p
        real(8) :: res
        res = max(p, self%smallp)
    end function floors_pfloor

    elemental function floors_blend(self, a, b) result(res)
        class(Floors), intent(in) :: self
        real(8), intent(in) :: a, b
        real(8) :: res
        res = self%weight * a + b
    end function floors_blend

    elemental subroutine floors_raise(self, x)
        class(Floors), intent(in) :: self
        real(8), intent(inout) :: x
        x = max(x, self%smallp)
    end subroutine floors_raise
end module traits_runtime_elemental_separate_01_provider_m
