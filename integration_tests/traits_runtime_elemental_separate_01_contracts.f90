! The trait of traits_runtime_elemental_separate_01: elemental messages, two
! functions and a subroutine.
module traits_runtime_elemental_separate_01_contracts_m
    implicit none
    abstract interface :: IFloors
        elemental function pfloor(self, p) result(res)
            type(itself), pass, intent(in) :: self
            real(8), intent(in) :: p
            real(8) :: res
        end function pfloor
        elemental function blend(self, a, b) result(res)
            type(itself), pass, intent(in) :: self
            real(8), intent(in) :: a, b
            real(8) :: res
        end function blend
        elemental subroutine raise(self, x)
            type(itself), pass, intent(in) :: self
            real(8), intent(inout) :: x
        end subroutine raise
    end interface
end module traits_runtime_elemental_separate_01_contracts_m
