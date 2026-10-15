! The outer trait of traits_runtime_view_argument_separate_01: its messages
! take an imported IEstimates view as an argument that is not their receiver,
! update after its intent(inout) receiver and blend before its read-only one.
module traits_runtime_view_argument_separate_01_contracts_m
    use traits_runtime_view_argument_separate_01_estimates_m, only: IEstimates
    implicit none
    abstract interface :: ICoeffs
        subroutine update(self, estimates)
            type(itself), pass, intent(inout) :: self
            class(IEstimates), intent(inout) :: estimates
        end subroutine update
        integer function blend(estimates, weight, self)
            class(IEstimates), intent(inout) :: estimates
            integer, intent(in) :: weight
            type(itself), pass, intent(in) :: self
        end function blend
    end interface
end module traits_runtime_view_argument_separate_01_contracts_m
