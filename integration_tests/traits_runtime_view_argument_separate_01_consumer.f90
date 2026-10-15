! Compiled against the contracts only, like MyType of the report: a model
! whose allocated coefficients update its allocated estimates, and procedures
! that forward a read-only receiver view, a mutable argument view and
! intent(in) pointers to the messages.
module traits_runtime_view_argument_separate_01_consumer_m
    use traits_runtime_view_argument_separate_01_estimates_m, only: IEstimates
    use traits_runtime_view_argument_separate_01_contracts_m, only: ICoeffs
    implicit none
    private
    public :: Model, blended, update_through

    type :: Model
        class(ICoeffs), allocatable :: coeffs
        class(IEstimates), allocatable :: estimates
    contains
        procedure :: coefficients
    end type Model
contains
    subroutine coefficients(self)
        class(Model), intent(inout) :: self
        call self%coeffs%update(self%estimates)
    end subroutine coefficients

    integer function blended(coeffs, estimates, weight)
        class(ICoeffs), intent(in) :: coeffs
        class(IEstimates), intent(inout) :: estimates
        integer, intent(in) :: weight
        blended = coeffs%blend(weight=weight, estimates=estimates)
    end function blended

    subroutine update_through(coeffs, estimates)
        class(ICoeffs), pointer, intent(in) :: coeffs
        class(IEstimates), pointer, intent(in) :: estimates
        call coeffs%update(estimates)
    end subroutine update_through
end module traits_runtime_view_argument_separate_01_consumer_m
