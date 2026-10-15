! The first provider, with passed objects in the first position: Coeffs steps
! the estimates it is given `stride` times per update, and Tally counts by one.
module traits_runtime_view_argument_separate_01_a_m
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    use traits_runtime_view_argument_separate_01_estimates_m, only: IEstimates
    use traits_runtime_view_argument_separate_01_contracts_m, only: ICoeffs
    implicit none
    type(c_ptr) :: a_seen = c_null_ptr

    type :: Coeffs
        integer :: stride = 1
        integer :: updates = 0
    end type Coeffs
    implements ICoeffs :: Coeffs
        procedure, pass :: update => a_update
        procedure, pass :: blend => a_blend
    end implements

    type :: Tally
        integer :: n = 0
    end type Tally
    implements IEstimates :: Tally
        procedure, pass :: some_method => a_step
        procedure, pass :: total => a_total
    end implements
contains
    subroutine a_update(self, estimates)
        class(Coeffs), intent(inout) :: self
        class(IEstimates), intent(inout) :: estimates
        integer :: i
        self%updates = self%updates + 1
        do i = 1, self%stride
            call estimates%some_method()
        end do
    end subroutine a_update

    integer function a_blend(self, estimates, weight)
        class(Coeffs), intent(in) :: self
        class(IEstimates), intent(inout) :: estimates
        integer, intent(in) :: weight
        call estimates%some_method()
        a_blend = weight * estimates%total() + self%updates
    end function a_blend

    subroutine a_step(self)
        class(Tally), target, intent(inout) :: self
        a_seen = c_loc(self%n)
        self%n = self%n + 1
    end subroutine a_step

    integer function a_total(self)
        class(Tally), intent(in) :: self
        a_total = self%n
    end function a_total
end module traits_runtime_view_argument_separate_01_a_m
