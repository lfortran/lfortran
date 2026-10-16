! Providers for traits_runtime_array_inout_01 (LFortran traits extension).
! Prior and Tally implement the empty marker trait IEstimates of the
! reported module; Coeffs and Damped implement its ICoeffs%update, which
! modifies the receiver, the borrowed estimates (through SELECT TYPE, as
! IEstimates has no messages) and every element of its real(8),
! intent(inout) argument beta(:). Nothing here refers to the module's beta.
module traits_runtime_array_inout_01_providers
    use mytype_mod, only: IEstimates, ICoeffs
    implicit none
    private
    public :: Prior, Tally, Coeffs, Damped

    type :: Prior
        integer :: seen = 0
    end type Prior
    implements IEstimates :: Prior
    end implements

    type :: Tally
        integer :: seen = 0
        real(8) :: total = 0
    end type Tally
    implements IEstimates :: Tally
    end implements

    type :: Coeffs
        integer :: updates = 0
    end type Coeffs
    implements ICoeffs :: Coeffs
        procedure, pass :: update => coeffs_update
    end implements

    type :: Damped
        real(8) :: factor = 0.5d0
        integer :: updates = 0
    end type Damped
    implements ICoeffs :: Damped
        procedure, pass :: update => damped_update
    end implements

contains

    ! beta(i) <- 2 beta(i) + i
    subroutine coeffs_update(self, estimates, beta)
        class(Coeffs), intent(inout) :: self
        class(IEstimates), intent(inout) :: estimates
        real(8), intent(inout) :: beta(:)
        integer :: i
        self%updates = self%updates + 1
        call observe(estimates, beta)
        do i = 1, size(beta)
            beta(i) = 2*beta(i) + i
        end do
    end subroutine coeffs_update

    ! beta <- factor beta - updates
    subroutine damped_update(self, estimates, beta)
        class(Damped), intent(inout) :: self
        class(IEstimates), intent(inout) :: estimates
        real(8), intent(inout) :: beta(:)
        self%updates = self%updates + 1
        call observe(estimates, beta)
        beta = self%factor*beta - self%updates
    end subroutine damped_update

    ! The borrowed estimates are the caller's object, of its dynamic type, and
    ! beta has the shape of the module's array.
    subroutine observe(estimates, beta)
        class(IEstimates), intent(inout) :: estimates
        real(8), intent(in) :: beta(:)
        if (size(beta) /= 12 .or. lbound(beta, 1) /= 1) error stop 1
        if (.not. is_contiguous(beta)) error stop 2
        select type (e => estimates)
        type is (Prior)
            e%seen = e%seen + 1
        type is (Tally)
            e%seen = e%seen + 1
            e%total = e%total + sum(beta)
        class default
            error stop 3
        end select
    end subroutine observe
end module traits_runtime_array_inout_01_providers
