! The providers, compiled after the consumer: two right-hand sides bound with
! nopass and a forward Euler solver that counts its steps.
module traits_runtime_solver_separate_01_provider_m
    use traits_runtime_solver_separate_01_contracts_m, only: IRhsFunc, ISolver
    implicit none
    private
    public :: Decay, Ramp, Euler

    type :: Decay
    end type Decay
    implements IRhsFunc :: Decay
        procedure, nopass :: sfunc => decay_sfunc
    end implements

    type :: Ramp
    end type Ramp
    implements IRhsFunc :: Ramp
        procedure, nopass :: sfunc => ramp_sfunc
    end implements

    type :: Euler
        integer :: steps = 0
    end type Euler
    implements ISolver :: Euler
        procedure, pass :: integrate => euler_integrate
    end implements
contains
    subroutine decay_sfunc(x, y, yp)
        real(8), intent(in)  :: x
        real(8), intent(in)  :: y(:)
        real(8), intent(out) :: yp(:)
        yp = -2 * y
    end subroutine decay_sfunc

    subroutine ramp_sfunc(x, y, yp)
        real(8), intent(in)  :: x
        real(8), intent(in)  :: y(:)
        real(8), intent(out) :: yp(:)
        integer :: i
        do i = 1, size(yp)
            yp(i) = i * x
        end do
    end subroutine ramp_sfunc

    subroutine euler_integrate(self, rhs, neqn, y, t, tout, relerr, abserr, iflag)
        class(Euler), intent(inout) :: self
        class(IRhsFunc), intent(in) :: rhs
        integer :: neqn, iflag
        real(8) :: t, tout, relerr, abserr
        real(8) :: y(neqn)
        real(8) :: yp(neqn)
        if (relerr < 0 .or. abserr < 0 .or. tout < t) then
            iflag = 6
            return
        end if
        call rhs%sfunc(t, y, yp)
        y = y + (tout - t) * yp
        t = tout
        self%steps = self%steps + 1
        iflag = 2
    end subroutine euler_integrate
end module traits_runtime_solver_separate_01_provider_m
