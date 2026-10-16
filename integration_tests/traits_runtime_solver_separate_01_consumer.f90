! Compiled against the contracts only: a driver that integrates to each
! output time through a borrowed mutable solver and a read-only right-hand
! side, and the derivative of a right-hand side.
module traits_runtime_solver_separate_01_consumer_m
    use traits_runtime_solver_separate_01_contracts_m, only: IRhsFunc, ISolver
    implicit none
    private
    public :: drive, derivative
contains
    ! Integrates y to touts(1), touts(2), ... and returns the steps taken;
    ! y(:) may be noncontiguous.
    integer function drive(solver, rhs, y, t, touts, iflag) result(steps)
        class(ISolver), intent(inout) :: solver
        class(IRhsFunc), intent(in) :: rhs
        real(8), intent(inout) :: y(:), t
        real(8), intent(in) :: touts(:)
        integer, intent(out) :: iflag
        integer :: i, neqn
        real(8) :: tout, relerr, abserr
        neqn = size(y)
        relerr = 1d-8
        abserr = 1d-8
        iflag = 1
        steps = 0
        do i = 1, size(touts)
            tout = touts(i)
            call solver%integrate(rhs, neqn, y, t, tout, relerr, abserr, iflag)
            if (iflag /= 2) return
            steps = steps + 1
        end do
    end function drive

    subroutine derivative(rhs, x, y, yp)
        class(IRhsFunc), intent(in) :: rhs
        real(8), intent(in) :: x, y(:)
        real(8), intent(out) :: yp(:)
        call rhs%sfunc(x, y, yp)
    end subroutine derivative
end module traits_runtime_solver_separate_01_consumer_m
