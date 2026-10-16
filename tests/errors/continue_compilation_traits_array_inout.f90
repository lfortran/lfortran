! Assumed-shape array arguments of runtime trait messages follow the ordinary
! rules of their INTENT. Each procedure whose name starts with from_ has
! exactly one invalid actual argument: an intent(inout) or intent(out) array
! argument of a message called through a view needs a definable actual, also
! in a function message. Mismatched implements scale with an intent(in) array.
module traits_array_inout_negative_m
    implicit none
    real(8), parameter :: fixed(3) = [1, 2, 3]
    abstract interface :: IEstimates
    end interface
    abstract interface :: ICoeffs
        subroutine update(self, estimates, beta)
            type(itself), pass, intent(inout) :: self
            class(IEstimates), intent(inout) :: estimates
            real(8), intent(inout) :: beta(:)
        end subroutine update
        subroutine fill(self, x)
            type(itself), pass, intent(in) :: self
            real(8), intent(out) :: x(0:)
        end subroutine fill
        integer function clear(self, k)
            type(itself), pass, intent(in) :: self
            integer, intent(inout) :: k(:)
        end function clear
    end interface
    abstract interface :: IScale
        subroutine scale(self, x)
            type(itself), pass, intent(in) :: self
            real(8), intent(inout) :: x(:)
        end subroutine scale
    end interface
    type :: Prior
    end type
    implements IEstimates :: Prior
    end implements
    type :: Coeffs
    end type
    implements ICoeffs :: Coeffs
        procedure, pass :: update => coeffs_update
        procedure, pass :: fill => coeffs_fill
        procedure, pass :: clear => coeffs_clear
    end implements
    type :: Mismatched
    end type
    implements IScale :: Mismatched
        procedure, pass :: scale => mismatched_scale
    end implements
contains
    subroutine coeffs_update(self, estimates, beta)
        class(Coeffs), intent(inout) :: self
        class(IEstimates), intent(inout) :: estimates
        real(8), intent(inout) :: beta(:)
        beta = 2*beta
    end subroutine
    subroutine coeffs_fill(self, x)
        class(Coeffs), intent(in) :: self
        real(8), intent(out) :: x(0:)
        x = 0
    end subroutine
    integer function coeffs_clear(self, k)
        class(Coeffs), intent(in) :: self
        integer, intent(inout) :: k(:)
        coeffs_clear = size(k)
        k = 0
    end function
    subroutine mismatched_scale(self, x)
        class(Mismatched), intent(in) :: self
        real(8), intent(in) :: x(:)
    end subroutine
    function values() result(r)
        real(8) :: r(3)
        r = 1
    end function

    subroutine from_expression(c, e, b)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(inout) :: e
        real(8), intent(inout) :: b(:)
        call c%update(e, b + 1)
    end subroutine
    subroutine from_constant(c, e)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(inout) :: e
        call c%update(e, fixed)
    end subroutine
    subroutine from_constructor(c, e)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(inout) :: e
        call c%update(e, [1.0d0, 2.0d0])
    end subroutine
    subroutine from_function(c, e)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(inout) :: e
        call c%update(estimates=e, beta=values())
    end subroutine
    subroutine from_readonly(c, e, b)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(inout) :: e
        real(8), intent(in) :: b(:)
        call c%update(e, b)
    end subroutine
    subroutine from_readonly_out(c, b)
        class(ICoeffs), intent(in) :: c
        real(8), intent(in) :: b(:)
        call c%fill(b)
    end subroutine
    subroutine from_function_message(c, k)
        class(ICoeffs), intent(in) :: c
        integer, intent(inout) :: k(:)
        integer :: n
        n = c%clear(2*k)
    end subroutine
end module
