! Explicit-shape array arguments of runtime messages. Each procedure whose
! name starts with from_ has exactly one invalid actual argument: through a
! view, an explicit-shape intent(inout) or intent(out) dummy needs a definable
! actual, as in an ordinary call, even when the view itself is read-only.
! Assumed-size dummies and explicit-shape dummies of generic messages remain
! outside the runtime slot surface, so each variable whose name ends with
! _argument is invalid.
module traits_explicit_shape_negative_m
    implicit none
    abstract interface :: INumeric
        integer | real(8)
    end interface INumeric
    abstract interface :: IStep
        subroutine step(n, y, yp)
            integer, intent(in) :: n
            real(8), intent(inout) :: y(n)
            real(8), intent(out) :: yp(n)
        end subroutine step
    end interface
    abstract interface :: IAssumedSize
        subroutine fill(self, y)
            type(itself), pass, intent(in) :: self
            real(8) :: y(*)
        end subroutine fill
    end interface
    abstract interface :: IGenericShape
        function total{INumeric :: T}(self, n, x) result(s)
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            type(T), intent(in) :: x(n)
            type(T) :: s
        end function total
    end interface
    class(IAssumedSize), allocatable :: assumed_size_argument
    class(IGenericShape), allocatable :: generic_shape_argument
contains
    subroutine from_read_only_array(stepper, a)
        class(IStep), intent(in) :: stepper
        real(8), intent(in) :: a(3)
        real(8) :: yp(3)
        call stepper%step(3, a, yp)
    end subroutine from_read_only_array

    subroutine from_array_constructor(stepper)
        class(IStep), intent(in) :: stepper
        real(8) :: y(3)
        y = 1
        call stepper%step(3, y, [1d0, 2d0, 3d0])
    end subroutine from_array_constructor
end module traits_explicit_shape_negative_m
