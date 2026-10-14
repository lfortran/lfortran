! Contracts of traits_generic_requirement_03, compiled separately.
module traits_generic_requirement_03_contracts
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, IValue, IApply

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    abstract interface :: IValue
        function value(self) result(r)
            type(itself), intent(in), pass :: self
            integer :: r
        end function value
    end interface IValue

    abstract interface :: IApply
        function apply{IValue :: T}(self, object) result(r)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: object
            integer :: r
        end function apply
    end interface IApply
end module traits_generic_requirement_03_contracts
