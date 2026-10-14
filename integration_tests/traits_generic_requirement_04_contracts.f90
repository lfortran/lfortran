! Contracts for traits_generic_requirement_04/05, compiled separately.
module traits_generic_requirement_04_contracts
    implicit none

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

    abstract interface :: IPair
        function pair{IValue :: T, IValue :: U}(x, y) result(r)
            type(T), intent(in) :: x
            type(U), intent(in) :: y
            integer :: r
        end function pair
    end interface IPair

end module traits_generic_requirement_04_contracts
