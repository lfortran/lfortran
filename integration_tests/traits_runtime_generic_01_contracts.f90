module traits_runtime_generic_01_contracts_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue
    abstract interface :: IAlgorithm
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function apply
    end interface IAlgorithm
    interface
        subroutine make_algorithm(choice, object)
            import :: IAlgorithm
            integer, intent(in) :: choice
            class(IAlgorithm), allocatable, intent(out) :: object
        end subroutine make_algorithm
    end interface
end module traits_runtime_generic_01_contracts_m
