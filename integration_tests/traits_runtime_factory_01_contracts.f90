module traits_runtime_factory_01_contracts_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    integer :: finals(0:1) = 0, sums(0:1) = 0, requests = 0
    interface
        function make_value(choice) result(object)
            import IValue
            integer, intent(in) :: choice
            class(IValue), allocatable :: object
        end function
    end interface
end module
