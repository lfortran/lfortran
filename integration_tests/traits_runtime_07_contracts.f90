module traits_runtime_07_contracts_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue
end module traits_runtime_07_contracts_m
