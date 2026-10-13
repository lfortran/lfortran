module traits_intrinsic_02_contracts_m
    implicit none

    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function value
    end interface IValue
end module traits_intrinsic_02_contracts_m
