module traits_runtime_owning_separate_01_contracts_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
        subroutine read_value(r)
            integer, intent(out) :: r
        end subroutine
    end interface
end module
