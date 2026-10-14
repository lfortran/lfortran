module traits_runtime_owning_separate_01_contracts_m
    implicit none
    abstract interface :: IValue
        function value(self) result(r)
            type(itself), intent(in), pass :: self
            integer :: r
        end function
        subroutine read_value(self, r)
            type(itself), intent(in), pass :: self
            integer, intent(out) :: r
        end subroutine
    end interface
end module
