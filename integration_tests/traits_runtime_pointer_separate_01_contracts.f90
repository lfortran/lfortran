module traits_runtime_pointer_separate_01_contracts
    implicit none
    abstract interface :: IValue
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine read_into(self, result)
            type(itself), intent(in), pass :: self
            integer, intent(out) :: result
        end subroutine
    end interface
end module
