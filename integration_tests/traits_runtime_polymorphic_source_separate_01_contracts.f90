! The contract of traits_runtime_polymorphic_source_separate_01.
module traits_runtime_polymorphic_source_separate_01_contracts_m
    implicit none
    abstract interface :: IStep
        subroutine step(self, y)
            type(itself), intent(inout), pass :: self
            real(8), intent(inout) :: y
        end subroutine
        integer function count(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
end module
