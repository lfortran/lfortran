module traits_runtime_polymorphic_source_separate_02_contracts_m
    implicit none
    abstract interface :: IRhs
        subroutine eval(self, y, yp)
            type(itself), pass, intent(in) :: self
            real(8), intent(in) :: y(:)
            real(8), intent(out) :: yp(:)
        end subroutine
        real(8) function stiffness(self)
            type(itself), pass, intent(in) :: self
        end function
        subroutine tune(self, k)
            type(itself), pass, intent(inout) :: self
            real(8), intent(in) :: k
        end subroutine
    end interface
end module
