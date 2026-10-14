module traits_runtime_separate_01_contracts_m
    implicit none
    abstract interface :: IValue
        function value(self) result(r)
            type(itself), intent(in), pass :: self
            integer :: r
        end function value
        function affine(self, left, right) result(r)
            type(itself), intent(in), pass :: self
            integer, intent(in) :: left, right
            integer :: r
        end function affine
        function tag(code) result(r)
            integer, intent(in) :: code
            integer :: r
        end function tag
        subroutine message(self, left, right, result)
            type(itself), intent(in), pass :: self
            integer, intent(in) :: left, right
            integer, intent(out) :: result
        end subroutine message
    end interface IValue
end module traits_runtime_separate_01_contracts_m
