module traits_runtime_separate_01_contracts_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
        function affine(left, right) result(r)
            integer, intent(in) :: left, right
            integer :: r
        end function affine
        function tag(code) result(r)
            integer, intent(in) :: code
            integer :: r
        end function tag
        subroutine message(left, right, result)
            integer, intent(in) :: left, right
            integer, intent(out) :: result
        end subroutine message
    end interface IValue
end module traits_runtime_separate_01_contracts_m
