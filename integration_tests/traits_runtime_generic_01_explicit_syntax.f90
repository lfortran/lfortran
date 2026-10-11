! Parser-stage companion, deliberately excluded from the first ABI executable.
module traits_runtime_generic_01_explicit_syntax_m
    use traits_runtime_generic_01_contracts_m, only: IAlgorithm
    use traits_runtime_generic_01_late_client_m, only: LateValue
    implicit none
contains
    function invoke_explicit(implementation, object) result(r)
        class(IAlgorithm), intent(in) :: implementation
        type(LateValue), intent(in) :: object
        integer :: r
        r = implementation%apply{LateValue}(object)
    end function invoke_explicit
end module traits_runtime_generic_01_explicit_syntax_m
