module traits_runtime_generic_01_late_client_m
    use traits_runtime_generic_01_contracts_m, only: IValue
    implicit none
    type :: LateValue
        integer :: payload
    end type LateValue
    implements IValue :: LateValue
        procedure, pass :: value => late_value
    end implements LateValue
contains
    function late_value(self) result(r)
        class(LateValue), intent(in) :: self
        integer :: r
        r = self%payload
    end function late_value
end module traits_runtime_generic_01_late_client_m
