module traits_separate_01_impl_m
    use traits_separate_01_contracts_m, only: IValue
    implicit none
    type :: Payload
        integer :: value
    end type Payload
    implements IValue :: Payload
        procedure, pass :: get_value => payload_value
    end implements Payload
contains
    function payload_value(self) result(value)
        class(Payload), intent(in) :: self
        integer :: value
        value = self%value
    end function payload_value
end module traits_separate_01_impl_m
