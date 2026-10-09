module traits_inheritance_05_impl_m
    use traits_inheritance_05_contracts_m, only: RenamedChild => IChild
    use traits_inheritance_05_types_m, only: RenamedPayload => Payload
    implicit none

    implements RenamedChild :: RenamedPayload
        procedure, pass(self) :: scaled => payload_scaled
        procedure, pass :: value => payload_value
    end implements RenamedPayload

contains

    function payload_value(self) result(res)
        class(RenamedPayload), intent(in) :: self
        integer :: res
        res = self%data
    end function payload_value

    function payload_scaled(factor, self) result(res)
        integer, intent(in) :: factor
        class(RenamedPayload), intent(in) :: self
        integer :: res
        res = factor * self%data
    end function payload_scaled
end module traits_inheritance_05_impl_m
