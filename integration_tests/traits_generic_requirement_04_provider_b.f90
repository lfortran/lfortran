! Another conformance of Value to IValue: value() = 100 + n. It is visible
! only where this module is used.
module traits_generic_requirement_04_provider_b
    use traits_generic_requirement_04_contracts, only: IValue
    use traits_generic_requirement_04_types, only: Value
    implicit none

    implements IValue :: Value
        procedure, pass :: value => value_b
    end implements

contains

    integer function value_b(self)
        class(Value), intent(in) :: self
        value_b = 100 + self%n
    end function value_b

end module traits_generic_requirement_04_provider_b
