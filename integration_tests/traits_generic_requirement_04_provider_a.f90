! One conformance of Value to IValue: value() = n.
module traits_generic_requirement_04_provider_a
    use traits_generic_requirement_04_contracts, only: IValue
    use traits_generic_requirement_04_types, only: Value
    implicit none

    implements IValue :: Value
        procedure, pass :: value => value_a
    end implements

contains

    integer function value_a(self)
        class(Value), intent(in) :: self
        value_a = self%n
    end function value_a

end module traits_generic_requirement_04_provider_a
