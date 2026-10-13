module traits_runtime_owning_separate_01_a_m
    use traits_runtime_owning_separate_01_contracts_m
    use traits_runtime_owning_separate_01_types_m
    implicit none
    class(IValue), allocatable :: left
    implements IValue :: Payload
        procedure, pass :: value => first_value
        procedure, pass :: read_value => first_read
    end implements
contains
    function first_value(self) result(r)
        class(Payload), intent(in) :: self
        integer :: r
        r = self%n + 100
    end function
    subroutine first_read(self, result)
        class(Payload), intent(in) :: self
        integer, intent(out) :: result
        result = first_value(self)
    end subroutine
    subroutine setup_a()
        allocate(left, source=Payload(7))
    end subroutine
end module
