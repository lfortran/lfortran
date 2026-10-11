module traits_runtime_owning_separate_01_b_m
    use traits_runtime_owning_separate_01_contracts_m
    use traits_runtime_owning_separate_01_types_m, only: Shared => Payload
    implicit none
    integer :: other_finalizations = 0
    type :: Payload
        integer :: n
    contains
        final :: finish_other
    end type
    class(IValue), allocatable :: right, other
    implements IValue :: Shared
        procedure, pass :: read_value => second_read
        procedure, pass :: value => second_value
    end implements
    implements IValue :: Payload
        procedure, pass :: value => other_value
        procedure, pass :: read_value => other_read
    end implements
contains
    function second_value(self) result(r)
        class(Shared), intent(in) :: self
        integer :: r
        r = self%n + 200
    end function
    subroutine second_read(self, result)
        class(Shared), intent(in) :: self
        integer, intent(out) :: result
        result = second_value(self)
    end subroutine
    function other_value(self) result(r)
        class(Payload), intent(in) :: self
        integer :: r
        r = self%n + 300
    end function
    subroutine other_read(self, result)
        class(Payload), intent(in) :: self
        integer, intent(out) :: result
        result = other_value(self)
    end subroutine
    subroutine finish_other(self)
        type(Payload), intent(inout) :: self
        other_finalizations = other_finalizations + 1
    end subroutine
    subroutine setup_b()
        right = Shared(7)
        other = Payload(7)
    end subroutine
end module
