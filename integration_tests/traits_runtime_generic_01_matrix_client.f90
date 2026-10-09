module traits_runtime_generic_01_matrix_client_m
    use traits_runtime_generic_01_contracts_m, only: IValue
    implicit none
    integer :: argument_finals = 0
    type :: LateValue
        integer :: payload
    contains
        final :: finalize_late
    end type
    type :: PaddedValue
        real(8) :: prefix(3)
        integer :: payload
    contains
        final :: finalize_padded
    end type
    type :: AlternateValue
        integer :: payload
    contains
        final :: finalize_alternate
    end type
    implements IValue :: LateValue
        procedure, pass :: value => late_value
    end implements
    implements IValue :: PaddedValue
        procedure, pass :: value => padded_value
    end implements
    implements IValue :: AlternateValue
        procedure, pass :: value => alternate_value
    end implements
contains
    integer function late_value(self)
        class(LateValue), intent(in) :: self
        late_value = self%payload
    end function
    integer function padded_value(self)
        class(PaddedValue), intent(in) :: self
        padded_value = self%payload
    end function
    integer function alternate_value(self)
        class(AlternateValue), intent(in) :: self
        alternate_value = 2 * self%payload
    end function
    subroutine finalize_late(self)
        type(LateValue), intent(inout) :: self
        argument_finals = argument_finals + 1
        self%payload = -1
    end subroutine
    subroutine finalize_padded(self)
        type(PaddedValue), intent(inout) :: self
        argument_finals = argument_finals + 1
        self%payload = -1
    end subroutine
    subroutine finalize_alternate(self)
        type(AlternateValue), intent(inout) :: self
        argument_finals = argument_finals + 1
        self%payload = -1
    end subroutine
end module
