module traits_call_argument_type_01_m
    implicit none
    abstract interface :: IValue
        subroutine get_value(value)
            integer, intent(out) :: value
        end subroutine
    end interface
    type :: Box
        integer :: value
    end type
    implements IValue :: Box
        procedure, pass :: get_value => box_value
    end implements
contains
    subroutine box_value(self, value)
        class(Box), intent(in) :: self
        integer, intent(out) :: value
        value = self%value
    end subroutine
    subroutine query{IValue :: T}(object)
        type(T), intent(in) :: object
        real :: value
        call object%get_value(value)
    end subroutine
end module
