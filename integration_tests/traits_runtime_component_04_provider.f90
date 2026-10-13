module traits_runtime_component_04_provider_m
    implicit none
    private
    public :: Holder
    abstract interface :: IValue
        pure integer function value()
        end function
    end interface
    type :: Payload
        integer :: n
    end type
    implements IValue :: Payload
        procedure :: value => payload_value
    end implements
    type :: Holder
        class(IValue), allocatable :: item
    contains
        initial :: make_holder
    end type
contains
    pure integer function payload_value(self)
        type(Payload), intent(in) :: self
        payload_value = self%n
    end function
    function make_holder(n) result(object)
        integer, intent(in) :: n
        type(Holder) :: object
        type(Payload) :: source
        source%n = n
        object%item = source
    end function
end module
