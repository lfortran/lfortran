module traits_runtime_component_06_m
    implicit none
    integer :: payload_finals = 0, holder_finals = 0
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Payload
        integer :: n = 7
    contains
        final :: finish_payload
    end type
    implements IValue :: Payload
        procedure :: value => get_value
    end implements
    type :: Holder
        class(IValue), allocatable :: item
    contains
        initial :: make
        final :: finish_holder
    end type
contains
    integer function get_value(self)
        type(Payload), intent(in) :: self
        get_value = self%n
    end function
    function make(source) result(object)
        type(Payload), intent(in) :: source
        type(Holder) :: object
        object%item = source
    end function
    subroutine finish_payload(self)
        type(Payload), intent(inout) :: self
        payload_finals = payload_finals + 1
        self%n = -1000
    end subroutine
    subroutine finish_holder(self)
        type(Holder), intent(inout) :: self
        holder_finals = holder_finals + 1
        if (allocated(self%item)) then
            if (self%item%value() /= 7) error stop 1
        end if
    end subroutine
end module

program traits_runtime_component_06
    use traits_runtime_component_06_m
    implicit none
    type(Payload) :: source
    type(Holder) :: object
    object = Holder(source)
    if (payload_finals /= 1 .or. holder_finals /= 2) error stop 2
    if (object%item%value() /= 7) error stop 3
    deallocate(object%item)
    if (payload_finals /= 2 .or. holder_finals /= 2) error stop 4
    block
        type(Holder) :: scoped
        scoped = Holder(source)
        if (payload_finals /= 3 .or. holder_finals /= 4) error stop 5
    end block
    if (payload_finals /= 4 .or. holder_finals /= 5) error stop 6
end program
