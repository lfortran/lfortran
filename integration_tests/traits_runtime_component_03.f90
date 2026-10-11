module traits_runtime_component_03_m
    implicit none
    abstract interface :: IValue
        pure integer function value()
        end function
    end interface
    type :: Payload
        integer, allocatable :: values(:)
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
        payload_value = sum(self%values)
    end function
    function make_holder(values) result(object)
        integer, intent(in) :: values(:)
        type(Holder) :: object
        type(Payload) :: source
        source%values = values
        object%item = source
    end function
    pure integer function observe(object)
        type(Holder), intent(in) :: object
        observe = object%item%value()
    end function
    integer function observe_slot(item)
        class(IValue), allocatable, intent(in) :: item
        if (.not. allocated(item)) error stop 1
        observe_slot = item%value()
    end function
    subroutine replace_slot(item, source)
        class(IValue), allocatable, intent(out) :: item
        type(Payload), intent(in) :: source
        if (allocated(item)) error stop 2
        allocate(item, source=source)
    end subroutine
    subroutine assign_slot(item, source)
        class(IValue), allocatable, intent(inout) :: item
        type(Payload), intent(in) :: source
        item = source
    end subroutine
end module

program traits_runtime_component_03
    use traits_runtime_component_03_m
    implicit none
    type(Holder) :: a, b
    type(Payload) :: source
    class(IValue), pointer :: view
    a = Holder([3, 4])
    if (observe(a) /= 7) error stop 3
    b = a
    source%values = [1, 2, 3]
    call replace_slot(a%item, source)
    if (observe_slot(a%item) /= 6) error stop 4
    if (observe(b) /= 7) error stop 5
    source%values(1) = 9
    if (observe(a) /= 6) error stop 6
    call assign_slot(a%item, source)
    if (observe(a) /= 14) error stop 7
    view => null(mold=a%item)
    if (associated(view)) error stop 8
    deallocate(a%item, b%item)
    if (allocated(a%item) .or. allocated(b%item)) error stop 9
    view => null(a%item)
    if (associated(view)) error stop 10
end program
