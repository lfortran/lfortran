! Runtime trait syntax is an LFortran extension.
module traits_runtime_borrow_02_m
    use iso_c_binding, only: c_ptr, c_loc, c_associated
    implicit none
    type(c_ptr) :: expected
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Payload
        integer :: n
    end type
    type :: Holder
        type(Payload) :: item
    end type
    implements IValue :: Payload
        procedure, pass :: value => read_value
    end implements
contains
    function read_value(self) result(r)
        type(Payload), target, intent(in) :: self
        integer :: r
        type(c_ptr) :: actual
        actual = c_loc(self%n)
        if (.not. c_associated(actual, expected)) error stop 1
        r = self%n
    end function
    function observe(object) result(r)
        class(IValue), intent(in) :: object
        integer :: r
        r = object%value()
    end function
end module

program traits_runtime_borrow_02
    use traits_runtime_borrow_02_m
    implicit none
    type(Holder), target :: container
    type(Payload), target :: items(2), scalar
    type(Payload), pointer :: alias
    type(Payload), allocatable, target :: allocation
    integer :: i
    container%item%n = 17
    expected = c_loc(container%item%n)
    if (observe(container%item) /= 17) error stop 2
    do i = 1, 2
        items(i)%n = 20 + i
        expected = c_loc(items(i)%n)
        if (observe(items(i)) /= 20 + i) error stop 3
    end do
    scalar%n = 31
    expected = c_loc(scalar%n)
    if (observe(scalar) /= 31) error stop 4
    alias => scalar
    if (observe(alias) /= 31) error stop 5
    allocate(allocation)
    allocation%n = 41
    expected = c_loc(allocation%n)
    if (observe(allocation) /= 41) error stop 6
    deallocate(allocation)
end program
