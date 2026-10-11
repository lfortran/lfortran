module traits_runtime_borrow_01_m
    use iso_c_binding, only: c_ptr, c_loc, c_associated
    implicit none
    integer :: finalized = 0
    type(c_ptr) :: expected
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue
    type :: Payload
        integer :: value
        integer, allocatable :: elements(:)
    contains
        final :: finish
    end type Payload
    implements IValue :: Payload
        procedure, pass :: value => read_value
    end implements Payload
contains
    function read_value(self) result(r)
        class(Payload), target, intent(in) :: self
        integer :: r
        type(c_ptr) :: actual
        actual = c_loc(self%value)
        if (.not. c_associated(actual, expected)) error stop 1
        r = self%value + sum(self%elements)
    end function read_value
    subroutine finish(self)
        type(Payload), intent(inout) :: self
        if (.not. allocated(self%elements)) error stop 2
        finalized = finalized + 1
    end subroutine finish
    function observe(object) result(r)
        class(IValue), intent(in) :: object
        integer :: r
        r = object%value()
    end function observe
    subroutine forward(object, expected_value)
        class(IValue), intent(in) :: object
        integer, intent(in) :: expected_value
        if (observe(object) /= expected_value) error stop 3
        if (finalized /= 0) error stop 4
    end subroutine forward
end module traits_runtime_borrow_01_m

program traits_runtime_borrow_01
    use traits_runtime_borrow_01_m
    implicit none
    type(Payload), allocatable, target :: object
    allocate(object)
    object%value = 11
    allocate(object%elements(2))
    object%elements = [2, 3]
    expected = c_loc(object%value)
    call forward(object, 16)
    if (finalized /= 0) error stop 5
    object%value = 29
    call forward(object, 34)
    if (finalized /= 0) error stop 6
    deallocate(object)
    if (finalized /= 1) error stop 7
end program traits_runtime_borrow_01
