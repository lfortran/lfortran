module traits_runtime_generic_02_m
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    implicit none
    type(c_ptr) :: seen_value = c_null_ptr, seen_provider = c_null_ptr
    integer :: argument_finals = 0
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface, extends(IValue) :: IMore
        integer function extra()
        end function
    end interface
    abstract interface :: IAlgorithm
        function apply{IValue :: Element}(object, increment) result(r)
            type(Element), intent(in) :: object
            integer, intent(in) :: increment
            integer :: r
        end function
    end interface
    abstract interface :: ITag
        integer function tag()
        end function
    end interface
    type :: Value
        real(8) :: padding(4)
        integer :: payload
    contains
        final :: finish_value
    end type
    type :: Other
        integer :: payload
    contains
        final :: finish_other
    end type
    type :: Offset
        integer :: bias
    end type
    type :: Scale
        real(8) :: unrelated(3)
    end type
    implements IMore :: Value
        procedure, pass :: value => value_read
        procedure, nopass :: extra
    end implements
    implements IMore :: Other
        procedure, pass :: value => other_read
        procedure, nopass :: extra
    end implements
    implements (IAlgorithm + ITag) :: Offset
        procedure, pass(self) :: apply => offset_apply
        procedure, nopass :: tag => first_tag
    end implements
    implements (IAlgorithm + ITag) :: Scale
        procedure, nopass :: apply => scale_apply
        procedure, nopass :: tag => second_tag
    end implements
contains
    integer function value_read(self) result(r)
        class(Value), intent(in), target :: self
        seen_value = c_loc(self%payload)
        r = self%payload
    end function
    integer function other_read(self) result(r)
        class(Other), intent(in), target :: self
        seen_value = c_loc(self%payload)
        r = 2*self%payload
    end function
    integer function extra()
        extra = 1
    end function
    integer function first_tag()
        first_tag = 1
    end function
    integer function second_tag()
        second_tag = 2
    end function
    function offset_apply{IValue :: Renamed}(arg, self, delta) result(r)
        type(Renamed), intent(in) :: arg
        class(Offset), intent(in), target :: self
        integer, intent(in) :: delta
        integer :: r
        seen_provider = c_loc(self%bias)
        r = add_value{Renamed}(arg, delta) + self%bias
    end function
    function scale_apply{IValue :: Q}(arg, delta) result(r)
        type(Q), intent(in) :: arg
        integer, intent(in) :: delta
        integer :: r
        r = 2*add_value(arg, delta) + 100
    end function
    function add_value{IValue :: V}(arg, delta) result(r)
        type(V), intent(in) :: arg
        integer, intent(in) :: delta
        integer :: r
        r = arg%value() + delta
    end function
    function relay{IMore :: T}(algorithm, object) result(r)
        class(IAlgorithm), intent(in) :: algorithm
        type(T), intent(in) :: object
        integer :: r
        r = algorithm%apply{T}(object=object, increment=3)
    end function
    subroutine finish_value(self)
        type(Value), intent(inout) :: self
        self%payload = -1
        argument_finals = argument_finals + 1
    end subroutine
    subroutine finish_other(self)
        type(Other), intent(inout) :: self
        self%payload = -1
        argument_finals = argument_finals + 1
    end subroutine
end module

program traits_runtime_generic_02
    use traits_runtime_generic_02_m
    use iso_c_binding, only: c_associated
    implicit none
    type(Value), target :: left
    type(Other), target :: right
    type(Offset), target :: first
    type(Scale) :: second
    class(IAlgorithm + ITag), allocatable, target :: owner
    class(IAlgorithm), pointer :: view
    type(c_ptr) :: left_address, right_address, provider_address
    integer :: i, expected_left, expected_right
    left%padding = [3.d0, -4.d0, 7.d0, 101.d0]
    left%payload = 37
    right%payload = 37
    first%bias = 10
    left_address = c_loc(left%payload)
    right_address = c_loc(right%payload)
    provider_address = c_loc(first%bias)
    if (first%apply(left, 3) /= 50) error stop 1
    if (.not. c_associated(seen_value, left_address)) error stop 2
    if (.not. c_associated(seen_provider, provider_address)) error stop 3
    if (first%apply{Other}(right, 3) /= 87) error stop 4
    if (.not. c_associated(seen_value, right_address)) error stop 5
    do i = 1, 2
        if (i == 1) then
            allocate(owner, source=first)
            expected_left = 50
            expected_right = 87
        else
            allocate(owner, source=second)
            expected_left = 180
            expected_right = 254
        end if
        if (owner%tag() /= i) error stop 6
        if (owner%apply(left, 3) /= expected_left) error stop 7
        if (.not. c_associated(seen_value, left_address)) error stop 8
        view => owner
        if (view%apply{Other}(right, 3) /= expected_right) error stop 9
        if (.not. c_associated(seen_value, right_address)) error stop 10
        if (relay(owner, left) /= expected_left) error stop 11
        if (relay{Other}(owner, right) /= expected_right) error stop 12
        if (i == 1) then
            select type (concrete => owner)
            type is (Offset)
                provider_address = c_loc(concrete%bias)
            class default
                error stop 13
            end select
            if (.not. c_associated(seen_provider, provider_address)) error stop 14
        end if
        if (argument_finals /= 0) error stop 15
        nullify(view)
        deallocate(owner)
        if (argument_finals /= 0) error stop 16
        if (any(left%padding /= [3.d0, -4.d0, 7.d0, 101.d0])) error stop 17
        if (left%payload /= 37 .or. right%payload /= 37) error stop 18
    end do
end program
