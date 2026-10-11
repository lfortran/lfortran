module traits_runtime_owning_12_m
    implicit none
    integer :: events = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Parent
        integer :: n = 17
    contains
        final :: parent_final
    end type
    type :: Leaf
        integer, allocatable :: data(:)
    contains
        final :: leaf_final
    end type
    type, extends(Parent) :: Child
        type(Leaf), allocatable :: leaf
    contains
        final :: child_final
    end type
    type :: Envelope
        type(Child), allocatable :: parts(:)
    end type
    implements IValue :: Envelope
        procedure, pass :: value => read_value
    end implements
contains
    impure elemental subroutine child_final(self)
        type(Child), intent(inout) :: self
        events = 10 * events + 2
    end subroutine
    impure elemental subroutine parent_final(self)
        type(Parent), intent(inout) :: self
        events = 10 * events + 1
    end subroutine
    impure elemental subroutine leaf_final(self)
        type(Leaf), intent(inout) :: self
        if (any(self%data /= 5)) error stop 10
        events = 10 * events + 3
    end subroutine
    function read_value(self) result(r)
        class(Envelope), intent(in) :: self
        integer :: r
        r = size(self%parts)
    end function
end module

program traits_runtime_owning_12
    use traits_runtime_owning_12_m
    implicit none
    type(Envelope) :: source, empty
    class(IValue), allocatable :: owner
    integer :: i
    allocate(source%parts(2))
    do i = 1, 2
        allocate(source%parts(i)%leaf)
        allocate(source%parts(i)%leaf%data(3))
        source%parts(i)%leaf%data = 5
    end do
    allocate(owner, source=source)
    if (events /= 0) error stop 1
    owner = owner
    if (events /= 223311) error stop 2
    if (owner%value() /= 2) error stop 3
    events = 0
    owner = empty
    if (events /= 223311) error stop 4
    events = 0
    owner = source
    if (events /= 0) error stop 5
    deallocate(owner)
    if (events /= 223311) error stop 6
    events = 0
    deallocate(source%parts)
    if (events /= 223311) error stop 7
end program
