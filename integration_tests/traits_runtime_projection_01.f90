module traits_runtime_projection_01_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface, extends(IValue) :: IChild
        integer function twice()
        end function
    end interface
    type :: Box
        integer :: n = 5
        integer, allocatable :: data(:)
    contains
        final :: finish
    end type
    implements IChild :: Box
        procedure, pass :: value => box_value
        procedure, pass :: twice => box_twice
    end implements
    type(Box), target :: source
    class(IValue), pointer :: saved => null()
    integer :: finals = 0, total = 0
contains
    integer function box_value(self)
        type(Box), intent(in) :: self
        box_value = self%n
        if (allocated(self%data)) box_value = box_value + sum(self%data)
    end function
    integer function box_twice(self)
        type(Box), intent(in) :: self
        box_twice = 2 * box_value(self)
    end function
    subroutine finish(self)
        type(Box), intent(inout) :: self
        finals = finals + 1
        total = total + box_value(self)
        self%n = -777
        if (allocated(self%data)) self%data = -777
    end subroutine
    pure logical function empty(view)
        class(IValue), pointer, intent(in) :: view
        empty = .not. associated(view)
    end function
    subroutine remember(view)
        class(IValue), pointer, intent(in) :: view
        saved => view
    end subroutine
    pure subroutine forward_pointer(child, parent)
        class(IChild), pointer, intent(inout) :: child
        class(IValue), pointer, intent(inout) :: parent
        parent => child
        if (empty(child)) nullify(parent)
    end subroutine
    subroutine copy_parent(view, owner)
        class(IValue), intent(in) :: view
        class(IValue), allocatable, intent(out) :: owner
        owner = view
    end subroutine
    function make_child(n) result(owner)
        integer, intent(in) :: n
        class(IChild), allocatable :: owner
        source%n = n
        if (allocated(source%data)) deallocate(source%data)
        allocate(owner, source=source)
    end function
    integer function observe(view)
        class(IValue), intent(in) :: view
        observe = view%value()
    end function
end module

program traits_runtime_projection_01
    use traits_runtime_projection_01_m
    implicit none
    class(IChild), pointer :: child => null()
    class(IValue), pointer :: parent => null(), alias => null()
    class(IChild), allocatable, target :: owner
    class(IValue), allocatable, target :: copy, second
    procedure(empty), pointer :: query
    query => empty
    if (.not. empty(child) .or. .not. query(null(child))) error stop 1
    parent => child
    if (associated(parent)) error stop 2
    source%n = 7
    allocate(source%data(2))
    source%data = [2, 3]
    child => source
    if (empty(child) .or. query(child) .or. empty(source)) error stop 3
    call forward_pointer(child, parent)
    if (.not. associated(parent, child)) error stop 21
    nullify(parent)
    call remember(child)
    alias => saved
    if (.not. associated(saved, child)) error stop 4
    nullify(child)
    if (saved%value() /= 12 .or. alias%value() /= 12) error stop 5
    call remember(null(child))
    if (associated(saved) .or. .not. associated(alias, source)) error stop 6
    nullify(alias)

    allocate(owner, source=source)
    copy = owner
    call copy_parent(owner, second)
    child => owner
    parent => copy
    if (associated(parent, child) .or. .not. associated(child, owner)) error stop 7
    call remember(owner)
    if (.not. associated(saved, owner) .or. saved%value() /= 12) error stop 8
    source%n = 100
    source%data = [20, 30]
    if (observe(owner) /= 12 .or. copy%value() /= 12 .or. second%value() /= 12) error stop 9
    if (finals /= 0) error stop 10
    nullify(child, parent, saved)
    deallocate(owner)
    if (finals /= 1 .or. total /= 12) error stop 11
    if (copy%value() /= 12 .or. second%value() /= 12) error stop 12
    deallocate(copy, second)
    if (finals /= 3 .or. total /= 36) error stop 13
    allocate(Box :: copy)
    allocate(second, mold=source)
    if (copy%value() /= 5 .or. second%value() /= 5) error stop 14
    deallocate(copy, second)
    if (finals /= 5 .or. total /= 46) error stop 15

    if (observe(make_child(17)) /= 17) error stop 16
    if (finals /= 6 .or. total /= 63) error stop 17
    copy = make_child(23)
    if (finals /= 7 .or. total /= 86 .or. copy%value() /= 23) error stop 18
    allocate(second, source=make_child(31))
    if (finals /= 8 .or. total /= 117 .or. second%value() /= 31) error stop 19
    deallocate(copy, second)
    if (finals /= 10 .or. total /= 171) error stop 20
end program
