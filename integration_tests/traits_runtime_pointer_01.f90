! Traits are an LFortran extension; ordinary pointer controls are separate.
module traits_runtime_pointer_01_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Cell
        integer :: n
    contains
        final :: finish
    end type
    type :: Other
        real(8) :: padding(2) = [13.0_8, 31.0_8]
        integer :: n
    end type
    type :: Container
        type(Other) :: inline
        type(Other), pointer :: linked
        type(Other), allocatable :: stored
    end type
    implements IValue :: Cell
        procedure, pass :: value => cell_value
    end implements
    implements IValue :: Other
        procedure, pass :: value => other_value
    end implements
    integer :: finals = 0, total = 0
contains
    integer function cell_value(self)
        class(Cell), intent(in) :: self
        cell_value = self%n
    end function
    integer function other_value(self)
        class(Other), intent(in) :: self
        if (any(self%padding /= [13.0_8, 31.0_8])) error stop 1
        other_value = 2 * self%n
    end function
    subroutine finish(self)
        type(Cell), intent(inout) :: self
        finals = finals + 1
        total = total + self%n
    end subroutine
    integer function observe(object)
        class(IValue), pointer, intent(in) :: object
        observe = object%value()
    end function
    integer function borrow(object)
        class(IValue), intent(in) :: object
        borrow = object%value()
    end function
    subroutine bind_cell(view, object)
        class(IValue), pointer, intent(out) :: view
        type(Cell), target, intent(inout) :: object
        view => object
    end subroutine
    subroutine rebind(view, other)
        class(IValue), pointer, intent(inout) :: view
        class(IValue), pointer, intent(in) :: other
        view => other
    end subroutine
    pure subroutine pure_bind(view, object)
        class(IValue), pointer, intent(inout) :: view
        type(Cell), target, intent(inout) :: object
        view => object
    end subroutine
    pure subroutine pure_forward(view, object)
        class(IValue), pointer, intent(inout) :: view
        type(Cell), target, intent(inout) :: object
        call pure_bind(view, object)
    end subroutine
    pure logical function linked(object)
        class(IValue), pointer, intent(in) :: object
        linked = associated(object)
    end function
    integer function remembered(object, install)
        type(Cell), target, intent(inout) :: object
        logical, intent(in) :: install
        class(IValue), pointer, save :: saved => null()
        if (install) saved => object
        remembered = saved%value()
        if (.not. install) nullify(saved)
    end function
    subroutine local_alias(object)
        class(IValue), pointer, intent(in) :: object
        class(IValue), pointer :: alias
        alias => object
        if (.not. associated(alias, object)) error stop 2
        if (alias%value() /= object%value()) error stop 3
    end subroutine
    subroutine subobjects(object)
        type(Other), target, intent(inout) :: object
        type(Container), target :: holder
        class(IValue), pointer :: view
        holder%inline%n = 7
        holder%linked => object
        view => holder%inline
        if (view%value() /= 14 .or. .not. associated(view, holder%inline)) error stop 28
        view => holder%linked
        if (view%value() /= 58 .or. .not. associated(view, object)) error stop 29
        allocate(holder%stored)
        holder%stored%n = 11
        view => holder%stored
        if (view%value() /= 22 .or. .not. associated(view, holder%stored)) error stop 30
        nullify(view)
        deallocate(holder%stored)
    end subroutine
end module

program traits_runtime_pointer_01
    use traits_runtime_pointer_01_m
    implicit none
    type(Cell), target :: first, second
    type(Other), target :: different
    class(IValue), pointer :: view => null(), alias => null()
    class(IValue), allocatable, target :: owner, copy
    integer :: i

    first%n = 13
    second%n = 47
    different%n = 29
    call subobjects(different)
    if (associated(view)) error stop 4
    if (linked(null(view))) error stop 24
    view => first
    if (.not. associated(view, first)) error stop 5
    if (associated(view, second)) error stop 6
    if (observe(view) /= 13 .or. borrow(view) /= 13) error stop 7
    if (observe(first) /= 13) error stop 8
    if (remembered(first, .true.) /= 13) error stop 25
    alias => view
    first%n = 31
    if (remembered(first, .false.) /= 31) error stop 26
    if (alias%value() /= 31) error stop 9
    view => second
    if (alias%value() /= 31 .or. view%value() /= 47) error stop 10
    if (associated(view, alias)) error stop 11
    view => different
    if (view%value() /= 58 .or. alias%value() /= 31) error stop 12
    call bind_cell(view, first)
    if (.not. associated(view, alias)) error stop 13
    nullify(alias)
    if (associated(alias) .or. .not. associated(view)) error stop 14
    call rebind(view, alias)
    if (associated(view)) error stop 15
    call pure_forward(view, first)
    if (.not. linked(view) .or. view%value() /= 31) error stop 27
    do i = 1, 3
        call bind_cell(view, second)
        call local_alias(view)
        view => view
        if (view%value() /= 47) error stop 16
    end do
    view => null()
    if (associated(view) .or. finals /= 0) error stop 17

    allocate(owner, source=first)
    copy = owner
    view => owner
    alias => view
    if (.not. associated(view, owner)) error stop 18
    if (associated(view, copy)) error stop 19
    call local_alias(alias)
    if (finals /= 0 .or. observe(owner) /= 31) error stop 20
    nullify(view, alias)
    deallocate(owner)
    if (finals /= 1 .or. total /= 31) error stop 21
    view => copy
    if (view%value() /= 31) error stop 22
    nullify(view)
    deallocate(copy)
    if (finals /= 2 .or. total /= 62) error stop 23
end program
