module traits_runtime_pointer_01_oracle_m
    implicit none
    type, abstract :: ValueBase
    contains
        procedure(value_signature), deferred :: value
    end type
    abstract interface
        integer function value_signature(self)
            import ValueBase
            class(ValueBase), intent(in) :: self
        end function
    end interface
    type, extends(ValueBase) :: Cell
        integer :: n = 17
    contains
        procedure :: value => cell_value
        final :: finish
    end type
    integer :: finals = 0, total = 0
contains
    integer function cell_value(self)
        class(Cell), intent(in) :: self
        cell_value = self%n
    end function
    subroutine finish(self)
        type(Cell), intent(inout) :: self
        finals = finals + 1
        total = total + self%n
    end subroutine
    pure subroutine bind(view, object)
        class(ValueBase), pointer, intent(inout) :: view
        type(Cell), target, intent(inout) :: object
        view => object
    end subroutine
    pure logical function linked(view)
        class(ValueBase), pointer, intent(in) :: view
        linked = associated(view)
    end function
    integer function remembered(object, install)
        type(Cell), target, intent(inout) :: object
        logical, intent(in) :: install
        class(ValueBase), pointer, save :: saved => null()
        if (install) saved => object
        remembered = saved%value()
        if (.not. install) nullify(saved)
    end function
    subroutine local_alias(object)
        class(ValueBase), pointer, intent(in) :: object
        class(ValueBase), pointer :: alias
        alias => object
        if (.not. associated(alias, object)) error stop 1
    end subroutine
end module

program traits_runtime_pointer_01_oracle
    use traits_runtime_pointer_01_oracle_m
    implicit none
    type(Cell), target :: first, second
    class(ValueBase), pointer :: view => null(), alias => null()
    class(ValueBase), allocatable, target :: owner, copy

    first%n = 13
    second%n = 47
    if (linked(null(view))) error stop 2
    call bind(view, first)
    if (.not. linked(view) .or. view%value() /= 13) error stop 3
    alias => view
    if (remembered(first, .true.) /= 13) error stop 4
    first%n = 31
    if (remembered(first, .false.) /= 31) error stop 5
    view => second
    if (alias%value() /= 31 .or. view%value() /= 47) error stop 6
    if (associated(view, alias)) error stop 7
    call local_alias(alias)
    nullify(view, alias)
    if (finals /= 0) error stop 8

    allocate(owner, source=first)
    copy = owner
    view => owner
    alias => view
    if (.not. associated(view, owner)) error stop 9
    if (associated(view, copy)) error stop 10
    call local_alias(alias)
    nullify(view, alias)
    deallocate(owner)
    if (finals /= 1 .or. total /= 31) error stop 11
    view => copy
    if (view%value() /= 31) error stop 12
    nullify(view)
    deallocate(copy)
    if (finals /= 2 .or. total /= 62) error stop 13
end program
