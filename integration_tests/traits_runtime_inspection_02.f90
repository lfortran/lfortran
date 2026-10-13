module traits_runtime_inspection_02_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Cell
        integer :: n = 17
        integer, allocatable :: data(:)
    contains
        final :: finish
    end type
    implements IValue :: Cell
        procedure, pass :: value => read_cell
    end implements
    integer :: finals = 0, total = 0
contains
    integer function read_cell(self)
        class(Cell), intent(in) :: self
        read_cell = self%n
    end function
    subroutine finish(self)
        type(Cell), intent(inout) :: self
        finals = finals + 1
        total = total + self%n
        self%n = -777
    end subroutine
    subroutine mutate_target(view)
        class(IValue), pointer, intent(in) :: view
        select type (concrete => view)
        type is (Cell)
            concrete%n = concrete%n + 1
        class default
            error stop 1
        end select
    end subroutine
    pure integer function readonly(view)
        class(IValue), intent(in) :: view
        select type (concrete => view)
        class is (Cell)
            readonly = concrete%n
        class default
            readonly = -1
        end select
    end function
    pure subroutine mutate_inout(view)
        class(IValue), pointer, intent(inout) :: view
        select type (concrete => view)
        type is (Cell)
            concrete%n = concrete%n + 1
        end select
    end subroutine
end module

program traits_runtime_inspection_02
    use traits_runtime_inspection_02_m
    implicit none
    type(Cell), target :: first, second
    type(Cell), pointer :: expected, escaped
    class(Cell), pointer :: escaped_class
    class(IValue), pointer :: view => null()
    class(IValue), allocatable, target :: owner, copy
    first%n = 17
    second%n = 29
    expected => first
    view => first
    select type (concrete => view)
    type is (Cell)
        if (.not. associated(expected, concrete)) error stop 2
        escaped => concrete
        concrete%n = 23
        view => second
        if (concrete%n /= 23 .or. .not. associated(expected, concrete)) error stop 3
        associate (stable => concrete)
            if (.not. associated(expected, stable)) error stop 18
            associate (scalar => stable%n)
                scalar = 24
            end associate
        end associate
    class default
        error stop 4
    end select
    if (.not. associated(escaped, first) .or. escaped%n /= 24) error stop 5
    if (first%n /= 24 .or. view%value() /= 29) error stop 6
    call mutate_target(view)
    if (second%n /= 30 .or. readonly(view) /= 30) error stop 7
    call mutate_inout(view)
    if (second%n /= 31) error stop 19
    select type (concrete => view)
    class default
        view => first
        if (concrete%value() /= 31) error stop 8
        select type (nested => concrete)
        type is (Cell)
            nested%n = 35
        end select
        if (concrete%value() /= 35) error stop 9
    end select
    if (second%n /= 35 .or. view%value() /= 24) error stop 10
    nullify(expected, escaped, view)
    if (finals /= 0) error stop 11

    allocate(Cell :: owner)
    select type (concrete => owner)
    type is (Cell)
        concrete%n = 31
        allocate(concrete%data(2))
        concrete%data = [3, 5]
        escaped => concrete
    class default
        error stop 12
    end select
    if (escaped%n /= 31 .or. sum(escaped%data) /= 8 .or. finals /= 0) error stop 13
    copy = owner
    view => copy
    select type (concrete => view)
    class is (Cell)
        escaped_class => concrete
        concrete%n = 41
        concrete%data(1) = 7
    class default
        error stop 14
    end select
    if (owner%value() /= 31 .or. copy%value() /= 41) error stop 15
    if (sum(escaped%data) /= 8 .or. finals /= 0) error stop 16
    if (escaped_class%n /= 41 .or. sum(escaped_class%data) /= 12) error stop 20
    nullify(view, escaped, escaped_class)
    deallocate(owner, copy)
    if (finals /= 2 .or. total /= 72) error stop 17
end program
