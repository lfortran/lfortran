! Intent(inout) borrowed trait views. Each procedure whose name starts with
! from_ or ends with _view has exactly one invalid actual argument, statement
! or dummy: a defining view needs a definable payload, and a borrowed view is
! neither an owner nor a pointer that could be replaced or reassociated. A
! message whose receiver is intent(inout) needs a definable object, so a
! read-only view cannot call it (TD-36); intent(out) and omitted intents of
! trait-object dummies stay unsupported (TD-35).
module traits_inout_negative_m
    implicit none
    abstract interface :: IValue
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface :: ICounter
        subroutine increment(self)
            type(itself), intent(inout), pass :: self
        end subroutine
    end interface
    type :: Cell
        integer :: n = 0
    end type
    implements IValue :: Cell
        procedure, pass :: value => read_cell
    end implements
    type :: Counter
        integer :: n = 0
    end type
    implements ICounter :: Counter
        procedure, pass :: increment => counter_increment
    end implements
    type :: Holder
        type(Cell) :: cell
        class(IValue), allocatable :: item
    end type
    type(Cell), parameter :: fixed = Cell(4)
contains
    integer function read_cell(self)
        class(Cell), intent(in) :: self
        read_cell = self%n
    end function
    subroutine counter_increment(self)
        class(Counter), intent(inout) :: self
        self%n = self%n + 1
    end subroutine
    type(Cell) function make_cell()
        make_cell%n = 5
    end function
    function make_owner() result(owner)
        class(IValue), allocatable :: owner
        allocate(Cell :: owner)
    end function
    subroutine bump(object)
        class(IValue), intent(inout) :: object
    end subroutine
    subroutine use_counter(object)
        class(ICounter), intent(inout) :: object
    end subroutine
    subroutine from_constant()
        call bump(fixed)
    end subroutine
    subroutine from_constructor()
        call bump(Cell(3))
    end subroutine
    subroutine from_function()
        call bump(make_cell())
    end subroutine
    subroutine from_owner_function()
        call bump(make_owner())
    end subroutine
    subroutine from_readonly_concrete(c)
        type(Cell), intent(in) :: c
        call bump(c)
    end subroutine
    subroutine from_readonly_component(h)
        type(Holder), intent(in) :: h
        call bump(h%cell)
    end subroutine
    subroutine from_readonly_owner_component(h)
        type(Holder), intent(in) :: h
        call bump(h%item)
    end subroutine
    subroutine from_readonly_view(object)
        class(IValue), intent(in) :: object
        call bump(object)
    end subroutine
    subroutine from_readonly_slot(object)
        class(IValue), allocatable, intent(in) :: object
        call bump(object)
    end subroutine
    subroutine from_readonly_association(object)
        class(IValue), intent(in) :: object
        select type (alias => object)
        class default
            call bump(alias)
        end select
    end subroutine
    subroutine from_readonly_mutator(object)
        class(ICounter), intent(in) :: object
        call object%increment()
    end subroutine
    subroutine assign_view(object, other)
        class(IValue), intent(inout) :: object
        class(IValue), intent(in) :: other
        object = other
    end subroutine
    subroutine allocate_view(object)
        class(IValue), intent(inout) :: object
        allocate(Cell :: object)
    end subroutine
    subroutine deallocate_view(object)
        class(IValue), intent(inout) :: object
        deallocate(object)
    end subroutine
    subroutine deallocate_readonly_view(object)
        class(IValue), intent(in) :: object
        deallocate(object)
    end subroutine
    subroutine associate_view(object, c)
        class(IValue), intent(inout) :: object
        type(Cell), target, intent(inout) :: c
        object => c
    end subroutine
    subroutine nullify_view(object)
        class(IValue), intent(inout) :: object
        nullify(object)
    end subroutine
    subroutine take_slot(object)
        class(IValue), allocatable, intent(inout) :: object
    end subroutine
    subroutine slot_from_view(object)
        class(IValue), intent(inout) :: object
        call take_slot(object)
    end subroutine
    subroutine take_pointer(object)
        class(IValue), pointer, intent(inout) :: object
    end subroutine
    subroutine pointer_from_view(object)
        class(IValue), intent(inout) :: object
        call take_pointer(object)
    end subroutine
    subroutine out_view(object)
        class(IValue), intent(out) :: object
    end subroutine
    subroutine unspecified_view(object)
        class(IValue) :: object
    end subroutine
    subroutine from_parenthesized(c)
        type(Cell), intent(inout) :: c
        call bump((c))
    end subroutine
    subroutine from_parenthesized_view(object)
        class(IValue), intent(inout) :: object
        call bump((object))
    end subroutine
    pure integer function pure_function_view(object)
        class(IValue), intent(inout) :: object
        pure_function_view = 0
    end function
end module
