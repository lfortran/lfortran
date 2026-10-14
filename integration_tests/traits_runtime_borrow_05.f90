! Forwarding intent(inout) borrowed trait views: to another intent(inout) or
! an intent(in) view, weakened to a parent or a combination member, from a
! definable SELECT TYPE association, through procedure dummies and pointers,
! generic resolution, a function, a type-bound procedure of another type and a
! pure subroutine. Named non-first PASS, NOPASS and keyword arguments still
! dispatch messages with intent(in) receivers and receiverless messages
! through the view.
module traits_runtime_borrow_05_m
    implicit none
    abstract interface :: IValue
        integer function value(self, scale)
            type(itself), intent(in), pass :: self
            integer, intent(in) :: scale
        end function
    end interface
    abstract interface :: ILabel
        integer function label()
        end function
    end interface
    abstract interface, extends(IValue) :: IChild
        integer function twice(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    type :: Cell
        integer :: n = 0
    end type
    implements IChild + ILabel :: Cell
        procedure, pass(self) :: value => scaled_cell
        procedure, pass :: twice => twice_cell
        procedure, nopass :: label => cell_label
    end implements
    abstract interface
        subroutine updater(object, by)
            import IChild
            class(IChild), intent(inout) :: object
            integer, intent(in) :: by
        end subroutine
    end interface
    interface adjust
        module procedure adjust_value, adjust_count
    end interface
    type :: Tool
        integer :: calls = 0
    contains
        procedure :: use => tool_use
    end type
contains
    integer function scaled_cell(scale, self)
        integer, intent(in) :: scale
        class(Cell), intent(in) :: self
        scaled_cell = scale * self%n
    end function
    integer function twice_cell(self)
        class(Cell), intent(in) :: self
        twice_cell = 2 * self%n
    end function
    integer function cell_label()
        cell_label = 7
    end function
    pure subroutine add(object, by)
        class(IValue) :: object
        integer, intent(in) :: by
        intent(inout) :: object
        select type (concrete => object)
        type is (Cell)
            concrete%n = concrete%n + by
        end select
    end subroutine
    integer function observe(object)
        class(IValue), intent(in) :: object
        observe = object%value(scale=1)
    end function
    subroutine forward_child(object, by)
        class(IChild), intent(inout) :: object
        integer, intent(in) :: by
        integer :: before
        before = object%value(1)
        call add(object, by)
        if (object%twice() /= 2 * (before + by)) error stop 1
        if (observe(object) /= before + by) error stop 2
        if (object%value(scale=3) /= 3 * (before + by)) error stop 3
    end subroutine
    subroutine forward_combination(object, by)
        class(ILabel + IChild), intent(inout) :: object
        integer, intent(in) :: by
        if (object%label() /= 7) error stop 4
        call forward_child(object, by)
    end subroutine
    subroutine forward_association(object, by)
        class(IChild), intent(inout) :: object
        integer, intent(in) :: by
        select type (alias => object)
        class default
            call forward_child(alias, by)
        end select
    end subroutine
    subroutine apply(update, object, by)
        procedure(updater) :: update
        class(IChild), intent(inout) :: object
        integer, intent(in) :: by
        call update(by=by, object=object)
    end subroutine
    subroutine adjust_value(object, by)
        class(IValue), intent(inout) :: object
        integer, intent(in) :: by
        call add(object, by)
    end subroutine
    subroutine adjust_count(n, by)
        integer, intent(inout) :: n
        integer, intent(in) :: by
        n = n + by
    end subroutine
    integer function take(object, by)
        class(IValue), intent(inout) :: object
        integer, intent(in) :: by
        call add(object, by)
        take = object%value(1)
    end function
    subroutine tool_use(self, object, by)
        class(Tool), intent(inout) :: self
        class(IChild), intent(inout) :: object
        integer, intent(in) :: by
        self%calls = self%calls + 1
        call forward_child(object, by)
    end subroutine
end module

program traits_runtime_borrow_05
    use traits_runtime_borrow_05_m
    implicit none
    type(Cell) :: c
    type(Tool) :: t
    procedure(updater), pointer :: update => null()
    integer :: k
    c%n = 1
    call forward_child(c, 2)
    if (c%n /= 3) error stop 10
    call forward_combination(c, 3)
    if (c%n /= 6) error stop 11
    call forward_association(c, 4)
    if (c%n /= 10) error stop 12
    call apply(forward_child, c, 5)
    if (c%n /= 15) error stop 13
    update => forward_child
    call update(c, 6)
    if (c%n /= 21) error stop 14
    call adjust(c, 9)
    if (c%n /= 30) error stop 15
    k = 1
    call adjust(k, 2)
    if (k /= 3) error stop 16
    k = take(c, 1)
    if (k /= 31 .or. c%n /= 31) error stop 17
    call t%use(c, 4)
    if (t%calls /= 1 .or. c%n /= 35) error stop 18
end program
