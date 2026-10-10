! A type-bound GENERIC subroutine whose specific takes a borrowed trait
! view, selected by conformance, associates its actuals like a direct call:
! a variable, a structure constructor and a keyword function result are
! borrowed for the call and evaluated once, for PASS and NOPASS specifics,
! and for a generic inherited by an extended type that overrides the
! specific, reached statically and through a polymorphic receiver.
module traits_runtime_resolution_02_m
    implicit none
    private
    public :: IValue, Cell, Visitor, Tally, make_cell, made

    integer :: made = 0

    abstract interface :: IValue
        integer function value()
        end function value
    end interface IValue

    type :: Cell
        integer :: n = 0
    end type Cell

    implements IValue :: Cell
        procedure, pass :: value => cell_value
    end implements Cell

    type :: Visitor
        integer :: seen = 0
    contains
        procedure :: visit_view
        procedure, nopass :: note_view
        generic :: visit => visit_view
        generic :: note => note_view
    end type Visitor

    type, extends(Visitor) :: Tally
    contains
        procedure :: visit_view => tally_view
    end type Tally

contains

    integer function cell_value(self)
        class(Cell), intent(in) :: self
        cell_value = self%n
    end function cell_value

    subroutine visit_view(self, item)
        class(Visitor), intent(inout) :: self
        class(IValue), intent(in) :: item
        self%seen = self%seen + item%value()
    end subroutine visit_view

    subroutine tally_view(self, item)
        class(Tally), intent(inout) :: self
        class(IValue), intent(in) :: item
        self%seen = self%seen + 1000 * item%value()
    end subroutine tally_view

    subroutine note_view(item, n)
        class(IValue), intent(in) :: item
        integer, intent(out) :: n
        n = 100 + item%value()
    end subroutine note_view

    function make_cell(n) result(c)
        integer, intent(in) :: n
        type(Cell) :: c
        made = made + 1
        c%n = n
    end function make_cell

end module traits_runtime_resolution_02_m

program traits_runtime_resolution_02
    use traits_runtime_resolution_02_m
    implicit none
    type(Visitor) :: v
    type(Tally) :: t
    class(Visitor), allocatable :: any
    type(Cell) :: c
    integer :: n

    c%n = 1
    call v%visit(c)
    if (v%seen /= 1) error stop 1
    call v%visit(Cell(20))
    if (v%seen /= 21) error stop 2
    call v%visit(item=make_cell(300))
    if (v%seen /= 321 .or. made /= 1) error stop 3
    call v%note(c, n)
    if (n /= 101) error stop 4
    call v%note(n=n, item=make_cell(5))
    if (n /= 105 .or. made /= 2) error stop 5
    call t%visit(c)
    if (t%seen /= 1000) error stop 6
    call t%visit(item=make_cell(2))
    if (t%seen /= 3000 .or. made /= 3) error stop 7
    allocate(Tally :: any)
    call any%visit(make_cell(4))
    if (any%seen /= 4000 .or. made /= 4) error stop 8
    call any%note(Cell(6), n)
    if (n /= 106) error stop 9
    print '(a)', 'traits_runtime_resolution_02: generic subroutine actuals borrowed'
end program traits_runtime_resolution_02
