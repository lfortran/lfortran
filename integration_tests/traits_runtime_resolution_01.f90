! Generic resolution matches exactly first: a derived actual selects its exact
! specific over a borrowed-view specific, and a view actual selects the
! specific of its own contract over that of an implied parent, in either
! declaration order. Only when nothing matches exactly does conformance select
! a view specific, for module, keyword, initializer, subroutine and type-bound
! references alike.
module traits_runtime_resolution_01_m
    implicit none
    private
    public :: IValue, IChild, Cell, Other, Holder, Box, Visitor, describe, &
        describe_rev, classify, classify_rev, only_view, record

    abstract interface :: IValue
        integer function value()
        end function value
    end interface IValue

    abstract interface, extends(IValue) :: IChild
        integer function extra()
        end function extra
    end interface IChild

    type, sealed, implements(IChild) :: Cell
        integer :: n = 0
    contains
        procedure, nopass :: value => cell_value
        procedure, nopass :: extra => cell_extra
    end type Cell

    type, sealed, implements(IValue) :: Other
    contains
        procedure, nopass :: value => other_value
    end type Other

    type :: Holder
        integer :: tag = 0
    contains
        initial :: holder_from_view, holder_from_cell
    end type Holder

    type :: Box
        integer :: tag = 0
    contains
        initial :: box_from_cell, box_from_view
    end type Box

    type :: Visitor
        integer :: base = 1000
    contains
        procedure :: probe_view
        procedure :: probe_cell
        generic :: probe => probe_view, probe_cell
    end type Visitor

    interface describe
        module procedure describe_view, describe_cell
    end interface describe

    interface describe_rev
        module procedure describe_cell, describe_view
    end interface describe_rev

    interface classify
        module procedure classify_parent, classify_child
    end interface classify

    interface classify_rev
        module procedure classify_child, classify_parent
    end interface classify_rev

    interface only_view
        module procedure describe_view
    end interface only_view

    interface record
        module procedure record_view, record_cell
    end interface record

contains

    integer function cell_value()
        cell_value = 1
    end function cell_value

    integer function cell_extra()
        cell_extra = 2
    end function cell_extra

    integer function other_value()
        other_value = 3
    end function other_value

    integer function describe_view(item)
        class(IValue), intent(in) :: item
        describe_view = 100 + item%value()
    end function describe_view

    integer function describe_cell(item)
        type(Cell), intent(in) :: item
        describe_cell = 200 + item%n
    end function describe_cell

    integer function classify_parent(item)
        class(IValue), intent(in) :: item
        classify_parent = 300 + item%value()
    end function classify_parent

    integer function classify_child(item)
        class(IChild), intent(in) :: item
        classify_child = 400 + item%extra()
    end function classify_child

    function holder_from_view(item) result(h)
        class(IValue), intent(in) :: item
        type(Holder) :: h
        h%tag = 10 + item%value()
    end function holder_from_view

    function holder_from_cell(item) result(h)
        type(Cell), intent(in) :: item
        type(Holder) :: h
        h%tag = 20 + item%n
    end function holder_from_cell

    function box_from_view(item) result(b)
        class(IValue), intent(in) :: item
        type(Box) :: b
        b%tag = 30 + item%value()
    end function box_from_view

    function box_from_cell(item) result(b)
        type(Cell), intent(in) :: item
        type(Box) :: b
        b%tag = 40 + item%n
    end function box_from_cell

    integer function probe_view(self, item)
        class(Visitor), intent(in) :: self
        class(IValue), intent(in) :: item
        probe_view = self%base + 100 + item%value()
    end function probe_view

    integer function probe_cell(self, item)
        class(Visitor), intent(in) :: self
        type(Cell), intent(in) :: item
        probe_cell = self%base + 200 + item%n
    end function probe_cell

    subroutine record_view(item, n)
        class(IValue), intent(in) :: item
        integer, intent(out) :: n
        n = 500 + item%value()
    end subroutine record_view

    subroutine record_cell(item, n)
        type(Cell), intent(in) :: item
        integer, intent(out) :: n
        n = 600 + item%n
    end subroutine record_cell

end module traits_runtime_resolution_01_m

program traits_runtime_resolution_01
    use traits_runtime_resolution_01_m
    implicit none
    type(Cell) :: c
    type(Other) :: o
    type(Holder) :: h
    type(Box) :: b
    type(Visitor) :: v
    integer :: n

    c%n = 5
    if (describe(c) /= 205) error stop 1
    if (describe_rev(c) /= 205) error stop 2
    if (describe(item=c) /= 205) error stop 3
    if (describe(o) /= 103) error stop 4
    if (describe_rev(item=o) /= 103) error stop 5
    if (only_view(c) /= 101) error stop 6
    h = Holder(c)
    if (h%tag /= 25) error stop 7
    h = Holder(o)
    if (h%tag /= 13) error stop 8
    b = Box(c)
    if (b%tag /= 45) error stop 9
    b = Box(o)
    if (b%tag /= 33) error stop 10
    call record(c, n)
    if (n /= 605) error stop 11
    call record(o, n)
    if (n /= 503) error stop 12
    if (v%probe(c) /= 1205) error stop 13
    if (v%probe(o) /= 1103) error stop 14
    call check_child(c)
    print '(a)', 'traits_runtime_resolution_01: exact matches before conformance'

contains

    subroutine check_child(child)
        class(IChild), intent(in) :: child
        if (classify(child) /= 402) error stop 20
        if (classify_rev(child) /= 402) error stop 21
        if (describe(child) /= 101) error stop 22
        call check_parent(child)
    end subroutine check_child

    subroutine check_parent(parent)
        class(IValue), intent(in) :: parent
        if (classify(parent) /= 301) error stop 30
        if (classify_rev(parent) /= 301) error stop 31
        if (describe(parent) /= 101) error stop 32
    end subroutine check_parent

end program traits_runtime_resolution_01
