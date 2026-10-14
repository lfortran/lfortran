! An intent(inout) borrowed trait view designates its actual's payload: a
! concrete variable, a component, an array element, an allocated owner, an
! owner component or a pointer target, also through an intent(in) pointer
! dummy. The callee modifies that payload
! through concrete SELECT TYPE inspection and observes it through a message
! with an intent(in) receiver at its original address. Borrowing copies,
! finalizes and rebinds nothing.
module traits_runtime_borrow_04_m
    use iso_c_binding, only: c_ptr, c_loc, c_associated
    implicit none
    type(c_ptr) :: expected
    integer :: finalized = 0
    abstract interface :: IValue
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    type :: Cell
        integer :: n = 0
        integer, allocatable :: data(:)
    contains
        final :: finish
    end type
    implements IValue :: Cell
        procedure, pass :: value => read_cell
    end implements
    type :: Holder
        type(Cell) :: cell
        class(IValue), allocatable :: item
    end type
contains
    integer function read_cell(self)
        class(Cell), target, intent(in) :: self
        type(c_ptr) :: actual
        actual = c_loc(self%n)
        if (.not. c_associated(actual, expected)) error stop 1
        read_cell = self%n
    end function
    subroutine finish(self)
        type(Cell), intent(inout) :: self
        finalized = finalized + 1
    end subroutine
    subroutine bump(object, by)
        class(IValue), intent(inout) :: object
        integer, intent(in) :: by
        integer :: before
        before = object%value()
        select type (concrete => object)
        type is (Cell)
            concrete%n = concrete%n + by
            if (allocated(concrete%data)) deallocate(concrete%data)
            allocate(concrete%data(1))
            concrete%data(1) = by
        class default
            error stop 2
        end select
        if (object%value() /= before + by) error stop 3
    end subroutine
    subroutine through_pointer(view, by)
        class(IValue), pointer, intent(inout) :: view
        integer, intent(in) :: by
        call bump(view, by)
    end subroutine
    ! INTENT(IN) protects the pointer association, not the target.
    subroutine through_readonly_pointer(view, by)
        class(IValue), pointer, intent(in) :: view
        integer, intent(in) :: by
        call bump(view, by)
    end subroutine
    subroutine through_concrete_pointer(payload, by)
        type(Cell), pointer, intent(in) :: payload
        integer, intent(in) :: by
        call bump(payload, by)
    end subroutine
end module

program traits_runtime_borrow_04
    use traits_runtime_borrow_04_m
    implicit none
    type(Cell), target :: c, cells(3)
    type(Holder), target :: h
    class(IValue), allocatable, target :: owner
    class(IValue), pointer :: view
    type(Cell), pointer :: payload
    c%n = 1
    expected = c_loc(c%n)
    call bump(c, 2)
    if (c%n /= 3 .or. c%data(1) /= 2) error stop 10
    expected = c_loc(cells(2)%n)
    call bump(cells(2), 5)
    if (cells(2)%n /= 5 .or. cells(1)%n /= 0 .or. cells(3)%n /= 0) error stop 11
    expected = c_loc(h%cell%n)
    call bump(h%cell, 7)
    if (h%cell%n /= 7 .or. h%cell%data(1) /= 7) error stop 12
    allocate(owner, source=c)
    select type (concrete => owner)
    type is (Cell)
        expected = c_loc(concrete%n)
    end select
    call bump(owner, 1)
    if (owner%value() /= 4 .or. c%n /= 3 .or. c%data(1) /= 2) error stop 13
    allocate(h%item, source=cells(2))
    select type (concrete => h%item)
    type is (Cell)
        expected = c_loc(concrete%n)
    end select
    call bump(h%item, 10)
    if (h%item%value() /= 15 .or. cells(2)%n /= 5) error stop 14
    view => c
    expected = c_loc(c%n)
    call through_pointer(view, 100)
    if (c%n /= 103 .or. c%data(1) /= 100) error stop 15
    call through_readonly_pointer(view, 1000)
    if (c%n /= 1103 .or. c%data(1) /= 1000) error stop 18
    payload => c
    call through_concrete_pointer(payload, 10000)
    if (c%n /= 11103 .or. c%data(1) /= 10000) error stop 19
    if (finalized /= 0) error stop 16
    deallocate(owner, h%item)
    if (finalized /= 2) error stop 17
end program
