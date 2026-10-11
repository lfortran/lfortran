! Standard-Fortran oracle for traits_runtime_borrow_03: the same named
! constants, renamed import, parenthesized reference, procedure-local constant
! and constant component passed to an ordinary class(IValue) dummy, with the
! same results and the same number of binding calls.
module traits_runtime_borrow_03_oracle_m
    implicit none
    private
    public :: IValue, Cell, Pair, observe, observe_value, observe_cell, forward, &
        K, KP, calls

    integer :: calls = 0

    type, abstract :: IValue
    contains
        procedure(value_interface), deferred, pass(self) :: value
    end type IValue

    abstract interface
        integer function value_interface(self)
            import :: IValue
            class(IValue), intent(in) :: self
        end function value_interface
    end interface

    type, extends(IValue) :: Cell
        integer :: n = 0
        integer :: m = 0
    contains
        procedure, pass(self) :: value => cell_value
    end type Cell

    type :: Pair
        type(Cell) :: first
        type(Cell) :: second
    end type Pair

    type(Cell), parameter :: K = Cell(5, 2)
    type(Pair), parameter :: KP = Pair(Cell(7, 3), Cell(11, 4))

    interface observe
        module procedure observe_value
    end interface observe

contains

    integer function cell_value(self)
        class(Cell), intent(in) :: self
        calls = calls + 1
        cell_value = 10 * self%n + self%m
    end function cell_value

    integer function observe_value(item)
        class(IValue), intent(in) :: item
        observe_value = item%value()
    end function observe_value

    integer function forward(item)
        class(IValue), intent(in) :: item
        forward = observe_value(item) + observe_value(item)
    end function forward

    integer function observe_cell(item)
        type(Cell), intent(in) :: item
        observe_cell = 10 * item%n + item%m
    end function observe_cell
end module traits_runtime_borrow_03_oracle_m

program traits_runtime_borrow_03_oracle
    implicit none
    call direct()
    call renamed()
    print '(a)', 'traits_runtime_borrow_03_oracle: ok'
contains

    subroutine direct()
        use traits_runtime_borrow_03_oracle_m
        type(Cell), parameter :: LOCAL = Cell(3, 1)
        integer :: i, total
        calls = 0
        total = 0
        do i = 1, 3
            total = total + observe_value(K) + observe(K) + observe((K))
            total = total + observe(KP%second) + observe_value(LOCAL) + forward(K)
        end do
        if (total /= 1215) error stop 1
        if (calls /= 21) error stop 2
        if (observe_cell(K) /= 52) error stop 3
        if (observe_cell(KP%second) /= 114) error stop 4
        if (observe_cell(LOCAL) /= 31) error stop 5
        if (calls /= 21) error stop 6
        if (K%n /= 5 .or. K%m /= 2) error stop 7
        if (KP%second%n /= 11 .or. LOCAL%m /= 1) error stop 8
    end subroutine direct

    subroutine renamed()
        use traits_runtime_borrow_03_oracle_m, only: Cell, observe, observe_cell, &
            KR => K, calls
        integer :: before
        before = calls
        if (observe(KR) /= observe_cell(KR)) error stop 9
        if (observe(KR) /= 52) error stop 10
        if (calls - before /= 2) error stop 11
    end subroutine renamed
end program traits_runtime_borrow_03_oracle
