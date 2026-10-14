! Standard-Fortran counterpart of traits_receiver_intent_04.f90: deferred
! bindings whose passed objects have no INTENT, called through class(IOpen)
! dummies with intent(in) and intent(inout), an owner and direct calls.
! Nothing is finalized, deallocated or default-initialized on entry.
module traits_receiver_intent_04_oracle_m
    implicit none
    integer :: finals = 0
    type, abstract :: IOpen
    contains
        procedure(peek_iface), deferred :: peek
        procedure(touch_iface), deferred, pass(self) :: touch
    end type
    abstract interface
        integer function peek_iface(self)
            import :: IOpen
            class(IOpen) :: self
        end function
        subroutine touch_iface(by, self)
            import :: IOpen
            integer, intent(in) :: by
            class(IOpen) :: self
        end subroutine
    end interface
    type, extends(IOpen) :: Cell
        integer :: n = 0
        integer :: tag = 5
        integer, allocatable :: data(:)
    contains
        procedure :: peek => cell_peek
        procedure, pass(self) :: touch => cell_touch
        final :: cell_final
    end type
contains
    integer function cell_peek(self)
        class(Cell) :: self
        cell_peek = self%n
    end function
    subroutine cell_touch(by, self)
        integer, intent(in) :: by
        class(Cell) :: self
        if (self%tag /= 9 .or. .not. allocated(self%data)) error stop 90
        self%n = self%n + by
        self%data(1) = self%n
    end subroutine
    subroutine cell_final(self)
        type(Cell), intent(inout) :: self
        finals = finals + 1
    end subroutine
    integer function observe(v)
        class(IOpen), intent(in) :: v
        observe = v%peek()
    end function
    subroutine nudge(v, by)
        class(IOpen), intent(inout) :: v
        integer, intent(in) :: by
        call v%touch(by=by)
    end subroutine
    subroutine nudge_generic(x, by)
        class(IOpen), intent(inout) :: x
        integer, intent(in) :: by
        call x%touch(by)
    end subroutine
    integer function observe_generic(x)
        class(IOpen), intent(in) :: x
        observe_generic = x%peek()
    end function
end module

program traits_receiver_intent_04_oracle
    use traits_receiver_intent_04_oracle_m
    implicit none
    type(Cell) :: c
    class(IOpen), allocatable :: owner
    c%tag = 9
    allocate(c%data(1))
    call nudge(c, 2)
    if (c%n /= 2 .or. c%data(1) /= 2 .or. observe(c) /= 2) error stop 1
    call nudge_generic(c, 3)
    if (observe_generic(c) /= 5 .or. c%data(1) /= 5) error stop 2
    call c%touch(4)
    if (c%peek() /= 9 .or. c%tag /= 9) error stop 3
    allocate(owner, source=c)
    call nudge(owner, 1)
    if (owner%peek() /= 10 .or. observe(owner) /= 10 .or. c%n /= 9) error stop 4
    if (finals /= 0) error stop 5
    deallocate(owner)
    if (finals /= 1) error stop 6
    print '(a)', 'traits_receiver_intent_04 ok'
end program
