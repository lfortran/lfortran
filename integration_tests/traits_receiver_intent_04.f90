! LFortran traits extension test; GFortran does not accept this syntax.
! Receivers without INTENT (TD-36) keep the ordinary meaning of a dummy
! without INTENT: neither intent(in) nor intent(inout) is substituted.
! peek reads its receiver, so a read-only trait object can call it; touch
! modifies its receiver through an intent(inout) trait object, a static
! generic call and direct calls. Nothing is finalized, deallocated or
! default-initialized on entry: touch sees the object's current tag and
! allocated data. traits_receiver_intent_04_oracle.f90 is the
! standard-Fortran counterpart.
module traits_receiver_intent_04_m
    implicit none
    integer :: finals = 0
    abstract interface :: IOpen
        integer function peek(self)
            type(itself), pass :: self
        end function
        subroutine touch(by, self)
            integer, intent(in) :: by
            type(itself), pass :: self
        end subroutine
    end interface
    type :: Cell
        integer :: n = 0
        integer :: tag = 5
        integer, allocatable :: data(:)
    contains
        final :: cell_final
    end type
    implements IOpen :: Cell
        procedure, pass :: peek => cell_peek
        procedure, pass :: touch => cell_touch
    end implements
contains
    integer function cell_peek(self)
        class(Cell) :: self
        cell_peek = self%n
    end function
    subroutine cell_touch(self, by)
        class(Cell) :: self
        integer, intent(in) :: by
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
    subroutine nudge_generic{IOpen :: T}(x, by)
        type(T), intent(inout) :: x
        integer, intent(in) :: by
        call x%touch(by)
    end subroutine
    integer function observe_generic{IOpen :: T}(x)
        type(T), intent(in) :: x
        observe_generic = x%peek()
    end function
end module

program traits_receiver_intent_04
    use traits_receiver_intent_04_m
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
