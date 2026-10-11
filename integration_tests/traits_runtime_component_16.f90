module traits_runtime_component_16_m
    implicit none
    integer :: finals = 0, final_sum = 0
    abstract interface :: IValue
        pure integer function value()
        end function
    end interface
    type :: Payload
        integer :: n = 7
    contains
        final :: finish_payload
    end type
    implements IValue :: Payload
        procedure :: value => payload_value
    end implements
    type :: Holder
        class(IValue), allocatable :: item
    end type
    ! Copying a Tagged through its defined assignment does no lifecycle work.
    type :: Tagged
        type(Holder) :: h
        integer :: tag = 0
    contains
        procedure :: assign_tag
        generic :: assignment(=) => assign_tag
    end type
    ! Copying a Box through its defined assignment copies its holder.
    type :: Box
        type(Holder) :: h
    contains
        procedure :: assign_box
        generic :: assignment(=) => assign_box
    end type
    type :: Copier
        integer :: k = 0
    contains
        procedure :: copy_into
    end type
contains
    pure integer function payload_value(self)
        type(Payload), intent(in) :: self
        payload_value = self%n
    end function
    subroutine finish_payload(self)
        type(Payload), intent(inout) :: self
        finals = finals + 1
        final_sum = final_sum + self%n
        self%n = -900
    end subroutine
    ! Each procedure below calls procedures defined after it.
    pure integer function observe(x)
        type(Holder), intent(in) :: x
        observe = peek(x) + 1
    end function
    pure subroutine copy_tag(x, y)
        type(Tagged), intent(inout) :: x
        type(Tagged), intent(in) :: y
        x = y
    end subroutine
    subroutine copy_later(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call relay(x, y)
    end subroutine
    subroutine copy_box(x, y)
        type(Box), intent(inout) :: x
        type(Box), intent(in) :: y
        x = y
    end subroutine
    subroutine relay(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call ping(x, y, 2)
    end subroutine
    recursive subroutine ping(x, y, n)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        integer, intent(in) :: n
        if (n > 0) call pong(x, y, n - 1)
    end subroutine
    recursive subroutine pong(x, y, n)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        integer, intent(in) :: n
        if (n == 0) then
            x = y
        else
            call ping(x, y, n)
        end if
    end subroutine
    pure integer function peek(x)
        type(Holder), intent(in) :: x
        peek = x%item%value()
    end function
    pure subroutine assign_tag(lhs, rhs)
        class(Tagged), intent(inout) :: lhs
        class(Tagged), intent(in) :: rhs
        lhs%tag = rhs%tag
    end subroutine
    subroutine assign_box(lhs, rhs)
        class(Box), intent(inout) :: lhs
        class(Box), intent(in) :: rhs
        lhs%h = rhs%h
    end subroutine
    subroutine copy_into(self, x, y)
        class(Copier), intent(in) :: self
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        x = y
    end subroutine
end module

program traits_runtime_component_16
    use traits_runtime_component_16_m
    implicit none
    type(Payload) :: seed
    type(Holder) :: p, q
    type(Tagged) :: a, b
    type(Box) :: c, d
    type(Copier) :: copying

    p%item = seed
    seed%n = 19
    q%item = seed
    finals = 0
    if (observe(q) /= 20 .or. finals /= 0) error stop 1

    call copy_later(p, q)
    if (finals /= 1 .or. final_sum /= 7 .or. p%item%value() /= 19) error stop 2

    seed%n = 23
    d%h%item = seed
    call copy_box(c, d)
    if (finals /= 1 .or. c%h%item%value() /= 23) error stop 3
    call copy_box(c, d)
    if (finals /= 2 .or. final_sum /= 30) error stop 4

    seed%n = 29
    q%item = seed
    if (finals /= 3 .or. final_sum /= 49) error stop 5
    call copying%copy_into(p, q)
    if (finals /= 4 .or. final_sum /= 68 .or. p%item%value() /= 29) error stop 6

    b%tag = 3
    b%h%item = seed
    call copy_tag(a, b)
    if (a%tag /= 3 .or. allocated(a%h%item) .or. finals /= 4) error stop 7
end program
