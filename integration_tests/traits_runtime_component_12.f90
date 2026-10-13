module traits_runtime_component_12_m
    implicit none
    integer :: finals = 0, final_sum = 0, assignments = 0
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
    type :: Tagged
        class(IValue), allocatable :: item
        integer :: tag = 0
    contains
        procedure :: assign_tag
        generic :: assignment(=) => assign_tag
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
    pure subroutine assign_tag(lhs, rhs)
        class(Tagged), intent(inout) :: lhs
        class(Tagged), intent(in) :: rhs
        lhs%tag = rhs%tag
    end subroutine
    subroutine overwrite(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        x = y
    end subroutine
    subroutine overwrite_target(x, y)
        type(Holder), pointer, intent(inout) :: x
        type(Holder), intent(in) :: y
        x = y
    end subroutine
    subroutine dispose(x)
        type(Holder), pointer, intent(inout) :: x
        deallocate(x)
    end subroutine
    subroutine wrapper(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call overwrite(x, y)
    end subroutine
    pure subroutine copy_tag(x, y)
        type(Tagged), intent(inout) :: x
        type(Tagged), intent(in) :: y
        x = y
    end subroutine
    pure integer function observe(x)
        type(Holder), intent(in) :: x
        observe = x%item%value()
    end function
    subroutine check(ok, code)
        logical, intent(in) :: ok
        integer, intent(in) :: code
        if (.not. ok) error stop code
    end subroutine
end module

program traits_runtime_component_12
    use traits_runtime_component_12_m
    implicit none
    type(Holder) :: x, y
    type(Holder), pointer :: p
    type(Tagged) :: s, t
    type(Payload) :: seed

    seed%n = 4
    x%item = seed
    seed%n = 9
    y%item = seed
    call wrapper(x, y)
    call check(finals == 1 .and. final_sum == 4, 1)
    call check(observe(x) == 9 .and. observe(y) == 9, 2)

    allocate(p)
    seed%n = 2
    p%item = seed
    call overwrite_target(p, x)
    call check(finals == 2 .and. final_sum == 6, 3)
    call check(observe(p) == 9, 4)
    call dispose(p)
    call check(finals == 3 .and. final_sum == 15, 5)

    seed%n = 1
    s%item = seed
    s%tag = 3
    t%tag = 8
    call copy_tag(s, t)
    call check(s%tag == 8 .and. s%item%value() == 1, 6)
    call check(finals == 3, 7)

    deallocate(x%item, y%item, s%item)
    call check(finals == 6 .and. final_sum == 34, 8)
end program
