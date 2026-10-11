module traits_runtime_component_05_m
    implicit none
    integer :: assignments = 0, finals = 0, final_sum = 0
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Part
        integer :: n = -2
    contains
        procedure :: assign_part
        generic :: assignment(=) => assign_part
    end type
    type :: Payload
        type(Part) :: part
    contains
        final :: finish_payload
    end type
    implements IValue :: Payload
        procedure :: value => payload_value
    end implements
    type :: Holder
        class(IValue), allocatable :: item
    end type
contains
    subroutine assign_part(lhs, rhs)
        class(Part), intent(inout) :: lhs
        class(Part), intent(in) :: rhs
        if (lhs%n /= -2) error stop 1
        assignments = assignments + 1
        lhs%n = rhs%n + 100
    end subroutine
    integer function payload_value(self)
        type(Payload), intent(in) :: self
        payload_value = self%part%n
    end function
    subroutine finish_payload(self)
        type(Payload), intent(inout) :: self
        finals = finals + 1
        final_sum = final_sum + self%part%n
        self%part%n = -900
    end subroutine
end module

program traits_runtime_component_05
    use traits_runtime_component_05_m
    implicit none
    type(Payload) :: seed
    type(Holder) :: source, copy
    seed%part%n = 7
    allocate(source%item, source=seed)
    if (assignments /= 0 .or. finals /= 0) error stop 2
    copy = source
    if (assignments /= 1 .or. finals /= 0) error stop 3
    if (copy%item%value() /= 107) error stop 4
    copy = copy
    if (assignments /= 2 .or. finals /= 1) error stop 5
    if (copy%item%value() /= 207) error stop 6
    if (final_sum /= 107 .or. source%item%value() /= 7) error stop 7
    deallocate(copy%item, source%item)
    if (finals /= 3 .or. final_sum /= 321) error stop 8
end program
