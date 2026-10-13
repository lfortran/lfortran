module traits_runtime_component_15_m
    implicit none
    integer :: assignments = 0, finals = 0, final_sum = 0, holder_finals = 0
    integer :: seen(8) = 0
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
        type(Part) :: direct
        class(IValue), allocatable :: item
    contains
        final :: finish_holder
    end type
contains
    subroutine assign_part(lhs, rhs)
        class(Part), intent(inout) :: lhs
        class(Part), intent(in) :: rhs
        assignments = assignments + 1
        seen(assignments) = lhs%n
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
    ! Finalizing the variable of an assignment changes the parts of the old
    ! value, but not those of the value being assigned.
    subroutine finish_holder(self)
        type(Holder), intent(inout) :: self
        holder_finals = holder_finals + 1
        self%direct%n = -50
        if (allocated(self%item)) then
            select type (item => self%item)
            type is (Payload)
                item%part%n = -60
            end select
        end if
    end subroutine
    ! The variable's direct part is assigned after its finalization, and the
    ! payload's part in a fresh, default-initialized payload.
    subroutine check_assigned(step, expected_holder_finals, expected_finals, expected_sum)
        integer, intent(in) :: step, expected_holder_finals, expected_finals, expected_sum
        if (holder_finals /= expected_holder_finals) error stop step
        if (finals /= expected_finals .or. final_sum /= expected_sum) error stop step + 1
        if (assignments /= 2) error stop step + 2
        if (count(seen(1:2) == -50) /= 1 .or. count(seen(1:2) == -2) /= 1) error stop step + 3
    end subroutine
    subroutine reset()
        assignments = 0
        seen = 0
        finals = 0
        final_sum = 0
        holder_finals = 0
    end subroutine
end module

program traits_runtime_component_15
    use traits_runtime_component_15_m
    implicit none
    type(Payload) :: seed
    type(Holder), allocatable :: x, y, z

    allocate(x, y, z)
    x%direct%n = 5
    seed%part%n = 7
    allocate(x%item, source=seed)
    call reset()

    x = x
    call check_assigned(10, 1, 1, -60)
    if (x%direct%n /= 105 .or. x%item%value() /= 107) error stop 14

    y%direct%n = 9
    seed%part%n = 11
    allocate(y%item, source=seed)
    call reset()
    y = x
    call check_assigned(20, 1, 1, -60)
    if (y%direct%n /= 205 .or. y%item%value() /= 207) error stop 24
    if (x%direct%n /= 105 .or. x%item%value() /= 107) error stop 25

    call reset()
    z = x
    call check_assigned(30, 1, 0, 0)
    if (z%direct%n /= 205 .or. z%item%value() /= 207) error stop 34

    deallocate(x%item)
    call reset()
    y = x
    if (holder_finals /= 1 .or. finals /= 1 .or. final_sum /= -60) error stop 40
    if (assignments /= 1 .or. seen(1) /= -50) error stop 41
    if (y%direct%n /= 205 .or. allocated(y%item)) error stop 42

    call reset()
    deallocate(x, y, z)
    if (holder_finals /= 3 .or. finals /= 1 .or. final_sum /= -60) error stop 50
end program
