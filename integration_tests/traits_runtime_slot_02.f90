module traits_runtime_slot_02_contracts
    implicit none
    abstract interface :: IValue
        pure function value() result(r)
            integer :: r
        end function
    end interface
end module

module traits_runtime_slot_02_aliases
    use traits_runtime_slot_02_contracts, only: Renamed => IValue
    implicit none
end module

module traits_runtime_slot_02_m
    use traits_runtime_slot_02_contracts, only: IValue
    use traits_runtime_slot_02_aliases, only: Again => Renamed
    implicit none
    type :: Payload
        integer :: n = 17
    contains
        final :: finish
    end type
    implements IValue :: Payload
        procedure, pass :: value => read_payload
    end implements
    integer :: finals = 0
contains
    pure integer function read_payload(self)
        class(Payload), intent(in) :: self
        read_payload = self%n
    end function
    subroutine finish(self)
        type(Payload), intent(inout) :: self
        if (self%n /= 17) error stop 1
        finals = finals + 1
        self%n = -1
    end subroutine
    pure logical function has_value(slot)
        class(Again) :: slot
        allocatable :: slot
        intent(in) :: slot
        has_value = allocated(slot)
    end function
    subroutine create(slot, expected_finals)
        class(Again) :: slot
        allocatable :: slot
        intent(out) :: slot
        integer, intent(in) :: expected_finals
        if (allocated(slot)) error stop 2
        if (finals /= expected_finals) error stop 3
        allocate(Payload :: slot)
    end subroutine
    subroutine observe(view)
        class(IValue), intent(in) :: view
        if (view%value() /= 17) error stop 4
    end subroutine
    subroutine nested(slot)
        class(Again) :: slot
        intent(inout) :: slot
        allocatable :: slot
        block
            call observe(slot)
        end block
        return
    end subroutine
    subroutine forward(slot)
        class(IValue) :: slot
        allocatable :: slot
        call nested(slot)
    end subroutine
    subroutine local_with_return()
        class(IValue), allocatable :: local
        call create(local, 0)
        call forward(local)
        return
    end subroutine
    subroutine clear(slot, expected_finals)
        class(IValue), allocatable, intent(out) :: slot
        integer, intent(in) :: expected_finals
        if (allocated(slot)) error stop 5
        if (finals /= expected_finals) error stop 6
    end subroutine
end module

program traits_runtime_slot_02
    use traits_runtime_slot_02_m
    implicit none
    class(Again), allocatable :: owner
    if (has_value(owner)) error stop 7
    call create(owner, 0)
    call forward(owner)
    if (finals /= 0) error stop 8
    call local_with_return()
    if (finals /= 1) error stop 9
    call observe(owner)
    call create(owner, 2)
    if (.not. has_value(owner)) error stop 10
    call clear(owner, 3)
    if (has_value(owner)) error stop 11
    if (finals /= 3) error stop 12
end program
