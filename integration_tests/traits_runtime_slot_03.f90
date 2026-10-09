module traits_runtime_slot_03_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Payload
        integer :: n = 17
    contains
        final :: finish
    end type
    implements IValue :: Payload
        procedure, pass :: value => get
    end implements
    integer :: finals = 0, total = 0
    type(Payload) :: replacement = Payload(31)
contains
    integer function get(self)
        class(Payload), intent(in) :: self
        get = self%n
    end function
    subroutine finish(self)
        type(Payload), intent(inout) :: self
        finals = finals + 1
        total = total + self%n
    end subroutine
    logical function has_value(slot, expected)
        class(IValue), allocatable, intent(in) :: slot
        integer, optional, intent(in) :: expected
        has_value = allocated(slot)
        if (present(expected)) then
            if (.not. has_value) error stop 1
            if (slot%value() /= expected) error stop 2
        end if
    end function
    integer function install(slot)
        class(IValue), allocatable, intent(out) :: slot
        if (allocated(slot)) error stop 3
        allocate(Payload :: slot)
        install = finals
    end function
    integer function update(slot)
        class(IValue), allocatable, intent(inout) :: slot
        slot = replacement
        update = slot%value()
    end function
    integer function release(slot)
        class(IValue), allocatable :: slot
        deallocate(slot)
        release = finals
    end function
    subroutine apply(reader, slot)
        procedure(has_value) :: reader
        class(IValue), allocatable, intent(in) :: slot
        if (.not. reader(slot, 17)) error stop 4
    end subroutine
    subroutine exercise()
        class(IValue), allocatable :: slot
        procedure(has_value), pointer :: query
        procedure(install), pointer :: create
        procedure(update), pointer :: replace
        procedure(release), pointer :: drop
        query => has_value
        create => install
        replace => update
        drop => release
        if (query(slot)) error stop 5
        if (create(slot) /= 0) error stop 6
        if (.not. query(slot, 17)) error stop 7
        call apply(query, slot)
        if (replace(slot) /= 31) error stop 8
        if (finals /= 1 .or. total /= 17) error stop 9
        if (drop(slot) /= 2) error stop 10
        if (query(slot)) error stop 11
        if (total /= 48) error stop 12
    end subroutine
end module

program traits_runtime_slot_03
    use traits_runtime_slot_03_m
    call exercise()
end program
