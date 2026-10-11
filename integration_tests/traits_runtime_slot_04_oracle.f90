module traits_runtime_slot_04_oracle_m
    implicit none
    type :: Payload
        integer :: n = 17
    end type
contains
    pure logical function has_value(slot)
        class(Payload), allocatable, intent(in) :: slot
        has_value = allocated(slot)
    end function
    pure subroutine through_pointer(slot, flag)
        class(Payload), allocatable, intent(in) :: slot
        logical, intent(out) :: flag
        block
            procedure(has_value), pointer :: query
            query => has_value
            associate(marker => 1)
                flag = query(slot)
            end associate
        end block
    end subroutine
    pure subroutine through_dummy(query, slot, flag)
        procedure(has_value) :: query
        class(Payload), allocatable, intent(in) :: slot
        logical, intent(out) :: flag
        flag = query(slot)
    end subroutine
end module

program traits_runtime_slot_04_oracle
    use traits_runtime_slot_04_oracle_m
    implicit none
    class(Payload), allocatable :: owner
    logical :: flag
    call through_pointer(owner, flag)
    if (flag) error stop 1
    call through_dummy(has_value, owner, flag)
    if (flag) error stop 2
    allocate(Payload :: owner)
    call through_pointer(owner, flag)
    if (.not. flag) error stop 3
    call through_dummy(has_value, owner, flag)
    if (.not. flag) error stop 4
    deallocate(owner)
end program
