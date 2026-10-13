module traits_runtime_component_02_extension_mod
    implicit none

    integer :: allocation_events = 0
    integer :: payload_final_calls = 0
    integer :: alternate_final_calls = 0
    integer :: double_final_calls = 0
    integer :: final_sum = 0

    abstract interface :: IValue
        integer function value()
        end function value
    end interface

    type :: Payload
        integer :: n = 0
        integer :: identity = 0
    contains
        final :: finish_payload
    end type Payload

    implements IValue :: Payload
        procedure, pass :: value => payload_value
    end implements

    type :: Alternate
        integer :: n = 0
        integer :: identity = 0
    contains
        final :: finish_alternate
    end type Alternate

    implements IValue :: Alternate
        procedure, pass :: value => alternate_value
    end implements

    type :: Holder
        class(IValue), allocatable :: item
    end type Holder

    type :: Wrapper
        type(Holder) :: child
    end type Wrapper

contains

    integer function payload_value(self)
        class(Payload), intent(in) :: self
        payload_value = self%n
    end function payload_value

    integer function alternate_value(self)
        class(Alternate), intent(in) :: self
        alternate_value = 10 * self%n
    end function alternate_value

    integer function contract_read(x)
        class(IValue), intent(in) :: x
        contract_read = x%value()
    end function contract_read

    subroutine finish_payload(self)
        type(Payload), intent(inout) :: self
        if (self%n == -777777) then
            double_final_calls = double_final_calls + 1
        else
            payload_final_calls = payload_final_calls + 1
            final_sum = final_sum + self%n
            self%n = -777777
        end if
    end subroutine finish_payload

    subroutine finish_alternate(self)
        type(Alternate), intent(inout) :: self
        if (self%n == -888888) then
            double_final_calls = double_final_calls + 1
        else
            alternate_final_calls = alternate_final_calls + 1
            final_sum = final_sum + self%n
            self%n = -888888
        end if
    end subroutine finish_alternate

    subroutine borrow_and_check(x, expected_value, expected_identity)
        type(Wrapper), intent(in) :: x
        integer, intent(in) :: expected_value
        integer, intent(in) :: expected_identity
        call require(contract_read(x%child%item) == expected_value, &
            "nested component dispatch mismatch")
        select type (p => x%child%item)
        type is (Payload)
            call require(p%identity == expected_identity, &
                "selected payload identity mismatch")
        type is (Alternate)
            call require(p%identity == expected_identity, &
                "selected alternate identity mismatch")
        class default
            error stop "borrowed component has an unexpected dynamic type"
        end select
    end subroutine borrow_and_check

    subroutine clear_wrapper(x)
        type(Wrapper), intent(out) :: x
        if (allocated(x%child%item)) then
            error stop "INTENT(OUT) left nested item allocated on entry"
        end if
    end subroutine clear_wrapper

    subroutine require(ok, message)
        logical, intent(in) :: ok
        character(*), intent(in) :: message
        if (.not. ok) error stop message
    end subroutine require

end module traits_runtime_component_02_extension_mod

program traits_runtime_component_02_extension
    use traits_runtime_component_02_extension_mod
    implicit none

    type(Wrapper) :: outer, copied, empty
    type(Payload) :: seed
    type(Alternate) :: alternate_seed

    seed%n = 3
    seed%identity = 301
    alternate_seed%n = 5
    alternate_seed%identity = 302

    call require(.not. allocated(outer%child%item), &
        "nested default component must be unallocated")

    allocate(outer%child%item, source=seed)
    allocation_events = allocation_events + 1
    call require(allocation_events == 1, "unexpected initial nested allocation count")
    call borrow_and_check(outer, 3, 301)

    copied = outer
    allocation_events = allocation_events + 1
    call require(allocation_events == 2, "unexpected nested parent-copy allocation count")
    call borrow_and_check(copied, 3, 301)

    select type (p => outer%child%item)
    type is (Payload)
        p%n = 10
    class default
        error stop "outer component lost its payload type"
    end select
    call borrow_and_check(outer, 10, 301)
    call borrow_and_check(copied, 3, 301)

    outer%child%item = alternate_seed
    allocation_events = allocation_events + 1
    call require(allocation_events == 3, "unexpected nested dynamic allocation count")
    call borrow_and_check(outer, 50, 302)
    call require(payload_final_calls == 1, &
        "dynamic type replacement must finalize payload exactly once")
    call require(final_sum == 10, &
        "dynamic type replacement finalized the wrong payload value")

    outer = copied
    allocation_events = allocation_events + 1
    call require(allocation_events == 4, "unexpected nested parent-replacement allocation count")
    call borrow_and_check(outer, 3, 301)
    call require(alternate_final_calls == 1, &
        "parent assignment must finalize the replaced alternate exactly once")
    call require(final_sum == 15, &
        "parent assignment finalized the wrong alternate value")

    ! F2023 containing-object assignment snapshots expr, then replaces the
    ! nested noncoarray allocatable component; the old Payload is finalized.
    copied = copied
    allocation_events = allocation_events + 1
    call require(allocation_events == 5, "unexpected nested self-assignment allocation count")
    call borrow_and_check(copied, 3, 301)
    call require(payload_final_calls == 2, &
        "nested self-assignment must finalize the old payload exactly once")
    call require(alternate_final_calls == 1, &
        "nested self-assignment changed the alternate FINAL count")
    call require(double_final_calls == 0, "nested self-assignment caused double FINAL")
    call require(final_sum == 18, &
        "nested self-assignment finalized the wrong payload value")

    deallocate(copied%child%item)
    call require(.not. allocated(copied%child%item), &
        "explicit nested deallocate failed")
    call require(payload_final_calls == 3, &
        "explicit nested deallocate must finalize exactly once")
    call require(final_sum == 21, &
        "explicit nested deallocate finalized the wrong payload value")

    allocate(copied%child%item, source=seed)
    allocation_events = allocation_events + 1
    call require(allocation_events == 6, "unexpected nested RHS allocation count")
    copied = empty
    call require(.not. allocated(copied%child%item), &
        "unallocated nested RHS did not clear the LHS")
    call require(payload_final_calls == 4, &
        "unallocated nested RHS must finalize exactly once")
    call require(final_sum == 24, &
        "unallocated nested RHS finalized the wrong payload value")

    call clear_wrapper(outer)
    call require(.not. allocated(outer%child%item), &
        "INTENT(OUT) did not clear the nested component")
    call require(payload_final_calls == 5, &
        "INTENT(OUT) nested cleanup must finalize exactly once")
    call require(final_sum == 27, &
        "INTENT(OUT) nested cleanup finalized the wrong payload value")

    block
        type(Wrapper) :: scoped
        allocate(scoped%child%item, source=seed)
        allocation_events = allocation_events + 1
        call require(allocation_events == 7, "unexpected nested scope allocation count")
        call borrow_and_check(scoped, 3, 301)
    end block
    call require(payload_final_calls == 6, &
        "nested scope cleanup must finalize exactly once")
    call require(final_sum == 30, "nested scope cleanup finalized the wrong payload value")

    call require(allocation_events == 7, "unexpected nested allocation count")
    call require(payload_final_calls == 6, "unexpected nested payload FINAL count")
    call require(alternate_final_calls == 1, "unexpected nested alternate FINAL count")
    call require(double_final_calls == 0, "nested payload was finalized twice")
    call require(final_sum == 30, "unexpected nested finalized payload values")
end program traits_runtime_component_02_extension
