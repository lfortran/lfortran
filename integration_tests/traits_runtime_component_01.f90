module traits_runtime_component_01_extension_mod
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

    subroutine clear_holder(x)
        type(Holder), intent(out) :: x
        if (allocated(x%item)) error stop "INTENT(OUT) left item allocated on entry"
    end subroutine clear_holder

    subroutine require(ok, message)
        logical, intent(in) :: ok
        character(*), intent(in) :: message
        if (.not. ok) error stop message
    end subroutine require

end module traits_runtime_component_01_extension_mod

program traits_runtime_component_01_extension
    use traits_runtime_component_01_extension_mod
    implicit none

    type(Holder) :: h, h_copy, empty
    type(Payload) :: seed
    type(Alternate) :: alternate_seed

    seed%n = 7
    seed%identity = 101
    alternate_seed%n = 4
    alternate_seed%identity = 202

    call require(.not. allocated(h%item), "default component must be unallocated")

    allocate(h%item, source=seed)
    allocation_events = allocation_events + 1
    call require(allocation_events == 1, "unexpected initial allocation count")
    call require(allocated(h%item), "allocate(source=concrete) did not allocate")
    call require(contract_read(h%item) == 7, "initial component dispatch mismatch")
    select type (p => h%item)
    type is (Payload)
        call require(p%identity == 101, "selected concrete identity mismatch")
    class default
        error stop "initial component has the wrong dynamic type"
    end select

    h_copy = h
    allocation_events = allocation_events + 1
    call require(allocation_events == 2, "unexpected parent-copy allocation count")
    call require(allocated(h_copy%item), "parent assignment did not allocate copy")
    call require(contract_read(h_copy%item) == 7, "copied component dispatch mismatch")

    select type (p => h%item)
    type is (Payload)
        p%n = 9
    class default
        error stop "source component lost its concrete type"
    end select
    call require(contract_read(h%item) == 9, "source mutation did not stick")
    call require(contract_read(h_copy%item) == 7, "parent assignment aliased payload")

    ! F2023 containing-object assignment snapshots expr, then replaces the
    ! noncoarray allocatable component; the old Payload is finalized once.
    h = h
    allocation_events = allocation_events + 1
    call require(allocation_events == 3, "unexpected self-assignment allocation count")
    call require(contract_read(h%item) == 9, "self-assignment changed the source")
    call require(contract_read(h_copy%item) == 7, "self-assignment changed the copy")
    call require(payload_final_calls == 1, &
        "self-assignment must finalize the old payload exactly once")
    call require(alternate_final_calls == 0, &
        "self-assignment finalized an unexpected alternate")
    call require(double_final_calls == 0, "self-assignment caused double FINAL")
    call require(final_sum == 9, "self-assignment finalized the wrong payload value")

    h_copy = empty
    call require(.not. allocated(h_copy%item), &
        "unallocated RHS component must deallocate the LHS component")
    call require(payload_final_calls == 2, &
        "unallocated RHS must finalize the old payload exactly once")
    call require(final_sum == 16, "unallocated RHS finalized the wrong payload value")
    call require(double_final_calls == 0, "payload was finalized twice")

    h%item = alternate_seed
    allocation_events = allocation_events + 1
    call require(allocation_events == 4, "unexpected dynamic-replacement allocation count")
    call require(contract_read(h%item) == 40, "dynamic type change dispatch mismatch")
    select type (p => h%item)
    type is (Alternate)
        call require(p%identity == 202, "alternate concrete identity mismatch")
    class default
        error stop "dynamic type change did not install alternate"
    end select
    call require(payload_final_calls == 3, &
        "dynamic type change must finalize the old payload exactly once")
    call require(final_sum == 25, &
        "dynamic type change finalized the wrong payload value")

    deallocate(h%item)
    call require(.not. allocated(h%item), "explicit deallocate left component allocated")
    call require(alternate_final_calls == 1, &
        "explicit deallocate must finalize alternate exactly once")
    call require(final_sum == 29, &
        "explicit deallocate finalized the wrong alternate value")
    call require(double_final_calls == 0, "explicit deallocate caused double FINAL")

    block
        type(Holder) :: scoped
        allocate(scoped%item, source=seed)
        allocation_events = allocation_events + 1
        call require(allocation_events == 5, "unexpected scope allocation count")
        call require(contract_read(scoped%item) == 7, "scoped component dispatch mismatch")
    end block
    call require(payload_final_calls == 4, &
        "scope cleanup must finalize the scoped payload exactly once")
    call require(final_sum == 36, "scope cleanup finalized the wrong payload value")

    allocate(h%item, source=seed)
    allocation_events = allocation_events + 1
    call require(allocation_events == 6, "unexpected INTENT(OUT) allocation count")
    call clear_holder(h)
    call require(.not. allocated(h%item), "INTENT(OUT) did not clear the component")
    call require(payload_final_calls == 5, &
        "INTENT(OUT) containing-object cleanup must finalize exactly once")
    call require(final_sum == 43, &
        "INTENT(OUT) cleanup finalized the wrong payload value")

    call require(allocation_events == 6, "unexpected component allocation count")
    call require(payload_final_calls == 5, "unexpected payload FINAL count")
    call require(alternate_final_calls == 1, "unexpected alternate FINAL count")
    call require(double_final_calls == 0, "a payload was finalized more than once")
    call require(final_sum == 43, "unexpected finalized payload values")
end program traits_runtime_component_01_extension
