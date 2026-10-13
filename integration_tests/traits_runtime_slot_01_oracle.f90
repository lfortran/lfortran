module r2b_slots_oracle_m
    implicit none
    type, abstract :: IValue
    contains
        procedure(value_interface), deferred :: value
    end type
    abstract interface
        integer function value_interface(self)
            import IValue
            class(IValue), intent(in) :: self
        end function
    end interface
    type, extends(IValue) :: ValueA
        integer, allocatable :: data(:)
        integer, pointer :: link => null()
    contains
        procedure :: value => value_a
        final :: finalize_a
    end type
    type, extends(IValue) :: ValueB
        integer :: factor = 4
        integer :: payload = 7
    contains
        procedure :: value => value_b
        final :: finalize_b
    end type
    type(ValueA) :: template_a
    type(ValueB) :: template_b
    integer :: a_finals = 0, b_finals = 0
    integer :: a_finalized_values = 0, b_finalized_values = 0
contains
    integer function value_a(self)
        class(ValueA), intent(in) :: self
        value_a = self%data(1) + self%link
    end function
    integer function value_b(self)
        class(ValueB), intent(in) :: self
        value_b = self%factor * self%payload + 1
    end function
    subroutine finalize_a(self)
        type(ValueA), intent(inout) :: self
        a_finals = a_finals + 1
        if (.not. allocated(self%data)) error stop 801
        a_finalized_values = a_finalized_values + self%data(1)
    end subroutine
    subroutine finalize_b(self)
        type(ValueB), intent(inout) :: self
        b_finals = b_finals + 1
        b_finalized_values = b_finalized_values + self%value()
    end subroutine
    subroutine check_slot(slot, is_allocated, expected)
        class(IValue), allocatable, intent(in) :: slot
        logical, intent(in) :: is_allocated
        integer, intent(in) :: expected
        if (allocated(slot) .neqv. is_allocated) error stop 802
        if (is_allocated) then
            if (slot%value() /= expected) error stop 803
            call observe(slot, expected)
        end if
    end subroutine
    subroutine observe(object, expected)
        class(IValue), intent(in) :: object
        integer, intent(in) :: expected
        if (object%value() /= expected) error stop 804
    end subroutine
    subroutine install_a(slot, n, link, expected_a, expected_b)
        class(IValue), allocatable, intent(out) :: slot
        integer, intent(in) :: n, expected_a, expected_b
        integer, target, intent(in) :: link
        if (allocated(slot)) error stop 805
        if (a_finals /= expected_a .or. b_finals /= expected_b) error stop 806
        if (.not. allocated(template_a%data)) allocate(template_a%data(2))
        template_a%data = [n, n + 1]
        template_a%link => link
        allocate(slot, source=template_a)
    end subroutine
    subroutine copy_slot(source, destination)
        class(IValue), allocatable, intent(in) :: source
        class(IValue), allocatable, intent(out) :: destination
        if (.not. allocated(source)) error stop 807
        if (allocated(destination)) error stop 808
        destination = source
    end subroutine
    subroutine replace_b(slot, expected_old)
        class(IValue), allocatable, intent(inout) :: slot
        integer, intent(in) :: expected_old
        if (.not. allocated(slot)) error stop 809
        if (slot%value() /= expected_old) error stop 810
        slot = template_b
    end subroutine
    subroutine forward_b(slot, expected_old)
        class(IValue), allocatable, intent(inout) :: slot
        integer, intent(in) :: expected_old
        call replace_b(slot, expected_old)
    end subroutine
    subroutine release_out(slot, expected_a, expected_b)
        class(IValue), allocatable, intent(out) :: slot
        integer, intent(in) :: expected_a, expected_b
        if (allocated(slot)) error stop 811
        if (a_finals /= expected_a .or. b_finals /= expected_b) error stop 812
    end subroutine
    subroutine assign_without_intent(slot)
        class(IValue), allocatable :: slot
        slot = template_b
    end subroutine
    subroutine local_owner(link)
        integer, target, intent(in) :: link
        class(IValue), allocatable :: slot
        call install_a(slot, 43, link, 3, 3)
        call check_slot(slot, .true., 43 + link)
    end subroutine
end module

program oracle_allocatable_slots
    use r2b_slots_oracle_m
    implicit none
    class(IValue), allocatable :: owner, copy
    integer, target :: link = 3

    call check_slot(owner, .false., 0)
    call install_a(owner, 17, link, 0, 0)
    call copy_slot(owner, copy)
    call check_slot(owner, .true., 20)
    call check_slot(copy, .true., 20)
    if (a_finals /= 0 .or. b_finals /= 0) error stop 813
    link = 5
    call check_slot(copy, .true., 22)
    call install_a(owner, 31, link, 1, 0)
    call check_slot(owner, .true., 36)
    call check_slot(copy, .true., 22)
    call replace_b(owner, 36)
    call check_slot(owner, .true., 29)
    call release_out(owner, 2, 1)
    call check_slot(owner, .false., 0)
    call forward_b(copy, 22)
    call check_slot(copy, .true., 29)
    call release_out(copy, 3, 2)
    call check_slot(copy, .false., 0)
    call assign_without_intent(owner)
    call check_slot(owner, .true., 29)
    deallocate(owner)
    if (a_finals /= 3 .or. b_finals /= 3) error stop 814
    if (a_finalized_values /= 65 .or. b_finalized_values /= 87) error stop 815
    call local_owner(link)
    if (a_finals /= 4 .or. a_finalized_values /= 108) error stop 816
    if (b_finals /= 3 .or. b_finalized_values /= 87) error stop 817
    deallocate(template_a%data)
    nullify(template_a%link)
end program
