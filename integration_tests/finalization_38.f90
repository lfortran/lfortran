module finalization_38_m
    implicit none
    logical :: body_finished = .false.
    integer :: finals = 0
    type :: Leaf
        integer :: n = 17
        integer, allocatable :: data(:)
    contains
        final :: finish
    end type
    type :: Envelope
        type(Leaf), allocatable :: leaf
    end type
    type(Envelope) :: module_value
contains
    impure elemental subroutine finish(self)
        type(Leaf), intent(inout) :: self
        if (body_finished) error stop 91
        if (self%n /= 17) error stop 92
        finals = finals + 1
    end subroutine
    subroutine initialize(value)
        class(Envelope), intent(inout) :: value
        allocate(value%leaf)
        allocate(value%leaf%data(3))
        value%leaf%data = 5
    end subroutine
    subroutine local_cleanup()
        type(Envelope) :: local
        call initialize(local)
        block
            type(Envelope) :: nested
            call initialize(nested)
        end block
        if (finals /= 1) error stop 1
    end subroutine
    subroutine saved_storage()
        type(Envelope), save :: saved
        if (.not. allocated(saved%leaf)) call initialize(saved)
    end subroutine
end module

program finalization_38
    use finalization_38_m
    implicit none
    type(Envelope) :: enclosing, fixed(2), explicit_value
    type(Envelope), allocatable :: allocated_scalar, allocated_array(:)
    class(Envelope), allocatable :: polymorphic, polymorphic_array(:)
    class(*), allocatable :: erased
    integer :: i

    call local_cleanup()
    if (finals /= 2) error stop 2
    call initialize(explicit_value)
    deallocate(explicit_value%leaf)
    if (finals /= 3) error stop 3
    call saved_storage()
    call saved_storage()
    if (finals /= 3) error stop 4

    call initialize(enclosing)
    call initialize(module_value)
    allocate(allocated_scalar, allocated_array(2))
    call initialize(allocated_scalar)
    allocate(Envelope :: polymorphic, polymorphic_array(2))
    call initialize(polymorphic)
    do i = 1, 2
        call initialize(fixed(i))
        call initialize(allocated_array(i))
        call initialize(polymorphic_array(i))
    end do
    allocate(Envelope :: erased)
    select type(erased)
    type is(Envelope)
        call initialize(erased)
    end select
    if (finals /= 3) error stop 5
    body_finished = .true.
end program
