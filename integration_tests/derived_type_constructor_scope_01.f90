module derived_type_constructor_scope_01_m
    implicit none
    integer :: finalizations = 0
    type :: base
        real :: learning_rate = 0.25
    end type
    type, extends(base) :: optimizer
        real :: momentum = 0
        real, allocatable :: velocity(:)
    contains
        procedure :: init
    end type
    type :: network
        class(base), allocatable :: state
    end type
    type :: watched
        real, allocatable :: data(:)
    contains
        final :: finish
    end type
contains
    subroutine init(self, n)
        class(optimizer), intent(inout) :: self
        integer, intent(in) :: n
        if (self%learning_rate /= 0.25 .or. self%momentum /= 0) error stop 1
        if (allocated(self%velocity)) error stop 2
        allocate(self%velocity(n))
        self%velocity = 4
        self%momentum = 2
    end subroutine

    subroutine update(net, enabled)
        type(network), intent(inout) :: net
        logical, intent(in) :: enabled
        if (enabled) then
            net%state = optimizer()
            select type (p => net%state)
            type is (optimizer)
                call p%init(3)
            class default
                error stop 3
            end select
        end if
    end subroutine

    subroutine from_source(net, source)
        type(network), intent(inout) :: net
        real, allocatable, intent(in) :: source(:)
        net%state = optimizer(momentum=0.5, velocity=source)
    end subroutine

    subroutine check_initialized(net)
        type(network), intent(in) :: net
        select type (p => net%state)
        type is (optimizer)
            if (p%learning_rate /= 0.25 .or. p%momentum /= 2) error stop 4
            if (.not. allocated(p%velocity)) error stop 5
            if (size(p%velocity) /= 3 .or. any(p%velocity /= 4)) error stop 6
        class default
            error stop 7
        end select
    end subroutine

    subroutine finish(self)
        type(watched), intent(inout) :: self
        if (.not. allocated(self%data)) error stop 8
        if (self%data(1) /= 17) error stop 9
        finalizations = finalizations + 1
    end subroutine
end module

program derived_type_constructor_scope_01
    use derived_type_constructor_scope_01_m
    implicit none
    type(network) :: net
    real, allocatable :: source(:)
    integer :: i

    call update(net, .false.)
    if (allocated(net%state)) error stop 10
    call update(net, .true.)
    do i = 1, 8
        call update(net, .false.)
        call check_initialized(net)
    end do
    call from_source(net, source)
    select type (p => net%state)
    type is (optimizer)
        if (allocated(p%velocity)) error stop 11
        if (p%momentum /= 0.5) error stop 12
    class default
        error stop 13
    end select
    allocate(source(2))
    source = [2., 3.]
    call from_source(net, source)
    source = -1
    select type (p => net%state)
    type is (optimizer)
        if (.not. allocated(p%velocity)) error stop 14
        if (size(p%velocity) /= 2) error stop 15
        if (any(p%velocity /= [2., 3.])) error stop 16
    class default
        error stop 17
    end select
    block
        type(watched) :: guard
        allocate(guard%data(1))
        guard%data = 17
        call update(net, .true.)
        call check_initialized(net)
        if (finalizations /= 0) error stop 18
    end block
    if (finalizations /= 1) error stop 19
    deallocate(net%state)
end program
