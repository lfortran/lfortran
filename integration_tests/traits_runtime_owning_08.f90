module traits_runtime_owning_08_m
    implicit none
    integer :: part_finals = 0, leaf_finals = 0
    integer :: part_values = 0, leaf_values = 0, final_order = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Leaf
        integer :: n = 17
        integer, allocatable :: data(:)
    contains
        final :: finish_leaf
    end type
    type :: Part
        integer :: n = 9
        type(Leaf), allocatable :: leaf
    contains
        final :: finish_part
    end type
    type :: Envelope
        type(Part), allocatable :: part
    end type
    implements IValue :: Envelope
        procedure, pass :: value => read_value
    end implements
contains
    subroutine finish_part(self)
        type(Part), intent(inout) :: self
        part_finals = part_finals + 1
        part_values = part_values + self%n
        final_order = 10 * final_order + 1
    end subroutine
    subroutine finish_leaf(self)
        type(Leaf), intent(inout) :: self
        leaf_finals = leaf_finals + 1
        leaf_values = leaf_values + self%n
        final_order = 10 * final_order + 2
    end subroutine
    function read_value(self) result(r)
        class(Envelope), intent(in) :: self
        integer :: r
        r = self%part%leaf%n + sum(self%part%leaf%data)
    end function
    subroutine initialize(source)
        type(Envelope), intent(inout) :: source
        allocate(source%part)
        allocate(source%part%leaf)
        allocate(source%part%leaf%data(2))
        source%part%leaf%data = [4, 5]
    end subroutine
    subroutine scope_cleanup()
        type(Envelope) :: source
        call initialize(source)
        block
            class(IValue), allocatable :: local
            allocate(local, source=source)
            if (local%value() /= 26) error stop 10
        end block
        if (part_finals /= 1 .or. leaf_finals /= 1) error stop 11
    end subroutine
end module

program traits_runtime_owning_08
    use traits_runtime_owning_08_m
    implicit none
    type(Envelope) :: source
    class(IValue), allocatable :: owner, copy

    call initialize(source)
    allocate(owner, source=source)
    copy = owner
    if (part_finals /= 0 .or. leaf_finals /= 0) error stop 1
    source%part%leaf%n = 23
    source%part%leaf%data = 99
    if (owner%value() /= 26 .or. copy%value() /= 26) error stop 2
    owner = owner
    if (part_finals /= 1 .or. leaf_finals /= 1) error stop 3
    if (part_values /= 9 .or. leaf_values /= 17) error stop 4
    if (final_order /= 12) error stop 5
    final_order = 0
    deallocate(owner, copy)
    if (part_finals /= 3 .or. leaf_finals /= 3) error stop 6
    if (part_values /= 27 .or. leaf_values /= 51) error stop 7
    if (final_order /= 1212) error stop 8
    deallocate(source%part)
    if (part_finals /= 4 .or. leaf_finals /= 4) error stop 9
    if (part_values /= 36 .or. leaf_values /= 74) error stop 12
    part_finals = 0
    leaf_finals = 0
    final_order = 0
    call scope_cleanup()
    if (part_finals /= 2 .or. leaf_finals /= 2) error stop 13
    if (final_order /= 1212) error stop 14
end program
