module traits_runtime_owning_08_oracle_m
    implicit none
    integer :: part_finals = 0, leaf_finals = 0
    integer :: leaf_values = 0, final_order = 0
    logical :: main_finished = .false.
    type :: Leaf
        integer :: n = 17
        integer, allocatable :: data(:)
    contains
        final :: finish_leaf
    end type
    type :: Part
        type(Leaf), allocatable :: leaf
    contains
        final :: finish_part
    end type
    type :: Envelope
        type(Part), allocatable :: part
    end type
contains
    subroutine finish_part(self)
        type(Part), intent(inout) :: self
        part_finals = part_finals + 1
        final_order = 10 * final_order + 1
    end subroutine
    subroutine finish_leaf(self)
        type(Leaf), intent(inout) :: self
        if (main_finished) error stop 8
        leaf_finals = leaf_finals + 1
        leaf_values = leaf_values + self%n
        final_order = 10 * final_order + 2
    end subroutine
end module

program traits_runtime_owning_08_oracle
    use traits_runtime_owning_08_oracle_m
    implicit none
    type(Envelope) :: source
    type(Envelope), allocatable :: owner
    type(Leaf), allocatable :: survivor
    type(Leaf), pointer :: target

    allocate(source%part)
    allocate(source%part%leaf)
    allocate(source%part%leaf%data(2))
    source%part%leaf%data = [4, 5]
    allocate(owner, source=source)
    if (part_finals /= 0 .or. leaf_finals /= 0) error stop 1
    source%part%leaf%n = 23
    source%part%leaf%data = 99
    if (owner%part%leaf%n /= 17) error stop 2
    if (sum(owner%part%leaf%data) /= 9) error stop 3
    deallocate(owner)
    if (part_finals /= 1 .or. leaf_finals /= 1) error stop 4
    if (leaf_values /= 17 .or. final_order /= 12) error stop 5
    deallocate(source%part)
    if (part_finals /= 2 .or. leaf_finals /= 2) error stop 6
    if (leaf_values /= 40 .or. final_order /= 1212) error stop 7
    allocate(target)
    deallocate(target)
    if (leaf_finals /= 3 .or. leaf_values /= 57) error stop 9
    allocate(survivor)
    allocate(survivor%data(2))
    survivor%data = 1
    main_finished = .true.
end program
