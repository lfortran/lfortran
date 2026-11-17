module c_reset_alloc_component_01_m
    implicit none
    type :: inner
        integer, allocatable :: nested_values(:)
    end type
    type :: node
        integer, allocatable :: values(:)
        type(node), allocatable :: next
    end type
    type :: item
        integer, allocatable :: values(:)
        integer, allocatable :: scalar
        type(inner) :: nested
        type(inner), allocatable :: owned
        type(node), allocatable :: linked
    end type
contains
    subroutine unused(x)
        class(item), intent(inout) :: x
    end subroutine
    subroutine reset(x)
        type(item), intent(out) :: x
    end subroutine
end module

program c_reset_alloc_component_01
    use c_reset_alloc_component_01_m
    implicit none
    type(item) :: x
    if (allocated(x%values)) error stop 1
    if (allocated(x%scalar)) error stop 2
    if (allocated(x%nested%nested_values)) error stop 3
    if (allocated(x%owned)) error stop 13
    if (allocated(x%linked)) error stop 16
    allocate(x%values(2))
    allocate(x%scalar)
    allocate(x%nested%nested_values(1))
    allocate(x%owned)
    allocate(x%owned%nested_values(1))
    allocate(x%linked)
    allocate(x%linked%next)
    allocate(x%linked%next%values(1))
    x%values(1) = 4
    x%values(2) = 5
    x%nested%nested_values(1) = 8
    x%owned%nested_values(1) = 10
    x%linked%next%values(1) = 14
    if (.not. allocated(x%values)) error stop 4
    if (.not. allocated(x%scalar)) error stop 5
    if (sum(x%values) /= 9) error stop 6
    if (x%nested%nested_values(1) /= 8) error stop 7
    if (x%owned%nested_values(1) /= 10) error stop 14
    if (x%linked%next%values(1) /= 14) error stop 17
    call reset(x)
    if (allocated(x%values)) error stop 8
    if (allocated(x%scalar)) error stop 9
    if (allocated(x%nested%nested_values)) error stop 10
    if (allocated(x%owned)) error stop 15
    if (allocated(x%linked)) error stop 18
    allocate(x%values(1))
    x%values(1) = 6
    if (x%values(1) /= 6) error stop 11
    deallocate(x%values)
    if (allocated(x%values)) error stop 12
end program
