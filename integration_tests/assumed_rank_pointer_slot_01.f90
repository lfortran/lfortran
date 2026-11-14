module assumed_rank_pointer_slot_m
    implicit none
contains
    subroutine forward(x)
        integer, pointer, optional, intent(inout) :: x(..)
        if (.not. present(x)) error stop 1
        select rank (x)
        rank (0)
            if (.not. associated(x)) error stop 2
            if (x /= 7) error stop 3
            x = 9
        rank (1)
            if (size(x) /= 2 .or. any(x /= [7, 8])) error stop 5
            x(2) = 10
        rank default
            error stop 6
        end select
    end subroutine

    subroutine forward_allocatable(x)
        integer, allocatable, optional, intent(inout) :: x(..)
        if (.not. present(x)) error stop 9
        select rank (x)
        rank (0)
            if (.not. allocated(x)) error stop 10
            if (x /= 7) error stop 11
            x = 9
        rank default
            error stop 12
        end select
    end subroutine
end module

program assumed_rank_pointer_slot
    use assumed_rank_pointer_slot_m
    implicit none
    integer, target :: scalar = 7, data(2) = [7, 8]
    integer, pointer :: p, a(:)
    integer, allocatable :: alloc_scalar
    p => scalar
    a => data
    call forward(p)
    if (scalar /= 9) error stop 7
    call forward(a)
    if (any(data /= [7, 10])) error stop 8
    allocate(alloc_scalar)
    alloc_scalar = 7
    call forward_allocatable(alloc_scalar)
    if (alloc_scalar /= 9) error stop 13
end program
