! allocate of the selector in rank (0) of an allocatable assumed-rank dummy
module select_rank_50_m
    implicit none
contains
    subroutine fill(slot)
        integer, allocatable, intent(inout) :: slot(..)
        select rank (slot)
        rank (0)
            allocate(slot)
            slot = 11
            if (.not. allocated(slot)) error stop 1
            if (slot /= 11) error stop 2
            deallocate(slot)
        rank default
            error stop 3
        end select
    end subroutine

    subroutine fill_assoc_outside(slot)
        integer, allocatable, intent(inout) :: slot(..)
        integer :: n
        n = 12
        associate (m => n)
            select rank (slot)
            rank (0)
                allocate(slot)
                slot = m
            rank default
                error stop 5
            end select
        end associate
    end subroutine

    subroutine fill_assoc_inside(slot)
        integer, allocatable, intent(inout) :: slot(..)
        integer :: n
        n = 13
        select rank (slot)
        rank (0)
            associate (m => n)
                allocate(slot)
                slot = m
            end associate
        rank default
            error stop 6
        end select
    end subroutine

    subroutine fill_nested(slot, other)
        integer, allocatable, intent(inout) :: slot(..)
        integer, intent(in) :: other(..)
        integer :: n
        n = 14
        associate (m => n)
            select rank (slot)
            rank (0)
                block
                    associate (q => m)
                        select rank (other)
                        rank (1)
                            allocate(slot)
                            slot = q + size(other)
                        rank default
                            error stop 7
                        end select
                    end associate
                end block
            rank default
                error stop 8
            end select
        end associate
    end subroutine
end module

program select_rank_50
    use select_rank_50_m
    implicit none
    integer, allocatable :: k
    call fill(k)
    if (allocated(k)) error stop 4
    call fill_assoc_outside(k)
    if (.not. allocated(k)) error stop 9
    if (k /= 12) error stop 10
    deallocate(k)
    call fill_assoc_inside(k)
    if (.not. allocated(k)) error stop 11
    if (k /= 13) error stop 12
    deallocate(k)
    call fill_nested(k, [1, 2])
    if (.not. allocated(k)) error stop 13
    if (k /= 16) error stop 14
    print *, "OK"
end program
