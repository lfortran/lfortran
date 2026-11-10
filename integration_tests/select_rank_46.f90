! allocate of the selector in rank (0) of an allocatable assumed-rank dummy
module select_rank_46_m
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
end module

program select_rank_46
    use select_rank_46_m
    implicit none
    integer, allocatable :: k
    call fill(k)
    if (allocated(k)) error stop 4
    print *, "OK"
end program
