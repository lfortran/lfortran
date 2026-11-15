! a scalar allocatable passed to an allocatable assumed-rank dummy sees
! the allocation and deallocation done by the callee
module select_rank_51_m
    implicit none
contains
    subroutine give(slot)
        integer, allocatable, intent(out) :: slot(..)
        select rank (slot)
        rank (0)
            allocate(slot)
            slot = 11
        rank default
            error stop 9
        end select
    end subroutine

    integer function give_f(slot) result(r)
        integer, allocatable, intent(out) :: slot(..)
        r = 0
        select rank (slot)
        rank (0)
            allocate(slot)
            slot = 42
            r = 1
        end select
    end function

    subroutine drop(slot)
        integer, allocatable, intent(inout) :: slot(..)
        select rank (slot)
        rank (0)
            if (slot /= 7) error stop 8
            deallocate(slot)
        rank default
            error stop 9
        end select
    end subroutine
end module

program select_rank_51
    use select_rank_51_m
    implicit none
    integer, allocatable :: k, j, d
    integer :: r

    call give(k)
    if (.not. allocated(k)) error stop 1
    if (k /= 11) error stop 2

    r = give_f(j)
    if (r /= 1) error stop 3
    if (.not. allocated(j)) error stop 4
    if (j /= 42) error stop 5

    allocate(d)
    d = 7
    call drop(d)
    if (allocated(d)) error stop 6
    print *, "OK"
end program
