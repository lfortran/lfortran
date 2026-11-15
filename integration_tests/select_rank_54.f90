! a scalar pointer passed to a pointer assumed-rank dummy sees the callee's
! nullify, allocate and deallocate in the rank (0) block
module select_rank_54_m
    implicit none
contains
    subroutine drop(slot)
        integer, pointer, intent(inout) :: slot(..)
        select rank (slot)
        rank (0)
            nullify(slot)
        rank default
            error stop 99
        end select
    end subroutine

    subroutine make(slot)
        integer, pointer, intent(out) :: slot(..)
        select rank (slot)
        rank (0)
            allocate(slot)
            slot = 9
        rank default
            error stop 98
        end select
    end subroutine

    subroutine release(slot)
        integer, pointer, intent(inout) :: slot(..)
        select rank (slot)
        rank (0)
            deallocate(slot)
        rank default
            error stop 97
        end select
    end subroutine
end module

program select_rank_54
    use select_rank_54_m
    implicit none
    integer, pointer :: p, q
    integer, target :: x

    x = 5
    p => x
    call drop(p)
    if (associated(p)) error stop 1
    if (x /= 5) error stop 2

    call make(q)
    if (.not. associated(q)) error stop 3
    if (q /= 9) error stop 4
    call release(q)
    if (associated(q)) error stop 5
    print *, "OK"
end program
