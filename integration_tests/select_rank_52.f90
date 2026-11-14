! a scalar allocatable or pointer passed to an assumed-rank bind(c) dummy
! sees the allocate and deallocate done by the callee, called from Fortran
! and from C
module select_rank_52_m
    use iso_c_binding, only: c_int
    implicit none
contains
    subroutine give(slot) bind(c, name="select_rank_52_give")
        integer(c_int), allocatable, intent(out) :: slot(..)
        select rank (slot)
        rank (0)
            allocate(slot)
            slot = 11
        rank default
            error stop 9
        end select
    end subroutine

    subroutine drop(slot) bind(c, name="select_rank_52_drop")
        integer(c_int), allocatable, intent(inout) :: slot(..)
        select rank (slot)
        rank (0)
            if (.not. allocated(slot)) error stop 10
            if (slot /= 11) error stop 11
            deallocate(slot)
        rank default
            error stop 12
        end select
    end subroutine

    integer(c_int) function give_f(slot) result(r) bind(c, name="select_rank_52_give_f")
        integer(c_int), allocatable, intent(out) :: slot(..)
        r = 0
        select rank (slot)
        rank (0)
            allocate(slot)
            slot = 42
            r = 1
        end select
    end function

    subroutine give_p(slot) bind(c, name="select_rank_52_give_p")
        integer(c_int), pointer, intent(inout) :: slot(..)
        select rank (slot)
        rank (0)
            allocate(slot)
            slot = 7
        rank default
            error stop 13
        end select
    end subroutine
end module

program select_rank_52
    use select_rank_52_m
    implicit none
    interface
        integer(c_int) function c_give() bind(c, name="select_rank_52_c_give")
            import :: c_int
        end function
    end interface
    integer(c_int), allocatable :: k, j
    integer(c_int), pointer :: p

    call give(k)
    if (.not. allocated(k)) error stop 1
    if (k /= 11) error stop 2
    call drop(k)
    if (allocated(k)) error stop 3
    if (give_f(j) /= 1) error stop 4
    if (.not. allocated(j)) error stop 5
    if (j /= 42) error stop 6
    nullify(p)
    call give_p(p)
    if (.not. associated(p)) error stop 7
    if (p /= 7) error stop 8
    deallocate(p)
    if (c_give() /= 11) error stop 14
    print *, "OK"
end program
