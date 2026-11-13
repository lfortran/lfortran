module assumed_rank_32_m
    use iso_c_binding, only: c_int
    implicit none
contains
    integer function pointer_value(x) result(n)
        integer, pointer, intent(in) :: x(..)

        n = -1
        if (.not. associated(x)) return

        select rank (x)
        rank (0)
            n = x
        rank default
            n = -100 - rank(x)
        end select
    end function pointer_value

    integer function alloc_value(x) result(n)
        integer, allocatable, intent(in) :: x(..)

        n = -1
        if (.not. allocated(x)) return

        select rank (x)
        rank (0)
            n = x
        rank default
            n = -100 - rank(x)
        end select
    end function alloc_value

    integer(c_int) function bindc_alloc_value(x) result(n) bind(c)
        integer(c_int), allocatable, intent(in) :: x(..)

        n = -1
        if (.not. allocated(x)) return

        select rank (x)
        rank (0)
            n = x
        rank default
            n = -100 - rank(x)
        end select
    end function bindc_alloc_value
end module assumed_rank_32_m

program assumed_rank_32
    use iso_c_binding, only: c_int
    use assumed_rank_32_m, only: pointer_value, alloc_value, bindc_alloc_value
    implicit none
    ! The scalar pointer targets the first element of an array whose
    ! following elements are nonzero, so a callee that mistakes the scalar's
    ! storage for its rank-0 descriptor reads a nonzero rank.
    integer, target :: storage(40)
    integer, pointer :: p
    integer, allocatable :: a
    integer(c_int), allocatable :: c

    storage = 7
    storage(1) = 42

    p => storage(1)
    if (pointer_value(p) /= 42) error stop 'associated pointer'

    if (alloc_value(a) /= -1) error stop 'unallocated scalar'
    allocate(a, source=13)
    if (alloc_value(a) /= 13) error stop 'allocated scalar'

    if (bindc_alloc_value(c) /= -1) error stop 'bind(c) unallocated scalar'
    allocate(c, source=17)
    if (bindc_alloc_value(c) /= 17) error stop 'bind(c) allocated scalar'
    print *, 'ok'
end program assumed_rank_32
