! The initialization of one saved variable depending on the allocation of
! another: `ptr` can only be associated once `co_var` has storage. Both are
! saved variables of the same module, so both belong to that module's startup
! initializer, which allocates before it initializes.
module coarrays_49_m
    implicit none
    integer, target, save :: co_var[*] = 1
    integer, pointer :: ptr => co_var
end module coarrays_49_m

program coarrays_49
    use coarrays_49_m
    implicit none
    integer :: me

    me = this_image()

    if (.not. associated(ptr, co_var)) error stop 1
    if (ptr /= 1) error stop 2

    co_var = me
    if (ptr /= me) error stop 3

    ptr = me + 100
    if (co_var /= me + 100) error stop 4

    sync all
    if (co_var[1] /= 101) error stop 5

    if (me == 1) print *, "ok"
end program coarrays_49
