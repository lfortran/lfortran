! coarrays_49 across modules: `p` is associated with a saved coarray of
! another module, so the module holding `p` can only be initialized after
! the one holding `co_var` has allocated it.
module coarrays_53_a
    implicit none
    integer, target, save :: co_var[*] = 1
end module coarrays_53_a

module coarrays_53_b
    use coarrays_53_a, only: co_var
    implicit none
    integer, pointer :: p => co_var
end module coarrays_53_b

program coarrays_53
    use coarrays_53_a, only: co_var
    use coarrays_53_b, only: p
    implicit none
    integer :: me

    me = this_image()

    if (.not. associated(p, co_var)) error stop 1
    if (p /= 1) error stop 2

    p = me + 100
    if (co_var /= me + 100) error stop 3

    sync all
    if (co_var[1] /= 101) error stop 4

    if (me == 1) print *, "ok"
end program coarrays_53
