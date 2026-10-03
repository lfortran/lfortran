! The only user of coarrays_54_a and coarrays_54_b.
subroutine coarrays_54_s()
    use coarrays_54_a, only: co_var
    use coarrays_54_b, only: p
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
end subroutine coarrays_54_s
