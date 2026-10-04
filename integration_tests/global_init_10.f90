! The C-callable procedures global_init_10c.c drives `global_init_10_m` with.
! They are external procedures of their own file, so that the module's object
! file can go last on the link line; see global_init_10c.c.
integer(c_int) function global_init_10_initial() bind(c)
    use iso_c_binding, only: c_int
    use global_init_10_m, only: state
    implicit none
    global_init_10_initial = 0
    if (state%n /= 7) return
    if (associated(state%p)) return
    global_init_10_initial = 1
end function global_init_10_initial

subroutine global_init_10_set() bind(c)
    use global_init_10_m, only: state, tgt
    implicit none
    state%n = 9
    state%p => tgt
end subroutine global_init_10_set

subroutine global_init_10_check() bind(c)
    use global_init_10_m, only: state, tgt
    implicit none
    if (state%n /= 9) error stop 1
    if (.not. associated(state%p, tgt)) error stop 2
    if (state%p /= 42) error stop 3
    if (allocated(state%a)) error stop 4
    allocate(state%a(3))
    state%a = [1, 2, 3]
    if (sum(state%a) /= 6) error stop 5
    deallocate(state%a)
    print *, "ok"
end subroutine global_init_10_check
