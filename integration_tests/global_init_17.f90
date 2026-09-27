! global_init_16 driven from C (global_init_17c.c), with the object files
! linked in the opposite order: CMakeLists.txt lists the pointers' module
! ahead of the module that holds their targets.
subroutine global_init_17_s() bind(c, name="global_init_17_s")
    use global_init_17_a, only: h
    use global_init_17_b
    implicit none
    if (.not. associated(ps)) error stop 1
    if (len(ps) /= 3 .or. ps /= "abc") error stop 2
    ps = "xyz"
    if (h%s /= "xyz") error stop 3
    if (.not. associated(pv, h%v(2))) error stop 4
    if (pv /= 2) error stop 5
    pv = 20
    if (h%v(2) /= 20) error stop 6
    print *, "ok"
end subroutine global_init_17_s
