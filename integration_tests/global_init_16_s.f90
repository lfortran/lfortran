! The only user of global_init_16_a and global_init_16_b.
subroutine global_init_16_s()
    use global_init_16_a, only: h
    use global_init_16_b
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
end subroutine global_init_16_s
