! The only user of test_init_show_llvm_a and test_init_show_llvm_b.
subroutine test_init_show_llvm_s()
    use test_init_show_llvm_a, only: h
    use test_init_show_llvm_b
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
end subroutine test_init_show_llvm_s
