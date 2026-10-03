program enum_07
    use enum_07_parent_mod
    implicit none
    integer :: x
    x = 10
    call do_work(x)
    if (x /= 11) error stop
    if (sparse_full /= 0) error stop
    if (wksp_a /= 3) error stop
    print *, "PASS"
end program
