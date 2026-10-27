! Compiled before the submodules (#14167).
program submodule_62
    use submodule_62_m1
    use submodule_62_m2
    use submodule_62_r
    implicit none
    integer :: r, q
    if (r_bump() /= 1) error stop 1
    call s_step(r, q)
    if (r /= 2 .or. q /= 101) error stop 2
    call t_step(r, q)
    if (r /= 3 .or. q /= 102) error stop 3
    call s_step(r, q)
    if (r /= 4 .or. q /= 103) error stop 4
    if (r_bump() /= 5) error stop 5
    if (r_total /= 50) error stop 6
    if (t_q_total() /= 3) error stop 7
    if (.not. s_class_ok()) error stop 8
    print *, "ok"
end program
