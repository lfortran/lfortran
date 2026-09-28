! The program uses only `top`, so `mid` and `base` are reached transitively.
! Compiled with --separate-compilation, so every module here is read back from
! its `.mod` file and its initializer is declared without a body. Wiring
! dependency calls into such a declaration used to abort the compiler.
!
! The pointer of each module is a link time constant that is laid out as the
! pointer's own static initializer, across translation units: the target is an
! `external global` of the object file that defines the module.
program global_init_06
    use global_init_06_top
    implicit none

    ! Every module's initializer ran, including the two the program never
    ! uses directly, and each ran before the first statement here.
    if (.not. associated(top_ptr)) error stop 1
    if (.not. associated(mid_ptr)) error stop 2
    if (.not. associated(base_ptr)) error stop 3

    if (top_ptr /= 1) error stop 4
    if (mid_ptr /= 2) error stop 5
    if (base_ptr /= 3) error stop 6

    ! They are associations, not copies.
    top_target = 10
    mid_target = 20
    base_target = 30
    if (top_ptr /= 10) error stop 7
    if (mid_ptr /= 20) error stop 8
    if (base_ptr /= 30) error stop 9

    ! The initializer that cannot be laid out as static data ran as well, in
    ! every module of the chain.
    if (top_arr(2)%h /= 1) error stop 10
    if (top_arr(2)%tag /= "ttt") error stop 11
    if (mid_arr(2)%h /= 2) error stop 12
    if (mid_arr(2)%tag /= "mmm") error stop 13
    if (base_arr(2)%h /= 3) error stop 14
    if (base_arr(2)%tag /= "bbb") error stop 15

    print *, "ok"
end program
