! The program uses only `top`, so `mid` and `base` are reached transitively.
! Compiled with --separate-compilation, so every module here is read back from
! its `.mod` file and its initializer is declared without a body. Wiring
! dependency calls into such a declaration used to abort the compiler.
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

    print *, "ok"
end program
