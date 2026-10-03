! Test: associated(p1, p2) for class(*) (unlimited polymorphic) pointers
program associate_40
    implicit none
    integer, target :: x = 42
    integer, target :: y = 99
    class(*), pointer :: p1, p2

    ! Both point to same target
    p1 => x
    p2 => x
    if (.not. associated(p1, p2)) error stop
    if (.not. associated(p1)) error stop
    if (.not. associated(p2)) error stop

    ! Point to different targets
    p2 => y
    if (associated(p1, p2)) error stop

    ! One-argument form
    nullify(p2)
    if (associated(p2)) error stop
    if (.not. associated(p1)) error stop

    ! Both null
    nullify(p1)
    if (associated(p1)) error stop

    print *, "PASS"
end program
