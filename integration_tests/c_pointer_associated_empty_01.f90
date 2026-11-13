program c_pointer_associated_empty_01
    implicit none
    integer, target :: values(3)
    integer, pointer :: p(:), q(:)

    values(1) = 11
    values(2) = 17
    values(3) = 23
    p => values(1:0)
    q => p
    if (.not. associated(p)) error stop 1
    if (.not. associated(q)) error stop 2
    if (associated(p, q)) error stop 3

    p => values(1:2)
    q => p
    if (.not. associated(p, q)) error stop 4
    if (p(2) /= 17) error stop 5
    p => values
    q => p
    if (.not. associated(p, values)) error stop 6

    p => null()
    q => null()
    if (associated(p)) error stop 7
    if (associated(p, q)) error stop 8
end program
