program c_pointer_host_reference_01
    implicit none
    integer, target :: values(3), alternate(3)
    integer, pointer :: p(:)

    values(1) = 11
    values(2) = 17
    values(3) = 23
    alternate(1) = 31
    alternate(2) = 37
    alternate(3) = 41
    p => values(:)
    call capture()
    if (values(2) /= 29) error stop 1
    if (.not. associated(p, alternate)) error stop 2
    if (p(2) /= 37) error stop 3

contains
    subroutine capture()
        if (.not. associated(p, values)) error stop 4
        if (p(1) /= 11) error stop 5
        p(2) = 29
        call forward(p, alternate)
        if (.not. associated(p, alternate)) error stop 6
        if (p(3) /= 41) error stop 7
        p => values
        if (p(2) /= 29) error stop 8
        p => alternate
    end subroutine

    subroutine forward(q, target)
        integer, pointer, intent(inout) :: q(:)
        integer, target, intent(in) :: target(:)
        q => target
    end subroutine
end program
