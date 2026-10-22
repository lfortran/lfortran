program pointer_section_05
    ! Pointer bounds-spec and bounds-remapping onto sections of a
    ! character array
    implicit none
    character(len=3), target :: c(5) = ['aaa', 'bbb', 'ccc', 'ddd', 'eee']
    character(len=3), pointer :: cp(:)

    cp(0:) => c(2:4)
    if (lbound(cp, 1) /= 0 .or. ubound(cp, 1) /= 2) error stop 1
    if (cp(0) /= 'bbb' .or. cp(1) /= 'ccc' .or. cp(2) /= 'ddd') error stop 2

    cp(-1:) => c(1:5:2)
    if (lbound(cp, 1) /= -1 .or. ubound(cp, 1) /= 1) error stop 3
    if (cp(-1) /= 'aaa' .or. cp(0) /= 'ccc' .or. cp(1) /= 'eee') error stop 4

    cp(1:2) => c(3:5)
    if (lbound(cp, 1) /= 1 .or. ubound(cp, 1) /= 2) error stop 5
    if (cp(1) /= 'ccc' .or. cp(2) /= 'ddd') error stop 6

    print *, "passed"
end program pointer_section_05
