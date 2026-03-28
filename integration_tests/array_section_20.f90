program array_section_20
    implicit none
    character(len=1), target :: a(3) = [character(len=1) :: 'a', 'b', 'c']
    character(len=1), pointer :: p(:)

    p => a(1:2:-4)
    print *, 'size(p) =', size(p)
    if (size(p) /= 0) error stop
end program array_section_20
