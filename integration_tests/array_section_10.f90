program array_section_10
    ! Test valid array section assignments with matching shapes
    implicit none
    integer :: a(1), b(2), c(3)
    integer :: temp(5)
    integer :: i

    ! Valid: single element section to size-1 array
    i = 2
    a = temp(i:i)
    if (size(a) /= 1) error stop

    ! Valid: two element section to size-2 array
    b = temp(1:2)
    if (size(b) /= 2) error stop

    ! Valid: three element section to size-3 array
    c = temp(2:4)
    if (size(c) /= 3) error stop

    print *, "PASS"
end program
