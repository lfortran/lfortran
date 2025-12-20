program array_section_07
    implicit none
    integer :: i0
    integer, dimension(:), allocatable :: subpass
    integer :: temp(2)
    i0 = 1
    subpass = [2,3]
    subpass = temp(i0:i0)
    print *, size(subpass)
    if (size(subpass) /= 1) error stop
end program array_section_07