program test
    implicit none

    integer :: x[*]

    if (this_image() == 1) then
        x[2] = 42
    end if
    if (this_image() == 2) then
        x[1] = 20
    end if

    print *, "Image: ", this_image(), ':', x

end program test