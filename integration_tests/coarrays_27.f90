! run with 4 images
program coarrays_27
    implicit none

    integer :: x[*] = 0

    ! Image 4 updates x on image 1
    if (this_image() == 4) then
        x[1] = 123
    end if

    sync all

    if (this_image() == 1) then
        if (x /= 123) error stop
    end if
end program coarrays_27