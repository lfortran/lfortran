program coarrays_22
    implicit none

    integer :: x[*]

    if (this_image() == 1) then
        x = 42
        sync images(2)
    else if (this_image() == 2) then
        sync images([1, 2])
        if (x[1] /= 42) error stop
    end if
end program coarrays_22