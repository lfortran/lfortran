program coarrays_23
    implicit none

    integer :: x[*]

    if (this_image() == 1) x = 42

    sync images(*)

    if (x[1] /= 42) error stop
end program coarrays_23