! run with 2 images
program coarrays_19
    integer :: a(3)[*] = 2

    sync all
    if (this_image() == 2) then
        if (a(1)[2] /= 2) error stop
        if (a(2)[2] /= 2) error stop
        if (a(3)[2] /= 2) error stop
    end if
end program coarrays_19