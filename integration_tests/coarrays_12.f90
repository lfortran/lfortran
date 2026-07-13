program coarrays_12
    integer :: x

    x = this_image()

    call co_min(x)

    if (x /= 1) error stop ! run with 2 images
end program coarrays_12
