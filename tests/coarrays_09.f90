program coarrays_09
    integer :: x

    x = this_image()

    call co_max(x)

    if (x /= num_images()) error stop ! run with 2 images
end program coarrays_09
