program coarrays_18
    integer :: x

    x = this_image()

    call co_broadcast(x, source_image=1)

    if (x /= 1) error stop ! run with 2 or more images
end program coarrays_18
