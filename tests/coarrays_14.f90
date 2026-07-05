program coarrays_14
    integer :: x

    x = this_image()

    call co_max(x, result_image=2)

    if (this_image() == 2) then
        if (x /= num_images()) error stop ! run with 2 images
    end if
end program coarrays_14
