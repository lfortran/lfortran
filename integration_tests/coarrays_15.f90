program coarrays_15
    integer :: x

    x = this_image()

    call co_min(x, result_image=1)

    if (this_image() == 1) then
        if (x /= 1) error stop ! run with 2 images
    end if
end program coarrays_15
