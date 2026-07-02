program coarrays_19
    integer :: x[*]

    x = 0
    sync all

    critical
        x[1] = x[1] + 1
    end critical

    sync all

    if (this_image() == 1) then
        if (x /= num_images()) error stop
    end if
end program coarrays_19