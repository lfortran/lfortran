program coarrays_11
    character(3) :: s = "abc"

    if (this_image() == 2) s = "xyz"

    call co_max(s)

    if (s /= "xyz") error stop ! run with 2 or more images
end program coarrays_11
