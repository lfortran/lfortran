program coarrays_13
    character(3) :: s = "xyz"

    if (this_image() == 2) s = "abc"

    call co_min(s)

    if (s /= "abc") error stop ! run with 2 or more images
end program coarrays_13
