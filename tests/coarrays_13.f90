program coarrays_13
    character(3) :: s

    if (this_image() == 1) s = "xyz"
    if (this_image() == 2) s = "abc"

    call co_min(s)

    if (s /= "abc") error stop ! run with 2 images
end program coarrays_13
