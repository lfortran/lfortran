program coarrays_10
    integer :: x

    x = this_image()

    call co_sum(x, result_image=2)

    if (this_image() == 2) then
        if (x /= 3) error stop
    end if
    
end program coarrays_10