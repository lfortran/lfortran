program coarrays_37
    implicit none

    real, allocatable :: src_arr(:,:)[:]

    allocate(src_arr(10:12, 20:23)[*])
    if (this_image() == 1) then
        if (any(lbound(src_arr) /= [10, 20])) error stop "Wrong lbounds"
        if (any(ubound(src_arr) /= [12, 23])) error stop "Wrong ubounds"
    end if

    deallocate(src_arr)
end program coarrays_37