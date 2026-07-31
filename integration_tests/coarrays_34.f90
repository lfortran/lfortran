program coarrays_34
    implicit none

    integer, allocatable :: target_arr[:, :]

    allocate(target_arr[30:31, 40:*])

end program coarrays_34