program coarrays_34
    implicit none

    integer, allocatable :: target_arr[:, :]

    allocate(target_arr[30:31, 40:*])

    if (any(lcobound(target_arr) /= [ 30, 40 ])) error stop "Wrong lcobounds"

    if (ucobound(target_arr,1) /= 31) error stop "Wrong ucobound"
    
end program coarrays_34