program coarrays_37
    implicit none

    real, allocatable :: src_arr(:,:)[:]
    integer :: static_arr(30:39, 40:49)[*]
    
    if (any(lbound(static_arr) /= [30, 40])) error stop "Wrong static_arr lbounds"
    if (any(ubound(static_arr) /= [39, 49])) error stop "Wrong static_arr ubounds"    
    
    allocate(src_arr(10:12, 20:23)[*])

    if (any(lbound(src_arr) /= [10, 20])) error stop "Wrong src_arr lbounds"
    if (any(ubound(src_arr) /= [12, 23])) error stop "Wrong src_arr ubounds"


    deallocate(src_arr)
end program coarrays_37