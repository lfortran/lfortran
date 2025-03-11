program intrinsics_360
    implicit none  
    real(8), dimension(2) :: dc_r = [1.3, 2.3]  
    real :: x_r = 1.3  
    integer, dimension(2) :: dc_i = [1, 2]  
    integer(8) :: x_i = 2  
    print *, findloc(dc_r, x_r) 
    if (any(findloc(dc_r, x_r) /= 1)) error stop  
    print *, findloc(dc_i, x_i)  
    if (any(findloc(dc_i, x_i) /= 2)) error stop 
    print *, findloc(dc_r, x_i)
    if (any(findloc(dc_r, x_i) /= 0)) error stop
    print *, findloc(dc_i, x_r)
    if (any(findloc(dc_i, x_r) /= 1)) error stop
end program intrinsics_360
