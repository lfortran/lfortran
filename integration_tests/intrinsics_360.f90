program intrinsics_360
    real(8), dimension(2) :: dc_r = [1.3, 2.3]
    real :: x_r = 1.3
    print *, findloc(dc_r, x_r)
    if (any(findloc(dc_r, x_r) /= 1)) error stop

    integer, dimension(2) :: dc_i = [1, 2]
    integer(8) :: x_i = 2
    print *, findloc(dc_i, x_i)
    if (any(findloc(dc_i, x_i) /= 2)) error stop
end program