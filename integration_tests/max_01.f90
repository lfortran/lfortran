program max_01
    real(8) :: y
    real :: z
    y = 5.2d0
    z = 9.0
    print *, max(y, z)
    if (abs (max(y, z) - 9.0d0) > 1d-16) error stop "Incorrect value of max"
    print *, "Kind of max:", kind( max( y, z))
    if ( kind( max( y, z)) /= 8 ) error stop
end program