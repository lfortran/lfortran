program intrinsics_108
    integer :: x(5) = [ 1, 2, 3, 4 ,5 ]
    logical :: mask(5) = [ .TRUE., .FALSE., .TRUE., .FALSE., .TRUE. ]
    if ( .not. product( array = x, mask = mask ) == 15 ) error stop
    if ( .not. product( x, mask = mask ) == 15 ) error stop
    if ( .not. product( x, mask ) == 15 ) error stop
end
