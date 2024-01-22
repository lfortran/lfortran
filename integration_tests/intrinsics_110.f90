program intrinsics_110
    integer :: a, b, result
    a = 10
    b = 7
    if ( .not. min0( a1 = a, a2 = b ) == 7 ) error stop
    if ( .not. min0( a, a2 = b ) == 7 ) error stop
    if ( .not. min0( a, b ) == 7 ) error stop
end program intrinsics_110
