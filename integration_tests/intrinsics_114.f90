program intrinsics_114
    real(4) x4
    real(8) x8
    x4 = 1.234E0_4
    x8 = 4.321_8
    print *, aint( a = x4, kind = 8 )
    if ( .not. aint( a = x8, kind = 4 ) == 4 ) error stop
    if ( .not. aint( x8, kind = 4 ) == 4 ) error stop
    if ( .not. aint( x8, 4 ) == 4 ) error stop
end program intrinsics_114
