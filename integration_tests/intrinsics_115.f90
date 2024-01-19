program intrinsics_115
    real(4) x4
    real(8) x8
    x4 = 1.234E0_4
    x8 = 4.821_8
    print *, anint( a = x4, kind = 8 )
    print *, anint( a = x8, kind = 4 )
    if ( .not. anint( a = x4, kind = 8 ) == 1 ) error stop
    if ( .not. anint( x4, kind = 8 ) == 1 ) error stop
    if ( .not. anint( x4, 8 ) == 1 ) error stop
end program intrinsics_115
