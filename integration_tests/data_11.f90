SUBROUTINE DKBVRC()
    INTEGER PLIM, KLIM
    PARAMETER ( PLIM = 28, KLIM = 100 )
    INTEGER P(PLIM), C(PLIM,KLIM-1)
    DATA P( 1),(C( 1,I),I = 1,99)/     31, 12, 2*9, 13, 8*12, 3*3, 12, &
    2*7, 9*12, 3*3, 12, 2*7, 9*12, 3*3, 12, 2*7, 9*12, 3*3, 12, 2*7, &
    8*12, 7, 3*3, 3*7, 21*3/
    if( P( 1) /= 31 ) error stop
    if( C( 1, 1) /= 12 ) error stop
    if( C( 1, 2) /= 9 ) error stop
    if( C( 1, 99) /= 3 ) error stop
END

program main
    call DKBVRC()
end