program integer_boz_01
    use iso_fortran_env, only: int64
    implicit none
    print * , int( z'DEADBEEF1EADBEEF', int64 )
    if ( int( z'DEADBEEF1EADBEEF', int64 ) /= -2401053092097442065_8 ) error stop
end program
