program intrinsics_115
    use iso_fortran_env, only: sp => real32, dp => real64
    implicit none
    real(4) x4
    real(8) x8
    real(4), parameter :: r1 = anint(4.23_4)
    real(8), parameter :: r2 = anint(4.23_8)
    real(4), parameter :: ar1(3) = anint([123.41_4, 4.23_4, -31.0_4])
    real(8), parameter :: ar2(3) = anint([123.41_8, 4.23_8, -31.0_8])
    real(4) :: arr1(3) = [123.41_4, 4.23_4, -31.0_4]
    real(8) :: arr2(3) = [123.41_8, 4.23_8, -31.0_8]

    print *, r1
    if (r1 /= 4) error stop
    print *, r2
    if (r2 /= 4) error stop
    print *, ar1
    if (any(ar1 /= [123, 4, -31])) error stop
    print *, ar2
    ! if (any(ar2 /= [123_8, 4_8, -31_8])) error stop
    
    print *, anint(arr1)
    if (any(anint(arr1) /= [123, 4, -31])) error stop
    print *, anint(arr2)
    if (any(anint(arr2) /= [123, 4, -31])) error stop

    x4 = 1.234E0_4
    x8 = 4.821_8
    print *, anint( a = x4, kind = 8 )
    print *, anint( a = x8, kind = 4 )
    if ( .not. anint( a = x4, kind = 8 ) == 1 ) error stop
    if ( .not. anint( x4, kind = 8 ) == 1 ) error stop
    if ( .not. anint( x4, 8 ) == 1 ) error stop
end program intrinsics_115
