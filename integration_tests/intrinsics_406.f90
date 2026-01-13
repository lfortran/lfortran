program intrinsics_iachar
    ! Test iachar intrinsic with array arguments (issue #9495)
    implicit none
    integer, parameter:: ch(2) = iachar(['A','a'])
    integer :: ch2(3)

    ! Test compile-time evaluation
    if (ch(1) /= 65) error stop
    if (ch(2) /= 97) error stop

    ! Test runtime evaluation
    ch2 = iachar(['X', 'y', 'Z'])
    if (ch2(1) /= 88) error stop  ! 'X'
    if (ch2(2) /= 121) error stop  ! 'y'
    if (ch2(3) /= 90) error stop   ! 'Z'

    ! Test with scalar
    if (iachar('B') /= 66) error stop

    print *, "All iachar tests passed"
end program
