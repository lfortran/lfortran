program intrinsics_182
    use iso_fortran_env, only: dp => real64
    integer :: i, j, shift1
    integer(8) :: a, b, shift2
    i = 10
    j = 4
    shift1 = 3
    a = 7
    b = 12
    shift2 = 5

    print *, dshiftl(10, 4, 3)
    if(dshiftl(10, 4, 3) /= 80) error stop

    print *, dshiftl(7_dp, 12_dp, 5)
    if(dshiftl(7_dp, 12_dp , 5) /= 224) error stop

    print *, dshiftl(10, 12, 7)
    if(dshiftl(10, 12, 7) /= 1280) error stop

    print *, dshiftl(i, j, shift1)
    if(dshiftl(i, j, shift1) /= 80) error stop

    print *, dshiftl(a, b, shift2)
    if(dshiftl(a, b, shift2) /= 224) error stop

    print *, kind(dshiftl(10, 4, 3))
    if(kind(dshiftl(10, 4, 3)) /= 4) error stop

    print *, kind(dshiftl(7_dp, 12_dp, 5))
    if(kind(dshiftl(7_dp, 12_dp, 5)) /= 8) error stop

    print *, kind(dshiftl(10, 12, 7))
    if(kind(dshiftl(10, 12, 7)) /= 4) error stop

    print *, kind(dshiftl(i, j, shift1))
    if(kind(dshiftl(i, j, shift1)) /= 4) error stop

    print *, kind(dshiftl(a, b, shift2))
    if(kind(dshiftl(a, b, shift2)) /= 8) error stop

end program 
