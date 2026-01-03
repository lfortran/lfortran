program intrinsics_237
    use iso_fortran_env, only: dp => real64, sp => real32
    implicit none
    
    real(sp) :: i1(2) = [1.0_sp, 2.0_sp]
    real(sp), parameter :: i2 = sum(sngl([1.0_sp, 2.0_sp]))
    real(dp) :: i3(2) = [1.0_dp, 2.0_dp]
    real(dp), parameter :: i4 = sum(sngl([1.0_dp, 2.0_dp]))
    real(sp), parameter :: i5 = sngl(8.89_8)
    real(sp), parameter :: ar1(3) = sngl([1.0_dp, 2.0_dp, 3.0_dp])

    print *, kind(i5)
    if (kind(i5) /= 4) error stop
    print *, ar1
    if (any(ar1 /= [1.0_sp, 2.0_sp, 3.0_sp])) error stop
    
    print *, sum(sngl(i1))
    if (sum(sngl(i1)) /= 3.0) error stop
    print *, i2
    if (i2 /= 3.0) error stop
    print *, sum(sngl(i3))
    if (sum(sngl(i3)) /= 3.0) error stop
    print *, i4
    if (i4 /= 3.0) error stop

end program