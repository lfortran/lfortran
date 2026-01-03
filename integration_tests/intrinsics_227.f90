program intrinsics_227
    use iso_fortran_env, only: sp => real32, dp => real64
    implicit none

    real(sp), parameter :: x1(3) = [1.0, 2.0, 3.0]
    real(dp), parameter :: x2(5) = [5.0, 7.0, 11.0, 13.0, 17.0]
    real(dp), parameter :: x = norm2([1.0, 2.0, 3.0, 4.0, 5.0])
    real(sp), parameter :: y = norm2([5.0, 7.0, 11.0, 13.0, 17.0])
    real(sp) :: y1(5) = [5.0, 7.0, 11.0, 13.0, 17.0]
    real(dp) :: y2(10) = [14.0, 21.0, 33.0, 39.0, 51.0, 5.0, 7.0, 11.0, 13.0, 17.0]  

    print*, norm2(x1)
    if (abs(norm2(x1) - 3.7416573867739413_sp) > 1e-6 ) error stop
    print*, norm2(x2)
    if (abs(norm2(x2) - 25.553864678361276_dp) > 1e-12 ) error stop
    print*, norm2(y1)
    if (abs(norm2(y1) - 25.5538654_sp) > 1e-6 ) error stop
    print*, norm2(y2)
    if (abs(norm2(y2) - 80.628778981204974_dp) > 1e-12 ) error stop
    print*, x
    if (abs(x - 7.4161982536315918_dp) > 1e-12 ) error stop
    print*, y
    if (abs(y - 25.5538654_sp) > 1e-6 ) error stop
    
 end program