program intrinsics_282
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    implicit none
    real(sp), parameter :: r1 = hypot(4.0_sp, 3.0_sp)
    real(dp), parameter :: r2 = hypot(4.0_dp, 3.0_dp)
    real(sp), parameter :: ar1(3) = hypot([4.0_sp, 3.0_sp, 0.0_sp], [3.0_sp, 4.0_sp, 0.0_sp])
    real(dp), parameter :: ar2(3) = hypot([2.0_dp, 1.0_dp, 9.0_dp], [6.0_dp, 4.0_dp, 0.0_dp])
    real(sp) :: x = 4.0_sp, y = 3.5_sp
    real(dp) :: z = 1.12_dp, w = 0.43_dp
    real(sp) :: arr1(3) = [4.0_sp, 3.0_sp, 0.0_sp], arr2(3) = [3.0_sp, 4.0_sp, 11.0_sp]
    real(dp) :: arr3(3) = [2.0_dp, 1.0_dp, 9.0_dp], arr4(3) = [6.0_dp, 4.0_dp, 0.0_dp]

    print *, r1
    if (abs(r1 - 5.0_sp) > 1e-6) error stop
    print *, r2
    if (abs(r2 - 5.0_dp) > 1e-12) error stop
    print *, ar1
    if (any(abs(ar1 - [5.0_sp, 5.0_sp, 0.0_sp]) > 1e-6)) error stop
    print *, ar2
    if (any(abs(ar2 - [6.32455532033676_dp, 4.12310562561766_dp, 9.0_dp]) > 1e-12)) error stop

    print *, hypot( x = 1.e0_4, y = 0.5e0_4 )
    if (abs(hypot( x = 1.e0_4, y = 0.5e0_4 ) - 1.11803401_sp) > 1e-6) error stop
    print *, hypot( 1.e0_4, y = 0.5e0_4 )
    if (abs(hypot( 1.e0_4, y = 0.5e0_4 ) - 1.11803401_sp) > 1e-6) error stop
    print *, hypot( 1.e0_4, 0.5e0_4 )
    if (abs(hypot( 1.e0_4, 0.5e0_4 ) - 1.11803401_sp) > 1e-6) error stop

    print *, hypot(x, y)
    if (abs(hypot(x, y) - 5.31507290636733_sp) > 1e-6) error stop
    print *, hypot(z, w)
    if (abs(hypot(z, w) - 1.1997082978791138_dp) > 1e-12) error stop
    print *, hypot(arr1, arr2)
    if (any(abs(hypot(arr1, arr2) - [5.0_sp, 5.0_sp, 11.0_sp]) > 1e-6)) error stop
    print *, hypot(arr3, arr4)
    if (any(abs(hypot(arr3, arr4) - [6.32455532033676_dp, 4.12310562561766_dp, 9.0_dp]) > 1e-12)) error stop

end program
