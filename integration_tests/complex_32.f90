program complex_32
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer :: i
    real(dp) :: sum
    complex(dp) :: z

    sum = 0.0_dp
    z = cmplx(1.0_dp, 2.0_dp, kind=dp)
    do i = 1, 500000
        sum = sum + real(z) + aimag(z)
        z = cmplx(1.0_dp, 2.0_dp, kind=dp)
    end do

    print *, int(sum)
end program complex_32
