program dcmplx_01
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    real(dp) :: x
    complex(dp) :: z
    real(dp) :: cs
    complex(dp) :: sn, r

    external :: foo

    x = 1.0_dp
    z = (1.0_dp, 2.0_dp)

    call foo(dcmplx(x), dconjg(z), cs, sn, r)
    call foo(-dconjg(z), dconjg(z), cs, sn, r)
end program dcmplx_01
