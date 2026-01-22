program intrinsics_408
    use iso_fortran_env, only: real64
    implicit none

    complex(real64) :: z
    real(real64) :: r

    ! This stresses abs(complex) lowering for very small magnitudes:
    ! naive hypot = sqrt(x*x + y*y) underflows to 0.0 for x=y=1e-200 (real64),
    ! while a scaled hypot algorithm must return a positive value.
    z = cmplx(1.0e-200_real64, 1.0e-200_real64, kind=real64)
    r = abs(z)

    if (r == 0.0_real64) error stop

    print *, "ok"
end program intrinsics_408
