program complex128_kind_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    complex(real128) :: z, w
    real(real128) :: tol

    tol = 1.0e-30_real128
    z = cmplx(1.0_real128, 2.0_real128, kind=real128)
    w = z * z
    if (kind(z) /= 16) error stop 1
    if (kind(real(z)) /= 16) error stop 2
    if (abs(real(z) - 1.0_real128) > tol) error stop 3
    if (abs(aimag(z) - 2.0_real128) > tol) error stop 4
    if (abs(real(w) + 3.0_real128) > tol) error stop 5
    if (abs(aimag(w) - 4.0_real128) > tol) error stop 6
    print *, z
end program
