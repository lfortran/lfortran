program real80_intrinsic_math_01
    ! The pi variants of the trigonometric intrinsics of x87 real(10)
    ! constants are constant expressions and can initialize named constants.
    ! Kind 10 exists only where the C long double kind is 10, so it is a
    ! kind parameter, not a literal 10.
    ! Not labelled gfortran: GFortran does not implement sinpi etc.
    use iso_c_binding, only: c_long_double
    implicit none
    integer, parameter :: kp = c_long_double

    real(kp), parameter :: a = acospi(-1.0_kp)
    real(kp), parameter :: b = asinpi(1.0_kp)
    real(kp), parameter :: c = atanpi(1.0_kp)
    real(kp), parameter :: d = cospi(1.0_kp)
    real(kp), parameter :: e = sinpi(0.5_kp)
    real(kp), parameter :: f = tanpi(0.25_kp)
    real(kp), parameter :: s1 = sinpi(0.125_kp)
    real(kp), parameter :: c1 = cospi(1.0_kp / 3)
    real(kp), parameter :: as1 = asinpi(0.75_kp)
    real(kp), parameter :: at1 = atanpi(0.5_kp)

    if (kp /= 10) stop 0
    call check(a, 1.0_kp, 1)
    call check(b, 0.5_kp, 2)
    call check(c, 0.25_kp, 3)
    call check(d, -1.0_kp, 4)
    call check(e, 1.0_kp, 5)
    call check(f, 1.0_kp, 6)
    call check(s1, 0.382683432365089771728_kp, 7)
    call check(c1, 0.5_kp, 8)
    call check(as1, 0.269946543837384114786_kp, 9)
    call check(at1, 0.147583617650433274175_kp, 10)

contains

    subroutine check(x, ref, code)
        real(kp), intent(in) :: x, ref
        integer, intent(in) :: code
        if (.not. (abs(x - ref) <= 4 * epsilon(ref) * abs(ref))) then
            print *, code, x, ref
            error stop code
        end if
    end subroutine

end program
