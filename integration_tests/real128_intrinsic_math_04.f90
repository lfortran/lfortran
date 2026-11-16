program real128_intrinsic_math_04
    ! The pi variants of the trigonometric intrinsics of real(128)
    ! constants are constant expressions and can initialize named constants.
    ! The references are correctly rounded values of the exact results.
    ! Not labelled gfortran: GFortran does not implement sinpi etc.
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    integer, parameter :: qp = real128

    real(qp), parameter :: a = acospi(-1.0_qp)
    real(qp), parameter :: b = asinpi(1.0_qp)
    real(qp), parameter :: c = atanpi(1.0_qp)
    real(qp), parameter :: d = cospi(1.0_qp)
    real(qp), parameter :: e = sinpi(0.5_qp)
    real(qp), parameter :: f = tanpi(0.25_qp)

    real(qp), parameter :: s1 = sinpi(0.125_qp)
    real(qp), parameter :: s2 = sinpi(-1.5_qp)
    real(qp), parameter :: s3 = sinpi(1.0e20_qp + 0.5_qp)
    real(qp), parameter :: s4 = sinpi(2.0_qp**120)
    real(qp), parameter :: c1 = cospi(0.375_qp)
    real(qp), parameter :: c2 = cospi(1.0_qp / 3)
    real(qp), parameter :: c3 = cospi(1.0e20_qp + 1)
    real(qp), parameter :: t1 = tanpi(0.125_qp)
    real(qp), parameter :: t2 = tanpi(0.625_qp)
    real(qp), parameter :: t3 = tanpi(-0.25_qp)
    real(qp), parameter :: t4 = tanpi(0.75_qp)
    real(qp), parameter :: t5 = tanpi(-1.25_qp)
    real(qp), parameter :: as1 = asinpi(0.5_qp)
    real(qp), parameter :: as2 = asinpi(0.75_qp)
    real(qp), parameter :: as3 = asinpi(-1.0_qp)
    real(qp), parameter :: ac1 = acospi(0.5_qp)
    real(qp), parameter :: ac2 = acospi(0.25_qp)
    real(qp), parameter :: ac3 = acospi(-0.75_qp)
    real(qp), parameter :: at1 = atanpi(0.5_qp)
    real(qp), parameter :: at2 = atanpi(1000.0_qp)
    real(qp), parameter :: at3 = atanpi(-1.0_qp)
    real(qp), parameter :: v(3) = sinpi([0.5_qp, 1.5_qp, 2.5_qp])

    call check(a, 1.0_qp, 1)
    call check(b, 0.5_qp, 2)
    call check(c, 0.25_qp, 3)
    call check(d, -1.0_qp, 4)
    call check(e, 1.0_qp, 5)
    call check(f, 1.0_qp, 6)

    call check(s1, 0.382683432365089771728459984030398866761_qp, 7)
    call check(s2, 1.0_qp, 8)
    call check(s3, 1.0_qp, 9)
    if (s4 /= 0) error stop 10
    call check(c1, 0.382683432365089771728459984030398866761_qp, 11)
    call check(c2, 0.5_qp, 12)
    call check(c3, -1.0_qp, 13)
    call check(t1, 0.414213562373095048801688724209698078570_qp, 14)
    call check(t2, -2.41421356237309504880168872420969807857_qp, 15)
    call check(t3, -1.0_qp, 16)
    call check(t4, -1.0_qp, 29)
    call check(t5, -1.0_qp, 30)
    call check(as1, 1.0_qp / 6, 17)
    call check(as2, 0.269946543837384114786219432294857106990_qp, 18)
    call check(as3, -0.5_qp, 19)
    call check(ac1, 1.0_qp / 3, 20)
    call check(ac2, 0.419569376744833756229049806671515744416_qp, 21)
    call check(ac3, 0.769946543837384114786219432294857106990_qp, 22)
    call check(at1, 0.147583617650433274175401076224740525951_qp, 23)
    call check(at2, 0.499681690219919441061127359033225501777_qp, 24)
    call check(at3, -0.25_qp, 25)
    call check(v(1), 1.0_qp, 26)
    call check(v(2), -1.0_qp, 27)
    call check(v(3), 1.0_qp, 28)

contains

    subroutine check(x, ref, code)
        real(qp), intent(in) :: x, ref
        integer, intent(in) :: code
        if (.not. (abs(x - ref) <= 2 * epsilon(ref) * abs(ref))) then
            print *, code, x, ref
            error stop code
        end if
    end subroutine

end program
