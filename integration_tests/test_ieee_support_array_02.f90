module ieee_support_array_user_02
    implicit none
    interface ieee_support_sqrt
        module procedure user_support_sqrt
    end interface
    interface ieee_support_nan
        module procedure user_support_nan
    end interface
contains
    ! Distinguishable from a one-argument specific: gfortran's inquiry dummy
    ! is assumed-rank, which is not distinguishable from real :: x(:) alone.
    logical function user_support_sqrt(x, tag)
        real, intent(in) :: x(:)
        integer, intent(in) :: tag
        user_support_sqrt = tag /= 7
    end function
    logical function user_support_nan(x, tag)
        real, intent(in) :: x(:)
        integer, intent(in) :: tag
        user_support_nan = tag /= 7
    end function
end module

program test_ieee_support_array_02
    ! Intrinsic specifics are merged first. An array actual must still reach
    ! the user specific when that specific matches, and the intrinsic when
    ! it does not (F2018 17.11, scalar logical).
    use, intrinsic :: ieee_arithmetic
    use ieee_support_array_user_02
    implicit none
    real :: a(3)
    double precision :: d(2)

    a = 1.0
    d = 1.0d0

    if (ieee_support_sqrt(a, 7)) error stop
    if (.not. ieee_support_sqrt(a, 3)) error stop
    if (ieee_support_nan(a, 7)) error stop
    if (.not. ieee_support_nan(a, 3)) error stop
    if (ieee_support_sqrt(x=a, tag=7)) error stop
    if (ieee_support_nan(x=a, tag=7)) error stop

    if (.not. ieee_support_sqrt(a)) error stop
    if (.not. ieee_support_nan(a)) error stop
    if (.not. ieee_support_sqrt(d)) error stop
    if (.not. ieee_support_nan(d)) error stop
    if (.not. ieee_support_sqrt(1.0)) error stop
    if (.not. ieee_support_nan(1.0)) error stop
    if (.not. ieee_support_sqrt(x=a)) error stop
    if (.not. ieee_support_nan(x=d)) error stop
end program
