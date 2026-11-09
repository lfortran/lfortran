program test_ieee_support_array
    use, intrinsic :: ieee_arithmetic
    implicit none
    real :: a(3)
    double precision :: d(2, 2)
    logical :: s

    a = 1.0
    d = 1.0d0

    ! The result is a scalar logical even when X is an array (F2018 17.11).
    s = ieee_support_sqrt(a)
    if (.not. s) error stop
    s = ieee_support_divide(d)
    if (.not. s) error stop
    s = ieee_support_denormal(a)
    if (.not. s) error stop
    s = ieee_support_standard(d)
    if (.not. s) error stop
    s = ieee_support_io(a)
    if (.not. s) error stop
    s = ieee_support_nan(a)
    if (.not. s) error stop
    s = ieee_support_inf(d)
    if (.not. s) error stop
    s = ieee_support_datatype(a)
    if (.not. s) error stop

    if (.not. ieee_support_sqrt(a)) error stop
    if (.not. ieee_support_divide(d)) error stop
    if (.not. ieee_support_denormal(a)) error stop
    if (.not. ieee_support_standard(d)) error stop
    if (.not. ieee_support_io(a)) error stop
    if (.not. ieee_support_nan(d)) error stop
    if (.not. ieee_support_inf(a)) error stop
    if (.not. ieee_support_datatype(d)) error stop

    if (.not. ieee_support_sqrt(x=a)) error stop
    if (.not. ieee_support_datatype(x=d)) error stop

    ! Scalar X and the absent-X form still match.
    if (.not. ieee_support_sqrt(1.0)) error stop
    if (.not. ieee_support_divide(1.0d0)) error stop
    if (.not. ieee_support_sqrt()) error stop
    if (.not. ieee_support_io()) error stop
end program test_ieee_support_array
