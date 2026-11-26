program ieee_is_nan_fast_01
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value, &
        ieee_quiet_nan
    implicit none
    integer, parameter :: xdp = selected_real_kind(18)
    integer, parameter :: qp = selected_real_kind(33)
    real :: s
    real(xdp) :: x
    real(qp) :: q
    s = 1.0
    x = 1.0_xdp
    q = 1.0_qp
    if (ieee_is_nan(s) .or. ieee_is_nan(x) .or. ieee_is_nan(q)) error stop 1
    s = ieee_value(s, ieee_quiet_nan)
    x = ieee_value(x, ieee_quiet_nan)
    q = ieee_value(q, ieee_quiet_nan)
    if (.not. (ieee_is_nan(s) .and. ieee_is_nan(x) .and. ieee_is_nan(q))) error stop 2
end program
