program intrinsics_249
    use iso_fortran_env, only: dp => real64
    integer, parameter :: n = 1000
    integer, parameter :: m = 1000
    integer, dimension(n,m) :: x
    real(dp) :: res
    x = 18
    res = var_2_iint8_dp(x, 1)
    print *, res
    if (abs(res - 18000000.0_dp) > 1e-12) error stop
    contains
    function var_2_iint8_dp(x, dim) result(res)
    use iso_fortran_env, only: dp => real64
    integer, intent(in) :: x(:,:)
    integer, intent(in) :: dim
    real(dp) :: res
    res = sum(sum(abs(x), dim))
    end function var_2_iint8_dp
end program