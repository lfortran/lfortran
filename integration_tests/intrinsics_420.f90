program intrinsics_420
    ! Test dble() and float() with array constructor arguments
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    real(dp) :: rhs(5)
    real :: rhs_f(3)
    integer :: i

    rhs = dble([0, 0, 5, 0, 0])
    if (abs(rhs(1) - 0.0_dp) > 1e-12_dp) error stop
    if (abs(rhs(2) - 0.0_dp) > 1e-12_dp) error stop
    if (abs(rhs(3) - 5.0_dp) > 1e-12_dp) error stop
    if (abs(rhs(4) - 0.0_dp) > 1e-12_dp) error stop
    if (abs(rhs(5) - 0.0_dp) > 1e-12_dp) error stop

    rhs_f = float([1, 2, 3])
    if (abs(rhs_f(1) - 1.0) > 1e-6) error stop
    if (abs(rhs_f(2) - 2.0) > 1e-6) error stop
    if (abs(rhs_f(3) - 3.0) > 1e-6) error stop

    print *, "All tests passed."
end program intrinsics_420
