subroutine bessel_j16_large_01()
implicit none
! Past Miller's recurrence the power series is not J0. Leave it unfolded
! rather than folding NaN (#15730).
real(16), parameter :: j = bessel_j0(1.0e6_16)
real(16), parameter :: jn = bessel_jn(300000, 300000.0_16)
end subroutine
