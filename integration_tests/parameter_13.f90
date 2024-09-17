program parameter_13
implicit none
integer, parameter :: N = 100           ! TODO: Set N = 65535
integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = kind(1.d0)
real(dp), parameter :: pi = 2*asin(1._dp)
real(dp), parameter :: a = 0, b = pi
real(dp), parameter :: dx = (b-a)/N
integer :: i
real(dp), parameter :: X(*) = [(sin(a+(b-a)*i/N), i = 1, N)]
real(dp), parameter :: S = sum(X)*dx
logical, parameter :: l = S < 2
real(kind=merge(sp, dp, l)) :: y
if (kind(y) /= sp) error stop
print *, S
if (abs(S - 1.9999999996170159_dp) > 1e-15_dp) error stop
end program
