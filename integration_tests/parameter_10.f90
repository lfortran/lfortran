program parameter_10
implicit none
integer, parameter :: N = 10
integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = kind(1.d0)
real(dp), parameter :: pi = 2*asin(1._dp)
real(dp), parameter :: a = 0, b = pi
real(dp), parameter :: dx = (b-a)/N
integer :: i
real(dp), parameter :: X(N) = [(sin(a+(b-a)*i/N), i = 1, N)]
real(dp), parameter :: S = sum(X)*dx
print *, S
if (abs(S - 1.9835235375094546_dp) > 1e-14) error stop
end program
