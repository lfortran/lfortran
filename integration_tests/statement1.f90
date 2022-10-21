program statement1
integer :: i,j
integer, parameter :: dp = kind(0.d0)
real(dp) :: dfloat
real :: sfloat, ti, sum
integer :: ifloat
sfloat(i,j) = i*j
dfloat(i,j) = i+j
ifloat(i,j) = i+j
ti = sfloat(1,8)
if (abs(ti-8) > 1e-5) error stop
sum = sfloat(12,9)+12.42
if (abs(sum-120.42) > 1e-5) error stop
if (abs(dfloat(5,4)-9) > 1e-10) error stop
if (ifloat(5,4) /= 9) error stop
end program