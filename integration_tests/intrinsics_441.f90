program intrinsics_sinpi_01
real(8) :: tol

tol = 1d-12

if (abs(sinpi(0d0) - 0d0) > tol) error stop
if (abs(sinpi(0.5d0) - 1d0) > tol) error stop
if (abs(cospi(1d0) + 1d0) > tol) error stop
if (abs(tanpi(0.25d0) - 1d0) > tol) error stop
if (abs(asinpi(1d0) - 0.5d0) > tol) error stop
if (abs(acospi(-1d0) - 1d0) > tol) error stop
if (abs(atanpi(1d0) - 0.25d0) > tol) error stop
if (abs(atan2pi(1d0, 1d0) - 0.25d0) > tol) error stop

end program intrinsics_sinpi_01