program implied_do_loops46
integer :: j
complex, parameter :: z = (-1., -2.)
complex, parameter :: complex_1D(*)   = [(z, j=1, 3)]
complex, parameter :: complex_2D(*,*) = reshape([(z, j=1, 4)], [2, 2])
complex(kind(1.0d0)), parameter :: zd = (-1.0d0, -2.0d0)
complex(kind(1.0d0)), parameter :: complex_double_1D(*) = [(zd, j=1, 2)]

if (size(complex_1D) /= 3) error stop
if (size(complex_2D) /= 4) error stop
if (size(complex_double_1D) /= 2) error stop

do j = 1, 3
    if (real(complex_1D(j)) /= -1.0) error stop
    if (aimag(complex_1D(j)) /= -2.0) error stop
end do

if (real(complex_2D(1, 1)) /= -1.0) error stop
if (aimag(complex_2D(2, 2)) /= -2.0) error stop

do j = 1, 2
    if (real(complex_double_1D(j)) /= -1.0d0) error stop
    if (aimag(complex_double_1D(j)) /= -2.0d0) error stop
end do

print *, "PASSED"
end program implied_do_loops46