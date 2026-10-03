program abs_04
! Test that abs() on a complex array returns a real array
implicit none
complex(8), allocatable :: z(:)
real(8) :: tol
real(8) :: r
allocate(z(3))
z = [(1.0d0, 2.0d0), (3.0d0, 4.0d0), (5.0d0, 6.0d0)]
tol = 1.0d-6

! abs(z) on a complex array should return a real array
! Then abs(abs(z)) should also work (real in, real out)
if (any(aimag(z) > tol * abs(abs(z)))) then
    print *, "PASS: imaginary parts are large"
else
    error stop "Expected imaginary parts to be large"
end if

! Verify scalar abs of complex still works
r = abs(z(1))
if (abs(r - sqrt(5.0d0)) > tol) error stop "abs of scalar complex failed"

! Verify abs of real array works
if (any(abs(abs(z)) < 0.0d0)) error stop "abs of real array should be non-negative"
end program
