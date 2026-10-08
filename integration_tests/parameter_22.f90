program parameter_22
! Narrowing real(8)/complex(8) constant array initializers to kind 4:
! no "change of value" warning when every element converts exactly,
! a warning (with Fortran type names) only when some element changes.
implicit none
real(4), parameter :: s = 1d0
real(4), parameter :: w(2) = [1d0, 2d0]
real(4), parameter :: v(2,2) = reshape([1d0, 2d0, 3d0, 4d0], [2,2])
complex(4), parameter :: c(2) = [(1d0, 0.5d0), (2d0, -0.25d0)]
real(4), parameter :: x(2) = [1d0, 0.1d0]
complex(4), parameter :: cx(2) = [(1d0, 0d0), (1d0, 0.1d0)]
print *, s, w, v
print *, c
print *, x
print *, cx
if (s /= 1.0) error stop
if (any(w /= [1.0, 2.0])) error stop
if (any(v /= reshape([1.0, 2.0, 3.0, 4.0], [2,2]))) error stop
if (any(c /= [(1.0, 0.5), (2.0, -0.25)])) error stop
if (x(1) /= 1.0) error stop
if (abs(x(2) - 0.1) > 1e-7) error stop
if (cx(1) /= (1.0, 0.0)) error stop
if (abs(cx(2) - (1.0, 0.1)) > 1e-7) error stop
end program
