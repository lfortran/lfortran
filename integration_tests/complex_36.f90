program complex_36
! Test mixed-precision real*complex binary operations.
! Fortran standard: real(dp)*complex(sp) should produce complex(dp).
implicit none
integer, parameter :: sp = kind(1.0), dp = kind(1.0d0)
real(dp) :: v
complex(sp) :: zsp
complex(dp) :: r1, r2, r3, r4

v = 1.1d0
zsp = (1.0_sp, 2.0_sp)

! real(dp) * complex(sp) -> complex(dp)
r1 = v * zsp
if (abs(real(r1) - 1.1d0) > 1.0d-15) error stop
if (abs(aimag(r1) - 2.2d0) > 1.0d-15) error stop

! complex(sp) * real(dp) -> complex(dp)
r2 = zsp * v
if (abs(real(r2) - 1.1d0) > 1.0d-15) error stop
if (abs(aimag(r2) - 2.2d0) > 1.0d-15) error stop

! real(dp) + complex(sp) -> complex(dp)
r3 = v + zsp
if (abs(real(r3) - 2.1d0) > 1.0d-15) error stop
if (abs(aimag(r3) - 2.0d0) > 1.0d-15) error stop

! real(dp) - complex(sp) -> complex(dp)
r4 = v - zsp
if (abs(real(r4) - 0.1d0) > 1.0d-15) error stop
if (abs(aimag(r4) + 2.0d0) > 1.0d-15) error stop

print *, "PASS"
end program
