program allocate_44
implicit none

integer :: a(3) = [10, 20, 30]
real, allocatable :: b(:)

! Test allocate with elemental function source expression
allocate(b, source=real(a))

if (size(b) /= 3) error stop
if (abs(b(1) - 10.0) > 1e-6) error stop
if (abs(b(2) - 20.0) > 1e-6) error stop
if (abs(b(3) - 30.0) > 1e-6) error stop

print *, "ok"

end program allocate_44
