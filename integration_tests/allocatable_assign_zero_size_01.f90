program allocatable_assign_zero_size_01
! Test that assigning a zero-size allocatable function result to a
! fixed-size array is treated as a no-op (no runtime shape mismatch
! error). This is technically non-conformable per the Fortran standard
! but real-world libraries rely on this behavior.
implicit none

real :: d(2)
integer :: i

! Initialize d
d = 1.0

! Assign zero-size allocatable function result to fixed-size array.
! Should be a no-op: d stays unchanged.
d = f(0)
do i = 1, 2
    if (abs(d(i) - 1.0) > 1.0e-6) error stop
end do

! Assign non-zero-size allocatable function result (should work normally)
d = f(2)
if (abs(d(1)) > 1.0e-6) error stop
if (abs(d(2)) > 1.0e-6) error stop

print *, "OK"

contains

function f(n) result(r)
    integer, intent(in) :: n
    real, allocatable :: r(:)
    allocate(r(n))
    r = 0.0
end function

end program
