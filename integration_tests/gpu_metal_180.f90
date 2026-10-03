program gpu_metal_180
! Test that Fortran variables named after C++ keywords (true, false, etc.)
! are properly sanitized in Metal/C++ codegen.
implicit none
real, parameter :: true = 1.0
real, parameter :: false = 0.0
real :: a(4)
integer :: i
do concurrent(i = 1:4)
    if (mod(i, 2) == 0) then
        a(i) = true
    else
        a(i) = false
    end if
end do
if (abs(a(1) - 0.0) > 1e-6) error stop
if (abs(a(2) - 1.0) > 1e-6) error stop
if (abs(a(3) - 0.0) > 1e-6) error stop
if (abs(a(4) - 1.0) > 1e-6) error stop
print *, "ok"
end program
