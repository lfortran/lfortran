program format_85
! Test that Ew.dEe format on READ consumes exactly w characters per field
implicit none
real :: x, y
character(len=*), parameter :: s_init = ' 1.00000000E+00  2.00000000E+00'
character(len=len(s_init)) :: s = s_init

! Ew.dEe: w=15, d=8, e=2 — only 15 characters should be consumed per field
read(s, '(e15.8e2,1x,e15.8e2)') x, y
print *, x, y
if (abs(x - 1.0) > 1.0e-6) error stop
if (abs(y - 2.0) > 1.0e-6) error stop

! Also test with Gw.dEe format
read(s, '(g15.8e2,1x,g15.8e2)') x, y
print *, x, y
if (abs(x - 1.0) > 1.0e-6) error stop
if (abs(y - 2.0) > 1.0e-6) error stop
end program
