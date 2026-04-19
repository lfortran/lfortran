program read_70
! Test that formatted read into array sections correctly respects
! intermediate format descriptors (like X) between edit descriptors.
! Previously, reading into y(:) with format (es15.8,1x,es15.8) would
! read all elements in a tight loop, skipping the 1X separator.
implicit none

real :: y(2)
character(31) :: s
integer :: ia(3)
character(45) :: si

! Test 1: ES format with 1X separator into real array section
s = " 3.00000000E-01 -5.00000000E-01"
read(s, "(es15.8,1x,es15.8)") y(:)
if (abs(y(1) - 0.3) > 0.01) error stop
if (abs(y(2) + 0.5) > 0.01) error stop

! Test 2: F format with X separator into real array section
s = "  1.50  2.50  3.50"
y = 0.0
read(s, "(f6.2,6x,f6.2)") y(:)
if (abs(y(1) - 1.5) > 0.01) error stop
if (abs(y(2) - 3.5) > 0.01) error stop

! Test 3: Integer array with X separator
si = "  10   20   30"
read(si, "(i4,2x,i4,2x,i4)") ia(:)
if (ia(1) /= 10) error stop
if (ia(2) /= 20) error stop
if (ia(3) /= 30) error stop

print *, "PASS"
end program
