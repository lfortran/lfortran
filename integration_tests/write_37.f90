program write_37
! Test list-directed output spacing for integers
implicit none
integer :: i
character(len=100) :: line

! Two integers: leading blank + 11-char field + 1-space sep + 11-char field = 24
write(line, *) 1, 2
if (len_trim(line) /= 24) error stop

! Three integers: leading blank + 11 + 1 + 11 + 1 + 11 = 36
write(line, *) 1, 2, 3
if (len_trim(line) /= 36) error stop

! Single integer: leading blank + 11-char field = 12
write(line, *) 42
if (len_trim(line) /= 12) error stop

! Negative integers: same widths
write(line, *) -1, -2
if (len_trim(line) /= 24) error stop

! Verify values via internal read
i = 0
write(line, *) 12345
read(line, *) i
if (i /= 12345) error stop

print *, "PASS"
end program
