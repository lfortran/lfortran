program write_37
! Test list-directed output spacing for integers.
!
! The leading record blank is processor-dependent, so each assertion
! accepts either the with-blank length N or the without-blank length N-1.
implicit none
integer :: i, n
character(len=100) :: line

! Two integers: leading blank + 11-char field + 1-space sep + 11-char field = 24
! Without leading blank: 23
write(line, *) 1, 2
n = len_trim(line)
if (n /= 24 .and. n /= 23) error stop

! Three integers: leading blank + 11 + 1 + 11 + 1 + 11 = 36 (or 35 without blank)
write(line, *) 1, 2, 3
n = len_trim(line)
if (n /= 36 .and. n /= 35) error stop

! Single integer: leading blank + 11-char field = 12 (or 11 without blank)
write(line, *) 42
n = len_trim(line)
if (n /= 12 .and. n /= 11) error stop

! Negative integers: same widths
write(line, *) -1, -2
n = len_trim(line)
if (n /= 24 .and. n /= 23) error stop

! Verify values via internal read
i = 0
write(line, *) 12345
read(line, *) i
if (i /= 12345) error stop

print *, "PASS"
end program
