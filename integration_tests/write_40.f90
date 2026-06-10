program write_40
implicit none
character(len=512) :: line
real :: x
real(8) :: y
integer :: i

x = huge(1.0)
write(line, '(F0.9)') x
do i = 1, len_trim(line)
   if (line(i:i) == '*') error stop "F0.9 with huge(1.0) produced asterisks"
end do
if (index(line, '.000000000') == 0) error stop "F0.9 missing decimal tail for huge(1.0)"

! 64-bit huge: integer part should be much longer than `long` can hold
y = huge(1.0d0)
write(line, '(F0.9)') y
do i = 1, len_trim(line)
   if (line(i:i) == '*') error stop "F0.9 with huge(1.0d0) produced asterisks"
end do
if (len_trim(line) <= 19) error stop "F0.9 with huge(1.0d0) too short"

! Round-trip a moderately large value through F0.d
write(line, '(F0.3)') 1234567890123.456_8
if (trim(line) /= '1234567890123.456') error stop "F0.3 round-trip mismatch"

! Sanity check: small values still work as before
write(line, '(F0.2)') 1.99_4
if (trim(line) /= '1.99') error stop "F0.2 on 1.99 broken"
write(line, '(F0.2)') -0.5_4
! Per Fortran rules, F0.2 may emit either ``-0.50`` or ``-.50``; allow both.
if (trim(line) /= '-0.50' .and. trim(line) /= '-.50') &
   error stop "F0.2 on -0.5 broken"
end program write_40