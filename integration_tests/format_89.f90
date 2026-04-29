program format_89
implicit none
character(len=10) :: lines(4)

! Test: slash (/) descriptor after parenthesized repeat group
lines = ''
write(lines, '(2(A)/2(A))') 'A', 'B', 'C', 'D'
if (trim(lines(1)) /= 'AB') error stop
if (trim(lines(2)) /= 'CD') error stop

! Test: slash after unadorned parenthesized group
lines = ''
write(lines, '((A)/(A))') 'X', 'Y'
if (trim(lines(1)) /= 'X') error stop
if (trim(lines(2)) /= 'Y') error stop

! Test: multiple slashes after paren groups
lines = ''
write(lines, '(2(A)/A/A)') 'A', 'B', 'C', 'D'
if (trim(lines(1)) /= 'AB') error stop
if (trim(lines(2)) /= 'C') error stop
if (trim(lines(3)) /= 'D') error stop

print *, "PASS"
end program
