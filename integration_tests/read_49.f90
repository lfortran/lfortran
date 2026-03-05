implicit none ! substringext.f90
character(8):: string = 'abcd'
integer:: unit = 42
open(newunit=unit, file='foobar', status='new')
write (unit, "(A)") string(1:4)
rewind unit
read  (unit,"(A)") string(5:8)
close (unit,status = 'delete')
if (string /= 'abcdabcd') stop 1
print "(A)",string
print *, 'Test passed'
end program