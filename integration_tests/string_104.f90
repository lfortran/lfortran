program string_104
implicit none
character(10) :: string
integer :: unit
string = 'ABCDEFGHIJ'

open(newunit=unit, file='string_104_tmp', status='new')
write(unit, '(A)') 'Hello World'
rewind(unit)
read(unit, '(A)') string(1:6)
close(unit, status='delete')

if (string(1:6) /= 'Hello ') error stop
if (string(7:10) /= 'GHIJ') error stop
if (string /= 'Hello GHIJ') error stop
print '(A)', string
end program string_104
