program write_28
! Test internal write to character array with format containing slash (/)
! The slash should advance to the next record (next array element)
implicit none

character(15) :: buf(2)
real(8) :: b(2)
character(10) :: ibuf(3)
integer :: a(3)

b(1) = 1.0d0
b(2) = 2.0d0

! Slash in format: E11.6 for first record, E9.4 for second record
write(unit=buf, fmt='(E11.6/E9.4)') b

print *, buf(1)
print *, buf(2)

! buf(1) should use E11.6 format
if (trim(adjustl(buf(1))) /= ".100000E+01") error stop
! buf(2) should use E9.4 format (not E11.6)
if (trim(adjustl(buf(2))) /= ".2000E+01") error stop

! Test with integer format and slash
a = (/10, 20, 30/)
write(ibuf, '(I5/I3/I4)') a
if (trim(adjustl(ibuf(1))) /= "10") error stop
if (trim(adjustl(ibuf(2))) /= "20") error stop
if (trim(adjustl(ibuf(3))) /= "30") error stop

end program

