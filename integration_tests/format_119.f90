program format_119
! Z editing of a character item (a GFortran extension) writes the code of
! its first character in hexadecimal.
implicit none
integer, parameter :: ucs4 = selected_char_kind('ISO_10646')
character(len=3) :: s
character(len=1) :: c
character(len=12) :: buf
character(len=1) :: arr(3)
character(len=5) :: str
character(kind=ucs4, len=2) :: u

c = ' '
write(s, '(a1,z2.2)') '%', c
if (s /= '%20') error stop 1

write(buf, '(z0)') 'ABCD'
if (buf /= '41') error stop 2
write(buf, '(z4)') achar(10)
if (buf /= '   A') error stop 3
write(buf, '(z0.4)') 'a'
if (buf /= '0061') error stop 4
write(buf, '(z1)') 'a'
if (buf /= '*') error stop 5
write(buf, '(z2.2)') achar(200)
if (buf /= 'C8') error stop 6

arr = ['a', 'b', 'c']
write(buf, '(3z3)') arr
if (buf /= ' 61 62 63') error stop 7

str = 'hello'
write(buf, '(z4)') str(2:3)
if (buf /= '  65') error stop 8

u = ucs4_'AB'
write(buf, '(z0)') u
if (buf /= '41') error stop 9

write(buf, '(a,z2,i3)') 'x', 'y', 5
if (buf /= 'x79  5') error stop 10

print *, "ok"
end program format_119
