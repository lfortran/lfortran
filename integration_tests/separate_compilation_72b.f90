! Companion to separate_compilation_72: fills the character components of the
! module variables of separate_compilation_72a.f90 to their full length.
subroutine fill(c, ch)
implicit none
character(len=*), intent(out) :: c
character, intent(in) :: ch
c = repeat(ch, len(c))
end subroutine

subroutine drv()
use separate_compilation_72_mod
implicit none
external :: fill
if (len(x%s) /= 8 .or. x%s /= '') error stop 1
if (len(b1%s) /= 64 .or. b1%s /= '') error stop 2
if (sh%s /= 'ab    ') error stop 3
if (o%n /= 3 .or. o%inner%s /= 'ab') error stop 4
x%s = 'xxxxxxxx'
y%s = 'yyyyyyyy'
print '(4A)', 'x%s = ', x%s, '  y%s = ', y%s
if (x%s /= 'xxxxxxxx') error stop 5
if (y%s /= 'yyyyyyyy') error stop 6
call fill(b1%s, 'p')
call fill(b2%s, 'q')
if (b1%s /= repeat('p', 64)) error stop 7
if (b2%s /= repeat('q', 64)) error stop 8
call fill(n1%s, 'r')
call fill(n2%s, 's')
if (n1%s /= repeat('r', 64)) error stop 9
if (n2%s /= repeat('s', 64)) error stop 10
sh%s = 'cdefgh'
o%inner%s = 'ijklmn'
if (sh%s /= 'cdefgh') error stop 11
if (o%n /= 3 .or. o%inner%s /= 'ijklmn') error stop 12
end subroutine
