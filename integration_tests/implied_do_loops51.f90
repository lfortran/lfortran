program implied_do_loops51
! An output implied-do loop with values of different types whose loop
! variable ends at the largest or smallest value of its kind is expanded
! at compile time without overflowing. The program is not conforming: the
! loop variable is incremented past its last value, which its kind cannot
! represent. It relies on LFortran's compile-time expansion, which never
! computes that value, so it is not run with GFortran, whose loop variable
! overflows there, so that the loop never ends.
implicit none
type :: pa
    integer :: i
end type
type :: pb
    real :: r
end type
integer(8), parameter :: h = huge(0_8)
type(pa) :: x(2)
type(pb) :: y(2)
integer :: i
integer(8) :: i8
real :: r(2)
character(len=80) :: line
r = [1.5, 2.5]
x%i = [1, 2]
y%r = r

line = ''
write(line, '(*(i0,1x,f3.1,1x))') (i8, r(1), i8=h-1,h)
if (line /= '9223372036854775806 1.5 9223372036854775807 1.5') error stop 1
line = ''
write(line, '(*(i0,1x,f3.1,1x))') (i, r(2), i=huge(0)-1,huge(0))
if (line /= '2147483646 2.5 2147483647 2.5') error stop 2
line = ''
write(line, '(*(i0,1x,f3.1,1x))') (x(1), y(2), i8=h-1,h)
if (line /= '1 2.5 1 2.5') error stop 3
line = ''
write(line, '(*(i0,1x,f3.1,1x))') (x(2), y(1), i8=-h,-h-1,-1)
if (line /= '2 1.5 2 1.5') error stop 4
print '(a)', trim(line)
end program
