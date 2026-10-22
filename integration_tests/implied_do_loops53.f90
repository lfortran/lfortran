program implied_do_loops53
! An output implied-do loop with constant bounds and no iterations writes
! nothing, whatever the bounds of its nested loops are, and the items after
! it are still written.
implicit none
type :: pa
    integer :: i
end type
type :: pb
    real :: r
end type
type(pa) :: x(3)
type(pb) :: y(3)
integer :: i, j, n, k(3), u
real :: r(3)
character(len=80) :: line

k = [1, 2, 3]
r = [1.5, 2.5, 3.5]
x%i = k
y%r = r
n = 2

open(newunit=u, status='scratch', action='readwrite', form='formatted')
write(u, '(*(g0,1x))') (k(i), (r(j), j=1,n), i=1,0), 'end'
write(u, '(*(g0,1x))') 'begin', (k(i), (r(j), j=1,n), i=3,1), 'end'
write(u, '(*(g0,1x))') (x(i), (y(j), j=1,n), i=1,0), 'end'
rewind(u)
read(u, '(a)') line
if (line /= 'end') error stop 1
read(u, '(a)') line
if (line /= 'begin end') error stop 2
read(u, '(a)') line
if (line /= 'end') error stop 3
close(u)

line = ''
write(line, '(*(g0,1x))') (k(i), (r(j), j=1,n), i=1,0), 'end'
if (line /= 'end') error stop 4
line = ''
write(line, '(*(g0,1x))') (x(i), (y(j), j=1,n), i=2,1), 'end'
if (line /= 'end') error stop 5

print '(*(g0,1x))', (k(i), (r(j), j=1,n), i=1,0), 'end'
end program
