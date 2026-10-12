! An implied-do loop over items of different derived types writes each item
! as its own type: the loop is not given the type of its first item.
program implied_do_loops54
implicit none
type :: w
    integer :: k = 0
end type
type :: x
    real :: v = 0
end type
type :: base
    integer :: a = 1
end type
type, extends(base) :: child
    real :: r = 2.5
end type
type(x) :: a(2)
type(w) :: b(2)
type(base) :: p(2)
type(child) :: c(2)
integer, parameter :: m = 2
integer :: i, u, k1, k2, a1, a2, a3, a4
real :: v1, v2, r1, r2
character(80) :: line

a(1)%v = 1.5; a(2)%v = 2.5; b(1)%k = 7; b(2)%k = 8
p(2)%a = 3
c(2)%a = 4; c(2)%r = 5.5

print '(2(f3.1,1x,i0,1x))', (a(i), b(i), i=1,2)
print '(*(i0,1x,f3.1,1x))', (b(i), a(i), i=1,2)
print *, (a(i), b(i), i=1,2)
print '(*(i0,1x,i0,1x,f3.1,1x))', (p(i), c(i), i=1,2)

open(newunit=u, status='scratch', action='readwrite', form='formatted')
write(u, '(2(f3.1,1x,i0,1x))') (a(i), b(i), i=1,2)
write(u, '(*(i0,1x,f3.1,1x))') (b(i), a(i), i=1,2)
write(u, *) (a(i), b(i), i=1,2)
write(u, '(*(i0,1x,i0,1x,f3.1,1x))') (p(i), c(i), i=1,2)
rewind(u)
read(u, '(a)') line
if (trim(line) /= '1.5 7 2.5 8') error stop 1
read(u, '(a)') line
if (trim(line) /= '7 1.5 8 2.5') error stop 2
read(u, *) v1, k1, v2, k2
if (abs(v1 - 1.5) > 1e-6 .or. k1 /= 7) error stop 3
if (abs(v2 - 2.5) > 1e-6 .or. k2 /= 8) error stop 4
read(u, '(a)') line
if (trim(line) /= '1 1 2.5 3 4 5.5') error stop 5
close(u)

open(newunit=u, status='scratch', action='readwrite', form='formatted')
write(u, *) (p(i), c(i), i=1,2)
rewind(u)
read(u, *) a1, a2, r1, a3, a4, r2
if (a1 /= 1 .or. a2 /= 1 .or. abs(r1 - 2.5) > 1e-6) error stop 6
if (a3 /= 3 .or. a4 /= 4 .or. abs(r2 - 5.5) > 1e-6) error stop 7
close(u)

write(line, '(*(f3.1,1x,i0,1x))') (a(i), b(i), i=1,2)
if (trim(line) /= '1.5 7 2.5 8') error stop 8
write(line, '(*(i0,1x,i0,1x,f3.1,1x))') (p(i), c(i), i=1,2)
if (trim(line) /= '1 1 2.5 3 4 5.5') error stop 9
write(line, '(*(i0,1x,f3.1,1x))') (i, real(i), i=1,2)
if (trim(line) /= '1 1.0 2 2.0') error stop 10

write(line, '(*(f3.1,1x,i0,1x))') (a(i), b(i), i=1,m)
if (trim(line) /= '1.5 7 2.5 8') error stop 11
write(line, '(*(f3.1,1x,i0,1x))') (a(i), b(i), i=m,1,-1)
if (trim(line) /= '2.5 8 1.5 7') error stop 12
open(newunit=u, status='scratch', action='readwrite', form='formatted')
write(u, '(*(f3.1,1x,i0,1x))') (a(i), b(i), i=1,m)
write(u, *) (a(i), b(i), i=m,1,-1)
rewind(u)
read(u, '(a)') line
if (trim(line) /= '1.5 7 2.5 8') error stop 13
read(u, *) v1, k1, v2, k2
if (abs(v1 - 2.5) > 1e-6 .or. k1 /= 8) error stop 14
if (abs(v2 - 1.5) > 1e-6 .or. k2 /= 7) error stop 15
close(u)
print *, 'ok'
end program
