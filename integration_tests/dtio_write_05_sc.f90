! Defined output of extended types with modules compiled separately: a binding
! inherited from, and one overriding, a binding of a type in another module,
! written in a module procedure and in the program.
program dtio_write_05_sc
use dtio_write_05_sc_base, only: t, out
use dtio_write_05_sc_ext, only: t2, t3, emit
implicit none
type(t2) :: y
type(t3) :: z
class(t), allocatable :: c
character(len=32) :: line

y%i = 2
z%i = 4
z%k = 5
allocate(c, source=z)
open(newunit=out, status='scratch', action='readwrite', form='formatted')
call emit(y, z, c)
write(out, '(dt,"|",dt,"|",dt)') z, y, c
rewind(out)
read(out, '(a)') line
if (trim(line) /= '<t:2> <t3:4,5> <t3:4,5>') error stop 1
read(out, '(a)') line
if (trim(line) /= '<t3:4,5>|<t:2>|<t3:4,5>') error stop 2
close(out)
print *, 'ok'
end program
