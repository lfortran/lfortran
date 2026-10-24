! Defined output with a module compiled separately: the statements of a module
! procedure and of the program, each with a type-bound and a non-type-bound
! defined output procedure of types from another module.
program dtio_write_01_sc
use dtio_write_01_sc_types, only: t, s, write(formatted), out, nunit
use dtio_write_01_sc_io, only: emit
implicit none
type(t) :: y
type(s) :: w
character(len=32) :: line

y%v = 1
w%v = 2
open(newunit=out, status='scratch', action='readwrite', form='formatted')
call emit(y, w)
if (nunit /= 1) error stop 1
write(out, '(dt,"|",dt)') w, y
rewind(out)
read(out, '(a)') line
if (trim(line) /= '<DT:1> {2}') error stop 2
read(out, '(a)') line
if (trim(line) /= '3 <DT:1>') error stop 3
read(out, '(a)') line
if (trim(line) /= '{2}|<DT:1>') error stop 4
close(out)
print *, 'ok'
end program
