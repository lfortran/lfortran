! Defined output of two types named t from different modules, each with a
! binding named wf, from a module and the program compiled separately.
program dtio_write_10
use dtio_write_10_ma, only: ta => t
use dtio_write_10_mb, only: tb => t
use dtio_write_10_user, only: show_a, show_b
implicit none
type(ta) :: x
type(tb) :: y
character(len=8) :: s

call show_a(x, s)
if (s /= "A1") error stop 1
call show_b(y, s)
if (s /= "B2") error stop 2
write(s, '(dt)') x
if (s /= "A1") error stop 3
print *, "ok"
end program
