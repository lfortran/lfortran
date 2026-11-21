! A module procedure and a separate module procedure with an assumed-shape
! dummy and a saved local, in a module compiled separately (save_48b.f90),
! called with whole arrays, with sections and through a procedure pointer:
! all calls share one saved variable.
program save_48
use save_48_m, only: save_48_counter, save_48_sub_counter
implicit none
procedure(save_48_counter), pointer :: p
real :: a(6)
integer :: n
a = 1
call save_48_counter(a, n)
if (n /= 1) error stop 1
call save_48_counter(a(1:6:2), n)
if (n /= 2) error stop 2
p => save_48_counter
call p(a, n)
if (n /= 3) error stop 3
call save_48_sub_counter(a, n)
if (n /= 1) error stop 4
call save_48_sub_counter(a(2:6:2), n)
if (n /= 2) error stop 5
p => save_48_sub_counter
call p(a(1:2), n)
if (n /= 3) error stop 6
print *, n
end program
