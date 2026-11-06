! A kernel that can stop, in a module of another file (--separate-compilation):
! the twin the caller calls replays the stop before it returns, so the
! program stops before it prints the result.
program gpu_deferred_sync_07
use gpu_deferred_sync_07_mod, only: scaled
implicit none
real :: x(100), y(100)
x = 1
x(63) = -1
call scaled(x, y)
print *, "a negative value was scaled:", sum(y)
end program
