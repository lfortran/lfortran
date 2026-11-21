! A module procedure with an assumed-shape dummy passes an internal
! procedure as an actual argument, for which nested_vars gives it frame
! slots. Those are not saved state, so the unit that defines the module
! (save_49b.f90) and this one, which reads the module file, agree that the
! procedure has a by-data copy (also with --separate-compilation).
program save_49
use save_49_m, only: save_49_h
implicit none
real :: a(4), s
a = [1, 2, 3, 4]
call save_49_h(a, s)
if (abs(s - 11) > 1e-6) error stop 1
call save_49_h(a(1:4:2), s)
if (abs(s - 5) > 1e-6) error stop 2
print *, s
end program
