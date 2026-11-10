! A deferred-shape array declared by a type declaration statement and given
! the POINTER attribute by a later POINTER statement (#15291)
program pointer_stmt_01
implicit none
real(8), dimension(:) :: p
pointer :: p
real(8), target :: t(3)
t = 2d0
p => t
if (.not. associated(p, t)) error stop
if (abs(sum(p) - 6d0) > 1d-12) error stop
p => null()
if (associated(p)) error stop
print *, "ok"
end program
