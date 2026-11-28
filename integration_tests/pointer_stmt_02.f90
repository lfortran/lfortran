! A DIMENSION statement, a type declaration without the array-spec and a
! POINTER statement (#15291): the attribute belongs to the array, not to its
! elements.
program pointer_stmt_02
implicit none
dimension q(:)
real(8) :: q
pointer q
real(8), target :: t(3)
t = 2d0
q => t
if (.not. associated(q, t)) error stop
if (abs(sum(q) - 6d0) > 1d-12) error stop
q => null()
if (associated(q)) error stop
print *, "ok"
end program
