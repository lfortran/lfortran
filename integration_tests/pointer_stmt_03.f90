! A POINTER statement before a derived-type definition does not apply to a
! component with the same name (#15291).
program pointer_stmt_03
implicit none
pointer :: a
type t
    real, allocatable :: a(:)
end type
real, dimension(:) :: a
real, target :: tt(2)
type(t) :: v
allocate(v%a(3))
v%a = 2
tt = 1
a => tt
if (sum(a) /= 2) error stop
if (sum(v%a) /= 6) error stop
print *, "ok"
end program
