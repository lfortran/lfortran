! An ALLOCATABLE statement applies only to the entity of its own scoping unit:
! not to a derived-type component or to a local of a contained procedure with
! the same name (#15291).
program allocatable_stmt_04
implicit none
type t
    integer :: a(3)
end type
real, dimension(:) :: a
allocatable :: a
type(t) :: v
allocate(a(2))
a = 1
v%a = 2
call s()
if (sum(a) /= 2) error stop
if (sum(v%a) /= 6) error stop
print *, "ok"
contains
subroutine s()
    real :: a(3)
    a = 2
    if (sum(a) /= 6) error stop
end subroutine
end program
