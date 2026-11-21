! An external procedure with an assumed-shape dummy and a saved local,
! whose dummies' bounds inquire about the bounds and length of automatic
! local objects (F2018 10.1.11), called directly, through a dummy procedure
! and through a procedure pointer: all calls share one saved variable.
subroutine save_38_s(a, n, x, y)
double precision, intent(in) :: a(:)
integer, intent(in) :: n
integer :: w(n+1)
character(len=n) :: c
double precision, intent(inout) :: x(ubound(w, 1))
double precision, intent(inout) :: y(len(c))
integer, save :: k = 0
k = k + 1
x(1) = k
y(1) = 10 * k
end subroutine

program save_39
implicit none
interface
    subroutine save_38_s(a, n, x, y)
    double precision, intent(in) :: a(:)
    integer, intent(in) :: n
    double precision, intent(inout) :: x(n+1)
    double precision, intent(inout) :: y(n)
    end subroutine
end interface
procedure(save_38_s), pointer :: p
double precision :: a(3), x(3), y(2)
a = 1
call save_38_s(a, 2, x, y)
if (x(1) /= 1 .or. y(1) /= 10) error stop 1
call run(save_38_s)
if (x(1) /= 2 .or. y(1) /= 20) error stop 2
p => save_38_s
call p(a, 2, x, y)
if (x(1) /= 3 .or. y(1) /= 30) error stop 3
call save_38_s(a(1:3:2), 2, x, y)
if (x(1) /= 4 .or. y(1) /= 40) error stop 4
call save_38_s(a, 2, x, y)
if (x(1) /= 5 .or. y(1) /= 50) error stop 5
print *, x(1), y(1)
contains
    subroutine run(f)
    procedure(save_38_s) :: f
    call f(a, 2, x, y)
    end subroutine
end program
