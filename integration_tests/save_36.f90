! An external procedure with an assumed-shape dummy and a saved local,
! declared by an interface body, called directly, through a dummy
! procedure and through a procedure pointer: all calls share one saved
! variable.
subroutine save_35_counter(a, n)
double precision, intent(in) :: a(:)
integer, intent(out) :: n
integer, save :: k = 0
k = k + 1
n = k
end subroutine

program save_36
implicit none
interface
    subroutine save_35_counter(a, n)
    double precision, intent(in) :: a(:)
    integer, intent(out) :: n
    end subroutine
end interface
procedure(save_35_counter), pointer :: p
double precision :: a(3)
integer :: n
a = 1
call save_35_counter(a, n)
if (n /= 1) error stop 1
call run(save_35_counter, n)
if (n /= 2) error stop 2
p => save_35_counter
call p(a, n)
if (n /= 3) error stop 3
call save_35_counter(a, n)
if (n /= 4) error stop 4
print *, n
contains
    subroutine run(f, n)
    procedure(save_35_counter) :: f
    integer, intent(out) :: n
    double precision :: b(2)
    b = 2
    call f(b, n)
    end subroutine
end program
