! An entry of an external subroutine with an assumed-shape dummy, declared
! by an interface body, is used as a procedure pointer target and as an
! actual argument.
subroutine entry_33_s(x, n)
real, intent(inout) :: x(:)
integer, intent(out) :: n
n = 1
return
entry entry_33_e(x, n)
n = 2 * size(x)
x = x + 2
end subroutine

program entry_33
implicit none
interface
    subroutine entry_33_e(x, n)
    real, intent(inout) :: x(:)
    integer, intent(out) :: n
    end subroutine
end interface
procedure(entry_33_e), pointer :: p
real :: a(4)
integer :: n
a = 1
p => entry_33_e
call p(a, n)
if (n /= 8) error stop 1
if (any(abs(a - 3) > 1e-6)) error stop 2
call p(a(1:4:2), n)
if (n /= 4) error stop 3
if (any(abs(a - [5, 3, 5, 3]) > 1e-6)) error stop 4
call run(entry_33_e, n)
if (n /= 6) error stop 5
print *, n, a
contains
    subroutine run(f, n)
    procedure(entry_33_e) :: f
    integer, intent(out) :: n
    real :: b(3)
    b = 0
    call f(b, n)
    if (any(abs(b - 2) > 1e-6)) error stop 6
    end subroutine
end program
