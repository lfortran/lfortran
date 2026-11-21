! An entry of an external subroutine with an assumed-shape dummy, declared
! by an interface body, is called by its own name.
subroutine entry_32_s(x, n)
real, intent(inout) :: x(:)
integer, intent(out) :: n
n = 1
x = x + 1
return
entry entry_32_e(x, n)
n = 2 * size(x)
x = x + 2
end subroutine

program entry_32
implicit none
interface
    subroutine entry_32_e(x, n)
    real, intent(inout) :: x(:)
    integer, intent(out) :: n
    end subroutine
    subroutine entry_32_s(x, n)
    real, intent(inout) :: x(:)
    integer, intent(out) :: n
    end subroutine
end interface
real :: a(4)
integer :: n
a = 1
call entry_32_e(a, n)
if (n /= 8) error stop 1
if (any(abs(a - 3) > 1e-6)) error stop 2
call entry_32_s(a(1:4:2), n)
if (n /= 1) error stop 3
if (any(abs(a - [4, 3, 4, 3]) > 1e-6)) error stop 4
call entry_32_e(a(2:3), n)
if (n /= 4) error stop 5
if (any(abs(a - [4, 5, 6, 3]) > 1e-6)) error stop 6
print *, n, a
end program
