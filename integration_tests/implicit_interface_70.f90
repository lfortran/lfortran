! An external is passed to a procedure whose dummy has an interface block, and
! is then called with an array element. The dummy's interface does not become
! the external's interface (#12825).
program implicit_interface_70
external s
real :: a(10)
a = 0
call lib(s, a)
call s(a(3))
if (abs(a(1) - 100) > 1e-5) error stop
if (abs(a(3) - 100) > 1e-5) error stop
print *, "ok"
end program

subroutine lib(p, a)
interface
subroutine p(x)
real :: x(*)
end subroutine
end interface
real :: a(10)
call p(a)
end subroutine

subroutine s(x)
real :: x(5)
x(1) = 100
end subroutine
