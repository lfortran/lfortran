subroutine compute(a, b, proc)
integer :: a(2), b(1)
external proc
call sub1(b, proc)
call sub1(a(2), proc)
end subroutine

subroutine sub1(x, proc)
integer :: x(*)
external proc
call proc(x(1))
end subroutine

subroutine add_one(val)
integer :: val
val = val + 1
end subroutine

program implicit_interface_37
implicit none
integer :: a(2), b(1)
external add_one

a = [10, 20]
b = [5]

call compute(a, b, add_one)

if (b(1) /= 6) error stop
if (a(2) /= 21) error stop

print *, "ok"
end program
