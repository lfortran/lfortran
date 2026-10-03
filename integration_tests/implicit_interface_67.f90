! External subroutines with different argument lists are passed to a
! procedure in this file that never calls its dummy (#12833).
program implicit_interface_67
external s0, s1
real(8) :: y
y = 0
call ap(s0, y)
call ap(s1, y)
if (abs(y - 2) > 1d-12) error stop
print *, "ok"
end program

subroutine s0()
end subroutine

subroutine s1(x)
real(8) :: x
x = 9
end subroutine

subroutine ap(f, y)
external f
real(8) :: y
y = y + 1
end subroutine
