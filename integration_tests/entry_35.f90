! A dummy argument that appears only in an ENTRY statement, used in an
! array bound of the subprogram (#15228).
subroutine entry_35_s(n, a)
integer :: n, m
real :: a(n), b(m)
a = 1
return
entry entry_35_e(m, b)
b = real(size(b))
end subroutine

program entry_35
implicit none
real :: x(3), y(4)
call entry_35_s(3, x)
if (any(abs(x - 1) > 1e-6)) error stop 1
call entry_35_e(4, y)
if (any(abs(y - 4) > 1e-6)) error stop 2
print *, x, y
end program
