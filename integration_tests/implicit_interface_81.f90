! An external subroutine is forwarded through a procedure in this file to a
! procedure in another file (implicit_interface_81b.f90) (#12819).
program implicit_interface_81
external s
real(8) :: y
y = 0
call wrap(s, y)
if (abs(y - 1) > 1d-12) error stop
print *, "ok"
end program

subroutine wrap(f, y)
external f
real(8) :: y
call lib(f, y)
end subroutine

subroutine s(x)
real(8) :: x
x = 1
end subroutine
