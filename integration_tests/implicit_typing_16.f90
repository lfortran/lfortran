subroutine sxy(r)
r = 1.0
end subroutine

function f(x)
f = 2*x
end function

! `sxy` and `f` are not referenced as procedures here, so they are
! implicitly typed local variables (F2023 19.3.1)
subroutine u(r1, r2)
sxy = 2.0
f = 5.0
r1 = sxy
r2 = f + sxy
end subroutine

subroutine w(r)
call sxy(r)
r = r + f(3.0)
end subroutine

program implicit_typing_16
call u(r1, r2)
if (abs(r1 - 2.0) > 1e-6) error stop
if (abs(r2 - 7.0) > 1e-6) error stop
call w(r3)
if (abs(r3 - 7.0) > 1e-6) error stop
print *, r1, r2, r3
end program
