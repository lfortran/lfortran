! A dummy procedure is passed to a procedure defined in another file
! (implicit_interface_73b.f90) after an external subroutine defined in this
! file was passed to it, so the dummy takes that subroutine's signature.
subroutine forward_73(f, a)
external s_73, f
real :: a(10)
call apply_73(s_73, a)
call s_73(a)
call apply_73(f, a)
end subroutine

subroutine forward_no_call_73(g, a)
external s_73, g
real :: a(10)
call apply_73(s_73, a)
call apply_73(g, a)
end subroutine

program implicit_interface_73
implicit none
external user_73
real :: a(10)
a = 0
call forward_73(user_73, a)
if (abs(a(1) - 102) > 1e-5) error stop
call forward_no_call_73(user_73, a)
if (abs(a(1) - 203) > 1e-5) error stop
print *, "ok"
end program

subroutine s_73(x)
real :: x(10)
x(1) = x(1) + 1
end subroutine

subroutine user_73(x)
real :: x(10)
x(1) = x(1) + 100
end subroutine
