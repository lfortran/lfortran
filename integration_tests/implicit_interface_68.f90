! Externals are passed from two different procedures to the same procedure
! in another file (implicit_interface_68b.f90) (#12832).
subroutine sa(y)
real(8) :: y
external set_two
call apply(set_two, y)
end subroutine

subroutine sb(y)
real(8) :: y
external ext_other
call apply(ext_other, y)
end subroutine

program implicit_interface_68
real(8) :: y
y = 0
call sa(y)
if (abs(y - 2) > 1d-12) error stop
call sb(y)
if (abs(y - 5) > 1d-12) error stop
print *, "ok"
end program

subroutine set_two(x)
real(8) :: x
x = 2
end subroutine
