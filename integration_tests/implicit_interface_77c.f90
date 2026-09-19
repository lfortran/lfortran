! Module of implicit_interface_77.
module implicit_interface_77_mod
implicit none
contains
subroutine via(y)
real(8) :: y
external set_two
call apply(set_two, y)
end subroutine
end module

subroutine set_two(x)
real(8) :: x
x = 2
end subroutine
