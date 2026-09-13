module implicit_interface_72_mod
implicit none
external add_half_72
contains

subroutine set_two_via_72(y)
real(8) :: y
external set_two_72
call apply_72(set_two_72, y)
end subroutine

subroutine add_half_via_72(y)
real(8) :: y
call apply_72(add_half_72, y)
end subroutine

end module

subroutine set_two_72(x)
real(8) :: x
x = 2
end subroutine

subroutine add_half_72(x)
real(8) :: x
x = x + 0.5d0
end subroutine
