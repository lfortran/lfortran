module common_44a_mod
implicit none
contains
subroutine set_first()
integer :: a
common /common_44_xb/ a
a = 5
end subroutine
end module
