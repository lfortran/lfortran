module decl_order_mod
implicit none
integer :: i
i = 1
end module

subroutine decl_order_sub()
implicit none
integer :: a
import :: y
a = 1
end subroutine

program decl_order
import :: x
integer :: b
use, intrinsic :: iso_c_binding
implicit none
block
implicit none
integer :: c
c = 1
end block
end program
