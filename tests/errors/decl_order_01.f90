module decl_order_mod
implicit none
integer :: i
i = 1

! A template accepts declarations only
template decl_order_t(T)
    type, deferred :: T
    use iso_fortran_env
    implicit none
    integer :: j
    j = 1
end template
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
