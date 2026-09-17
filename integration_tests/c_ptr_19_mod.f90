module lfortran_intrinsic_fake
implicit none

type :: c_ptr
    integer :: x
end type

type :: c_funptr
    integer :: x
end type

type(c_ptr), parameter :: c_null_ptr = c_ptr(17)
type(c_funptr), parameter :: c_null_funptr = c_funptr(23)
end module
