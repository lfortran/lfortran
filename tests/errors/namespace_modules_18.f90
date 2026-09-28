! Error: non-type-bound defined operators are not imported by a namespace
! import (they have no name that could be qualified).
module namespace_modules_18_m
    implicit none
    type :: vec_t
        real :: x = 0
    end type
    interface operator(.dot.)
        module procedure dot
    end interface
contains
    real function dot(a, b)
        type(vec_t), intent(in) :: a, b
        dot = a%x*b%x
    end function
end module

program namespace_modules_18
    use, namespace :: v => namespace_modules_18_m
    implicit none
    type(v%vec_t) :: a, b
    print *, a .dot. b
end program
