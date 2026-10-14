module separate_compilation_63a_funcs
implicit none
abstract interface
    integer function ifunc(x)
        integer, intent(in) :: x
    end function
end interface
contains
    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
    end function
end module

module separate_compilation_63a
use separate_compilation_63a_funcs
implicit none
type :: pf_t
    procedure(ifunc), pointer, nopass :: f => null()
end type
type(pf_t), parameter :: t = pf_t(twice)
end module
