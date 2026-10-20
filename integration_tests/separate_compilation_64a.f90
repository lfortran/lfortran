module separate_compilation_64a_funcs
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
    integer function thrice(x)
        integer, intent(in) :: x
        thrice = 3*x
    end function
end module

module separate_compilation_64a
use separate_compilation_64a_funcs
implicit none
type :: pf_t
    procedure(ifunc), pointer, nopass :: f => null()
end type
type :: outer_t
    type(pf_t) :: inner
end type
type :: top_t
    type(outer_t) :: mid
end type
type(outer_t), parameter :: o = outer_t(pf_t(twice))
type(outer_t), parameter :: os(2) = [outer_t(pf_t(thrice)), outer_t(pf_t(twice))]
type(top_t), parameter :: t = top_t(outer_t(pf_t(thrice)))
end module
