module separate_compilation_65a_funcs
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

module separate_compilation_65a
use separate_compilation_65a_funcs
implicit none
type :: pc
    procedure(ifunc), pointer, nopass :: f => null()
    integer :: n = 5
    integer :: k(2) = [0, 0]
end type
type :: outer
    type(pc) :: inner
    integer :: m = 7
end type
type(pc), parameter :: os(2) = [pc(twice, 1, [10, 20]), pc(twice, 2, [30, 40])]
type(outer), parameter :: o = outer(pc(twice, 4, [50, 60]), 8)
type(outer), parameter :: ts(2) = [outer(pc(twice, 11, [1, 2]), 12), &
    outer(pc(twice, 13, [3, 4]), 14)]
end module
