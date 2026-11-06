! Explicit-shape dummies and function results whose upper bound precedes the
! lower bound have zero extent, also for a caller in another translation unit.
module separate_compilation_68a
implicit none
contains
    function seq(n) result(r)
        integer, intent(in) :: n
        integer :: r(n), i
        r = [(i, i=1,n)]
    end function

    function bounded(lo, hi) result(r)
        integer, intent(in) :: lo, hi
        integer :: r(lo:hi), i
        r = [(i, i=lo,hi)]
    end function

    integer function total(lo, hi, a)
        integer, intent(in) :: lo, hi
        integer, intent(in) :: a(lo:hi)
        total = 100*size(a) + 10*lbound(a, 1) + ubound(a, 1)
    end function
end module
