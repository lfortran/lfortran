! Used by specification_inquiry_01.f90.
module specification_inquiry_01_m
implicit none
contains
    function f(n) result(r)
    integer, intent(in) :: n
    integer :: w(n + 2)
    integer :: r(size(w))
    w = 1
    r = 7
    end function

    function g(n) result(r)
    integer, intent(in) :: n
    integer :: w(-1:n)
    character(len=n + 1) :: c
    character(len=len(c)) :: r(ubound(w, 1) - lbound(w, 1) + 1)
    c = "x"
    r = c
    end function

    subroutine s(a, n, k)
    integer, intent(in) :: n
    integer :: w(0:n)
    real, intent(inout) :: a(size(w) - 1)
    integer, intent(out) :: k
    a = a + 1
    k = size(a)
    end subroutine
end module
