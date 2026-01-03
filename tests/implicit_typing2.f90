subroutine f(ifault, r, n, b)
integer :: a(n)
integer :: b(n)
ifault = 4
r = 3
end subroutine

integer function g(ifault, r, n, b)
integer :: a(n)
integer :: b(n)
ifault = 4
r = 3
end function
