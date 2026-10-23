program spread_scalar_fortran_codegen
implicit none
integer(4) :: a
character(len=2, kind=1) :: c
logical(4) :: l
integer(4), dimension(3) :: r
real(4) :: x
a = 5
x = 1.50000000e+00
l = .true.
c = "ab"
r = Spread(a, 1, 3)
print *, r
print *, Spread(7, 1, 2)
print *, Spread(x, 1, 2)
print *, Spread(l, 1, 2)
print *, Spread(c, 1, 2)
print *, Spread("cd", 1, 2)
end program spread_scalar_fortran_codegen
