program dfloat_01
integer :: a
a = -2
print *, DFLOAT(a)
if (abs(DFLOAT(a) - (-2.0D0)) > 1e-10) error stop
end program
