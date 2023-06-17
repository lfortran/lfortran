program intrinsics_17b
implicit none
real :: A, B
B = 2.0D0
A = log(B)
print *, A
if (abs(A - 0.693147182) > 1e-5) error stop
end program
