program intrinsics_87
implicit none
integer, allocatable :: C(:, :)
allocate(C(5, 10))
C = -10
C = abs(C)
print *, C
if (any(C /= 10)) error stop
end program

