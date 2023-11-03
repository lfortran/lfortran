program complex_15
implicit none
complex(8) :: k, z
k = (3, -4)
z = -k
print *, k, z

if (abs(z) == k) error stop
if (z /= (-3, 4)) error stop
end program
