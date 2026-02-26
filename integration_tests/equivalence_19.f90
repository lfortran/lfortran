program equivalence_19
implicit none

integer :: i1, i2
equivalence (i1, i2)

integer :: j1, j2, j3
equivalence (j1, j2, j3)

integer :: k1, k2, k4
real :: r3
equivalence (k1, k2, r3, k4)

i1 = 1; i2 = 2
if (i1 /= 2) error stop

j1 = 1; j2 = 2; j3 = 3
if (j1 /= 3) error stop
if (j2 /= 3) error stop

k1 = 1; k2 = 2; r3 = 3.3; k4 = 4
if (k1 /= 4) error stop
if (k2 /= 4) error stop

end program
