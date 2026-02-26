program equivalence_18
implicit none

integer :: i1, i2
equivalence (i1, i2)
data i1/19/

if (i2 /= 19) error stop

print *, 'i2 =', i2, merge(': pass', ': fail', i2 == 19)

end
