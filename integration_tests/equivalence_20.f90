program equivalence_20
implicit none

integer :: ia_long(5)
integer :: ia(2)
equivalence (ia_long(2), ia(2))

ia_long = -42
ia(2) = 2

if (ia_long(2) /= 2) error stop
if (ia(1) /= -42) error stop
if (ia(2) /= 2) error stop

end program
