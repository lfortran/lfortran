program intrinsics_452
implicit none

character(len=1) :: w(3)
character(len=len(w)) :: v(3)
integer :: idx

w(1) = "a"
w(2) = "b"
w(3) = "c"
v = w

idx = findloc(v(:3), w(2), dim=1)
print *, idx
if (idx /= 2) error stop

idx = findloc(v(:3), w(1), dim=1)
print *, idx
if (idx /= 1) error stop

idx = findloc(v(:3), "x", dim=1)
print *, idx
if (idx /= 0) error stop

end program
