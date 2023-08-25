subroutine pbdv(dv)
implicit double precision (a-h,o-z)
dimension dv(0:*)
dv(0) = 1.0D0
end subroutine

program arrays_33
double precision :: dv(0:100)
call pbdv(dv)
print *, dv(0)
if (abs(dv(0)-1.0D0) > 1.0D-10) error stop
end program
