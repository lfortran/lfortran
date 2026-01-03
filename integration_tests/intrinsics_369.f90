program intrinsics_369
implicit none
real(kind(1d0)):: x = huge(x)
print *, x
if (abs(x - huge(x)) > 1e-8) error stop
end program intrinsics_369
