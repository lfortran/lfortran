program intrinsics_102
implicit none

real, allocatable :: arr(:), sin1d(:)
allocate(arr(10), sin1d(10))
arr = 1.0
sin1d = sin(arr)

print *, sin1d
if( any(abs(sin1d - 8.41471016e-01) > 1e-6) ) error stop

end program
