program intrinsics_104
implicit none

integer, parameter :: new_len = len(new_line('a'))

print *, new_len
if (new_len /= 1) error stop

end program
