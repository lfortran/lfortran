program intrinsics_45
use iso_fortran_env, only : real_kinds, integer_kinds, character_kinds, logical_kinds
use iso_fortran_env, only : iostat_end
print *, size(real_kinds), size(integer_kinds), size(character_kinds), size(logical_kinds)
print *, iostat_end
if (iostat_end /= -1) error stop
! Does not work yet
!print *, real_kinds
end program
