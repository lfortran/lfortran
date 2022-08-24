program intrinsics_45
use iso_fortran_env, only : real_kinds, integer_kinds, character_kinds, logical_kinds
print *, size(real_kinds), size(integer_kinds), size(character_kinds), size(logical_kinds)
! Does not work yet
!print *, real_kinds
end program
