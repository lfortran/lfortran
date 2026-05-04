module shadow_a
use, intrinsic :: ISO_FORTRAN_ENV, only: int64
implicit none
end module shadow_a

module shadow_b
use shadow_a
use, intrinsic :: ISO_FORTRAN_ENV, only: int64
implicit none
end module shadow_b

program shadow_same_symbol
use shadow_b
implicit none

! no warning should be there

print *, "test passed"
end program shadow_same_symbol
