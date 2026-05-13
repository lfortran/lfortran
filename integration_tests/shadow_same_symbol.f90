module a
use, intrinsic :: ISO_FORTRAN_ENV, only: int64
implicit none
end module a

module b
use a
use, intrinsic :: ISO_FORTRAN_ENV, only: int64
implicit none
end module b

program c
use b
implicit none
!no warnign should be there
print *, "test passed"
end program c

