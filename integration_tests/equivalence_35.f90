! Test: EQUIVALENCE + DATA in BLOCK DATA initializes common block variable
! A variable initialized via DATA in a BLOCK DATA subprogram, where that
! variable is EQUIVALENCEd to a COMMON block variable, must be properly
! initialized (storage association with bit-reinterpretation).
program equivalence_35
  implicit none
  real :: x
  integer :: n
  common /blk/ x
  equivalence (n, x)
  if (n /= 34) error stop
end program

block data mydata
  implicit none
  real :: x
  integer :: n
  common /blk/ x
  equivalence (n, x)
  data n /34/
end block data
