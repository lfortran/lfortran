program equivalence_36
  ! Test: EQUIVALENCE in BLOCK DATA propagates DATA initialization
  ! to COMMON block variables
  implicit none
  character(2) :: d(2)
  common /blk/ d
  save /blk/

  if (d(1) /= 'TE') error stop
  if (d(2) /= 'ST') error stop
  print *, "OK"
end program

block data init_blk
  implicit none
  character(2) :: d(2), a(2)
  common /blk/ d
  equivalence (a, d)
  data a / 'TE', 'ST' /
end block data
