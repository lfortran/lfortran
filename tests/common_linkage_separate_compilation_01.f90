program common_linkage_separate_compilation_01
  implicit none
  integer :: x
  common /blk/ x
  x = 5
  if (x /= 5) error stop 1
end program common_linkage_separate_compilation_01

