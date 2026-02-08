program merge_01
  implicit none
  integer :: ios
  ! Error: Unequal character lengths (11/7) in MERGE intrinsic
  print *, merge("fail caught", "no fail", ios /= 0)
end program
