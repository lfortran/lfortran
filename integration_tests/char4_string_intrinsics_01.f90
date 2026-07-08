program char4_string_intrinsics_01
  implicit none
  character(kind=4, len=5) :: s = 4_"ab"
  character(kind=4, len=1) :: c

  ! len_trim with character(kind=4) previously ICE'd
  if (len_trim(s) /= 2) error stop 1

  ! repeat with character(kind=4) previously ICE'd
  if (repeat(4_"x", 3) /= 4_"xxx") error stop 2
  c = "x"
  if (repeat(c, 3) /= 4_"xxx") error stop 3

  print *, "ok"
end program
