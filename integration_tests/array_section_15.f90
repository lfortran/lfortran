program array_section_15
  implicit none
  integer, parameter :: k = selected_int_kind(0)
  integer :: iarray(3,4)
  iarray(1,1:2) = [bit_size(1_k), huge(1_k)]
  if (kind(1_k) /= k) error stop
  if (iarray(1,1) /= bit_size(1_k)) error stop
  if (iarray(1,2) /= huge(1_k)) error stop
  print *, kind(1_k), iarray(1,1:2)
end program array_section_15
