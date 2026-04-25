program array_section_14
  implicit none
  integer, parameter :: k = selected_int_kind(9)
  integer :: iarray(3,4)
  iarray(1,1:2) = [bit_size(1_k), huge(1_k)]
  if (iarray(1,1) /= bit_size(1_k)) error stop
  if (iarray(1,2) /= huge(1_k)) error stop
  if (kind(1_k) /= k) error stop
  print *, kind(1_k), iarray(1,1:2)
end program array_section_14
