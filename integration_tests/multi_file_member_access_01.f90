program multi_file_member_access_01
  use member_access_mod
  implicit none

  type(string_buffer) :: buf

  if (buf%number_of_elems() /= 0) error stop 1
  print *, "OK"
end program multi_file_member_access_01
