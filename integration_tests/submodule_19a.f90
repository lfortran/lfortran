program submodule_19a
  use m_submodule_19
  implicit none
  type(string_t) :: s
  s%s = "hello"
  call assign_character_to_string_t(s)
  if (s%s /= "hello") error stop
  print *, "ok"
end program submodule_19a
