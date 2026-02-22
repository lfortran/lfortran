program submodule_21a
  use submodule_21b_string_mod, only : string_t
  implicit none
  type(string_t) :: s
  s = "hello"
  if (s%string_ /= "hello") error stop
  s = s%file_extension()
  if (s%string_ /= "") error stop
  print *, "ok"
end program submodule_21a
