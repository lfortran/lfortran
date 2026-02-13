program submodule_21c
  use submodule_21a
  implicit none
  type(string_t) :: s
  s = "test"
  call do_stuff(s)
  if (s%string_ /= "hello") error stop
  print *, "ok"
end program
