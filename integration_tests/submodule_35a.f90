program submodule_35
  use submodule_35_mod, only: fill
  use submodule_35_util_mod, only: tensor_t
  implicit none
  type(tensor_t) :: t
  t%rows = 2
  t%cols = 3
  call fill(t)
  print *, "ok"
end program submodule_35
