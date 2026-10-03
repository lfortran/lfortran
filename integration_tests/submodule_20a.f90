program submodule_20a
  use submodule_20b_mod
  implicit none
  type(t) :: x
  call x%foo()
  print *, "ok"
end program
