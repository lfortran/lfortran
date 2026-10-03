program submodule_23a
  use submodule_23_mod, only: mytype
  implicit none
  type(mytype) :: obj
  if (.not. obj%check([1, 2])) error stop
  print *, "ok"
end program