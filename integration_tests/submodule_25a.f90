program submodule_25a
  use submodule_25_m, only: tensor_t, compute
  implicit none
  type(tensor_t) :: t
  t = compute()
  if (t%n_ /= 2) error stop
  if (abs(t%data_(1) - 2.0d0) > 1.0d-10) error stop
  if (abs(t%data_(2) - 4.0d0) > 1.0d-10) error stop
  print *, "ok"
end program submodule_25a
