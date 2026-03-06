program submodule_37
  use submodule_37_mod, only: mytype
  implicit none
  type(mytype) :: t
  integer, allocatable :: arr(:)
  real, allocatable :: rarr(:)

  t = mytype("1.0 2.0 3.0")

  rarr = t%get(mold=[0.])
  if (size(rarr) /= 3) error stop
  if (abs(rarr(1) - 1.0) > 1e-5) error stop
  if (abs(rarr(2) - 2.0) > 1e-5) error stop
  if (abs(rarr(3) - 3.0) > 1e-5) error stop

  arr = t%get(mold=[integer::])
  if (size(arr) /= 3) error stop
  if (arr(1) /= 1) error stop
  if (arr(2) /= 2) error stop
  if (arr(3) /= 3) error stop

  print *, "ok"
end program submodule_37
