program submodule_38
  use submodule_38_mod, only : child_t
  implicit none
  type(child_t) :: c
  double precision :: v(3)
  double precision, allocatable :: r(:)

  c%k_ = 2
  v = [1.0d0, 2.0d0, 3.0d0]
  r = c .x. v

  if (size(r) /= 3) error stop
  if (abs(r(1) - 2.0d0) > 1.0d-10) error stop
  if (abs(r(2) - 4.0d0) > 1.0d-10) error stop
  if (abs(r(3) - 6.0d0) > 1.0d-10) error stop
  print *, "ok"
end program
