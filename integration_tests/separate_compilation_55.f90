program separate_compilation_55
  ! dot_product on a section of a derived-type array component
  use separate_compilation_55a, only: vector_t, transform_index, rdot, mixed
  implicit none
  type(vector_t) :: v
  integer :: res
  v%x = [1, 2, 3]
  v%r = [1.5, 2.0, 0.5]
  v%d = [0.5d0, 2.0d0]
  call transform_index(v, res)
  print *, res, rdot(v), mixed(v)
  if (res /= 14) error stop
  if (abs(rdot(v) - 6.5) > 1e-6) error stop
  if (abs(mixed(v) - 7.0d0) > 1d-12) error stop
end program
