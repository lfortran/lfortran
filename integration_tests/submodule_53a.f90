program submodule_53a
  use submodule_53_mod
  implicit none
  type(array_type) :: c
  real, dimension(2,3) :: input, output

  input = 1.0
  output = 0.0
  call setup(c)
  call c%get_partial_left_val(input, output)
  if (any(output /= input)) error stop "mismatch"
  print *, "PASSED"
end program submodule_53a
