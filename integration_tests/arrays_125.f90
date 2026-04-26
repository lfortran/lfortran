program arrays_125
  use arrays_125_types
  implicit none

  type(op_type), target :: a, b
  type(op_type), pointer :: c
  real :: inp(2,3), out(2,3)

  allocate(a%val(2,3), b%val(2,3))
  a%val = 1.0
  b%val = 2.0

  c => multiply_op(a, b)

  inp = 1.0
  call c%get_partial_left_val(inp, out)

  if (abs(out(1,1) - 1.0) > 1.0e-5) error stop
  print *, "ok"
end program
