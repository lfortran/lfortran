#define WRAP(exp, desc) result = (exp)

program cpp_pre_11
  implicit none
  logical :: result
  result = .false.
  WRAP (.true., "space before paren")
  if (.not. result) error stop
  print *, "passed"
end program cpp_pre_11
