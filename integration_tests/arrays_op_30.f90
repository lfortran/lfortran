module arrays_op_30_mod
  integer, parameter :: arr(3) = [1, 2, 3]
end module arrays_op_30_mod

program arrays_op_30
  use arrays_op_30_mod
  integer, parameter :: arr2(3) = [1, 2, 3]
  integer :: val1, val2
  
  val1 = arr(1)
  if (val1 /= 1) error stop
  
  val2 = arr2(1)
  if (val2 /= 1) error stop
  
end program arrays_op_30
