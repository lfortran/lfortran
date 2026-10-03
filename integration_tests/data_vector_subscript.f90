program data_vector_subscript
  implicit none

  integer :: values(2)

  data values([1, 2]) /3, 4/

  print *, values
  if (any(values /= [3, 4])) error stop
end program data_vector_subscript
