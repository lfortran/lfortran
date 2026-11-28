program array_constructor_shape_01
  implicit none
  print *, [shape(1)]
  print *, [shape(1), 2]
  print *, [shape([1, 2])]
end program
