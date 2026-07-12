program test
  integer :: x[*]
  x = 42
  sync all
  print *, x[2]
end program
