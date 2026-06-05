program test
  implicit none
  character(200) :: arr(201)
  
  arr(1) = "A"
  arr(201) = "B"
  
  print *, arr(1)
  print *, arr(201)
end program
