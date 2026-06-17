program test
  implicit none
  character(200) :: arr(201)
  
  arr(1) = "A"
  arr(201) = "B"
  
  print *, arr(1)
  if(arr(1) /= "A") error stop
  print *, arr(201)
  if(arr(201) /= "B") error stop
end program
