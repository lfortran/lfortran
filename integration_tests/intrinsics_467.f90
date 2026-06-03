program main
  implicit none
  character(len=:), allocatable :: text

  text = "x"
  if(storage_size(text) /= 8) error stop "Expected storage size is 8"
  print *, "test passed"
  print *, storage_size(text)
end program main
