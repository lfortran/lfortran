program test
  implicit none
  character(:), allocatable :: c(:)
  
  c = ["apple", "mango", "peach"]
  if (.not. allocated(c)) error stop 1
  
  deallocate(c)
  
  if (allocated(c)) error stop 2
  
  print *, "PASS"
end program