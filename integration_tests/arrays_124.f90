program test
  implicit none
  character(:), allocatable :: c(:)
  

  allocate(character(5) :: c(3))
  c(1) = "apple"
  c(2) = "mango"
  c(3) = "peach"
  
  if (.not. allocated(c)) error stop 1
  
  deallocate(c)
  
  if (allocated(c)) error stop 2
end program
