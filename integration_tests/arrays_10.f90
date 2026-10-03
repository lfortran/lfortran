program arrays_10
  implicit none
  integer, pointer :: x(:)
  allocate(x(1))
  x = [ 1 ]
  deallocate(x)
end program arrays_10
