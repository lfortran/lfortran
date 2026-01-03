program arrays_84
  implicit none
  integer :: arr(-5)
  print *, size(arr)
  if ( size(arr) /= 0 ) error stop
end program arrays_84
