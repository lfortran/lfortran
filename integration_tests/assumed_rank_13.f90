program main
  implicit none

  character(len=:), allocatable :: text

  text = "x"
  print *, storage_size(text)
end program main