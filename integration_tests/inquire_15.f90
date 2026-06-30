program inquire_15
  implicit none

  integer :: recl4
  integer(8) :: recl8

  inquire(10, recl=recl4)
  inquire(10, recl=recl8)

  if (recl4 /= -1) error stop
  if (recl8 /= -1_8) error stop
  print *, "test passed"
end program inquire_15
