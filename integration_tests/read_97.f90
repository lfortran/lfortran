program read_97
  implicit none

  integer :: value
  character(len=2) :: text(1)

  text = ["42"]
  read(text(1:1), "(I2)") value

  if (value /= 42) error stop
end program read_97
