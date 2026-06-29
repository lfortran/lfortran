program internal_read_section
  implicit none

  integer :: value
  character(len=2) :: text(1)
  

  text = ["42"]

  read(text(1:1), "(I2)") value

  print *, value
end program internal_read_section
