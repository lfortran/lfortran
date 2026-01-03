program file_25
  implicit none
  integer :: io
  integer :: x_write, x_read
  character(len=20) :: char_read
  x_write = 12345
  open(newunit=io, status="scratch")
  write(io, *) x_write
  rewind(io)
  read(io, *) x_read
  close(io)
  if (x_write /= x_read) error stop

  open(newunit=io, status="scratch")
  write(io, *) "Hello"
  rewind(io)
  read(io, *) char_read
  close(io)
  print *, char_read
  if (char_read /= "Hello") error stop
end program 