program file_25
  implicit none
  integer :: io
  integer :: x_write, x_read
  x_write = 12345
  open(newunit=io, status="scratch")
  write(io, *) x_write
  rewind(io)
  read(io, *) x_read
  close(io)
  if (x_write /= x_read) error stop
end program 