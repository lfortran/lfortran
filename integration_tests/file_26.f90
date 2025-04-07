program file_26
  implicit none
  integer, parameter :: n = 5
  integer :: data_out(n), data_in(n)
  integer :: i, write_unit, read_unit, ios
  character(len=5) :: tmp
  data_out = (/ 1, 2, 3, 4, 5 /)

  open(newunit=write_unit, file='data.dat', form='unformatted', status='replace')
  write(write_unit) data_out
  close(write_unit)
  open(newunit=read_unit, file='data.dat', form='unformatted', status='old')
  read(read_unit) data_in
  close(read_unit)
  print *, data_out
  print *, data_in
  if (any(data_out /= data_in)) error stop

  open(newunit=write_unit, file='data.dat', form='unformatted', status='replace', access="stream")
  write(write_unit) data_out
  close(write_unit)
  open(newunit=read_unit, file='data.dat', form='unformatted', status='old', access="stream")
  read(read_unit) data_in
  close(read_unit)
  print *, data_out
  print *, data_in
  if (any(data_out /= data_in)) error stop

! TODO: Support stream for character type
!   open(newunit=write_unit, file='data2.dat', form='unformatted', status='replace', access="stream")
!   write(write_unit) "Hello"
!   close(write_unit)
!   open(newunit=read_unit, file='data2.dat', form='unformatted', status='old', access="stream")
!   read(read_unit) tmp
!   close(read_unit)
!   print *, tmp
!   if (trim(tmp) /= "Hello") error stop

end program