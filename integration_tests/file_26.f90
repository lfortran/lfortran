program file_26
  implicit none
  integer, parameter :: n = 5
  integer :: data_out(n), data_in(n)
  integer :: i, write_unit, read_unit, ios
  character(len=2) :: tmp
  character(len=3) :: tmp2
  data_out = (/ 1, 2, 3, 4, 5 /)

  open(newunit=write_unit, file='data.dat', form='unformatted', status='replace')
  write(write_unit) data_out
  write(9, *) data_out
  close(write_unit)
  open(newunit=read_unit, file='data.dat', form='unformatted', status='old')
  read(read_unit) data_in
  close(read_unit)
  print *, data_out
  print *, data_in
  if (any(data_out /= data_in)) error stop

  open(newunit=write_unit, file='data.dat', form='unformatted', status='replace', access="stream")
  write(write_unit) data_out
  write(9, *) data_out
  close(write_unit)
  open(newunit=read_unit, file='data.dat', form='unformatted', status='old', access="stream")
  read(read_unit) data_in
  close(read_unit)
  print *, data_out
  print *, data_in
  if (any(data_out /= data_in)) error stop

  open(newunit=write_unit, file='data2.dat', form='unformatted', status='replace')
  write(write_unit) "Hello"
  close(write_unit)
  open(newunit=read_unit, file='data2.dat', form='unformatted', status='old')
  read(read_unit) tmp, tmp2
  close(read_unit)
  print *, tmp, " ", tmp2
  if (tmp /= "He" .or. tmp2 /= "llo") error stop
  open(newunit=write_unit, file='data2.dat', form='unformatted', status='replace', access="stream")
  write(write_unit) "Hello"
  close(write_unit)
  open(newunit=read_unit, file='data2.dat', form='unformatted', status='old', access="stream")
  read(read_unit) tmp, tmp2
  close(read_unit)
  print *, tmp, " ", tmp2
  if (tmp /= "He" .or. tmp2 /= "llo") error stop

end program