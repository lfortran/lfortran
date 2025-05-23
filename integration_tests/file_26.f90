program file_26
  implicit none
  integer, parameter :: n = 5
  integer :: data_out(n), data_in(n)
  integer, allocatable :: data_out_alloc(:)
  real, allocatable :: data_out_alloc_real(:)
  real :: data_in_real(n)
  integer :: i, write_unit, read_unit, ios
  character(len=2) :: tmp
  character(len=3) :: tmp2
  data_out = (/ 1, 2, 3, 4, 5 /)
  allocate(data_out_alloc(n))
  allocate(data_out_alloc_real(n))
  data_out_alloc = [1, 2, 3, 4, 5]
  data_out_alloc_real = [1, 2, 3, 4, 5]

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

  open(newunit=write_unit, file='data.dat', form='unformatted', status='replace', access="stream")
  write(write_unit) data_out_alloc
  close(write_unit)
  open(newunit=read_unit, file='data.dat', form='unformatted', status='old', access="stream")
  read(read_unit) data_in
  close(read_unit)
  print *, data_out_alloc
  print *, data_in
  if (any(data_out_alloc /= data_in)) error stop

  open(newunit=write_unit, file='data.dat', form='unformatted', status='replace', access="stream")
  write(write_unit) data_out_alloc_real
  close(write_unit)
  open(newunit=read_unit, file='data.dat', form='unformatted', status='old', access="stream")
  read(read_unit) data_in_real
  close(read_unit)
  print *, data_out_alloc_real
  print *, data_in_real
  if (any(data_out_alloc_real /= data_in_real)) error stop

  call test_sub(data_out)
  if(any(data_out /= [10, 20, 30, 40, 50])) error stop
contains

subroutine test_sub(res2)
    integer :: lun
    integer, parameter :: res(5) = [10, 20, 30, 40, 50]
    integer, intent(out) :: res2(:)

    open(newunit=lun, file="data.dat", status="replace", form="unformatted", access="stream")
    write(lun) res
    rewind(lun)
    read(lun) res2
    close(lun)
    
end subroutine test_sub
end program