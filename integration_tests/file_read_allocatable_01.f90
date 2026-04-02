program file_read_allocatable_01
  implicit none
  integer :: i, n
  real, allocatable :: vals(:)

  n = 3
  allocate(vals(n))

  open(10, file='file_read_allocatable_01.dat', status='replace')
  write(10, *) 1.0, 2.0, 3.0
  close(10)

  open(10, file='file_read_allocatable_01.dat', status='old')
  read(10, *) (vals(i), i=1,n)
  close(10)

  if (abs(vals(1) - 1.0) > 1e-5) error stop
  if (abs(vals(2) - 2.0) > 1e-5) error stop
  if (abs(vals(3) - 3.0) > 1e-5) error stop

  print *, "PASS"
end program
