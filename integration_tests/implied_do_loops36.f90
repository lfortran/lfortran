program implied_do_loops36
  implicit none
  real, allocatable :: x(:)
  integer :: i
  allocate(x(3))
  open(10, file='implied_do_loops36.dat', status='replace')
  write(10,*) 1.0, 2.0, 3.0
  close(10)
  open(10, file='implied_do_loops36.dat', status='old')
  call read_data(10, x)
  close(10, status='delete')
  if (abs(x(1) - 1.0) > 1e-6) error stop
  if (abs(x(2) - 2.0) > 1e-6) error stop
  if (abs(x(3) - 3.0) > 1e-6) error stop
  print *, "PASSED"
contains
  subroutine read_data(u, arr)
    integer, intent(in) :: u
    real, allocatable, intent(inout) :: arr(:)
    integer :: i
    read(u, *) (arr(i), i=1, size(arr))
  end subroutine
end program
