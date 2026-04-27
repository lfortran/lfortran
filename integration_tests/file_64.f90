program file_64
  implicit none
  real, allocatable :: x(:)
  integer :: i
  allocate(x(3))

  open(10, file='file_64_data.dat', status='replace')
  write(10,*) 1.0, 2.0, 3.0
  close(10)

  open(10, file='file_64_data.dat', status='old')
  call read_data(10, x)
  close(10)

  if (abs(x(1) - 1.0) > 1e-6) error stop
  if (abs(x(2) - 2.0) > 1e-6) error stop
  if (abs(x(3) - 3.0) > 1e-6) error stop
contains
  subroutine read_data(unit_num, arr)
    integer, intent(in) :: unit_num
    real, allocatable, intent(inout) :: arr(:)
    integer :: i
    read(unit_num, *) (arr(i), i=1, size(arr))
  end subroutine
end program
