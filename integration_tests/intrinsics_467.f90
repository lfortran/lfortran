program intrinsics_467
  implicit none

  real(8), allocatable :: data(:,:)
  integer :: expected(6)

  allocate(data(2,3))

  data(1,:) = [1.0d0, -5.0d0, 3.0d2]
  data(2,:) = [4.0d0, 2.0d0, 6.0d0]
  expected = [1, 4, -5, 2, 300, 6]

  if (.not. all([nint(data)] == expected)) error stop
  if (.not. all([nint(data, 8)] == int(expected, 8))) error stop
  if (.not. all(abs([anint(data)] - real(expected, 8)) < 1.0d-12)) error stop

  print *, "pass"
end program intrinsics_467
