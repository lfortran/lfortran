program associate_29
  implicit none

  integer :: n1, n2, i
  real, allocatable, target :: data(:)
  real, pointer :: p(:,:)

  n1 = 3
  n2 = 2

  allocate(data(n1*n2))

  do i = 1, n1*n2
     data(i) = real(i)
  end do

  p(1:n1, 1:n2) => data(1:n1*n2)

  if (any(p /= reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [3, 2]))) error stop

end program associate_29
