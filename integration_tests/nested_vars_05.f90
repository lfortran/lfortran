program nested_vars_05
  implicit none

  type :: array_type
    real, allocatable, dimension(:,:) :: val
  end type array_type

  type(array_type), dimension(1,1) :: output
  integer :: n

  n = 3
  allocate(output(1,1)%val(n, n))
  output(1,1)%val = 1.0
  output(1,1)%val(2,2) = 5.0

  call proc()

contains

  subroutine proc()
    implicit none
    if (abs(output(1,1)%val(1,1) - 1.0) > 1e-5) error stop "Expected val(1,1) = 1.0"
    if (abs(output(1,1)%val(2,2) - 5.0) > 1e-5) error stop "Expected val(2,2) = 5.0"
  end subroutine proc

end program nested_vars_05
