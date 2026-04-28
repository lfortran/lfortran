module nested_vars_05_m
  implicit none
  type :: data_type
    real, allocatable :: val(:,:)
  end type
contains
  subroutine use_data(d, n)
    type(data_type), dimension(:,:), intent(in) :: d
    integer, intent(out) :: n
    n = size(d, 1) + size(d, 2)
  end subroutine
end module

program nested_vars_05
  use nested_vars_05_m
  implicit none
  type(data_type), dimension(1,1) :: output
  integer :: res
  allocate(output(1,1)%val(1, 10))
  call inner()
  if (res /= 2) error stop
  print *, "PASSED"
contains
  subroutine inner()
    call use_data(output, res)
  end subroutine
end program
