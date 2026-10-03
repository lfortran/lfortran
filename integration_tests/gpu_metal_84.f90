program gpu_metal_84
! Test: lbound/ubound on allocatable derived-type array member
! inside do concurrent (Metal GPU offload)
  implicit none
  type :: t
    integer, allocatable :: arr(:)
  end type
  type(t) :: x
  integer :: i, r(2)
  allocate(x%arr(3))
  x%arr = [10, 20, 30]
  do concurrent (i = 1:2)
    r(i) = get_lb(x)
  end do
  if (r(1) /= 1) error stop
  if (r(2) /= 1) error stop
  print *, r(1), r(2)
contains
  pure integer function get_lb(x)
    type(t), intent(in) :: x
    get_lb = lbound(x%arr, 1)
  end function
end program
