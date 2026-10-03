! Test: struct with type-bound procedure not called inside do concurrent.
! Previously, the Metal codegen emitted the struct method as an inline
! function even though it was only referenced by the struct type, causing
! a duplicate definition error in the Metal shader.
module gpu_metal_128_m
  implicit none
  type :: inner_t
    real :: x
  end type
  type :: container_t
    type(inner_t), allocatable :: items_(:)
  contains
    procedure :: get_items
  end type
contains
  function get_items(self) result(res)
    class(container_t), intent(in) :: self
    type(inner_t), allocatable :: res(:)
    allocate(res(size(self%items_)))
    res = self%items_
  end function
end module

program gpu_metal_128
  use gpu_metal_128_m
  implicit none
  type(container_t) :: c(2)
  type(inner_t), allocatable :: items(:)
  real :: arr(4)
  integer :: i

  c(1) = container_t([inner_t(1.0)])
  c(2) = container_t([inner_t(2.0)])
  items = c(1)%get_items()

  do concurrent(i = 1:4)
    arr(i) = real(i * size(c))
  end do

  if (abs(arr(1) - 2.0) > 1e-5) error stop
  if (abs(arr(2) - 4.0) > 1e-5) error stop
  if (abs(arr(3) - 6.0) > 1e-5) error stop
  if (abs(arr(4) - 8.0) > 1e-5) error stop
  if (size(items) /= 1) error stop
  if (abs(items(1)%x - 1.0) > 1e-5) error stop
  print *, "PASSED"
end program
