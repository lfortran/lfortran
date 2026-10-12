module gpu_metal_433_m
  ! With --realloc-lhs-arrays, a loop gives the component of an element of
  ! an array argument the one of the value a function assigns its result
  ! whole, `y = twice(x)`. The host cannot tell the size before the launch,
  ! so the value is made on the device heap and stored after the launch.
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  end type
  type :: net_t
    real :: scale = 2
  contains
    procedure :: infer
  end type
contains
  elemental function twice(x) result(y)
    type(tensor_t), intent(in) :: x
    type(tensor_t) :: y
    y%values_ = 2*x%values_
  end function
  elemental function infer(self, x) result(y)
    class(net_t), intent(in) :: self
    type(tensor_t), intent(in) :: x
    type(tensor_t) :: y
    y = twice(x)
  end function
end module
program gpu_metal_433
  use gpu_metal_433_m
  implicit none
  type(net_t) :: net
  type(tensor_t), allocatable :: inputs(:), outputs(:)
  integer :: i
  allocate(inputs(3), outputs(3))
  do i = 1, 3
    inputs(i)%values_ = [real(i), 10.*i]
  end do
  do concurrent (i = 1:3)
    outputs(i) = net%infer(inputs(i))
  end do
  do i = 1, 3
    if (.not. allocated(outputs(i)%values_)) error stop 1
    if (any(outputs(i)%values_ /= [2.*i, 20.*i])) error stop 2
  end do
  print *, "ok"
end program
