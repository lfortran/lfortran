module gpu_metal_428_m
  ! With --realloc-lhs-arrays, a loop stores into an element of an array
  ! argument a value whose component has a size the host cannot tell before
  ! the launch, so the value is made on the device heap and copied into the
  ! element after the launch -- also without bounds checking, as with
  ! --fast.
  implicit none
  type :: tensor_t
    integer :: n = 0
    real, allocatable :: values_(:)
  end type
contains
  pure function large(x) result(t)
    real, intent(in) :: x(:)
    type(tensor_t) :: t
    integer :: c, j
    c = 0
    do j = 1, size(x)
      if (x(j) > 0.5) c = c + 1
    end do
    t%values_ = x(1:c)
    t%n = c
  end function
end module

program gpu_metal_428
  use gpu_metal_428_m
  implicit none
  integer, parameter :: n = 3
  real :: x(4, n)
  type(tensor_t), allocatable :: outputs(:)
  integer :: i
  x = 0.7
  allocate(outputs(n))
  do concurrent (i = 1:n)
    if (i /= 2) outputs(i) = large(x(:, i))
  end do
  do i = 1, n
    print *, allocated(outputs(i)%values_), outputs(i)%n
  end do
  if (.not. allocated(outputs(1)%values_)) error stop 1
  if (allocated(outputs(2)%values_)) error stop 2
  if (any(outputs%n /= [4, 0, 4])) error stop 3
  if (size(outputs(3)%values_) /= 4) error stop 4
  if (any(outputs(3)%values_ /= 0.7)) error stop 5
end program
