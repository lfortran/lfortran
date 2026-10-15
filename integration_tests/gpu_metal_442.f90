module gpu_metal_442_m
  ! With --realloc-lhs-arrays, the host allocates the component a loop
  ! gives an element of an array argument, with the size the function
  ! called for the element gives it: here a type-bound function, whose
  ! passed object is the actual argument of its passed-object dummy
  ! argument, the first one or the one `pass` names.
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  end type
  type :: net_t
    real :: scale = 2
  contains
    procedure :: infer
    procedure, pass(self) :: shift
  end type
contains
  elemental function infer(self, x) result(y)
    class(net_t), intent(in) :: self
    type(tensor_t), intent(in) :: x
    type(tensor_t) :: y
    y%values_ = self%scale*x%values_
  end function

  elemental function shift(x, self) result(y)
    type(tensor_t), intent(in) :: x
    class(net_t), intent(in) :: self
    type(tensor_t) :: y
    y%values_ = x%values_ + self%scale
  end function
end module
program gpu_metal_442
  use gpu_metal_442_m
  implicit none
  type(net_t) :: net
  type(tensor_t), allocatable :: inputs(:), outputs(:), shifted(:)
  integer :: i
  allocate(inputs(3), outputs(3), shifted(3))
  do i = 1, 3
    inputs(i)%values_ = [real(i), 10.*i]
  end do
  do concurrent (i = 1:3)
    outputs(i) = net%infer(inputs(i))
  end do
  do concurrent (i = 1:3)
    shifted(i) = net%shift(inputs(i))
  end do
  do i = 1, 3
    if (.not. allocated(outputs(i)%values_)) error stop 1
    if (any(outputs(i)%values_ /= [2.*i, 20.*i])) error stop 2
    if (.not. allocated(shifted(i)%values_)) error stop 3
    if (size(shifted(i)%values_) /= 2) error stop 4
    if (any(shifted(i)%values_ /= [i + 2., 10.*i + 2])) error stop 5
  end do
  print *, "ok"
end program
