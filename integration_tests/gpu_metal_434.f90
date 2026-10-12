module gpu_metal_434_m
  ! With --realloc-lhs-arrays, the host allocates the component a loop
  ! gives an element of an array argument, with the size the function
  ! called for the element gives it, here inside an `associate` construct,
  ! whose statements run whenever the function does.
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
  elemental function infer(self, x) result(y)
    class(net_t), intent(in) :: self
    type(tensor_t), intent(in) :: x
    type(tensor_t) :: y
    associate (s => self%scale)
      y%values_ = s*x%values_
    end associate
  end function
end module
program gpu_metal_434
  use gpu_metal_434_m
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
