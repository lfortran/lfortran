module gpu_metal_444_m
  ! With --realloc-lhs-arrays, the host allocates the component a loop
  ! gives an element of an array argument, with the size the function
  ! called for the element gives it: here an internal function assigns its
  ! whole result inside an `associate` construct, as the XOR test of FIATS
  ! does.
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  contains
    procedure :: values
  end type
  interface tensor_t
    module procedure construct
  end interface
contains
  pure function construct(values) result(tensor)
    real, intent(in) :: values(:)
    type(tensor_t) :: tensor
    tensor%values_ = values
  end function
  pure function values(self) result(v)
    class(tensor_t), intent(in) :: self
    real, allocatable :: v(:)
    v = self%values_
  end function
end module
program gpu_metal_444
  use gpu_metal_444_m
  implicit none
  type(tensor_t), allocatable :: inputs(:,:), outputs(:,:)
  real :: harvest(2,3,4)
  integer :: b, k
  harvest = reshape([(mod(b*7, 10) / 10., b = 1, size(harvest))], shape(harvest))
  allocate(inputs(3,4), outputs(3,4))
  do concurrent (b = 1:3, k = 1:4)
    inputs(b,k) = tensor_t(merge(1., 0., harvest(:,b,k) < 0.5))
    outputs(b,k) = local_xor(inputs(b,k))
  end do
  do b = 1, 3
    do k = 1, 4
      if (size(outputs(b,k)%values_) /= 1) error stop 1
      if (outputs(b,k)%values_(1) /= merge(1., 0., sum(inputs(b,k)%values_) == 1)) error stop 2
    end do
  end do
  print *, "ok"
contains
  pure function local_xor(x) result(y)
    type(tensor_t), intent(in) :: x
    type(tensor_t) :: y
    associate (s => sum(x%values()))
      y = tensor_t([merge(1., 0., s > 0.99 .and. s < 1.01)])
    end associate
  end function
end program
