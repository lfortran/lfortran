module gpu_metal_431_m
  ! With --realloc-lhs-arrays, the host allocates the component a loop
  ! gives an element of an array argument, with the size the function
  ! called for the element gives it: here a generic constructor that is a
  ! separate module procedure, which gives the size in the body its
  ! submodule has, not in the interface the call reaches.
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  end type
  interface tensor_t
    pure module function construct(values) result(tensor)
      implicit none
      real, intent(in) :: values(:)
      type(tensor_t) :: tensor
    end function
  end interface
end module
submodule (gpu_metal_431_m) gpu_metal_431_s
  implicit none
contains
  module procedure construct
    tensor%values_ = values
  end procedure
end submodule
program gpu_metal_431
  use gpu_metal_431_m
  implicit none
  type(tensor_t), allocatable :: inputs(:)
  real :: c(3, 5)
  integer :: i
  allocate(inputs(3))
  call random_number(c)
  do concurrent (i = 1:3)
    inputs(i) = tensor_t(c(i,:))
  end do
  if (.not. allocated(inputs(2)%values_)) error stop 3
  if (any(inputs(2)%values_ /= c(2,:))) error stop 4
  print *, "ok"
end program
