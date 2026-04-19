! Test: generic type-bound function returning allocatable array
! inside do concurrent offloaded to Metal GPU.
! Ensures VLA temporaries from subroutine_from_function pass
! are backed by device buffers instead of stack VLAs.
module gpu_metal_176_m
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  contains
    generic :: values => get_values
    procedure, private :: get_values
  end type
contains
  pure function get_values(self) result(v)
    class(tensor_t), intent(in) :: self
    real, allocatable :: v(:)
    allocate(v(size(self%values_)))
    v = self%values_
  end function
end module

program gpu_metal_176
  use gpu_metal_176_m
  implicit none
  integer :: i, n, mbs
  type(tensor_t) :: arr(2)
  real :: cost(2)
  n = 3
  mbs = 2
  do i = 1, mbs
    allocate(arr(i)%values_(n))
    arr(i)%values_ = real(i)
  end do
  cost = 0.0
  do concurrent (i = 1:mbs)
    block
      real :: a(n)
      a = arr(i)%values()
      cost(i) = sum(a**2)
    end block
  end do
  print *, cost(1)
  print *, cost(2)
  if (abs(cost(1) - 3.0) > 1.0e-6) error stop
  if (abs(cost(2) - 12.0) > 1.0e-6) error stop
end program
