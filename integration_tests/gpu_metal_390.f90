module gpu_metal_390_m
  ! The result of a type-bound function of an element, copied into a
  ! temporary, in a loop whose arguments have two allocatable components:
  ! the temporary takes the shape of the array assigned to it.
  implicit none
  type tensor_t
    real, allocatable :: values_(:)
  contains
    procedure :: values
  end type
contains
  pure function values(self) result(v)
    class(tensor_t), intent(in) :: self
    real, allocatable :: v(:)
    v = self%values_
  end function
end module

program gpu_metal_390
  use gpu_metal_390_m
  implicit none
  type(tensor_t) :: x(3), y(3)
  real :: r(3)
  integer :: i
  do i = 1, 3
    x(i)%values_ = [real(i), real(2*i)]
    y(i)%values_ = [real(i)]
  end do
  do concurrent (i = 1:3)
    block
      real :: a(2)
      a(1:2) = x(i)%values()
      r(i) = sum(a) + y(i)%values_(1)
    end block
  end do
  print *, r
  if (any(r /= [4., 8., 12.])) error stop
end program
