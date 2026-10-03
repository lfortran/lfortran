module select_rank_29_mod
  implicit none
  type :: init_type
    real, allocatable :: data(:)
  contains
    procedure :: initialise
  end type
contains
  pure subroutine initialise(self, input)
    class(init_type), intent(inout) :: self
    real, dimension(..), intent(out) :: input
    select rank(input)
    rank(0)
      input = self%data(1)
    rank(1)
      input(:) = self%data(:)
    end select
  end subroutine
end module

program select_rank_29
  use select_rank_29_mod
  implicit none
  type(init_type) :: obj
  real :: x
  real :: arr(3)

  allocate(obj%data(3))
  obj%data = [10.0, 20.0, 30.0]

  call obj%initialise(x)
  if (abs(x - 10.0) > 1e-5) error stop

  call obj%initialise(arr)
  if (abs(arr(1) - 10.0) > 1e-5) error stop
  if (abs(arr(2) - 20.0) > 1e-5) error stop
  if (abs(arr(3) - 30.0) > 1e-5) error stop

  print *, "PASS"
end program
