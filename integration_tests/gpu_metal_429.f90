module gpu_metal_429_m
  ! A derived-type component that holds allocatable array components,
  ! `self%m`, used by device code: read in a routine, passed whole to a
  ! type-bound routine, named in the loop body, and one level further down,
  ! `w%net%m`. Its arrays reach the device as those of an argument do.
  implicit none
  type :: map_t
    real, allocatable :: a(:), b(:)
  contains
    procedure :: apply
  end type
  type :: net_t
    integer :: k = 2
    type(map_t) :: m
  contains
    procedure :: infer
  end type
  type :: wrap_t
    type(net_t) :: net
  end type
contains
  elemental function apply(self, x) result(y)
    class(map_t), intent(in) :: self
    real, intent(in) :: x
    real :: y
    y = self%a(1) + x*self%b(size(self%b))
  end function

  elemental function infer(self, x) result(y)
    class(net_t), intent(in) :: self
    real, intent(in) :: x
    real :: y
    y = self%k*self%m%apply(x) + sum(self%m%a)
  end function
end module

program gpu_metal_429
  use gpu_metal_429_m
  implicit none
  type(net_t) :: net
  type(wrap_t) :: w
  real :: x(4), y(4), z(4), e(4)
  integer :: i
  allocate(net%m%a(2), net%m%b(3))
  net%m%a = [1., 10.]
  net%m%b = [5., 6., 3.]
  w%net = net
  w%net%k = 3
  x = [1., 2., 3., 4.]
  do concurrent (i = 1:4)
    y(i) = net%infer(x(i)) + net%m%b(1)
  end do
  do concurrent (i = 1:4)
    z(i) = w%net%infer(x(i))
  end do
  print *, y
  print *, z
  e = 2*(1 + 3*x) + 11 + 5
  if (any(abs(y - e) > 1e-5)) error stop 1
  e = 3*(1 + 3*x) + 11
  if (any(abs(z - e) > 1e-5)) error stop 2
end program
