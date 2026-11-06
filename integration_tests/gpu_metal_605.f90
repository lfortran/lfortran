module gpu_metal_605_m
  ! The temporary of `a(0:last)` in a routine the loop calls has the extent
  ! last - 0 + 1, which the host computes from an integer(4) size() and,
  ! with --descriptor-index-64, an integer(8) lower bound (#15892).
  implicit none
  type :: t
    integer, allocatable :: n(:)
  end type
contains
  pure real function f(x)
    type(t), intent(in) :: x
    real, allocatable :: a(:)
    integer :: last
    last = size(x%n) - 1
    allocate(a(0:last))
    a = 1
    f = sum(a)
  end function
end module

program gpu_metal_605
  use gpu_metal_605_m
  implicit none
  type(t) :: x
  real :: p(3)
  integer :: i
  allocate(x%n(3))
  x%n = [2, 5, 3]
  do concurrent (i = 1:3)
    p(i) = 1 + f(x)
  end do
  print *, p
  if (any(p /= 4)) error stop
end program
