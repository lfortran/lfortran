module gpu_metal_444_a
  ! Two modules each define a derived type `state_t`, with different
  ! components. Device code creates a value of the first on the device
  ! heap, as its size is a count the routine works out, and a value of the
  ! second in storage the host sizes: the two types are told apart, so the
  ! second keeps its own components.
  implicit none
  type :: state_t
    real, allocatable :: v(:)
  end type
contains
  pure real function large(x)
    real, intent(in) :: x(:)
    type(state_t) :: t
    integer :: c, j
    c = 0
    do j = 1, size(x)
      if (x(j) > 0.5) c = c + 1
    end do
    allocate(t%v(c))
    t%v = x(1:c)
    large = sum(t%v)
  end function
end module

module gpu_metal_444_b
  implicit none
  type :: state_t
    integer :: n = 0
    real, allocatable :: w(:)
    real :: s = 0
  end type
contains
  pure real function small(x)
    real, intent(in) :: x(:)
    type(state_t) :: t
    allocate(t%w(size(x)))
    t%w = 2*x
    t%n = size(x)
    t%s = sum(t%w)
    small = t%s + t%n
  end function
end module

program gpu_metal_444
  use gpu_metal_444_a, only: large
  use gpu_metal_444_b, only: small
  implicit none
  integer, parameter :: n = 6
  real :: x(10, n), r(n), s(n)
  integer :: i, k, c
  do i = 1, n
    do k = 1, 10
      x(k, i) = mod(k * 7 + i * 3, 11) / 10.0
    end do
  end do
  do concurrent (i = 1:n)
    r(i) = large(x(:, i))
  end do
  do concurrent (i = 1:n)
    s(i) = small(x(:, i))
  end do
  do i = 1, n
    c = count(x(:, i) > 0.5)
    if (abs(r(i) - sum(x(1:c, i))) > 1e-4) error stop 1
    if (abs(s(i) - (2*sum(x(:, i)) + 10)) > 1e-4) error stop 2
  end do
  print *, "ok"
end program
