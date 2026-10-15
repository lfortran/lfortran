module gpu_metal_399_m
  ! A derived-type value that device code creates -- the result of a
  ! function such as a structure constructor, bound to an associate name --
  ! has an allocatable array component the device has no heap to allocate.
  ! Its storage is an array of the shape the function gives the component,
  ! which the kernel gets from the host, sized per iteration. A procedure
  ! the value is passed to reads the component and its size, and sizes a
  ! local of its own from it. A value the kernel itself creates is stored
  ! the same way.
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  end type
contains
  pure function tensor(x) result(r)
    real, intent(in) :: x(:)
    type(tensor_t) :: r
    r%values_ = x
  end function

  pure real function total(s)
    type(tensor_t), intent(in) :: s
    total = sum(s%values_) + size(s%values_)
  end function

  pure real function doubled(s)
    type(tensor_t), intent(in) :: s
    real :: w(size(s%values_))
    w = s%values_
    doubled = 2 * sum(w)
  end function

  pure real function f(x, n)
    real, intent(in) :: x(:)
    integer, intent(in) :: n
    associate (s => tensor(x(1:n)))
      f = total(s) + doubled(s) + s%values_(n)
    end associate
  end function
end module

program gpu_metal_399
  use gpu_metal_399_m
  implicit none
  real :: x(4, 3), o(3), p(3)
  integer :: i, k
  do i = 1, 3
    do k = 1, 4
      x(k, i) = k + 10 * i
    end do
  end do
  do concurrent (i = 1:3)
    o(i) = 1 + f(x(:, i), i)
  end do
  do concurrent (i = 1:3)
    p(i) = total(tensor(x(1:i + 1, i)))
  end do
  do i = 1, 3
    if (abs(o(i) - (1 + 3 * sum(x(1:i, i)) + i + x(i, i))) > 1e-4) error stop 1
    if (abs(p(i) - (sum(x(1:i + 1, i)) + i + 1)) > 1e-4) error stop 2
  end do
  print *, o, p
end program
