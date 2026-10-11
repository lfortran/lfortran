module gpu_metal_394_m
  ! Shared lowering allocates a temporary of device code and then
  ! reallocates it to the shape it already has, or allocates it several
  ! times with that one shape: the copy of `s%values_` in `2 * s%values_`,
  ! or the result of `values(s)` bound to an associate name. A device has
  ! no heap, so such a temporary is an array of that shape, known on entry
  ! to the routine, as one allocated once is.
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

  pure function values(s) result(y)
    type(tensor_t), intent(in) :: s
    real, allocatable :: y(:)
    y = s%values_
  end function

  pure function scaled(s, a) result(r)
    type(tensor_t), intent(in) :: s
    real, intent(in) :: a
    type(tensor_t) :: r
    associate (y => values(s))
      r = tensor(a * y)
    end associate
  end function

  pure real function doubled_sum(s)
    type(tensor_t), intent(in) :: s
    real :: w(size(s%values_))
    w = 2 * s%values_
    doubled_sum = sum(w)
  end function

  pure real function total(s)
    type(tensor_t), intent(in) :: s
    total = sum(s%values_)
  end function
end module

program gpu_metal_394
  use gpu_metal_394_m
  implicit none
  type(tensor_t) :: t
  real :: o(3), p(3)
  integer :: i
  allocate(t%values_(3))
  t%values_ = [1., 2., 3.]
  do concurrent (i = 1:3)
    o(i) = i + doubled_sum(t)
    p(i) = 1 + total(scaled(t, real(i)))
  end do
  do i = 1, 3
    if (abs(o(i) - (i + 12)) > 1e-5) error stop 1
    if (abs(p(i) - (1 + 6 * i)) > 1e-5) error stop 2
  end do
  print *, o, p
end program
