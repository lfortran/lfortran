module gpu_metal_445_a
  ! Two modules each define a derived type `state_t`, with different
  ! components, which the routines two loops call hold: the device code of
  ! the two kernels has a struct for each.
  implicit none
  type :: state_t
    real :: v = 1
  end type
contains
  pure real function fa(x)
    real, intent(in) :: x
    type(state_t) :: t
    t%v = x
    fa = 2*t%v
  end function
end module
module gpu_metal_445_b
  implicit none
  type :: state_t
    integer :: n = 0
    real :: s = 0
  end type
contains
  pure real function fb(x)
    real, intent(in) :: x
    type(state_t) :: t
    t%n = 3
    t%s = x
    fb = t%s + t%n
  end function
end module
program gpu_metal_445
  use gpu_metal_445_a, only: fa
  use gpu_metal_445_b, only: fb
  implicit none
  real :: x(4), r(4), s(4)
  integer :: i
  x = [1., 2., 3., 4.]
  do concurrent (i = 1:4)
    r(i) = fa(x(i))
  end do
  do concurrent (i = 1:4)
    s(i) = fb(x(i))
  end do
  if (any(r /= 2*x)) error stop 1
  if (any(s /= x + 3)) error stop 2
  print *, "ok"
end program
