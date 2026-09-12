program gpu_metal_210
  ! Test: do concurrent reading components inherited from a parent type
  implicit none

  type :: base_t
    integer :: n_
    real :: upper_(3)
  end type

  type, extends(base_t) :: mid_t
    real :: mid_val_
  end type

  type, extends(mid_t) :: child_t
    real :: own_
  end type

  type(child_t) :: c
  real :: d(3)
  integer :: i

  c%n_ = 2
  c%upper_ = [1.0, 2.0, 3.0]
  c%mid_val_ = 5.0
  c%own_ = 10.0
  d = 0.0

  do concurrent (i = 1:3)
    d(i) = c%upper_(i) * real(c%n_) + c%mid_val_ + c%own_
  end do

  if (abs(d(1) - 17.0) > 1.0e-6) error stop
  if (abs(d(2) - 19.0) > 1.0e-6) error stop
  if (abs(d(3) - 21.0) > 1.0e-6) error stop
  print *, "PASS"
end program
