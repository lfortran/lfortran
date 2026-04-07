! Test: Metal codegen element-wise array binop in do concurrent
!
! Verifies that array binary operations (e.g., d = a - y) inside
! do concurrent with associate generate element-wise Metal code
! instead of pointer arithmetic between device/thread address spaces.

module gpu_metal_158_m
  implicit none
  type :: t
    real :: v(3)
  contains
    procedure :: get_v
  end type
contains
  pure function get_v(self) result(r)
    class(t), intent(in) :: self
    real :: r(3)
    r = self%v
  end function
end module

program gpu_metal_158
  use gpu_metal_158_m
  implicit none
  integer :: i
  real :: a(3), d(3)
  type(t) :: arr(1)

  a = [2.0, 4.0, 6.0]
  arr(1)%v = [1.0, 2.0, 3.0]

  do concurrent (i = 1:1)
    associate(y => arr(i)%get_v())
      d = a - y
    end associate
  end do

  if (abs(d(1) - 1.0) > 1e-6) error stop
  if (abs(d(2) - 2.0) > 1e-6) error stop
  if (abs(d(3) - 3.0) > 1e-6) error stop
  print *, "PASS"
end program
