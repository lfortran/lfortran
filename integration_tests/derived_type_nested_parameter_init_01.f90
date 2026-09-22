module derived_type_nested_parameter_init_01_m
implicit none
type :: a_t
  integer :: x = 1
  real :: r = 2.0
end type
type :: b_t
  type(a_t) :: a
  integer :: y = 3
end type
type(a_t), parameter :: pa = a_t(10, 20.0)
type(b_t), parameter :: pb = b_t(pa, 30)
type(b_t), parameter :: pc = b_t(a_t(40, 50.0), 60)
type(b_t) :: mb = pb
type(b_t) :: mc = pc
end module

program derived_type_nested_parameter_init_01
use derived_type_nested_parameter_init_01_m, only: mb, mc
implicit none
if (mb%a%x /= 10 .or. abs(mb%a%r - 20.0) > 1e-6 .or. mb%y /= 30) error stop
if (mc%a%x /= 40 .or. abs(mc%a%r - 50.0) > 1e-6 .or. mc%y /= 60) error stop
call check()
print *, mb%a%x, mb%y, mc%a%x, mc%y
contains
subroutine check()
  if (mb%a%x /= 10 .or. mc%a%x /= 40) error stop
end subroutine
end program
