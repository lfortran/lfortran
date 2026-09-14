module derived_type_nested_parameter_init_02_m
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
contains
subroutine check_module()
  type(b_t) :: lb = pb
  type(b_t) :: lc = pc
  if (lb%a%x /= 10 .or. abs(lb%a%r - 20.0) > 1e-6 .or. lb%y /= 30) error stop
  if (lc%a%x /= 40 .or. abs(lc%a%r - 50.0) > 1e-6 .or. lc%y /= 60) error stop
end subroutine
end module

program derived_type_nested_parameter_init_02
use derived_type_nested_parameter_init_02_m, only: check_module
implicit none
type :: c_t
  integer :: x = 1
end type
type :: d_t
  type(c_t) :: c
  integer :: y = 3
end type
type(c_t), parameter :: qc = c_t(70)
type(d_t), parameter :: qd = d_t(qc, 80)
type(d_t), parameter :: qe = d_t(c_t(90), 100)
call check_module()
call check_program()
print *, "ok"
contains
subroutine check_program()
  type(d_t) :: ld = qd
  type(d_t) :: le = qe
  if (ld%c%x /= 70 .or. ld%y /= 80) error stop
  if (le%c%x /= 90 .or. le%y /= 100) error stop
end subroutine
end program
