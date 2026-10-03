module derived_type_nested_parameter_init_05_m
implicit none
type :: base_t
  integer :: arr(2) = [1, 2]
  integer :: x = 1
end type
type, extends(base_t) :: e_t
  integer :: z = 5
end type
type, extends(e_t) :: f_t
  integer :: w = 6
end type
type :: h_t
  type(e_t) :: e
  integer :: y = 3
end type
type(e_t), parameter :: pe = e_t([7, 8], 10, 50)
type(f_t), parameter :: pf = f_t([7, 8], 10, 50, 60)
type(h_t), parameter :: ph = h_t(pe, 30)
contains
subroutine check_e(e)
  type(e_t), intent(in) :: e
  if (any(e%arr /= [7, 8]) .or. e%x /= 10 .or. e%z /= 50) error stop
end subroutine
end module

program derived_type_nested_parameter_init_05
use derived_type_nested_parameter_init_05_m
implicit none
type(e_t) :: le
type(f_t) :: lf
type(h_t) :: lh
type(h_t) :: li = ph
le = pe
call check_e(le)
lf = pf
call check_e(lf%e_t)
if (lf%w /= 60) error stop
lh = ph
call check_e(lh%e)
if (lh%y /= 30) error stop
call check_e(li%e)
if (li%y /= 30) error stop
print *, "ok"
end program
