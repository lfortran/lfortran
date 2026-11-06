! The parent component of an extended type (F2008 7.5.7.2), named after the
! parent type, read and written when the types are defined in the program,
! in a subroutine, in a block, through a grandparent, and in a type-bound
! procedure.
module parent_component_01_m
implicit none
type :: base_t
  integer :: x
end type
type, extends(base_t) :: mid_t
  integer :: y
contains
  procedure :: get_base
end type
contains
function get_base(self) result(b)
  class(mid_t), intent(in) :: self
  type(base_t) :: b
  b = self%base_t
end function
end module

program parent_component_01
use parent_component_01_m, only: mid_t, base_t
implicit none
type :: a_t
  integer :: x
end type
type, extends(a_t) :: c_t
  integer :: y
end type
type, extends(c_t) :: d_t
  integer :: z
end type
type :: point
  real :: x, y
end type
type, extends(point) :: point3d
  real :: z
end type
type(c_t) :: v
type(a_t) :: pa
type(d_t) :: w
type(c_t) :: pc
type(point3d) :: q
type(mid_t) :: m
type(base_t) :: b

v%x = 4
pa = v%a_t
if (pa%x /= 4) error stop 1
v%a_t = a_t(7)
if (v%x /= 7) error stop 2

q%point%x = 1.5
q%point%y = 2.5
if (q%x /= 1.5) error stop 3
if (q%point%y /= 2.5) error stop 4

w%x = 1
w%y = 2
w%z = 3
pa = w%a_t
if (pa%x /= 1) error stop 5
pc = w%c_t
if (pc%x /= 1 .or. pc%y /= 2) error stop 6
pa = w%c_t%a_t
if (pa%x /= 1) error stop 7
w%c_t%a_t%x = 10
if (w%x /= 10) error stop 8
w%c_t = c_t(20, 30)
if (w%x /= 20 .or. w%y /= 30 .or. w%z /= 3) error stop 9

call in_host()
call local_types()

block
  type :: e_t
    integer :: k
  end type
  type, extends(e_t) :: f_t
    integer :: l
  end type
  type(f_t) :: f
  type(e_t) :: e
  f%e_t%k = 31
  e = f%e_t
  if (e%k /= 31 .or. f%k /= 31) error stop 16
end block

m%x = 5
m%y = 6
b = m%base_t
if (b%x /= 5) error stop 10
m%base_t%x = 8
b = m%get_base()
if (b%x /= 8) error stop 11
print *, "ok"

contains

subroutine in_host()
  type(a_t) :: p
  v%x = 11
  p = v%a_t
  if (p%x /= 11) error stop 12
  v%a_t%x = 12
  if (v%x /= 12) error stop 13
end subroutine

subroutine local_types()
  type :: s_t
    integer :: k
  end type
  type, extends(s_t) :: t_t
    integer :: l
  end type
  type(t_t) :: t
  type(s_t) :: s
  t%k = 21
  s = t%s_t
  if (s%k /= 21) error stop 14
  t%s_t%k = 22
  if (t%k /= 22) error stop 15
end subroutine

end program
