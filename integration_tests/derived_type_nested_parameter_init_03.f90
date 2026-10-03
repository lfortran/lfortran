module derived_type_nested_parameter_init_03_m
implicit none
type :: a_t
  integer :: arr(3) = [1, 2, 3]
  character(len=4) :: s = 'dflt'
  integer, allocatable :: al(:)
  integer, pointer :: p => null()
  integer :: x = 1
end type
type :: b_t
  type(a_t) :: a
  character(len=3) :: t = 'zzz'
  integer :: y = 3
end type
type :: c_t
  type(b_t) :: b
  integer :: z = 5
end type
type(a_t), parameter :: pa = a_t([7, 8, 9], 'qq', null(), null(), 10)
type(b_t), parameter :: pb = b_t(pa, 'ab', 30)
type(b_t), parameter :: pc = b_t(a_t([4, 5, 6], 'wxyz', null(), null(), 40), 'cd', 60)
type(c_t), parameter :: pd = c_t(pb, 50)
contains
subroutine check_a(a, arr, s, x)
  type(a_t), intent(in) :: a
  integer, intent(in) :: arr(3), x
  character(len=*), intent(in) :: s
  if (any(a%arr /= arr)) error stop
  if (a%s /= s) error stop
  if (allocated(a%al)) error stop
  if (associated(a%p)) error stop
  if (a%x /= x) error stop
end subroutine

subroutine check_locals()
  type(a_t) :: la = pa
  type(b_t) :: lb = pb
  type(b_t) :: lc = pc
  type(c_t) :: ld = pd
  call check_a(la, [7, 8, 9], 'qq  ', 10)
  call check_a(lb%a, [7, 8, 9], 'qq  ', 10)
  if (lb%t /= 'ab ' .or. lb%y /= 30) error stop
  call check_a(lc%a, [4, 5, 6], 'wxyz', 40)
  if (lc%t /= 'cd ' .or. lc%y /= 60) error stop
  call check_a(ld%b%a, [7, 8, 9], 'qq  ', 10)
  if (ld%b%t /= 'ab ' .or. ld%b%y /= 30 .or. ld%z /= 50) error stop
end subroutine

subroutine check_assignment()
  type(b_t) :: lb
  type(c_t) :: ld
  lb = pb
  call check_a(lb%a, [7, 8, 9], 'qq  ', 10)
  if (lb%t /= 'ab ' .or. lb%y /= 30) error stop
  lb = pc
  call check_a(lb%a, [4, 5, 6], 'wxyz', 40)
  if (lb%t /= 'cd ' .or. lb%y /= 60) error stop
  ld = pd
  call check_a(ld%b%a, [7, 8, 9], 'qq  ', 10)
  if (ld%b%t /= 'ab ' .or. ld%b%y /= 30 .or. ld%z /= 50) error stop
end subroutine
end module

program derived_type_nested_parameter_init_03
use derived_type_nested_parameter_init_03_m
implicit none
type(b_t) :: lp = pb
call check_a(lp%a, [7, 8, 9], 'qq  ', 10)
if (lp%t /= 'ab ' .or. lp%y /= 30) error stop
call check_locals()
call check_assignment()
print *, "ok"
end program
