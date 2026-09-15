module derived_type_nested_parameter_init_04_m
implicit none
integer :: nfin = 0
type :: a_t
  integer :: x = 1
end type
type :: b_t
  type(a_t) :: a
  integer :: y = 3
end type
type :: f_t
  integer :: x = 1
contains
  final :: fin
end type
type :: g_t
  type(f_t) :: f
  integer :: y = 3
end type
type :: n_t
  integer, allocatable :: al(:)
  integer, pointer :: p => null()
  integer :: x = 1
end type
type :: m_t
  type(n_t) :: n
  integer :: y = 3
end type
type :: c_t
  character(len=8) :: name = 'dflt'
  integer :: n = 5
end type
type :: d_t
  type(c_t) :: c
  integer :: k(8) = 0
end type
type(b_t), parameter :: pb = b_t(a_t(10), 30)
type(d_t), parameter :: pd = d_t(c_t('x', 7), 2)
type(g_t), parameter :: pg = g_t(f_t(10), 30)
type(m_t), parameter :: pm = m_t(n_t(null(), null(), 10), 30)
contains
subroutine fin(this)
  type(f_t), intent(inout) :: this
  nfin = nfin + 1
end subroutine

subroutine check(b)
  type(b_t), intent(in) :: b
  if (b%a%x /= 10 .or. b%y /= 30) error stop
end subroutine

subroutine check_targets()
  type(b_t) :: lb
  type(b_t), allocatable :: xa
  type(b_t) :: arr(2)
  class(b_t), allocatable :: xc
  type(b_t), pointer :: xp
  type(b_t) :: larr(2) = pb
  lb = pb
  call check(lb)
  xa = pb
  call check(xa)
  arr = pb
  call check(arr(1))
  call check(arr(2))
  xc = pb
  call check(xc)
  allocate(xp)
  xp = pb
  call check(xp)
  deallocate(xp)
  call check(larr(1))
  call check(larr(2))
end subroutine

subroutine assign_final()
  type(g_t) :: lg
  lg = pg
  if (lg%f%x /= 10 .or. lg%y /= 30) error stop
end subroutine

subroutine assign_null_components()
  type(m_t) :: lm
  integer, target :: t
  allocate(lm%n%al(3))
  lm%n%p => t
  lm = pm
  if (allocated(lm%n%al)) error stop
  if (associated(lm%n%p)) error stop
  if (lm%n%x /= 10 .or. lm%y /= 30) error stop
end subroutine

subroutine assign_in_loop()
  type(d_t) :: ld
  integer :: i, total
  total = 0
  do i = 1, 1000000
    ld = pd
    total = total + ld%c%n + ld%k(8)
  end do
  if (total /= 9000000) error stop
  if (ld%c%name /= 'x') error stop
end subroutine
end module

program derived_type_nested_parameter_init_04
use derived_type_nested_parameter_init_04_m
implicit none
call check_targets()
nfin = 0
call assign_final()
if (nfin /= 2) error stop
call assign_null_components()
call assign_in_loop()
print *, "ok"
end program
